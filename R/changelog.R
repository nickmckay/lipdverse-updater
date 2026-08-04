#' Per-run changelogs
#'
#' Every dataset carries a `changelog`: a list of entries, each recording a
#' version, a curator, a timestamp and the changes made. The corpus format is
#'
#' ```
#' list(version = "1.0.13", lastVersion = "1.0.12", curator = "nicholas",
#'      timestamp = "2025-11-26 21:11:37 UTC", notes = "...",
#'      changes = list(`Paleo Column metadata` = list(list("d18O (T1): units ..."))))
#' ```
#'
#' Two things are added here that `createChangelog()` did not record, and their
#' absence is why a cross-compilation overwrite could not be traced. 56% of
#' datasets belong to two or more compilations, and compilations share the
#' fields stored in the file, so the last compilation to run silently wins. An
#' entry saying only *what* changed cannot answer *which run did it*, so every
#' entry now carries `compilation` and `run_id`.
#'
#' Ordering is deterministic — sorted by path — so two runs over the same change
#' set produce byte-identical entries and a shadow diff stays readable.
#'
#' @name changelog
NULL

# The category names the corpus already uses, in rough order of how often they
# appear. Emitting anything else would fragment a vocabulary that curators and
# createMarkdownChangelog() already read.
LV_CHANGE_CATEGORY <- c(
  root        = "Base metadata",
  geo         = "Geographic metadata",
  pub         = "Publication metadata",
  column      = "Paleo Column metadata",
  interp      = "Paleo Interpretation metadata",
  calibration = "Paleo Calibration metadata",
  chron       = "Chron Column metadata",
  values      = "PaleoData values",
  membership  = "Base metadata"
)

#' Flatten a LiPD object to comparable cells
#'
#' One row per addressable value, with a stable `path` so two objects can be
#' compared by joining rather than by walking them in step.
#'
#' @param L A LiPD object.
#' @return A tibble of `path`, `category`, `tsid`, `variableName`, `field`, `value`.
#' @keywords internal
lv_changelog_cells <- function(L) {
  rows <- list()
  add <- function(path, category, field, value, tsid = NA_character_, vn = NA_character_) {
    v <- scalar_chr(value)
    if (length(v) != 1 || is.na(v) || !nzchar(v)) return(invisible())
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      path = path, category = category, tsid = tsid, variableName = vn,
      field = field, value = v)
  }

  for (nm in setdiff(names(L), c("paleoData", "chronData", "pub", "geo", "changelog", "@context"))) {
    if (is.list(L[[nm]])) next
    add(nm, LV_CHANGE_CATEGORY[["root"]], nm, L[[nm]])
  }
  for (nm in names(L$geo)) {
    if (is.list(L$geo[[nm]])) next
    add(paste0("geo_", nm), LV_CHANGE_CATEGORY[["geo"]], paste0("geo_", nm), L$geo[[nm]])
  }
  for (i in seq_along(L$pub)) {
    for (nm in names(L$pub[[i]])) {
      if (is.list(L$pub[[i]][[nm]])) next
      k <- paste0("pub", i, "_", nm)
      add(k, LV_CHANGE_CATEGORY[["pub"]], k, L$pub[[i]][[nm]])
    }
  }

  for (blk in c("paleoData", "chronData")) {
    cat_col <- if (blk == "paleoData") LV_CHANGE_CATEGORY[["column"]] else LV_CHANGE_CATEGORY[["chron"]]
    for (pi in seq_along(L[[blk]])) {
      for (ti in seq_along(L[[blk]][[pi]]$measurementTable)) {
        tb <- L[[blk]][[pi]]$measurementTable[[ti]]
        cols <- if (!is.null(tb$columns)) tb$columns else tb
        for (cn in seq_along(cols)) {
          cl <- cols[[cn]]
          if (!is.list(cl) || is.null(cl$variableName)) next
          tsid <- as_chr1(cl$TSid) %||% NA_character_
          vn <- as_chr1(cl$variableName) %||% NA_character_
          # Keyed by TSid, not by position: a column that moves within a table
          # is the same timeseries and must not read as a delete plus an add.
          base <- paste0(blk, ".", tsid %||% cn)

          for (nm in names(cl)) {
            if (nm == "interpretation" || nm == "calibration" || nm == "inCompilation") next
            if (is.list(cl[[nm]])) next
            add(paste0(base, ".", nm), cat_col, nm, cl[[nm]], tsid, vn)
          }
          for (nm in names(cl$calibration)) {
            if (is.list(cl$calibration[[nm]])) next
            add(paste0(base, ".calibration.", nm), LV_CHANGE_CATEGORY[["calibration"]],
                paste0("calibration_", nm), cl$calibration[[nm]], tsid, vn)
          }
          seen <- integer()
          for (it in cl$interpretation) {
            if (!is.list(it)) next
            sc <- scalar_chr(it$scope)
            sc <- if (length(sc) == 1 && !is.na(sc) && nzchar(sc)) tolower(sc) else ""
            n <- if (sc %in% names(seen)) seen[[sc]] + 1L else 1L
            seen[[sc]] <- n
            pre <- if (nzchar(sc)) paste0(sc, "Interpretation", n) else paste0("interpretation", n)
            for (nm in names(it)) {
              if (is.list(it[[nm]])) next
              add(paste0(base, ".", pre, "_", nm), LV_CHANGE_CATEGORY[["interp"]],
                  paste0(pre, "_", nm), it[[nm]], tsid, vn)
            }
          }
          for (e in cl$inCompilation) {
            n <- if (is.list(e)) as_chr1(e$compilationName) else as_chr1(e)
            if (is.null(n)) next
            add(paste0(base, ".inCompilation.", n), LV_CHANGE_CATEGORY[["membership"]],
                "inCompilation", n, tsid, vn)
          }
        }
      }
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(path = character(), category = character(), tsid = character(),
                          variableName = character(), field = character(), value = character()))
  }
  out <- purrr::list_rbind(rows)
  out[order(out$path), , drop = FALSE]
}

#' Diff two versions of a dataset
#'
#' @param old,new LiPD objects.
#' @param ignore Field names to disregard, e.g. values that change every run.
#' @return A tibble of `path`, `category`, `tsid`, `variableName`, `field`,
#'   `kind` (`added`, `removed`, `changed`), `from`, `to`.
#' @export
lv_changelog_diff <- function(old, new, ignore = lv_changelog_ignore()) {
  a <- lv_changelog_cells(old)
  b <- lv_changelog_cells(new)
  j <- dplyr::full_join(a, b, by = "path", suffix = c("_old", "_new"))

  coalesce_col <- function(x, y) ifelse(is.na(x), y, x)
  out <- tibble::tibble(
    path = j$path,
    category = coalesce_col(j$category_new, j$category_old),
    tsid = coalesce_col(j$tsid_new, j$tsid_old),
    variableName = coalesce_col(j$variableName_new, j$variableName_old),
    field = coalesce_col(j$field_new, j$field_old),
    from = j$value_old, to = j$value_new)

  out$kind <- dplyr::case_when(
    is.na(out$from) & !is.na(out$to) ~ "added",
    !is.na(out$from) & is.na(out$to) ~ "removed",
    # Compared the same way the merge compares, so a value rounded differently
    # by two writers is not reported as a change on every run.
    !values_equal(out$from, out$to)  ~ "changed",
    TRUE ~ "same")

  out <- out[out$kind != "same" & !out$field %in% ignore, , drop = FALSE]
  out[order(out$path), , drop = FALSE]
}

#' Fields that change on every run and say nothing
#' @export
lv_changelog_ignore <- function() {
  c("changelog", "tagMD5", "measurementTableMD5", "paleoMeasurementTableMD5",
    "dataMD5", "lipdverseLink", "datasetVersion", "googleMetadataWorksheet",
    "googleSpreadSheetKey", "googleWorkSheetKey")
}

#' Render a diff as a changelog entry
#'
#' @param diff From [lv_changelog_diff()].
#' @param version Version this entry records.
#' @param last_version The version it succeeds.
#' @param compilation Compilation whose run made the change.
#' @param run_id Run that made it.
#' @param curator Who ran it.
#' @param notes Free text.
#' @param timestamp Defaults to now, UTC.
#' @return A changelog entry, ready to append.
#' @export
lv_changelog_entry <- function(diff, version, last_version = NA_character_,
                               compilation = NA_character_, run_id = NA_character_,
                               curator = Sys.info()[["user"]], notes = NULL,
                               timestamp = Sys.time()) {
  sentence <- function(r) {
    who <- if (!is.na(r$tsid)) sprintf("%s (%s): ", r$variableName %||% "?", r$tsid) else ""
    switch(r$kind,
      added   = sprintf("%s%s: '%s' has been added", who, r$field, r$to),
      removed = sprintf("%s%s: '%s' has been removed", who, r$field, r$from),
      sprintf("%s%s: '%s' has been replaced by '%s'", who, r$field, r$from, r$to))
  }

  changes <- list()
  if (nrow(diff)) {
    # Ordered by category then path, so the same change set always renders the
    # same way and a diff of two runs is about the data, not the ordering.
    d <- diff[order(diff$category, diff$path), , drop = FALSE]
    for (cat in unique(d$category)) {
      rows <- d[d$category == cat, , drop = FALSE]
      changes[[cat]] <- lapply(seq_len(nrow(rows)), function(i) list(sentence(rows[i, ])))
    }
  }

  entry <- list(
    version = as.character(version),
    lastVersion = if (is.na(last_version)) NULL else as.character(last_version),
    curator = curator,
    timestamp = paste(format(timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "UTC"),
    # The two fields createChangelog() never recorded. Without them an entry
    # says what changed but not which run did it, and with 56% of datasets in
    # two or more compilations that is the question worth answering.
    compilation = if (is.na(compilation)) NULL else compilation,
    run_id = if (is.na(run_id)) NULL else run_id,
    notes = notes)
  entry <- entry[!vapply(entry, is.null, logical(1))]
  if (length(changes)) entry$changes <- changes
  entry
}

#' Append an entry to a dataset's changelog
#'
#' @param L A LiPD object.
#' @param entry From [lv_changelog_entry()].
#' @return `L`, with the entry appended.
#' @export
lv_changelog_append <- function(L, entry) {
  if (is.null(L$changelog)) L$changelog <- list()
  L$changelog[[length(L$changelog) + 1L]] <- entry
  L
}

#' The version a dataset's changelog last recorded
#'
#' Dataset versions in the corpus are dotted (`1.0.13`), unlike compilation
#' versions, which are underscored (`0_4_0`). They count different things and
#' are not interchangeable.
#'
#' @param L A LiPD object.
#' @return A version string, or `NA`.
#' @export
lv_changelog_last_version <- function(L) {
  if (is.null(L$changelog) || !length(L$changelog)) return(NA_character_)
  v <- vapply(L$changelog, function(e) as_chr1(e$version) %||% NA_character_, character(1))
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  as.character(max(numeric_version(v, strict = FALSE), na.rm = TRUE))
}

#' The next dataset version after a change
#'
#' Patch-level: a run that edits metadata takes `1.0.12` to `1.0.13`. Matches
#' what the corpus does.
#'
#' @param version Current version, or `NA` for a dataset with no changelog.
#' @return The next version.
#' @export
lv_changelog_next_version <- function(version) {
  if (is.na(version) || !nzchar(version)) return("1.0.0")
  p <- as.integer(strsplit(as.character(version), ".", fixed = TRUE)[[1]])
  p <- c(p, rep(0L, max(0, 3 - length(p))))[1:3]
  paste(p[1], p[2], p[3] + 1L, sep = ".")
}
