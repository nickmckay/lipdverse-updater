#' Compilation versioning
#'
#' A compilation version is `publication_dataset_metadata`, e.g. `1_0_3`:
#'
#' \describe{
#'   \item{`publication`}{Bumped by hand, when the compilation is published.}
#'   \item{`dataset`}{Bumped when the set of datasets changed. Resets metadata.}
#'   \item{`metadata`}{Bumped when the datasets are the same but their content
#'     changed.}
#' }
#'
#' Replaces `tickVersion()` (`lipdverseR/R/nightlyUpdateDrake.R:110`), which read
#' a Google Sheet inline and so could not be tested, and which decided the bump
#' with `all(lastUdsn == thisUdsn)` on two sorted character vectors. `==`
#' recycles when the lengths differ, so adding datasets could compare a name
#' against the wrong name and, when the recycled comparison happened to hold,
#' tick metadata for a run that had actually changed the dataset set.
#'
#' Here the comparison is on sets, the function is pure, and the ledger is a
#' git-tracked CSV rather than a sheet a network failure can corrupt.
#'
#' @name versioning
NULL

#' Parse and render version strings
#' @param v A version string like `"1_0_3"`.
#' @return An integer vector of `publication`, `dataset`, `metadata`.
#' @export
lv_version_parse <- function(v) {
  if (length(v) != 1 || is.na(v) || !grepl("^[0-9]+_[0-9]+_[0-9]+$", v)) {
    cli::cli_abort("{.val {v}} is not a version of the form {.val publication_dataset_metadata}.",
                   class = "lv_error_version")
  }
  p <- as.integer(strsplit(v, "_", fixed = TRUE)[[1]])
  c(publication = p[1], dataset = p[2], metadata = p[3])
}

#' @rdname lv_version_parse
#' @param publication,dataset,metadata Components.
#' @export
lv_version_string <- function(publication, dataset, metadata) {
  paste(as.integer(publication), as.integer(dataset), as.integer(metadata), sep = "_")
}

#' Decide the next version
#'
#' @param prev Previous version string, or `NULL` for a compilation's first.
#' @param before Dataset identifiers in the previous version.
#' @param now Dataset identifiers now.
#' @param publish Bump the publication component and reset the rest.
#' @return An `lv_version`.
#' @export
lv_tick_version <- function(prev, before, now, publish = FALSE) {
  before <- unique(stats::na.omit(as.character(before)))
  now    <- unique(stats::na.omit(as.character(now)))
  added   <- setdiff(now, before)
  removed <- setdiff(before, now)
  # Sets, not element-wise: the vectors are different lengths precisely when
  # the answer matters most.
  same <- length(added) == 0 && length(removed) == 0

  # A compilation that has never been versioned starts unpublished. Every
  # compilation in LiPDverse begins at 0_0_1 and the publication component stays
  # 0 until it is actually published -- hydroclimate2k is at 0_4_0 after four
  # rounds of dataset changes. Defaulting to 1 would claim a publication that
  # has not happened.
  v <- if (is.null(prev) || (length(prev) == 1 && is.na(prev))) {
    c(publication = 0L, dataset = 0L, metadata = 0L)
  } else {
    lv_version_parse(prev)
  }
  first <- is.null(prev) || (length(prev) == 1 && is.na(prev))

  if (publish) {
    v <- c(publication = v[["publication"]] + 1L, dataset = 0L, metadata = 0L)
    reason <- "published"
  } else if (first) {
    v[["metadata"]] <- 1L
    reason <- "first version"
  } else if (same) {
    v[["metadata"]] <- v[["metadata"]] + 1L
    reason <- "metadata only: the dataset set is unchanged"
  } else {
    v[["dataset"]] <- v[["dataset"]] + 1L
    v[["metadata"]] <- 0L
    reason <- sprintf("dataset set changed: %d added, %d removed",
                      length(added), length(removed))
  }

  structure(list(
    version = lv_version_string(v[["publication"]], v[["dataset"]], v[["metadata"]]),
    previous = if (first) NA_character_ else prev,
    publication = v[["publication"]], dataset = v[["dataset"]], metadata = v[["metadata"]],
    reason = reason, n_datasets = length(now),
    datasets = sort(now), added = added, removed = removed,
    # The set, not its size: two runs can both hold 700 datasets and not be the
    # same 700, and the ledger has to be able to say so.
    dataset_set_hash = lv_dataset_set_hash(now)
  ), class = "lv_version")
}

#' Hash a dataset set, order-independently
#' @param ids Dataset identifiers.
#' @export
lv_dataset_set_hash <- function(ids) {
  ids <- sort(unique(stats::na.omit(as.character(ids))))
  if (!length(ids)) return(NA_character_)
  digest::digest(paste(ids, collapse = "\n"), algo = "md5", serialize = FALSE)
}

#' @export
print.lv_version <- function(x, ...) {
  cli::cli_h3("lv_version {x$version}")
  cli::cli_bullets(c(
    "*" = "from {if (is.na(x$previous)) 'nothing' else x$previous}: {x$reason}",
    "*" = "{x$n_datasets} dataset{?s} (+{length(x$added)}, -{length(x$removed)})"
  ))
  invisible(x)
}

# ---- the ledger ------------------------------------------------------------

LV_VERSION_COLS <- c("compilation", "version", "publication", "dataset", "metadata",
                     "created_at", "run_id", "reason", "n_datasets", "n_added", "n_removed",
                     "dataset_set_hash", "db_fingerprint", "qc_state_hash",
                     "lipdr_version", "updater_version", "notes")

#' The version ledger
#'
#' Append-only and git-tracked, replacing the pipe-joined `dsns` mega-string
#' `finalize()` wrote into a Google Sheet.
#'
#' @param store A `qc_store`.
#' @return A tibble.
#' @export
lv_versions <- function(store = qc_store()) {
  p <- fs::path(store$path, "versions.csv")
  if (!fs::file_exists(p)) {
    return(stats::setNames(
      tibble::as_tibble(rep(list(character()), length(LV_VERSION_COLS)), .name_repair = "minimal"),
      LV_VERSION_COLS))
  }
  readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                  na = "", progress = FALSE)
}

#' @rdname lv_versions
#' @param compilation Compilation name.
#' @export
lv_version_current <- function(store, compilation) {
  x <- lv_versions(store)
  x <- x[x$compilation == compilation, , drop = FALSE]
  if (!nrow(x)) return(NULL)
  x$version[nrow(x)]
}

#' @rdname lv_versions
#' @param version An `lv_version` from [lv_tick_version()].
#' @param run_id Run that produced it.
#' @param ... Extra ledger columns (`db_fingerprint`, `qc_state_hash`, `notes`).
#' @export
lv_version_append <- function(store, compilation, version, run_id = lv_run_id(), ...) {
  stopifnot(inherits(version, "lv_version"))
  extra <- list(...)
  # `v`, not `version`: tibble() evaluates its arguments in order and exposes
  # the columns already built, so a later `version$publication` would resolve to
  # the character column named `version` rather than to this argument.
  v <- version
  row <- tibble::tibble(
    compilation = compilation, version = v$version,
    publication = as.character(v$publication),
    dataset = as.character(v$dataset),
    metadata = as.character(v$metadata),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    run_id = run_id, reason = v$reason,
    n_datasets = as.character(v$n_datasets),
    n_added = as.character(length(v$added)),
    n_removed = as.character(length(v$removed)),
    dataset_set_hash = v$dataset_set_hash,
    db_fingerprint = extra$db_fingerprint %||% NA_character_,
    qc_state_hash = extra$qc_state_hash %||% NA_character_,
    lipdr_version = as.character(utils::packageVersion("lipdR")),
    updater_version = as.character(utils::packageVersion("lipdverseUpdater")),
    notes = extra$notes %||% NA_character_)

  p <- fs::path(store$path, "versions.csv")
  fs::dir_create(fs::path_dir(p))
  out <- dplyr::bind_rows(lv_versions(store), row)
  readr::write_csv(out, p, na = "")

  # Which datasets were in which version: one row per membership, so
  # "what was in v1.0.3" is a filter rather than parsing a pipe-joined string.
  m <- fs::path(store$path, "version_datasets.csv")
  members <- tibble::tibble(compilation = compilation, version = v$version,
                            dataset = v$datasets)
  prior <- if (fs::file_exists(m)) {
    readr::read_csv(m, col_types = readr::cols(.default = readr::col_character()),
                    na = "", progress = FALSE)
  } else NULL
  readr::write_csv(dplyr::bind_rows(prior, members), m, na = "")

  invisible(row)
}
