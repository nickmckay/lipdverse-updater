#' Read and write vocabulary review files
#'
#' JSON is canonical. A review is not really a table: `datasets`, `candidates`
#' and the PaST matches are lists, the PaST matches are records with their own
#' fields, and every value carries two parallel sets of the same six fields, one
#' proposed and one decided. Flattened into CSV all of that becomes pipe-joined
#' strings and `proposed_`-prefixed columns, which is ambiguous exactly where it
#' matters: a `|` inside a rationale is indistinguishable from a separator.
#'
#' In JSON they are arrays and nested objects, and `null` means undecided rather
#' than an empty cell that might be an empty string.
#'
#' CSV is still read, so review files written before the switch are not
#' stranded, but it is never written.
#'
#' @name review_io
NULL

LV_REVIEW_SCHEMA <- "lipdverse-vocab-review/1"

# Columns that live under `proposed` and under `decision` in the JSON, and as
# `proposed_*` / bare columns in the tibble the rest of the package works with.
LV_REVIEW_PROPOSED <- c("decision", "map_to", "also_field", "also_value",
                        "past_name", "past_id", "confidence", "rationale")
LV_REVIEW_DECIDED  <- c("decision", "map_to", "also_field", "also_value",
                        "past_name", "past_id", "note")

#' @rdname review_io
#' @param path A `.json` (or legacy `.csv`) review file.
#' @return A tibble of one row per value, with `datasets`, `candidates`,
#'   `source_pdfs` and `past_candidates` as list columns, and a `meta` attribute.
#' @export
lv_review_read <- function(path) {
  path <- path.expand(path)
  if (!fs::file_exists(path)) cli::cli_abort("Review file not found: {.path {path}}")

  if (grepl("\\.csv$", path, ignore.case = TRUE)) return(lv_review_read_csv(path))

  j <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(j$schema, LV_REVIEW_SCHEMA)) {
    cli::cli_warn("{.path {path}} declares schema {.val {j$schema %||% 'none'}}, expected {.val {LV_REVIEW_SCHEMA}}.")
  }
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
  lst <- function(x) if (is.null(x)) character() else vapply(x, as.character, character(1))

  it <- j$items %||% list()
  r <- tibble::tibble(
    field = vapply(it, function(e) chr(e$field), character(1)),
    value = vapply(it, function(e) chr(e$value), character(1)),
    n     = vapply(it, function(e) as.integer(e$n %||% NA), integer(1)),
    example = vapply(it, function(e) chr(e$example), character(1)),
    datasets    = lapply(it, function(e) lst(e$datasets)),
    candidates  = lapply(it, function(e) lst(e$candidates)),
    source_pdfs = lapply(it, function(e) lst(e$source_pdfs)),
    past_candidates = lapply(it, function(e) {
      p <- e$past_candidates
      if (is.null(p) || !length(p)) return(tibble::tibble(
        pastId = character(), pastName = character(), rule = character(),
        definition = character()))
      tibble::tibble(
        pastId     = vapply(p, function(z) chr(z$pastId), character(1)),
        pastName   = vapply(p, function(z) chr(z$pastName), character(1)),
        rule       = vapply(p, function(z) chr(z$rule), character(1)),
        definition = vapply(p, function(z) chr(z$definition), character(1)))
    }))
  for (nm in LV_REVIEW_PROPOSED) {
    r[[paste0("proposed_", nm)]] <- vapply(it, function(e) chr(e$proposed[[nm]]), character(1))
  }
  for (nm in LV_REVIEW_DECIDED) {
    r[[nm]] <- vapply(it, function(e) chr(e$decision[[nm]]), character(1))
  }
  attr(r, "meta") <- j[setdiff(names(j), "items")]
  r
}

#' @rdname review_io
#' @param r A review tibble.
#' @param meta Named list written alongside the items. Defaults to whatever
#'   `r` carries, so a round trip preserves it.
#' @export
lv_review_write <- function(r, path, meta = attr(r, "meta")) {
  path <- path.expand(path)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    cli::cli_abort(c("Review files are written as JSON.",
                     i = "Use a {.path .json} path, or {.fn lv_review_export_csv} for a spreadsheet copy."))
  }
  nul <- function(x) if (length(x) != 1 || is.na(x) || !nzchar(x)) NULL else unbox_chr(x)
  getl <- function(x) if (is.null(x)) list() else as.list(unname(x))

  items <- lapply(seq_len(nrow(r)), function(i) {
    pc <- r$past_candidates[[i]]
    list(
      field = unbox_chr(r$field[i]),
      value = unbox_chr(r$value[i]),
      n = jsonlite::unbox(as.integer(r$n[i])),
      example = nul(r$example[i]),
      datasets = getl(r$datasets[[i]]),
      candidates = getl(r$candidates[[i]]),
      source_pdfs = getl(r$source_pdfs[[i]]),
      past_candidates = if (is.null(pc) || !nrow(pc)) list() else
        lapply(seq_len(nrow(pc)), function(k) list(
          pastId = unbox_chr(pc$pastId[k]), pastName = unbox_chr(pc$pastName[k]),
          rule = unbox_chr(pc$rule[k]), definition = nul(pc$definition[k]))),
      proposed = stats::setNames(
        lapply(LV_REVIEW_PROPOSED, function(nm) nul(r[[paste0("proposed_", nm)]][i])),
        LV_REVIEW_PROPOSED),
      decision = stats::setNames(
        lapply(LV_REVIEW_DECIDED, function(nm) nul(r[[nm]][i])), LV_REVIEW_DECIDED))
  })

  meta <- meta %||% list()
  out <- c(list(schema = unbox_chr(LV_REVIEW_SCHEMA)),
           lapply(meta[setdiff(names(meta), "schema")],
                  function(x) if (length(x) == 1 && !is.list(x)) jsonlite::unbox(x) else x),
           list(items = items))
  fs::dir_create(fs::path_dir(path))
  jsonlite::write_json(out, path, auto_unbox = FALSE, pretty = TRUE, null = "null")
  invisible(path)
}

unbox_chr <- function(x) jsonlite::unbox(as.character(x)[1])

#' @rdname review_io
#' @export
lv_review_export_csv <- function(r, path) {
  flat <- r
  for (nm in c("datasets", "candidates", "source_pdfs")) {
    flat[[nm]] <- vapply(r[[nm]], function(x) paste(x, collapse = " | "), character(1))
  }
  flat$past_candidates <- vapply(r$past_candidates, function(p) {
    if (!nrow(p)) return(NA_character_)
    paste(sprintf("%s (%s, %s)", p$pastName, p$pastId, p$rule), collapse = " | ")
  }, character(1))
  readr::write_csv(flat, path, na = "")
  invisible(path)
}

# Legacy reader. Splits the pipe-joined columns back into lists, which is
# lossy where a value itself contained a pipe -- one reason the format changed.
lv_review_read_csv <- function(path) {
  x <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       na = "", progress = FALSE)
  split1 <- function(v) lapply(v, function(s)
    if (is.na(s) || !nzchar(s)) character() else trimws(strsplit(s, "\\|")[[1]]))
  r <- tibble::tibble(
    field = x$field, value = x$value, n = as.integer(x$n),
    example = x$example %||% NA_character_,
    datasets = if ("datasets" %in% names(x)) split1(x$datasets) else vector("list", nrow(x)),
    candidates = if ("candidates" %in% names(x)) split1(x$candidates) else vector("list", nrow(x)),
    source_pdfs = if ("source_pdf" %in% names(x)) split1(x$source_pdf) else vector("list", nrow(x)))
  r$datasets[vapply(r$datasets, is.null, logical(1))] <- list(character())
  r$candidates[vapply(r$candidates, is.null, logical(1))] <- list(character())
  r$source_pdfs[vapply(r$source_pdfs, is.null, logical(1))] <- list(character())
  # past_candidates was "name (id, rule)" text; parse what we can.
  r$past_candidates <- lapply(x$past_candidates %||% rep(NA_character_, nrow(x)), function(s) {
    e <- tibble::tibble(pastId = character(), pastName = character(),
                        rule = character(), definition = character())
    if (is.na(s) || !nzchar(s)) return(e)
    parts <- trimws(strsplit(s, "\\|")[[1]])
    m <- regmatches(parts, regexec("^(.*) \\(([^,]+), ([^)]+)\\)$", parts))
    ok <- vapply(m, length, integer(1)) == 4
    if (!any(ok)) return(e)
    tibble::tibble(pastName = vapply(m[ok], `[`, character(1), 2),
                   pastId   = vapply(m[ok], `[`, character(1), 3),
                   rule     = vapply(m[ok], `[`, character(1), 4),
                   definition = NA_character_)[, c("pastId","pastName","rule","definition")]
  })
  for (nm in LV_REVIEW_PROPOSED) {
    k <- paste0("proposed_", nm)
    r[[k]] <- if (k %in% names(x)) x[[k]] else if (nm %in% c("confidence","rationale") && nm %in% names(x))
      x[[nm]] else NA_character_
  }
  for (nm in LV_REVIEW_DECIDED) r[[nm]] <- if (nm %in% names(x)) x[[nm]] else NA_character_
  attr(r, "meta") <- list(converted_from = basename(path))
  r
}

#' @rdname review_io
#' @export
lv_review_empty <- function() {
  r <- tibble::tibble(
    field = character(), value = character(), n = integer(), example = character(),
    datasets = list(), candidates = list(), source_pdfs = list(), past_candidates = list())
  for (nm in LV_REVIEW_PROPOSED) r[[paste0("proposed_", nm)]] <- character()
  for (nm in LV_REVIEW_DECIDED) r[[nm]] <- character()
  r
}

lv_now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' @rdname review_io
#' @param out Where to write the converted file. Defaults to the same path with
#'   a `.json` extension.
#' @export
lv_review_convert <- function(path, out = sub("\\.csv$", ".json", path)) {
  r <- lv_review_read(path)
  lv_review_write(r, out, meta = list(created_utc = lv_now_utc(),
                                      converted_from = basename(path)))
  cli::cli_alert_success("Converted {nrow(r)} value{?s} to {.path {out}}")
  invisible(out)
}
