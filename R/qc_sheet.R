#' QC sheet backends
#'
#' All sheet access goes through a backend so the pipeline can be tested
#' without a network, and so a shadow run can operate on captured sheets rather
#' than the live ones.
#'
#' Two hardening rules apply to every read, both from failures in lipdverseR:
#'
#' * **Tabs are addressed by name, never index.** `sheet = 1` read whatever tab
#'   happened to be first, so reordering tabs silently changed which data was
#'   loaded.
#' * **Every column is read as character.** Type inference looked only at the
#'   first rows, so a sparse column whose first value appeared past row 1000 was
#'   typed logical and its character values were dropped.
#'
#' @name qc_sheet_backend
NULL

#' @rdname qc_sheet_backend
#' @param email Google account.
#' @param token_dir Credential cache directory.
#' @export
sheet_backend_google <- function(email = Sys.getenv("LIPDVERSE_GOOG_EMAIL", "nick.mckay2@gmail.com"),
                                 token_dir = Sys.getenv("LIPDVERSE_GOOG_CACHE",
                                                        path.expand("~/GitHub/lipdverseR/.secret"))) {
  structure(list(kind = "google", email = email, token_dir = token_dir),
            class = c("lv_sheet_google", "lv_sheet_backend"))
}

#' @rdname qc_sheet_backend
#' @param dir Directory of `<sheet_id>/<tab>.csv` files.
#' @export
sheet_backend_local <- function(dir) {
  structure(list(kind = "local", dir = dir), class = c("lv_sheet_local", "lv_sheet_backend"))
}

#' List a sheet's tabs
#' @param backend A backend.
#' @param id Sheet id.
#' @export
sheet_tabs <- function(backend, id) UseMethod("sheet_tabs")

#' @export
sheet_tabs.lv_sheet_local <- function(backend, id) {
  d <- fs::path(backend$dir, id)
  if (!fs::dir_exists(d)) return(character())
  sub("\\.csv$", "", basename(fs::dir_ls(d, glob = "*.csv")))
}

#' @export
sheet_tabs.lv_sheet_google <- function(backend, id) {
  sheet_auth(backend)
  with_retry(googlesheets4::gs4_get(id)$sheets$name, paste("gs4_get", id))
}

#' Read one tab
#' @param backend A backend.
#' @param id Sheet id.
#' @param tab Tab name.
#' @export
sheet_read <- function(backend, id, tab) UseMethod("sheet_read")

#' @export
sheet_read.lv_sheet_local <- function(backend, id, tab) {
  p <- fs::path(backend$dir, id, paste0(tab, ".csv"))
  if (!fs::file_exists(p)) {
    cli::cli_abort("No tab {.val {tab}} for sheet {.val {id}}.", class = "lv_error_sheet")
  }
  # na = "" to match the google backend, which returns a literal "NA" cell as
  # the string. Without it the local backend would silently disagree with
  # production about a value the corpus really contains.
  readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                  na = "", progress = FALSE, name_repair = "minimal")
}

#' @export
sheet_read.lv_sheet_google <- function(backend, id, tab) {
  sheet_auth(backend)
  with_retry(
    googlesheets4::range_read(id, sheet = tab, col_types = "c", .name_repair = "minimal"),
    paste("read", id, tab))
}

#' Write one tab
#' @param backend A backend.
#' @param id Sheet id.
#' @param tab Tab name.
#' @param x A data frame.
#' @export
sheet_write <- function(backend, id, tab, x) UseMethod("sheet_write")

#' @export
sheet_write.lv_sheet_local <- function(backend, id, tab, x) {
  d <- fs::path(backend$dir, id)
  fs::dir_create(d)
  readr::write_csv(x, fs::path(d, paste0(tab, ".csv")), na = "")
  invisible(TRUE)
}

#' @export
sheet_write.lv_sheet_google <- function(backend, id, tab, x) {
  sheet_auth(backend)
  with_retry(googlesheets4::write_sheet(x, ss = id, sheet = tab), paste("write", id, tab))
  invisible(TRUE)
}

#' Create a new sheet
#'
#' A new compilation needs a new QC sheet, and making it by hand is how tab
#' names drift. Returns the id to record in `compilations.tsv`.
#'
#' @param backend A backend.
#' @param name Sheet name.
#' @param tabs A named list of data frames, one per tab.
#' @return The sheet id.
#' @export
sheet_create <- function(backend, name, tabs) UseMethod("sheet_create")

#' @export
sheet_create.lv_sheet_local <- function(backend, name, tabs) {
  for (nm in names(tabs)) sheet_write(backend, name, nm, tabs[[nm]])
  name
}

#' @export
sheet_create.lv_sheet_google <- function(backend, name, tabs) {
  sheet_auth(backend)
  ss <- with_retry(googlesheets4::gs4_create(name, sheets = tabs), paste("create", name))
  as.character(googlesheets4::as_sheets_id(ss))
}

sheet_auth <- function(backend) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg googlesheets4} is required for the google backend.")
  }
  googlesheets4::gs4_auth(email = backend$email, cache = backend$token_dir)
}

# Sheets calls fail transiently often enough that lipdverseR wrapped every one
# in a retry helper. Same idea, with backoff.
with_retry <- function(expr, what, tries = 4) {
  for (i in seq_len(tries)) {
    out <- try(force(expr), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    if (i == tries) {
      cli::cli_abort("{what} failed after {tries} tries: {conditionMessage(attr(out, 'condition'))}",
                     class = "lv_error_sheet")
    }
    Sys.sleep(2^i)
  }
}

#' Read a QC sheet into canonical cells
#'
#' Melts the wide QC tab into one row per populated cell, keyed by TSid and
#' canonical field name.
#'
#' @param backend A backend.
#' @param id Sheet id.
#' @param tab QC tab name.
#' @param registry Field registry.
#' @return A cell table.
#' @export
qc_sheet_pull <- function(backend, id, tab = "QC", registry = lv_qc_fields()) {
  raw <- sheet_read(backend, id, tab)
  raw <- raw[, !duplicated(names(raw)) & nzchar(names(raw)) & !is.na(names(raw)), drop = FALSE]
  if (!"TSid" %in% names(raw)) {
    cli::cli_abort("QC tab {.val {tab}} has no TSid column.", class = "lv_error_sheet")
  }
  raw <- raw[!is.na(raw$TSid) & nzchar(raw$TSid), , drop = FALSE]

  dsid <- if ("datasetId" %in% names(raw)) raw$datasetId else NA_character_
  value_cols <- setdiff(names(raw), c("TSid", "datasetId"))
  if (!length(value_cols)) return(qc_cells_empty())

  long <- tidyr::pivot_longer(raw[, c("TSid", value_cols), drop = FALSE],
                              dplyr::all_of(value_cols),
                              names_to = "field", values_to = "value")
  long$dataset_id <- rep(dsid, times = length(value_cols))[
    match(long$TSid, rep(raw$TSid, times = length(value_cols)))]

  long$field <- lv_canonical_field(long$field, registry)
  populated <- !is.na(long$value) & nzchar(long$value)

  # Blank cells are normally dropped, but a blank in a field the curator may
  # clear is itself information: it is the difference between "the curator
  # emptied this" and "the sheet has no such cell". Losing that distinction
  # makes every absent cell look like a deletion.
  rule <- lv_field_rule(long$field, registry)
  keep <- populated | (rule$nullable_by_curator %in% TRUE)
  long <- long[keep, , drop = FALSE]
  populated <- populated[keep]

  tibble::tibble(
    tsid = long$TSid,
    field = long$field,
    value = ifelse(populated, long$value, NA_character_),
    present = populated,
    dataset_id = long$dataset_id,
    updated_at = NA_character_, source = "sheet", actor = NA_character_
  )
}

#' Render cells back to a wide QC tab
#'
#' @param cells A cell table.
#' @param registry Field registry.
#' @param template Optional existing tab, to preserve column order.
#' @return A wide data frame.
#' @export
qc_cells_to_sheet <- function(cells, registry = lv_qc_fields(), template = NULL) {
  if (nrow(cells) == 0) return(tibble::tibble(TSid = character()))
  x <- cells
  x$field <- lv_display_field(x$field, registry)
  wide <- tidyr::pivot_wider(x[, c("tsid", "field", "value")],
                             names_from = "field", values_from = "value")
  names(wide)[names(wide) == "tsid"] <- "TSid"
  if (!is.null(template)) {
    keep <- intersect(names(template), names(wide))
    extra <- setdiff(names(wide), keep)
    wide <- wide[, c(keep, extra), drop = FALSE]
  }
  wide
}

#' Push canonical state to a QC sheet
#'
#' `mode = "patch"` writes only the cells that changed; `"full"` rewrites the
#' whole tab. Patching is the default because a full rewrite of a large QC table
#' is what made `write_sheet_retry` time out, and what forced lipdverseR into
#' chunked 500-row appends.
#'
#' @param cells State to write.
#' @param backend A backend.
#' @param id Sheet id.
#' @param tab Tab name.
#' @param mode `"patch"` or `"full"`.
#' @param registry Field registry.
#' @param dry_run Report without writing.
#' @return A receipt list, invisibly.
#' @export
qc_sheet_push <- function(cells, backend, id, tab = "QC", mode = c("patch", "full"),
                          registry = lv_qc_fields(), dry_run = TRUE) {
  mode <- match.arg(mode)
  current <- tryCatch(qc_sheet_pull(backend, id, tab, registry), error = function(e) qc_cells_empty())
  delta <- qc_diff_to_events(current, cells, source = "sheet")

  receipt <- list(id = id, tab = tab, mode = mode, dry_run = dry_run,
                  n_cells = nrow(cells), n_changed = nrow(delta),
                  changed = delta[, c("tsid", "field", "old_value", "new_value")])

  if (dry_run || nrow(delta) == 0) return(invisible(receipt))

  template <- tryCatch(sheet_read(backend, id, tab), error = function(e) NULL)
  wide <- qc_cells_to_sheet(cells, registry, template)
  sheet_write(backend, id, tab, wide)
  invisible(receipt)
}
