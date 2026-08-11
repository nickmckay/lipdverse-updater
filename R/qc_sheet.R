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

#' Write values into a tab without disturbing its formatting
#'
#' `write_sheet()` clears the worksheet and rewrites it, which discards the
#' colour coding compilation leads navigate by -- 213 of the 222 cells in the
#' first three rows of the hydroclimate2k QC tab carry a background colour.
#' `range_write(reformat = FALSE)` overwrites the values in place and leaves the
#' formatting alone.
#'
#' It also does not shrink a sheet, so a frame with fewer rows than the tab
#' would leave stale rows below it. That case falls back to the clearing write,
#' because stale data is worse than lost colour.
#'
#' @param backend A backend.
#' @param id Sheet id.
#' @param tab Tab name.
#' @param x A data frame.
#' @export
sheet_write_values <- function(backend, id, tab, x) UseMethod("sheet_write_values")

#' @export
sheet_write_values.lv_sheet_local <- function(backend, id, tab, x) sheet_write(backend, id, tab, x)

#' @export
sheet_write_values.lv_sheet_google <- function(backend, id, tab, x) {
  sheet_auth(backend)
  cur <- tryCatch(sheet_read(backend, id, tab), error = function(e) NULL)
  shrinks <- !is.null(cur) && (nrow(x) < nrow(cur) || ncol(x) < ncol(cur))
  if (shrinks) {
    cli::cli_alert_warning(
      "New tab is smaller than the existing one; clearing and rewriting, which drops formatting.")
    return(sheet_write(backend, id, tab, x))
  }
  with_retry(googlesheets4::range_write(id, data = x, sheet = tab, range = "A1",
                                        col_names = TRUE, reformat = FALSE),
             paste("range_write", id, tab))
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
  } else {
    wide <- wide[, c("TSid", lv_sheet_column_order(setdiff(names(wide), "TSid"), registry)),
                 drop = FALSE]
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
                          registry = lv_qc_fields(), dry_run = TRUE,
                          add_columns = FALSE, add_rows = TRUE) {
  mode <- match.arg(mode)
  current <- tryCatch(qc_sheet_pull(backend, id, tab, registry), error = function(e) qc_cells_empty())
  delta <- qc_diff_to_events(current, cells, source = "sheet")

  receipt <- list(id = id, tab = tab, mode = mode, dry_run = dry_run,
                  n_cells = nrow(cells), n_changed = nrow(delta),
                  changed = delta[, c("tsid", "field", "old_value", "new_value")])

  if (dry_run || nrow(delta) == 0) return(invisible(receipt))

  template <- tryCatch(sheet_read(backend, id, tab), error = function(e) NULL)

  # A patch writes only the cells that changed, addressed by (TSid, column), and
  # leaves everything else -- including the colour coding the leads navigate by,
  # which a rewrite discards. It cannot add or remove rows, so the caller asks
  # for `full` when the row set changes.
  #
  # `mode` used to be accepted, recorded in the receipt, and ignored, so every
  # push was a rewrite whatever was asked for.
  if (identical(mode, "patch") && !is.null(template)) {
    key <- intersect(c("TSid", "tsid"), names(template))[1]
    if (is.na(key)) {
      cli::cli_abort("Cannot patch {.val {tab}}: no TSid column to address rows by.",
                     class = "lv_error_sheet")
    }
    # A patch only touches the (TSid, field) cells this run actually carries.
    # The diff reports everything else on the sheet as a deletion, because the
    # state is partial by row and by field alike: hydroclimate2k's state covers
    # 4,849 of the sheet's 7,525 rows, and only the fields in the registry.
    # Patching that wholesale would blank the 2,676 axis rows meant to be left
    # for deliberate removal, along with every column the run does not track.
    managed <- paste(delta$tsid, delta$field, sep = "\r") %in%
      paste(cells$tsid, cells$field, sep = "\r")
    if (any(!managed)) {
      receipt$unmanaged_cells <- sum(!managed)
      cli::cli_alert_info(
        "{sum(!managed)} sheet cell{?s} are outside this run and left untouched.")
    }
    delta <- delta[managed, , drop = FALSE]
    if (!nrow(delta)) {
      cli::cli_alert_success("Nothing to patch.")
      receipt$n_written <- 0L
      return(invisible(receipt))
    }
    disp <- lv_display_field(delta$field, registry)
    row <- match(delta$tsid, template[[key]])
    col <- match(disp, names(template))
    ok <- !is.na(row) & !is.na(col)
    if (any(!ok)) {
      # A cell with nowhere to go is skipped, not written somewhere else.
      cli::cli_alert_warning(
        "{sum(!ok)} change{?s} have no cell in {.val {tab}} and are not written.")
      receipt$skipped_cells <- delta[!ok, c("tsid", "field")]
    }
    if (any(ok)) {
      addr <- paste0(lv_col_letter(col[ok]), row[ok] + 1L)   # +1 for the header
      # A curator's clear writes an empty cell, not the word NA.
      val <- delta$new_value[ok]
      val[is.na(val)] <- ""
      sheet_write_cells(backend, id, tab, addr, val)
    }
    receipt$n_written <- sum(ok)

    # A timeseries with no row yet is appended, not skipped. Appending leaves
    # every existing row and its formatting untouched, which is the whole reason
    # to patch; the new rows arrive at the bottom, unformatted and out of the
    # tab's grouping, because the alternative is a curator never seeing them.
    fresh <- setdiff(unique(cells$tsid), template[[key]])
    if (add_rows && length(fresh)) {
      w <- qc_cells_to_sheet(cells[cells$tsid %in% fresh, , drop = FALSE], registry)
      out <- tibble::tibble(.rows = nrow(w))
      for (nm in names(template)) {
        out[[nm]] <- if (nm %in% names(w)) as.character(w[[nm]]) else NA_character_
      }
      if (key %in% names(w)) out[[key]] <- as.character(w[[key]])
      sheet_append(backend, id, tab, out)
      receipt$n_appended <- nrow(out)
      cli::cli_alert_success("Appended {nrow(out)} new row{?s} to {.val {tab}}.")
    } else {
      receipt$n_appended <- 0L
      if (length(fresh)) {
        receipt$rows_not_added <- fresh
        cli::cli_alert_info("{length(fresh)} new row{?s} not added ({.code add_rows = FALSE}).")
      }
    }
    cli::cli_alert_success("Patched {sum(ok)} cell{?s} in {.val {tab}}.")
    return(invisible(receipt))
  }

  wide <- qc_cells_to_sheet(cells, registry, template)

  # Columns are added by a curator, never by a run. Adding one silently commits
  # a compilation to a field nobody asked for, and the sheet is where that
  # decision belongs: to start curating a field, add a blank column for it.
  #
  # The block is written positionally from A1, so it must keep the sheet's exact
  # column set and order. Simply dropping the surplus columns would leave a
  # narrower block and shift every column after the first gap onto its
  # neighbour's data.
  if (!is.null(template) && !add_columns) {
    extra <- setdiff(names(wide), names(template))
    receipt$skipped_fields <- extra
    if (length(extra)) {
      cli::cli_alert_warning(
        "{length(extra)} field{?s} have no column in {.val {tab}}, so {?it is/they are} not written: {.val {utils::head(extra, 8)}}")
      cli::cli_alert_info("Add a blank column named for the field to start curating it.")
    }
    key <- intersect(c("TSid", "tsid"), names(template))[1]
    out <- tibble::tibble(.rows = nrow(wide))
    for (nm in names(template)) {
      out[[nm]] <- if (nm %in% names(wide)) wide[[nm]]
                   # A column this run has nothing for keeps what the sheet
                   # already holds, rather than being blanked.
                   else if (!is.na(key) && key %in% names(wide))
                     template[[nm]][match(wide[[key]], template[[key]])]
                   else NA
    }
    wide <- out
  }
  # Values only: the QC tab's colour coding is how leads find their way around
  # it, and rewriting the worksheet would discard it.
  sheet_write_values(backend, id, tab, wide)
  invisible(receipt)
}

#' Order sheet columns thematically
#'
#' Alphabetical order is diffable and unreadable. This is the grouping the
#' hydroclimate2k sheet already uses -- identity, archive, publication,
#' geography, chronology, measurement, then each interpretation scope, then
#' compilation and provenance -- taken from the registry so the layout is data
#' rather than code.
#'
#' @param field Display names, without TSid.
#' @param registry Field registry.
#' @return `field`, reordered.
#' @export
lv_sheet_column_order <- function(field, registry = lv_qc_fields()) {
  canon <- lv_canonical_field(field, registry)
  i <- match(canon, registry$qc_name)
  ord <- registry$group_order[i]
  # Anything the registry does not place goes last rather than first, so an
  # unrecognised column never displaces the identity columns.
  ord[is.na(ord)] <- max(registry$group_order, na.rm = TRUE) + 1L
  field[order(ord, field)]
}

#' Colour the header of each thematic group
#'
#' hydroclimate2k's sheet is colour-coded by hand and leads navigate by it; a
#' generated sheet that drops the colour is harder to use than the one it
#' replaces. Applied to the header row only, so no curator's own cell colouring
#' is disturbed.
#'
#' @param backend A sheet backend.
#' @param id Sheet id.
#' @param tab Tab name.
#' @param registry Field registry.
#' @param dry_run Report the ranges without writing.
#' @return A tibble of group, columns and colour, invisibly.
#' @export
qc_sheet_colour_groups <- function(backend, id, tab = "QC", registry = lv_qc_fields(),
                                   dry_run = TRUE) {
  hdr <- names(sheet_read(backend, id, tab))
  canon <- lv_canonical_field(hdr, registry)
  grp <- registry$group[match(canon, registry$qc_name)]
  grp[is.na(grp)] <- "other"

  pal <- c(identity = "#D9D2E9", archive = "#D9EAD3", publication = "#FFF2CC",
           geography = "#D0E0E3", chronology = "#CFE2F3", measurement = "#F4CCCC",
           interpretation_climate = "#FCE5CD", interpretation_environment = "#D9EAD3",
           interpretation_isotope = "#EAD1DC", interpretation_other = "#EFEFEF",
           calibration = "#FFF2CC", compilation = "#C9DAF8", provenance = "#EFEFEF",
           other = "#FFFFFF")

  # Contiguous runs of one group become one range, so a 75-column sheet is a
  # dozen requests rather than 75.
  r <- rle(grp)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  out <- tibble::tibble(group = r$values, from = starts, to = ends,
                        colour = unname(pal[r$values]))
  out$range <- sprintf("%s!%s1:%s1", tab, lv_col_letter(out$from), lv_col_letter(out$to))

  if (!dry_run) {
    for (i in seq_len(nrow(out))) {
      googlesheets4::range_flood(id, sheet = tab,
                                 range = sprintf("%s1:%s1", lv_col_letter(out$from[i]),
                                                 lv_col_letter(out$to[i])),
                                 cell = googlesheets4::cell_limits(),
                                 reformat = TRUE)
    }
  }
  invisible(out)
}

#' Spreadsheet column letter for a 1-based index
#' @param i Column index.
#' @export
lv_col_letter <- function(i) {
  vapply(i, function(k) {
    s <- ""
    while (k > 0) {
      r <- (k - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      k <- (k - 1L) %/% 26L
    }
    s
  }, character(1))
}

#' Append rows to a sheet tab
#'
#' Appends rather than rewriting, so existing rows and their formatting are
#' untouched. A full rewrite of `datasetsInCompilation` would put 7,000 rows of
#' other people's curation at risk to add ninety.
#'
#' @param backend From [sheet_backend_google()] or [sheet_backend_local()].
#' @param id Sheet id.
#' @param tab Tab name.
#' @param x Data frame whose columns match the tab's.
#' @export
sheet_append <- function(backend, id, tab, x) UseMethod("sheet_append")

#' @export
sheet_append.lv_sheet_local <- function(backend, id, tab, x) {
  cur <- sheet_read(backend, id, tab)
  sheet_write(backend, id, tab, dplyr::bind_rows(cur, x))
}

#' @export
sheet_append.lv_sheet_google <- function(backend, id, tab, x) {
  sheet_auth(backend)
  googlesheets4::sheet_append(id, x, sheet = tab)
  invisible(nrow(x))
}

#' The name a compilation's QC sheet should carry
#'
#' The sheet's title is where most people read the version, so a title left
#' behind quietly misreports which version everyone is editing. hydroclimate2k
#' sat at "v.0_4_0" while the ledger had moved to 0_5_0.
#'
#' Derived by substituting the version into the existing title rather than
#' rebuilding it, so anything else in there survives. Titles vary more than they
#' look: some carry a compilation alias, some a note.
#'
#' @param current The sheet's current title.
#' @param version The version it should name, e.g. `"0_5_0"`.
#' @param compilation Used only when `current` carries no version at all.
#' @return The title it should have.
#' @export
lv_qc_sheet_name <- function(current, version, compilation = NULL) {
  pat <- "v\\.?[0-9]+_[0-9]+_[0-9]+"
  if (grepl(pat, current)) return(sub(pat, paste0("v.", version), current))
  # No version in the title: append one rather than inventing a whole new title.
  if (nzchar(trimws(current))) return(paste0(trimws(current), " v.", version))
  paste0(compilation %||% "compilation", " v.", version, " QC sheet")
}

#' Rename a QC sheet to match its version
#'
#' Renames the spreadsheet document, not a tab, which is a Drive operation rather
#' than a Sheets one. Refuses to run if the title already names the version, so
#' it is safe to call on every run.
#'
#' @param cfg From [lv_config()].
#' @param version Version to name.
#' @param dry_run Report without renaming. Defaults to `TRUE`.
#' @return The new title, invisibly.
#' @export
lv_rename_qc_sheet <- function(cfg, version, dry_run = TRUE) {
  for (p in c("googlesheets4", "googledrive")) {
    if (!requireNamespace(p, quietly = TRUE)) cli::cli_abort("{.pkg {p}} is required.")
  }
  current <- googlesheets4::gs4_get(cfg$qc_sheet_id)$name
  target <- lv_qc_sheet_name(current, version, cfg$compilation)

  if (identical(current, target)) {
    cli::cli_alert_success("Title already names {.val {version}}: {.val {current}}")
    return(invisible(current))
  }
  cli::cli_alert_info("{.val {current}}  ->  {.val {target}}")
  if (dry_run) {
    cli::cli_alert_info("Dry run. Not renamed.")
    return(invisible(target))
  }
  # Drive, not Sheets: googlesheets4 renames tabs, not documents.
  googledrive::drive_rename(googledrive::as_id(cfg$qc_sheet_id), name = target)
  cli::cli_alert_success("Renamed to {.val {target}}")
  invisible(target)
}
