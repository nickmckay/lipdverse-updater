#' Detect and repair mojibake
#'
#' Mojibake here is one specific accident: UTF-8 bytes decoded as Mac OS Roman.
#' An en-dash, `e2 80 93`, becomes `‚Äì`; a delta, `ce b4`, becomes `Œ¥`. It
#' entered the LiPDverse QC sheets somewhere upstream and reaches the `.lpd`
#' files from there.
#'
#' Detection is by round trip rather than by looking for suspicious characters,
#' which matters because the suspicious characters are also ordinary ones:
#' `Ã`, `â` and `Â` appear in perfectly good Portuguese, French and Welsh. A
#' string is mojibake only if re-encoding it to Mac OS Roman yields bytes that
#' are *valid UTF-8 and decode to something different*. `São` fails that test,
#' because `ã` is a single Mac Roman byte that is not valid UTF-8 on its own, so
#' it is left alone. `Œ¥` passes, because those two bytes are `ce b4`.
#'
#' The repair is therefore exact rather than a guess, and reversible in
#' principle, though there is no reason to reverse it.
#'
#' Two encodings are tried, because both accidents occur: Mac OS Roman is what
#' the LiPDverse sheets carry (`Œ¥`, `‚Äì`), and Windows-1252 is the commoner
#' variety everywhere else (`Ã©`, `BÃ¼ntgen`). Mac Roman is tried first, since
#' that is what these sheets have, and the first encoding that round trips wins.
#'
#' @param x A character vector.
#' @param encodings Encodings to try, in order.
#' @return A tibble of `input`, `repaired`, `is_mojibake` and `encoding`.
#' @export
lv_detect_mojibake <- function(x, encodings = c("macintosh", "CP1252")) {
  x <- as.character(x)
  try_one <- function(s, enc) {
    raw <- tryCatch(iconv(s, from = "UTF-8", to = enc, toRaw = TRUE)[[1]],
                    error = function(e) NULL)
    if (is.null(raw) || anyNA(raw)) return(NA_character_)
    out <- tryCatch(rawToChar(raw), error = function(e) NULL)
    if (is.null(out)) return(NA_character_)
    Encoding(out) <- "UTF-8"
    if (!validUTF8(out) || identical(out, s)) return(NA_character_)
    out
  }
  res <- lapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(c(NA_character_, NA_character_))
    for (e in encodings) {
      r <- try_one(s, e)
      if (!is.na(r)) return(c(r, e))
    }
    c(NA_character_, NA_character_)
  })
  rep <- vapply(res, `[`, character(1), 1L)
  enc <- vapply(res, `[`, character(1), 2L)

  tibble::tibble(input = x, repaired = rep, is_mojibake = !is.na(rep), encoding = enc)
}

#' @rdname lv_detect_mojibake
#' @param cfg From [lv_config()].
#' @param backend From [sheet_backend_google()].
#' @param tab Tab to repair. Defaults to the QC tab.
#' @param dry_run Report without writing. Defaults to `TRUE`.
#' @return A tibble of the cells repaired (or that would be), invisibly.
#' @export
lv_repair_mojibake <- function(cfg, backend, tab = cfg$qc_tabs$qc, dry_run = TRUE) {
  raw <- sheet_read(backend, cfg$qc_sheet_id, tab)
  chr <- vapply(raw, is.character, logical(1))
  if (!any(chr)) {
    cli::cli_alert_success("Nothing to repair.")
    return(invisible(NULL))
  }

  hits <- list()
  for (nm in names(raw)[chr]) {
    d <- lv_detect_mojibake(raw[[nm]])
    if (!any(d$is_mojibake)) next
    i <- which(d$is_mojibake)
    hits[[length(hits) + 1L]] <- tibble::tibble(
      column = nm, col_index = match(nm, names(raw)), row = i,
      before = d$input[i], after = d$repaired[i])
  }
  if (!length(hits)) {
    cli::cli_alert_success("No mojibake in {.val {tab}}.")
    return(invisible(NULL))
  }
  hits <- dplyr::bind_rows(hits)
  # +1 for the header row: a value in data row n lives in spreadsheet row n + 1.
  hits$cell <- paste0(lv_col_letter(hits$col_index), hits$row + 1L)

  cli::cli_alert_info("{nrow(hits)} cell{?s} in {.val {tab}} across {length(unique(hits$column))} column{?s}.")
  print(dplyr::count(hits, column, name = "cells"), n = 20)

  if (dry_run) {
    cli::cli_alert_info("Dry run. Nothing written.")
    return(invisible(hits))
  }

  sheet_write_cells(backend, cfg$qc_sheet_id, tab, hits$cell, hits$after)
  cli::cli_alert_success("Repaired {nrow(hits)} cell{?s} in {.val {tab}}.")
  invisible(hits)
}

#' Write individual cells by address
#'
#' One cell at a time, leaving every other cell untouched. The QC tab is edited
#' by several people at once, so rewriting a whole values block to correct
#' nineteen cells would silently discard whatever anyone else changed while the
#' run was in progress.
#'
#' @param backend From [sheet_backend_google()].
#' @param id Sheet id.
#' @param tab Tab name.
#' @param cells Character vector of A1 addresses.
#' @param values Character vector of the same length.
#' @export
sheet_write_cells <- function(backend, id, tab, cells, values) {
  UseMethod("sheet_write_cells")
}

#' @export
sheet_write_cells.lv_sheet_local <- function(backend, id, tab, cells, values) {
  x <- sheet_read(backend, id, tab)
  for (k in seq_along(cells)) {
    m <- regmatches(cells[k], regexec("^([A-Z]+)([0-9]+)$", cells[k]))[[1]]
    col <- lv_col_index(m[2]); row <- as.integer(m[3]) - 1L
    x[[col]][row] <- values[k]
  }
  sheet_write(backend, id, tab, x)
  invisible(length(cells))
}

#' @export
sheet_write_cells.lv_sheet_google <- function(backend, id, tab, cells, values) {
  sheet_auth(backend)
  for (k in seq_along(cells)) {
    googlesheets4::range_write(
      id, data.frame(x = values[k]), sheet = tab, range = cells[k],
      col_names = FALSE, reformat = FALSE)
  }
  invisible(length(cells))
}

#' @rdname lv_col_letter
#' @param letter A spreadsheet column letter.
#' @export
lv_col_index <- function(letter) {
  chars <- utf8ToInt(toupper(letter)) - utf8ToInt("A") + 1L
  Reduce(function(a, b) a * 26L + b, chars, accumulate = FALSE)
}
