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
# CP1252 first, then macintosh. This corpus comes through Excel and Google
# Sheets, so cp1252 is the common mis-decoding -- and trying Mac Roman first
# produces confident nonsense: for iso2k's damaged per-mille, "a\u20ac\u00b0" reverses
# under Mac Roman to bytes 61 DB A1, which is *valid* UTF-8 for an Arabic
# character, so validUTF8() waved it through and the repair wrote "a\u06a1".
lv_detect_mojibake <- function(x, encodings = c("CP1252", "macintosh")) {
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

  # A repair that introduces a script the input never had is not a repair. The
  # text in this database is Latin, Greek and symbols; a reversal that yields
  # Arabic or CJK has found a byte sequence that merely happens to be valid.
  # Reported rather than silently dropped, because the cell *is* mis-decoded --
  # it just cannot be fixed by reversing, and a person has to read it.
  high <- function(s) !is.na(s) && grepl("[\u0590-\u1CFF\u2E80-\uFFFD]", s, perl = TRUE)
  implausible <- vapply(seq_along(rep), function(i) {
    !is.na(rep[i]) && high(rep[i]) && !high(x[i])
  }, logical(1))

  tibble::tibble(input = x, repaired = rep, is_mojibake = !is.na(rep), encoding = enc,
                 repairable = !is.na(rep) & !implausible)
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
      before = d$input[i], after = d$repaired[i], repairable = d$repairable[i])
  }
  if (!length(hits)) {
    cli::cli_alert_success("No mojibake in {.val {tab}}.")
    return(invisible(NULL))
  }
  hits <- dplyr::bind_rows(hits)

  # Mojibake that has since been damaged further cannot be undone. iso2k's sheet
  # holds "2.6a\u20ac\u00b0": the correct mis-decoding of a per-mille sign is
  # "\u00e2\u20ac\u00b0", and the \u00e2 has already been flattened to a plain a, so no reversal
  # recovers it. Left alone and reported, rather than written over with whatever
  # byte sequence happens to decode.
  reversible <- hits$repairable %in% TRUE
  if (any(!reversible)) {
    cli::cli_alert_warning(
      "{sum(!reversible)} cell{?s} carry mojibake that cannot be reversed and {?is/are} left alone.")
    print(as.data.frame(hits[!reversible, c("column", "row", "before")]), right = FALSE)
    hits <- hits[reversible, , drop = FALSE]
    if (!nrow(hits)) {
      cli::cli_alert_info("Nothing left to repair automatically.")
      return(invisible(NULL))
    }
  }

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
sheet_write_cells.lv_sheet_google <- function(backend, id, tab, cells, values,
                                              chunk = 500L) {
  sheet_auth(backend)
  if (!length(cells)) return(invisible(0L))

  # One request per cell hits the per-minute write quota and then crawls behind
  # exponential backoff: 485 cells took minutes and four 429s. The values
  # batchUpdate endpoint takes many disjoint ranges in a single call, so a whole
  # patch is one request.
  #
  # reformat is not a concern here: writing values to a range leaves the cell's
  # formatting alone, which is the whole reason a patch is preferable to a
  # rewrite.
  val <- as.character(values)
  val[is.na(val)] <- ""
  n <- length(cells)
  for (i in seq(1, n, by = chunk)) {
    j <- i:min(i + chunk - 1L, n)
    data <- lapply(j, function(k) list(
      range = paste0("'", tab, "'!", cells[k]),
      majorDimension = "ROWS",
      values = list(list(val[k]))))
    req <- googlesheets4::request_generate(
      "sheets.spreadsheets.values.batchUpdate",
      params = list(spreadsheetId = id, data = data,
                    valueInputOption = "USER_ENTERED"))
    googlesheets4::request_make(req)
  }
  invisible(n)
}

#' @rdname lv_col_letter
#' @param letter A spreadsheet column letter.
#' @export
lv_col_index <- function(letter) {
  chars <- utf8ToInt(toupper(letter)) - utf8ToInt("A") + 1L
  Reduce(function(a, b) a * 26L + b, chars, accumulate = FALSE)
}
