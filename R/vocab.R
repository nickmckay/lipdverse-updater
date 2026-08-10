#' The pinned LiPDverse vocabulary
#'
#' Read from `inst/extdata/vocab`, never from the network. `getStandardTables()`
#' in lipdverseR fetches `standardTables.RDS` over HTTP every time it is called,
#' so two runs a month apart are not comparable and an old run cannot be
#' reproduced at all. Refresh deliberately with `scripts/pin-vocabulary.R`.
#'
#' Each table is one row per `(lipdName, synonym)` pair: `archiveType` has 53
#' rows for 17 archive types.
#'
#' @param validate Check the tables before returning them.
#' @return A named list of tibbles, with a `pin` attribute.
#' @export
lv_vocab <- function(validate = TRUE) {
  d <- lv_extdata("vocab")
  f <- fs::dir_ls(d, glob = "*.csv")
  if (!length(f)) {
    cli::cli_abort("No pinned vocabulary in {.path {d}}. Run {.code scripts/pin-vocabulary.R --commit}.",
                   class = "lv_error_vocab")
  }
  out <- lapply(f, function(p) {
    readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                    na = "", progress = FALSE)
  })
  names(out) <- sub("\\.csv$", "", fs::path_file(f))

  meta <- fs::path(d, "vocab-pin.json")
  attr(out, "pin") <- if (fs::file_exists(meta)) jsonlite::read_json(meta)$pin else NA_character_
  if (validate) {
    # synonym is optional: paleoData_proxyGeneral lists definitions instead.
    for (k in names(out)) {
      if (!"lipdName" %in% names(out[[k]])) {
        cli::cli_abort("Vocabulary table {.val {k}} lacks lipdName.", class = "lv_error_vocab")
      }
    }
  }
  out
}

#' @rdname lv_vocab
#' @export
lv_vocab_pin <- function() attr(lv_vocab(validate = FALSE), "pin")

#' Standardize values against a controlled vocabulary
#'
#' A pure function over a character vector, so it can be tested and so a run can
#' report what it *would* change before changing it. `standardizeValue()` in
#' lipdverseR conflates detection with mutation, which is why that path resists
#' testing; and `standardizeLipdBatch()` reaches the values by round-tripping the
#' whole dataset through `as.lipdTsTibble()`/`as.lipd()`, which fabricates an
#' empty interpretation on every column that has fewer than the dataset's
#' maximum. Nothing here touches structure.
#'
#' Matching is tried in order, most exact first, and the rule is reported:
#'
#' \describe{
#'   \item{`canonical`}{already the standard name}
#'   \item{`synonym`}{an exact listed synonym}
#'   \item{`case`}{differs only in case}
#'   \item{`trim`}{differs only in surrounding whitespace}
#'   \item{`none`}{no match; the value is left alone and reported}
#' }
#'
#' @param x Character vector of values.
#' @param key Vocabulary key, e.g. `"archiveType"`.
#' @param vocab From [lv_vocab()].
#' @return A tibble of `input`, `value`, `matched`, `rule`.
#' @export
vocab_standardize <- function(x, key, vocab = lv_vocab()) {
  if (!key %in% names(vocab)) {
    cli::cli_abort("Unknown vocabulary key {.val {key}}. Known: {.val {names(vocab)}}",
                   class = "lv_error_vocab")
  }
  tb <- vocab[[key]]
  canon <- unique(stats::na.omit(tb$lipdName))
  syn <- if ("synonym" %in% names(tb)) tb$synonym else rep(NA_character_, nrow(tb))
  ok <- !is.na(syn) & nzchar(syn)
  syn_map <- stats::setNames(tb$lipdName[ok], syn[ok])

  x <- as.character(x)
  out <- tibble::tibble(input = x, value = x, matched = FALSE, rule = "none")
  have <- !is.na(x) & nzchar(x)

  hit <- have & x %in% canon
  out$matched[hit] <- TRUE; out$rule[hit] <- "canonical"

  todo <- have & !out$matched
  hit <- todo & x %in% names(syn_map)
  out$value[hit] <- unname(syn_map[x[hit]])
  out$matched[hit] <- TRUE; out$rule[hit] <- "synonym"

  # Case-insensitive, against both the canonical names and the synonyms.
  lower <- c(stats::setNames(canon, tolower(canon)),
             stats::setNames(unname(syn_map), tolower(names(syn_map))))
  lower <- lower[!duplicated(names(lower))]
  todo <- have & !out$matched
  hit <- todo & tolower(x) %in% names(lower)
  out$value[hit] <- unname(lower[tolower(x[hit])])
  out$matched[hit] <- TRUE; out$rule[hit] <- "case"

  todo <- have & !out$matched
  tr <- trimws(x)
  hit <- todo & tolower(tr) %in% names(lower)
  out$value[hit] <- unname(lower[tolower(tr[hit])])
  out$matched[hit] <- TRUE; out$rule[hit] <- "trim"

  out
}

#' Report values a vocabulary does not recognise
#'
#' Detection separated from mutation: this never changes anything.
#'
#' @param x Character vector.
#' @param key Vocabulary key.
#' @param vocab From [lv_vocab()].
#' @return An `lv_issues` tibble, one row per distinct unmatched value.
#' @export
vocab_check <- function(x, key, vocab = lv_vocab()) {
  r <- vocab_standardize(x, key, vocab)
  bad <- unique(r$input[!r$matched & !is.na(r$input) & nzchar(r$input)])
  if (!length(bad)) return(lv_issues_empty())
  lv_issues(check = "unknown_vocabulary", severity = "warn",
            message = sprintf("Not in the %s vocabulary.", key),
            field = key, value = bad)
}

#' Check the values a run is about to write against the vocabulary
#'
#' Vocabulary was only ever enforced at ingest. Nothing looked at values arriving
#' from a QC sheet, so `archiveType` drifted to `diatoms from lake core`, `w` and
#' `Pollen` without anything noticing — a curator asked whether archiveType was
#' a controlled vocabulary at all, which is a fair question when nothing enforces
#' it.
#'
#' Reports rather than blocks. A run that has already merged several hundred
#' datasets should not abort over one cell, and the cell is a curator's to
#' resolve, not a run's to overwrite. The unmatched values come back as issues
#' and go into a review file, the same shape [lv_vocab_review()] produces for
#' ingest, so the decisions land in the same store and are asked once.
#'
#' @param cells Cells about to be written, from the merge plan.
#' @param registry Field registry.
#' @param vocab From [lv_vocab_overlay()].
#' @param index From [lv_db_index()]; supplies the dataset each TSid belongs to.
#' @return An `lv_issues` tibble, one row per offending cell.
#' @export
lv_check_vocabulary <- function(cells, registry = lv_qc_fields(),
                                vocab = lv_vocab_overlay(), index = NULL) {
  if (!nrow(cells)) return(lv_issues_empty())
  keyed <- registry[!is.na(registry$vocab_key) & nzchar(registry$vocab_key), ]
  if (!nrow(keyed)) return(lv_issues_empty())
  key_of <- stats::setNames(keyed$vocab_key, keyed$qc_name)

  x <- cells[cells$field %in% names(key_of) & !is.na(cells$value) & nzchar(cells$value), ,
             drop = FALSE]
  if (!nrow(x)) return(lv_issues_empty())

  x$key <- unname(key_of[x$field])
  # One lookup per distinct value, not per cell: archiveType repeats across every
  # timeseries of a dataset.
  u <- unique(x[, c("key", "value")])
  u$ok <- vapply(seq_len(nrow(u)), function(i) {
    if (!u$key[i] %in% names(vocab)) return(TRUE)
    vocab_standardize(u$value[i], u$key[i], vocab)$matched
  }, logical(1))

  bad <- dplyr::inner_join(x, u[!u$ok, , drop = FALSE], by = c("key", "value"))
  if (!nrow(bad)) return(lv_issues_empty())

  dsn <- if (!is.null(index))
    unname(stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)[bad$tsid])
  else NA_character_

  lv_issues(check = "unknown_vocabulary", severity = "warn",
            message = sprintf("Not in the %s vocabulary; written as typed.", bad$key),
            dataSetName = dsn, TSid = bad$tsid, field = bad$field, value = bad$value)
}

#' Audit the whole QC sheet against the controlled vocabularies
#'
#' [lv_check_vocabulary()] runs on the cells a merge is about to write, which is
#' the right scope for catching a curator typing something new: that value wins
#' and reaches the files. It is the wrong scope for finding what is already
#' wrong. A bad value the sheet and the store both hold is not a change, so it
#' never appears in the plan, and no merge-time check will ever see it.
#'
#' hydroclimate2k carried `diatoms from lake core`, `w` and three others across
#' 16 rows that way. Nothing surfaced them until the sheet was read end to end.
#'
#' So this reads every cell, and then says what the run will do about each one,
#' which is the part that decides whether it needs attention:
#'
#' \describe{
#'   \item{`the files will correct it`}{The files hold a different, valid value
#'     and the sheet has not moved from the baseline, so the files win and the
#'     sheet is corrected on push. Nothing to do.}
#'   \item{`in the files too`}{The files hold the same bad value. The run changes
#'     nothing, and this is the case that needs a real decision -- either a
#'     vocabulary entry or a correction at the source.}
#'   \item{`the sheet has moved`}{The sheet differs from both the baseline and
#'     the files. Ownership decides, so read the plan: for a curator-owned field
#'     this value reaches the files.}
#'   \item{`no value in the files`}{Nothing on the file side to correct it.}
#' }
#'
#' Reports; never blocks. Intended as a pre-flight, before the merge.
#'
#' @param sheet Long cell tibble from [qc_sheet_pull()].
#' @param base Long cell tibble from [qc_state_current()]. Optional.
#' @param frame Long cell tibble from [qc_frame()]. Optional.
#' @param registry Field registry.
#' @param vocab Vocabulary tables, overlay included.
#' @param index Database index, used to name datasets.
#' @return One row per field, value and disposition, commonest first, with
#'   `datasets` as a list column.
#' @export
lv_audit_vocabulary <- function(sheet, base = NULL, frame = NULL,
                                registry = lv_qc_fields(),
                                vocab = lv_vocab_overlay(), index = NULL) {
  empty <- tibble::tibble(field = character(), value = character(),
                          disposition = character(), n_cells = integer(),
                          n_datasets = integer(), datasets = list())
  iss <- lv_check_vocabulary(sheet, registry = registry, vocab = vocab, index = index)
  if (!nrow(iss)) return(empty)

  # A cell that is absent is not a value. Reading `present` here keeps a
  # tombstone from looking like a real disagreement.
  lookup <- function(x) {
    if (is.null(x) || !nrow(x)) return(rep(NA_character_, nrow(iss)))
    v <- as.character(x$value)
    if ("present" %in% names(x)) v[!x$present] <- NA_character_
    m <- stats::setNames(v, paste(x$tsid, x$field, sep = "\r"))
    unname(m[paste(iss$TSid, iss$field, sep = "\r")])
  }
  bv <- lookup(base); fv <- lookup(frame); sv <- as.character(iss$value)
  same <- function(a, b) !is.na(a) & !is.na(b) & a == b

  disposition <- dplyr::case_when(
    is.na(fv)    ~ "no value in the files",
    same(sv, fv) ~ "in the files too",
    same(sv, bv) ~ "the files will correct it",
    TRUE         ~ "the sheet has moved"
  )

  out <- tibble::tibble(field = iss$field, value = sv, disposition = disposition,
                        dataSetName = iss$dataSetName)
  out <- dplyr::summarise(
    dplyr::group_by(out, .data$field, .data$value, .data$disposition),
    n_cells = dplyr::n(),
    n_datasets = dplyr::n_distinct(.data$dataSetName[!is.na(.data$dataSetName)]),
    datasets = list(sort(unique(.data$dataSetName[!is.na(.data$dataSetName)]))),
    .groups = "drop")
  out[order(-out$n_cells, out$field, out$value), ]
}
