#' Curator decisions on unrecognised vocabulary
#'
#' The pinned tables in `inst/extdata/vocab` are a snapshot of
#' `standardTables.RDS`, which is itself built from seven Google Sheets listed in
#' the "LiPD-PaST alignment directory". Those sheets are the source of truth, and
#' this package does not write to them: a decision recorded here takes effect
#' locally at once, and is *also* emitted in the schema of the target sheet so it
#' can be appended upstream deliberately, by a person, in one batch.
#'
#' Decisions live in `vocab/decisions.csv` in the QC store, append-only and
#' git-tracked, so a value is reviewed once and never resurfaces.
#'
#' Four decisions:
#'
#' \describe{
#'   \item{`synonym`}{the value means an existing `lipdName`. Overlaid onto the
#'     pinned table, so [vocab_standardize()] matches it by the `synonym` rule.}
#'   \item{`new_term`}{the vocabulary lacks a term this value needs. With no
#'     `map_to` the value itself becomes canonical. With a `map_to`, that name is
#'     created instead and the value maps onto it, which is what you want when
#'     the value is not a good term: `Annual HHI` should not become a term, but
#'     `moistureIndex` should, with `Annual HHI` pointing at it. Add
#'     `also_field` and `also_value` as well and the value decomposes onto the
#'     new term, so `Annual HHI` becomes `moistureIndex` plus a seasonality of
#'     `Annual` rather than baking the season into the name.}
#'   \item{`decompose`}{the value carries two facts at once, e.g.
#'     `MJJASO precip index` is `precipitation` *plus* a seasonality of `MJJASO`.
#'     Deliberately **not** overlaid as a synonym: a synonym would rewrite the
#'     variable name and silently drop the season. Applied only by
#'     [lv_ingest_standardize()], which writes both fields together.}
#'   \item{`leave`}{correct as it stands, or not yet decidable. Recorded so it is
#'     not offered again.}
#' }
#'
#' @name vocab_decisions
NULL

lv_vocab_decision_cols <- c("decided_utc", "actor", "field", "value", "decision",
                            "map_to", "also_field", "also_value", "past_name", "past_id",
                            "note", "run_id")

lv_vocab_decisions_path <- function(store = qc_store()) {
  fs::path(store$path, "vocab", "decisions.csv")
}

#' @rdname vocab_decisions
#' @param store From [qc_store()].
#' @return A tibble of recorded decisions; zero rows if none have been made.
#' @export
lv_vocab_decisions <- function(store = qc_store()) {
  p <- lv_vocab_decisions_path(store)
  if (!fs::file_exists(p)) {
    out <- tibble::as_tibble(stats::setNames(
      rep(list(character()), length(lv_vocab_decision_cols)), lv_vocab_decision_cols))
    return(out)
  }
  readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                  na = "", progress = FALSE)
}

#' Overlay recorded decisions onto the pinned vocabulary
#'
#' `synonym` and `new_term` decisions become rows in the relevant table, so
#' [vocab_standardize()] recognises them. `decompose` is excluded by design, and
#' `leave` never changes a table.
#'
#' @param vocab From [lv_vocab()].
#' @param store From [qc_store()].
#' @return `vocab`, with an `overlay` attribute recording how many rows were added.
#' @export
lv_vocab_overlay <- function(vocab = lv_vocab(), store = qc_store()) {
  d <- lv_vocab_decisions(store)
  d <- d[d$decision %in% c("synonym", "new_term") & d$field %in% names(vocab), , drop = FALSE]
  if (!nrow(d)) {
    attr(vocab, "overlay") <- 0L
    return(vocab)
  }
  for (k in unique(d$field)) {
    dk <- d[d$field == k, , drop = FALSE]
    tb <- vocab[[k]]
    named <- dk$decision == "new_term" & !is.na(dk$map_to) & nzchar(dk$map_to)
    decomposing <- named & !is.na(dk$also_field) & nzchar(dk$also_field)
    add <- tibble::tibble(
      # new_term with a map_to creates that name; without one the value is the name.
      lipdName = ifelse(dk$decision == "new_term",
                        ifelse(named, dk$map_to, dk$value), dk$map_to),
      # A value that decomposes onto its new term must not also be a synonym of
      # it. A synonym would rewrite the name and drop the second field, which is
      # the whole loss decompose exists to prevent.
      synonym  = ifelse(dk$decision == "new_term" & (!named | decomposing),
                        NA_character_, dk$value))
    for (nm in setdiff(names(tb), names(add))) add[[nm]] <- NA_character_
    add <- add[, names(tb), drop = FALSE]
    # A decision is the later authority: drop any pinned row claiming the same
    # synonym, so re-deciding a term changes where it points.
    if ("synonym" %in% names(tb)) tb <- tb[!(!is.na(tb$synonym) & tb$synonym %in% dk$value), , drop = FALSE]
    vocab[[k]] <- dplyr::bind_rows(tb, add)
  }
  attr(vocab, "overlay") <- nrow(d)
  vocab
}

#' The remap table for `decompose` decisions
#'
#' @param store From [qc_store()].
#' @return A tibble of `field`, `value`, `map_to`, `also_field`, `also_value`.
#' @export
lv_vocab_remap <- function(store = qc_store()) {
  d <- lv_vocab_decisions(store)
  # A new_term that names its target and carries a second field decomposes onto
  # that target exactly as `decompose` does; the only difference is that the
  # target did not exist until this decision created it.
  keep <- d$decision == "decompose" |
    (d$decision == "new_term" & !is.na(d$map_to) & nzchar(d$map_to) &
       !is.na(d$also_field) & nzchar(d$also_field))
  d <- d[keep, , drop = FALSE]
  d[, c("field", "value", "map_to", "also_field", "also_value"), drop = FALSE]
}

#' Build a review file for unrecognised vocabulary
#'
#' One row per distinct `(field, value)` still unrecognised after existing
#' decisions are applied, with the occurrence count, an example dataset, and the
#' closest candidates from the vocabulary. Fill in `decision` (and `map_to`,
#' `also_field`, `also_value` where it applies) and feed it back to
#' [lv_vocab_apply_review()].
#'
#' Follows the review-file convention: generated once, then owned by the person
#' editing it. Regenerating over your edits would discard them, so this refuses
#' to overwrite an existing file unless `overwrite = TRUE`.
#'
#' @param issues The `issues` element of [lv_ingest_standardize()], or any
#'   `lv_issues` tibble carrying `check == "unknown_vocabulary"`.
#' @param out Path to write.
#' @param vocab From [lv_vocab()]; pass the overlaid vocabulary to avoid
#'   re-offering values already decided.
#' @param store From [qc_store()].
#' @param n_candidates How many suggestions per row.
#' @param overwrite Replace an existing review file.
#' @return The review tibble, invisibly.
#' @export
lv_vocab_review <- function(issues, out, vocab = lv_vocab_overlay(store = store),
                            store = qc_store(), n_candidates = 5, overwrite = FALSE,
                            sources = NULL, past = NULL) {
  if (fs::file_exists(out) && !overwrite) {
    cli::cli_abort(c("{.path {out}} already exists.",
                     i = "Regenerating would discard decisions already recorded in it.",
                     i = "Pass {.code overwrite = TRUE} only if you have applied or copied it."),
                   class = "lv_error_vocab")
  }
  x <- issues[issues$check %in% "unknown_vocabulary" & !is.na(issues$value), , drop = FALSE]
  x <- x[x$field %in% names(vocab), , drop = FALSE]

  # Anything already decided is settled, including `leave`.
  done <- lv_vocab_decisions(store)
  if (nrow(done)) {
    key <- paste(x$field, x$value, sep = "\r")
    x <- x[!key %in% paste(done$field, done$value, sep = "\r"), , drop = FALSE]
  }
  # And anything the overlay now recognises.
  if (nrow(x)) {
    keep <- !vapply(seq_len(nrow(x)), function(i)
      vocab_standardize(x$value[i], x$field[i], vocab)$matched, logical(1))
    x <- x[keep, , drop = FALSE]
  }

  if (!nrow(x)) {
    cli::cli_alert_success("Nothing left to review.")
    r <- lv_review_empty()
    lv_review_write(r, out, meta = list(created_utc = lv_now_utc(), vocab_pin = attr(vocab, "pin")))
    return(invisible(r))
  }

  agg <- x |>
    dplyr::group_by(field, value) |>
    dplyr::summarise(n = dplyr::n(),
                     example = dplyr::first(stats::na.omit(dataSetName)),
                     datasets = list(unique(stats::na.omit(dataSetName))),
                     .groups = "drop") |>
    dplyr::arrange(field, dplyr::desc(n))

  agg$candidates <- lapply(seq_len(nrow(agg)), function(i)
    lv_vocab_candidates(agg$value[i], agg$field[i], vocab, n_candidates))

  # PaST alignment, for terms that may need adding. The alignment sheets carry
  # paleoData_pastName and paleoData_pastId, so a proposed new term without one
  # is a term someone has to look up later.
  empty_past <- tibble::tibble(pastId = character(), pastName = character(),
                               rule = character(), definition = character())
  agg$past_candidates <- rep(list(empty_past), nrow(agg))
  if (!isFALSE(past)) {
    pt <- tryCatch(if (is.null(past)) lv_past() else past, error = function(e) NULL)
    if (!is.null(pt)) {
      agg$past_candidates <- lapply(agg$value, function(v) lv_past_match(v, 3, pt))
    } else {
      cli::cli_alert_warning("PaST thesaurus unavailable; {.field past_candidates} left empty.")
    }
  }

  # The paper, where the submission arrived with one. A value like `HHI` cannot
  # be resolved from the vocabulary at all, only from what the authors wrote.
  agg$source_pdfs <- rep(list(character()), nrow(agg))
  if (!is.null(sources)) {
    pdf_of <- stats::setNames(sources$pdfs, sources$dataSetName)
    agg$source_pdfs <- lapply(agg$datasets, function(ds) {
      f <- unique(stats::na.omit(unlist(pdf_of[ds])))
      f[nzchar(f)]
    })
  }

  # Proposals and decisions are separate columns on purpose. An agent filling in
  # this file writes only the `proposed_*` side; `lv_vocab_apply_review()` reads
  # only `decision`. So a guess cannot become a decision by being written down,
  # and you can always see which rows you ruled on yourself.
  for (nm in LV_REVIEW_PROPOSED) agg[[paste0("proposed_", nm)]] <- NA_character_
  for (nm in LV_REVIEW_DECIDED) agg[[nm]] <- NA_character_

  agg <- agg[, c("field", "value", "n", "example", "datasets", "candidates",
                 "source_pdfs", "past_candidates",
                 paste0("proposed_", LV_REVIEW_PROPOSED), LV_REVIEW_DECIDED)]
  lv_review_write(agg, out, meta = list(created_utc = lv_now_utc(),
                                        vocab_pin = attr(vocab, "pin"),
                                        n_values = nrow(agg)))
  cli::cli_alert_info(
    "{nrow(agg)} value{?s} to review in {.path {out}}. Decisions: {.val synonym}, {.val new_term}, {.val decompose}, {.val leave}.")
  invisible(agg)
}

#' Closest vocabulary terms to a value
#'
#' @param value A single value.
#' @param key Vocabulary key.
#' @param vocab From [lv_vocab()].
#' @param n How many to return.
#' @return A character vector of candidate `lipdName`s.
#' @export
lv_vocab_candidates <- function(value, key, vocab = lv_vocab(), n = 5) {
  tb <- vocab[[key]]
  if (is.null(tb)) return(character())
  terms <- unique(stats::na.omit(c(tb$lipdName, if ("synonym" %in% names(tb)) tb$synonym)))
  # The vocabulary itself contains placeholders. Offering `needsToBeChanged` as
  # the answer to "what should this be" helps nobody.
  terms <- setdiff(terms, LV_VOCAB_PLACEHOLDERS)
  if (!length(terms)) return(character())
  to_canon <- c(stats::setNames(tb$lipdName, tb$lipdName),
                if ("synonym" %in% names(tb)) stats::setNames(tb$lipdName, tb$synonym))

  lv <- tolower(value); lt <- tolower(terms)
  d <- utils::adist(lv, lt, partial = FALSE)[1, ]
  # Normalise by length so a short term is not favoured purely for being short.
  d <- d / pmax(nchar(terms), nchar(value))
  # Containment beats edit distance on compound values: `MJJASO precip index` is
  # 0.74 from `precipitation` by edit distance, which buries it, but the word
  # stem is right there. Ranked ahead of everything else.
  contained <- vapply(lt, function(t) grepl(t, lv, fixed = TRUE) ||
                        grepl(substr(lv, 1, 40), t, fixed = TRUE), logical(1))
  stem <- vapply(lt, function(t) {
    w <- strsplit(lv, "[^a-z0-9]+")[[1]]
    w <- w[nchar(w) >= 4]
    length(w) > 0 && any(startsWith(t, w) | startsWith(w, substr(t, 1, 5)))
  }, logical(1))
  score <- d - 0.6 * contained - 0.25 * stem

  o <- order(score)[seq_len(min(n * 3L, length(terms)))]
  # Filter again after mapping to canonical: a synonym can be perfectly ordinary
  # while the lipdName it points at is a placeholder.
  cand <- unique(unname(to_canon[terms[o]]))
  utils::head(setdiff(cand, LV_VOCAB_PLACEHOLDERS), n)
}

#' Apply a completed vocabulary review
#'
#' Appends the decisions to the store, and writes one patch file per affected
#' vocabulary in the schema of its Google Sheet, ready to be appended upstream.
#' **Nothing is written to any Google Sheet**: the alignment sheets are shared and
#' authoritative, so pushing to them stays a deliberate human act.
#'
#' @param path A review file produced by [lv_vocab_review()] and filled in.
#' @param store From [qc_store()].
#' @param actor Who decided; recorded with each row.
#' @param run_id Run identifier.
#' @param patch_dir Where to write the upstream patch files. `NULL` to skip.
#' @param dry_run Report without appending. Defaults to `TRUE`.
#' @return A list of `decisions`, `patches` and `remap`, invisibly.
#' @export
lv_vocab_apply_review <- function(path, store = qc_store(), actor = lv_actor(),
                                  run_id = lv_run_id(),
                                  patch_dir = fs::path(store$path, "vocab", "patches"),
                                  dry_run = TRUE) {
  r <- lv_review_read(path)
  r <- r[!is.na(r$decision) & nzchar(r$decision), , drop = FALSE]
  if (!nrow(r)) {
    cli::cli_alert_info("No decisions filled in.")
    return(invisible(list(decisions = NULL, patches = NULL, remap = NULL)))
  }

  ok <- c("synonym", "new_term", "decompose", "leave")
  bad <- setdiff(unique(r$decision), ok)
  if (length(bad)) {
    cli::cli_abort("Unknown decision{?s} {.val {bad}}. Use {.val {ok}}.", class = "lv_error_vocab")
  }
  need_map <- r$decision %in% c("synonym", "decompose") & (is.na(r$map_to) | !nzchar(r$map_to))
  if (any(need_map)) {
    cli::cli_abort(c("{sum(need_map)} row{?s} need {.field map_to}.",
                     i = "{.val {utils::head(r$value[need_map], 5)}}"), class = "lv_error_vocab")
  }
  # A half-filled also_ pair silently drops the half that is there, on either
  # decompose or a new_term that is decomposing onto its new name.
  has_af <- !is.na(r$also_field) & nzchar(r$also_field)
  has_av <- !is.na(r$also_value) & nzchar(r$also_value)
  need_also <- (r$decision == "decompose" & !(has_af & has_av)) |
               (r$decision == "new_term" & xor(has_af, has_av))
  if (any(need_also)) {
    cli::cli_abort(c("{sum(need_also)} row{?s} need both {.field also_field} and {.field also_value}.",
                     i = "{.val {utils::head(r$value[need_also], 5)}}",
                     i = "That second field is the whole reason to decompose rather than map."),
                   class = "lv_error_vocab")
  }
  vocab <- lv_vocab()
  # map_to must be a real term for synonym and decompose, or the decision just
  # moves the problem. For new_term the opposite holds: map_to is the name being
  # created, so one that already exists means this is a synonym.
  chk <- r[r$decision %in% c("synonym", "decompose"), , drop = FALSE]
  if (nrow(chk)) {
    unknown <- vapply(seq_len(nrow(chk)), function(i)
      !chk$map_to[i] %in% vocab[[chk$field[i]]]$lipdName, logical(1))
    if (any(unknown)) {
      cli::cli_abort(c("{sum(unknown)} {.field map_to} value{?s} not in the vocabulary: {.val {unique(chk$map_to[unknown])}}",
                       i = "Use {.val new_term} with that name as {.field map_to} to create it."),
                     class = "lv_error_vocab")
    }
  }
  nt <- r[r$decision == "new_term" & !is.na(r$map_to) & nzchar(r$map_to), , drop = FALSE]
  if (nrow(nt)) {
    exists_already <- vapply(seq_len(nrow(nt)), function(i)
      nt$map_to[i] %in% vocab[[nt$field[i]]]$lipdName, logical(1))
    if (any(exists_already)) {
      cli::cli_abort(c("{sum(exists_already)} {.val new_term} row{?s} name a term that already exists: {.val {unique(nt$map_to[exists_already])}}",
                       i = "Use {.val synonym} or {.val decompose} to map onto an existing term."),
                     class = "lv_error_vocab")
    }
  }

  d <- tibble::tibble(
    decided_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    actor = actor, field = r$field, value = r$value, decision = r$decision,
    map_to = r$map_to, also_field = r$also_field, also_value = r$also_value,
    past_name = if ("past_name" %in% names(r)) r$past_name else NA_character_,
    past_id   = if ("past_id"   %in% names(r)) r$past_id   else NA_character_,
    note = r$note, run_id = run_id)

  patches <- lv_vocab_patches(d, vocab)

  if (dry_run) {
    cli::cli_alert_info("Dry run. Would record {nrow(d)} decision{?s}:")
    print(dplyr::count(d, field, decision), n = 50)
    return(invisible(list(decisions = d, patches = patches, remap = NULL)))
  }

  p <- lv_vocab_decisions_path(store)
  fs::dir_create(fs::path_dir(p))
  readr::write_csv(d, p, na = "", append = fs::file_exists(p))
  cli::cli_alert_success("Recorded {nrow(d)} decision{?s} in {.path {p}}")

  if (!is.null(patch_dir) && length(patches)) {
    fs::dir_create(patch_dir)
    for (k in names(patches)) {
      f <- fs::path(patch_dir, paste0(k, "-", run_id, ".csv"))
      readr::write_csv(patches[[k]], f, na = "")
    }
    cli::cli_alert_info(
      "Wrote {length(patches)} upstream patch file{?s} to {.path {patch_dir}}. Append these to the alignment sheets by hand.")
  }
  invisible(list(decisions = d, patches = patches, remap = lv_vocab_remap(store)))
}

#' Rows to append to the upstream alignment sheets
#'
#' @param decisions From [lv_vocab_decisions()].
#' @param vocab From [lv_vocab()].
#' @return A named list of tibbles, one per vocabulary key, in that sheet's schema.
#' @export
lv_vocab_patches <- function(decisions, vocab = lv_vocab()) {
  d <- decisions[decisions$decision %in% c("synonym", "new_term"), , drop = FALSE]
  if (!nrow(d)) return(list())
  out <- list()
  for (k in unique(d$field)) {
    dk <- d[d$field == k, , drop = FALSE]
    tb <- vocab[[k]]
    named <- dk$decision == "new_term" & !is.na(dk$map_to) & nzchar(dk$map_to)
    decomposing <- named & !is.na(dk$also_field) & nzchar(dk$also_field)
    row <- tibble::tibble(
      lipdName = ifelse(dk$decision == "new_term",
                        ifelse(named, dk$map_to, dk$value), dk$map_to),
      synonym  = ifelse(dk$decision == "new_term" & (!named | decomposing),
                        NA_character_, dk$value))
    for (nm in setdiff(names(tb), names(row))) row[[nm]] <- NA_character_
    # The alignment columns are named per vocabulary: paleoData_pastName in the
    # proxy sheet, pastName elsewhere. Fill whichever this sheet uses.
    pn <- grep("pastName$", names(row), value = TRUE)
    pi <- grep("pastId$", names(row), value = TRUE)
    if (length(pn) && "past_name" %in% names(dk)) row[[pn[1]]] <- dk$past_name
    if (length(pi) && "past_id" %in% names(dk)) row[[pi[1]]] <- dk$past_id
    row <- row[, names(tb), drop = FALSE]

    # Five HHI values all create moistureIndex, and each produced its own
    # identical row. These are rows destined for a shared sheet, so emit each
    # once.
    row <- dplyr::distinct(row)
    # And never re-append what the vocabulary already has. A synonym that is
    # already listed upstream would otherwise arrive as a duplicate.
    have <- paste(tb$lipdName, if ("synonym" %in% names(tb)) tb$synonym else NA, sep = "\r")
    key <- paste(row$lipdName, if ("synonym" %in% names(row)) row$synonym else NA, sep = "\r")
    row <- row[!key %in% have, , drop = FALSE]
    if (!nrow(row)) next
    out[[k]] <- row
  }
  out
}

#' Apply `decompose` decisions to one column
#'
#' Sets the primary field and the second field together. The second field is
#' written only where it is empty: a decision inferred from a variable-name
#' string must never overwrite a seasonality a curator stated explicitly.
#'
#' @param cl A LiPD column.
#' @param remap From [lv_vocab_remap()].
#' @param dsn,tsid For the change log.
#' @param log A function called with each change row.
#' @return The column.
#' @keywords internal
lv_apply_remap <- function(cl, remap, dsn, tsid, log = function(e) NULL) {
  if (is.null(remap) || !nrow(remap)) return(cl)

  get <- function(field) {
    switch(field,
           paleoData_variableName = cl$variableName,
           paleoData_units = cl$units,
           paleoData_proxy = cl$proxy,
           NULL)
  }
  for (i in seq_len(nrow(remap))) {
    cur <- as_chr1(get(remap$field[i]))
    if (is.null(cur) || is.na(cur) || !identical(cur, remap$value[i])) next

    switch(remap$field[i],
           paleoData_variableName = cl$variableName <- remap$map_to[i],
           paleoData_units        = cl$units <- remap$map_to[i],
           paleoData_proxy        = cl$proxy <- remap$map_to[i])
    log(tibble::tibble(dataSetName = dsn, TSid = tsid %||% NA_character_,
                       field = remap$field[i], from = cur, to = remap$map_to[i],
                       rule = "decompose"))

    af <- remap$also_field[i]
    if (is.na(af) || !nzchar(af)) next
    if (grepl("^interpretation_", af)) {
      slot <- sub("^interpretation_", "", af)
      if (!length(cl$interpretation)) cl$interpretation <- list(list())
      have <- as_chr1(cl$interpretation[[1]][[slot]])
      if (is.null(have) || is.na(have) || !nzchar(have)) {
        cl$interpretation[[1]][[slot]] <- remap$also_value[i]
        log(tibble::tibble(dataSetName = dsn, TSid = tsid %||% NA_character_,
                           field = af, from = NA_character_, to = remap$also_value[i],
                           rule = "decompose"))
      }
    }
  }
  cl
}

#' Who is making a decision
#'
#' @return A single string.
#' @export
lv_actor <- function() {
  Sys.getenv("LIPDVERSE_ACTOR",
             unset = paste0(Sys.info()[["user"]], "@", Sys.info()[["nodename"]]))
}

# Placeholders that exist in the alignment sheets as work markers. They are
# never a useful suggestion.
LV_VOCAB_PLACEHOLDERS <- c("deleteMe", "needsToBeChanged", "deleteThisColumn",
                           "changeMe", "TBD", "unknown")

#' Accept proposed vocabulary decisions
#'
#' Copies the `proposed_*` columns of a review file into the `decision` columns,
#' which is the only side [lv_vocab_apply_review()] reads. This is the step where
#' a suggestion becomes your call, and it is deliberately explicit: an agent can
#' fill in every proposal in the file and nothing happens until this runs.
#'
#' Rows that already carry a `decision` are never overwritten.
#'
#' @param path A review file.
#' @param which Optional character vector of `value`s to accept. Default accepts
#'   every proposed row permitted by `min_confidence`.
#' @param min_confidence Accept only proposals at or above this confidence.
#'   One of `"low"`, `"medium"`, `"high"`. Rows with no stated confidence are
#'   treated as `"low"`.
#' @param dry_run Report what would be accepted without editing the file.
#' @return The updated review tibble, invisibly.
#' @export
lv_vocab_accept <- function(path, which = NULL, min_confidence = c("high", "medium", "low"),
                            dry_run = TRUE) {
  min_confidence <- match.arg(min_confidence)
  rank <- c(low = 1L, medium = 2L, high = 3L)
  r <- lv_review_read(path)
  if (!"proposed_decision" %in% names(r)) {
    cli::cli_abort("{.path {path}} has no {.field proposed_decision} column.",
                   class = "lv_error_vocab")
  }
  # Confidence is part of the proposal, so it is `proposed_confidence`. Reading a
  # bare `confidence` silently made every row look low and accepted nothing.
  cf <- r$proposed_confidence
  conf <- rank[ifelse(is.na(cf), "low", tolower(cf))]
  conf[is.na(conf)] <- 1L

  take <- !is.na(r$proposed_decision) & nzchar(r$proposed_decision) &
    (is.na(r$decision) | !nzchar(r$decision)) &
    conf >= rank[[min_confidence]]
  if (!is.null(which)) take <- take & r$value %in% which

  if (!any(take)) {
    cli::cli_alert_info("Nothing to accept at confidence {.val {min_confidence}} or above.")
    return(invisible(r))
  }

  r$decision[take] <- r$proposed_decision[take]
  r$map_to[take] <- r$proposed_map_to[take]
  r$also_field[take] <- r$proposed_also_field[take]
  r$also_value[take] <- r$proposed_also_value[take]
  if ("proposed_past_name" %in% names(r)) r$past_name[take] <- r$proposed_past_name[take]
  if ("proposed_past_id" %in% names(r)) r$past_id[take] <- r$proposed_past_id[take]

  cli::cli_alert_info("Accepting {sum(take)} proposal{?s}:")
  print(dplyr::count(tibble::as_tibble(r[take, c("field", "decision")]), field, decision), n = 50)

  skipped <- !is.na(r$proposed_decision) & nzchar(r$proposed_decision) & !take &
    (is.na(r$decision) | !nzchar(r$decision))
  if (any(skipped)) {
    cli::cli_alert_warning(
      "{sum(skipped)} proposal{?s} left for you: below {.val {min_confidence}} confidence.")
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run. {.path {path}} unchanged.")
    return(invisible(r))
  }
  lv_review_write(r, path)
  cli::cli_alert_success("Wrote {.path {path}}. Review it, then {.code lv_vocab_apply_review()}.")
  invisible(r)
}
