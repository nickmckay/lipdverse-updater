#' The QC store
#'
#' QC state is an append-only log of cell-level events. The current state is the
#' latest event per `(tsid, field)`; the state at any past moment is the same
#' query with a time bound.
#'
#' The store is the reason this rewrite exists. lipdverseR kept QC state only in
#' a Google Sheet, with `lastUpdate.csv` overwritten in place each run, so when
#' a merge dropped a curated value there was nothing to diff against and no way
#' to tell when it went.
#'
#' Every event records `old_present` and `new_present` alongside the values.
#' That distinction — a cell that is absent versus a cell that holds nothing —
#' is what `daff` could not express, and why NA arriving from the files was read
#' as "delete this" and wiped curated fields.
#'
#' @section Layout:
#' Text is the source of truth; git supplies the history.
#' ```
#' compilations/<compilation>/
#'   events/<timestamp>_<run_id>.csv   append-only cell deltas
#'   state.csv                         materialised current state
#' ```
#'
#' @name qc_store
NULL

LV_EVENT_COLS <- c("run_id", "event_seq", "ts", "compilation", "tsid", "dataset_id",
                   "field", "old_value", "old_present", "new_value", "new_present",
                   "source", "actor", "reason")

LV_EVENT_SOURCES <- c("sheet", "lipd", "curator", "migration", "resolver", "vocab")

#' Open (creating if needed) a QC store
#'
#' @param path Store root. Defaults to [lv_path()]`("qcstore")`.
#' @return An `lv_qc_store` object.
#' @export
qc_store <- function(path = lv_path("qcstore")) {
  fs::dir_create(path)
  structure(list(path = path), class = "lv_qc_store")
}

store_dir <- function(store, compilation, what = NULL) {
  p <- fs::path(store$path, "compilations", compilation)
  if (!is.null(what)) p <- fs::path(p, what)
  p
}

#' An empty event table
#' @export
qc_events_empty <- function() {
  tibble::tibble(
    run_id = character(), event_seq = integer(), ts = character(),
    compilation = character(), tsid = character(), dataset_id = character(),
    field = character(), old_value = character(), old_present = logical(),
    new_value = character(), new_present = logical(),
    source = character(), actor = character(), reason = character()
  )
}

validate_qc_events <- function(e) {
  miss <- setdiff(LV_EVENT_COLS, names(e))
  if (length(miss)) {
    cli::cli_abort("Events missing column{?s}: {.field {miss}}", class = "lv_error_store")
  }
  bad <- setdiff(unique(stats::na.omit(e$source)), LV_EVENT_SOURCES)
  if (length(bad)) {
    cli::cli_abort("Unknown event source{?s}: {.val {bad}}", class = "lv_error_store")
  }
  if (any(is.na(e$tsid) | !nzchar(e$tsid))) {
    cli::cli_abort("Every event needs a tsid.", class = "lv_error_store")
  }
  if (any(is.na(e$field) | !nzchar(e$field))) {
    cli::cli_abort("Every event needs a field.", class = "lv_error_store")
  }
  # A present cell must have a value; an absent one must not.
  if (any(e$new_present & is.na(e$new_value))) {
    cli::cli_abort("new_present = TRUE requires a new_value.", class = "lv_error_store")
  }
  if (any(!e$new_present & !is.na(e$new_value))) {
    cli::cli_abort("new_present = FALSE must have new_value = NA (a tombstone carries no value).",
                   class = "lv_error_store")
  }
  invisible(e)
}

#' Append events to a compilation's log
#'
#' @param store A store from [qc_store()].
#' @param compilation Compilation name.
#' @param events An event tibble.
#' @param run_id Run identifier.
#' @return The path written, invisibly.
#' @export
qc_store_append <- function(store, compilation, events, run_id = lv_run_id()) {
  if (nrow(events) == 0) return(invisible(NULL))
  events$compilation <- compilation
  # Absent and all-NA both mean "fill this in". Testing the column directly
  # warns about an uninitialised column when a caller builds events by hand,
  # which is noise on every append -- and "uninitialised column" during a write
  # to the store is exactly the kind of warning that should mean something.
  unset <- function(nm) is.null(events[[nm]]) || all(is.na(events[[nm]]))
  if (unset("run_id")) events$run_id <- run_id
  if (unset("ts")) events$ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (unset("event_seq")) events$event_seq <- seq_len(nrow(events))
  validate_qc_events(events)

  d <- store_dir(store, compilation, "events")
  fs::dir_create(d)
  # A zero-padded append sequence, not the timestamp, establishes order. Two
  # appends inside the same second carry identical timestamps, and ordering by
  # anything else (run_id is random) would let a later event sort first and be
  # overwritten by the earlier one when materialising "latest wins".
  n <- length(fs::dir_ls(d, glob = "*.csv"))
  p <- fs::path(d, sprintf("%06d_%s_%s.csv", n + 1L,
                           gsub("[^0-9]", "", events$ts[1]), events$run_id[1]))
  readr::write_csv(events[, LV_EVENT_COLS], p, na = "")
  invisible(p)
}

#' Read a compilation's whole event log
#' @param store A store.
#' @param compilation Compilation name.
#' @return An event tibble, oldest first.
#' @export
qc_store_events <- function(store, compilation) {
  d <- store_dir(store, compilation, "events")
  if (!fs::dir_exists(d)) return(qc_events_empty())
  f <- sort(fs::dir_ls(d, glob = "*.csv"))
  if (!length(f)) return(qc_events_empty())
  e <- purrr::list_rbind(lapply(seq_along(f), function(i) {
    x <- readr::read_csv(f[i], col_types = readr::cols(
      .default = readr::col_character(),
      event_seq = readr::col_integer(),
      old_present = readr::col_logical(),
      new_present = readr::col_logical()
    # na = "": only an empty field is missing. readr's default also treats a
    # bare NA as missing, so a cell whose value is the literal string "NA" --
    # 669 of them in lipdverseTest, on collectionYear and climateCorrelation --
    # came back empty and the store could never converge with the files.
    ), na = "", progress = FALSE)
    x$append_seq <- i
    x
  }))
  # Filename order is append order, so this is the true sequence.
  e[order(e$append_seq, e$event_seq), , drop = FALSE]
}

#' Materialise the current (or a historical) QC state
#'
#' The latest event per `(tsid, field)`, keeping only cells that are present.
#' Passing `as_of` bounds the log by time, which is what makes "what did this
#' cell say last Tuesday" answerable.
#'
#' @param store A store.
#' @param compilation Compilation name.
#' @param as_of Optional ISO timestamp; events after it are ignored.
#' @return A long tibble of `tsid`, `field`, `value`, plus provenance.
#' @export
qc_state_at <- function(store, compilation, as_of = NULL) {
  e <- qc_store_events(store, compilation)
  if (nrow(e) == 0) return(qc_cells_empty())
  if (!is.null(as_of)) e <- e[e$ts <= as_of, , drop = FALSE]
  if (nrow(e) == 0) return(qc_cells_empty())

  e <- e[order(e$tsid, e$field, e$append_seq, e$event_seq), , drop = FALSE]
  last <- !duplicated(paste(e$tsid, e$field), fromLast = TRUE)
  x <- e[last, , drop = FALSE]
  x <- x[x$new_present, , drop = FALSE]

  tibble::tibble(
    tsid = x$tsid, field = x$field, value = x$new_value, present = TRUE,
    dataset_id = x$dataset_id, updated_at = x$ts, source = x$source, actor = x$actor
  )
}

#' @rdname qc_state_at
#' @export
qc_state_current <- function(store, compilation) qc_state_at(store, compilation, NULL)

#' An empty cell table
#' @export
qc_cells_empty <- function() {
  tibble::tibble(tsid = character(), field = character(), value = character(),
                 present = logical(), dataset_id = character(),
                 updated_at = character(), source = character(), actor = character())
}

#' The history of one cell
#'
#' @param store A store.
#' @param compilation Compilation name.
#' @param tsid TSid.
#' @param field Field name.
#' @return The events touching that cell, oldest first.
#' @export
qc_history <- function(store, compilation, tsid, field = NULL) {
  e <- qc_store_events(store, compilation)
  e <- e[e$tsid %in% tsid, , drop = FALSE]
  if (!is.null(field)) e <- e[e$field %in% field, , drop = FALSE]
  e
}

#' Diff two cell tables into events
#'
#' Turns "here is the new state" into the events that get from `before` to
#' `after`. A cell present in `before` and absent from `after` becomes an
#' explicit tombstone rather than silently vanishing.
#'
#' @param before,after Cell tables.
#' @param source Event source.
#' @param actor Who or what made the change.
#' @param reason Free text.
#' @return An event tibble.
#' @export
qc_diff_to_events <- function(before, after, source = "sheet", actor = NA_character_,
                              reason = NA_character_) {
  b <- before[, c("tsid", "field", "value", "dataset_id")]
  a <- after[,  c("tsid", "field", "value", "dataset_id")]
  names(b) <- c("tsid", "field", "old_value", "old_dsid")
  names(a) <- c("tsid", "field", "new_value", "new_dsid")

  j <- dplyr::full_join(b, a, by = c("tsid", "field"))
  # Presence is membership of the cell table, which is exactly the distinction
  # a plain NA cannot carry: a cell that is absent is not a cell holding NA.
  bkey <- paste(before$tsid, before$field, sep = "\r")
  akey <- paste(after$tsid,  after$field,  sep = "\r")
  jkey <- paste(j$tsid, j$field, sep = "\r")
  j$old_present <- jkey %in% bkey
  j$new_present <- jkey %in% akey

  same_value <- (is.na(j$old_value) & is.na(j$new_value)) |
                (!is.na(j$old_value) & !is.na(j$new_value) & j$old_value == j$new_value)
  j <- j[!(j$old_present == j$new_present & same_value), , drop = FALSE]
  if (nrow(j) == 0) return(qc_events_empty())

  tibble::tibble(
    run_id = NA_character_, event_seq = NA_integer_, ts = NA_character_,
    compilation = NA_character_,
    tsid = j$tsid,
    dataset_id = dplyr::coalesce(j$new_dsid, j$old_dsid),
    field = j$field,
    old_value = j$old_value, old_present = j$old_present,
    # A tombstone carries no value.
    new_value = ifelse(j$new_present, j$new_value, NA_character_),
    new_present = j$new_present,
    source = source, actor = actor, reason = reason
  )
}

#' @export
print.lv_qc_store <- function(x, ...) {
  cli::cli_h3("lv_qc_store")
  d <- fs::path(x$path, "compilations")
  comps <- if (fs::dir_exists(d)) basename(fs::dir_ls(d, type = "directory")) else character()
  cli::cli_bullets(c("*" = "{.path {x$path}}", "*" = "{length(comps)} compilation{?s}"))
  invisible(x)
}

#' Retire cells from a compilation's QC state
#'
#' Appends tombstones rather than deleting anything: the store is append-only,
#' so the history of a retired cell stays readable and the retirement itself is
#' an event with a reason attached.
#'
#' Written for a specific mess. The hydroclimate2k baseline was seeded with 6,495
#' chronData timeseries, each carrying a lone `inThisCompilation` flag and no
#' dataSetName or archiveType, because the seeder took its scope straight off the
#' index. Fixing the scope stops more arriving; it does not remove what was
#' already recorded, and the merge would keep re-surfacing them.
#'
#' @param store From [qc_store()].
#' @param compilation Compilation name.
#' @param tsids Timeseries to retire. All their cells are tombstoned.
#' @param reason Recorded on every event.
#' @param actor Who did it.
#' @param run_id Run identifier.
#' @param dry_run Report without appending. Defaults to `TRUE`.
#' @return The events appended (or that would be), invisibly.
#' @export
lv_qc_retire <- function(store, compilation, tsids, reason,
                         actor = lv_actor(), run_id = lv_run_id(), dry_run = TRUE) {
  if (missing(reason) || !nzchar(reason)) {
    cli::cli_abort("{.arg reason} is required; a retirement without one is indistinguishable from a bug.")
  }
  state <- qc_state_current(store, compilation)
  hit <- state[state$tsid %in% tsids, , drop = FALSE]
  if (!nrow(hit)) {
    cli::cli_alert_success("Nothing to retire.")
    return(invisible(NULL))
  }
  ev <- tibble::tibble(
    tsid = hit$tsid, field = hit$field,
    old_value = hit$value, old_present = TRUE,
    new_value = NA_character_, new_present = FALSE,
    dataset_id = hit$dataset_id, source = "curator", actor = actor, reason = reason)

  cli::cli_alert_info(
    "{nrow(ev)} cell{?s} across {dplyr::n_distinct(ev$tsid)} timeseries would be retired.")
  print(dplyr::count(ev, field, sort = TRUE), n = 10)
  if (dry_run) {
    cli::cli_alert_info("Dry run. Nothing appended.")
    return(invisible(ev))
  }
  qc_store_append(store, compilation, ev, run_id = run_id)
  cli::cli_alert_success("Retired {nrow(ev)} cell{?s}.")
  invisible(ev)
}
