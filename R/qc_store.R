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
  # From the highest sequence already present, not the file COUNT. Counting
  # assumes the sequence starts at 1 and has no gaps, and qc_store_compress()
  # breaks both: it removes early files and leaves the later ones numbered as
  # they were. NAm21k-noPollen was left holding 000003 and 000004, so a count of
  # 2 produced another 000003 -- a collision that sorted the new events BEFORE
  # the older ones and inverted "latest wins" on an append-only log.
  n <- lv_store_max_seq(d)
  # Gzipped. The hydroclimate2k log reached 50 MB in one file and GitHub had
  # already warned; events are long-format text and compress about tenfold. The
  # sequence prefix still orders them, so the extension is free to change.
  p <- fs::path(d, sprintf("%06d_%s_%s.csv.gz", n + 1L,
                           gsub("[^0-9]", "", events$ts[1]), events$run_id[1]))
  readr::write_csv(events[, LV_EVENT_COLS], p, na = "")
  invisible(p)
}


# The highest sequence number already used in an event directory. Zero when
# empty. Parsed from the filename rather than counted, so gaps are harmless.
lv_store_max_seq <- function(dir) {
  f <- basename(lv_store_event_files(dir))
  if (!length(f)) return(0L)
  n <- suppressWarnings(as.integer(sub("^([0-9]+)_.*$", "\\1", f)))
  n <- n[!is.na(n)]
  if (!length(n)) length(f) else max(n)
}

# Event files, in append order. Both extensions: everything written before the
# switch to gzip is plain .csv and stays readable, and the zero-padded sequence
# prefix orders the two kinds together correctly.
lv_store_event_files <- function(dir) {
  if (!fs::dir_exists(dir)) return(character())
  f <- fs::dir_ls(dir, regexp = "[.]csv([.]gz)?$", type = "file")
  sort(f)
}

#' Read a compilation\'s whole event log
#' @param store A store.
#' @param compilation Compilation name.
#' @return An event tibble, oldest first.
#' @export
qc_store_events <- function(store, compilation) {
  d <- store_dir(store, compilation, "events")
  if (!fs::dir_exists(d)) return(qc_events_empty())
  f <- lv_store_event_files(d)
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

#' Compress a store's existing event logs
#'
#' Events are long-format text and compress about tenfold. The hydroclimate2k log
#' reached 50 MB in a single file, which git stores whole on every change and
#' which GitHub warns about; new appends are gzipped, and this brings the
#' existing ones over.
#'
#' The property worth checking is that the materialised state does not move.
#' Compression is lossless, so it should not, and this verifies it per
#' compilation before deleting anything: the plain file is only removed once the
#' gzipped one reads back to a byte-identical state hash.
#'
#' @param store From [qc_store()].
#' @param compilations Which to compress. Default all.
#' @param dry_run Report without writing. Defaults to `TRUE`.
#' @return A tibble of what was done, invisibly.
#' @export
qc_store_compress <- function(store, compilations = NULL, dry_run = TRUE) {
  comps <- compilations %||% {
    d <- fs::path(store$path, "compilations")
    if (fs::dir_exists(d)) basename(fs::dir_ls(d, type = "directory")) else character()
  }
  out <- list()
  for (comp in comps) {
    d <- store_dir(store, comp, "events")
    if (!fs::dir_exists(d)) next
    plain <- fs::dir_ls(d, regexp = "[.]csv$", type = "file")
    if (!length(plain)) next

    before_hash <- lv_state_hash(qc_state_current(store, comp))
    before_size <- sum(fs::file_size(plain))

    if (dry_run) {
      out[[comp]] <- tibble::tibble(compilation = comp, files = length(plain),
                                    before = before_size, after = NA_real_,
                                    verified = NA)
      cli::cli_alert_info("{comp}: {length(plain)} plain file{?s}, {prettyunits::pretty_bytes(as.numeric(before_size))}")
      next
    }

    for (f in plain) {
      x <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                           na = "", progress = FALSE)
      readr::write_csv(x, paste0(f, ".gz"), na = "")
    }
    # Verify before removing anything. If the state moved, put it back.
    after_hash <- lv_state_hash(qc_state_current(store, comp))
    if (!identical(before_hash, after_hash)) {
      for (f in plain) fs::file_delete(paste0(f, ".gz"))
      cli::cli_abort(c("{comp}: state changed after compressing; rolled back.",
                       i = "before {before_hash}, after {after_hash}"),
                     class = "lv_error_store")
    }
    for (f in plain) fs::file_delete(f)
    after_size <- sum(fs::file_size(fs::dir_ls(d, regexp = "[.]csv[.]gz$", type = "file")))
    cli::cli_alert_success(
      "{comp}: {length(plain)} file{?s}, {prettyunits::pretty_bytes(as.numeric(before_size))} -> {prettyunits::pretty_bytes(as.numeric(after_size))}")
    out[[comp]] <- tibble::tibble(compilation = comp, files = length(plain),
                                  before = before_size, after = after_size, verified = TRUE)
  }
  res <- dplyr::bind_rows(out)
  if (!nrow(res)) cli::cli_alert_success("Nothing to compress.")
  invisible(res)
}

# A stable fingerprint of materialised state, for checking that a change to how
# events are stored has not changed what they mean.
lv_state_hash <- function(state) {
  if (!nrow(state)) return("empty")
  o <- order(state$tsid, state$field)
  digest::digest(paste(state$tsid[o], state$field[o], state$value[o], state$present[o]))
}
