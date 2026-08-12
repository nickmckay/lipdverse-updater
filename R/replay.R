#' Replay the recorded version history through the merge
#'
#' The correctness gate that needs no legacy run. Every published version of
#' every compilation left two machine-readable records beside its website:
#'
#' \describe{
#'   \item{`qcGoog.csv`}{the QC sheet as it was pulled for that run}
#'   \item{`qcTs.csv`}{the state that run resolved to, which is what reached the
#'     files}
#' }
#'
#' So each consecutive pair of versions is a merge with its inputs and its answer
#' both recorded: replay `qcGoog` of version N against `qcTs` of version N-1, and
#' compare what [qc_merge()] produces against `qcTs` of version N. 178 such
#' version-runs exist across 23 compilations, hydroclimate2k contributing 17.
#'
#' Divergence is the point, not the failure condition. lipdverseR lost curated
#' values through `daff`'s NA-as-deletion, let a stale sheet overwrite derived
#' fields, and wrote conflict markers into values; this rewrite deliberately does
#' none of those. Those cases should show up here as *intended* divergence, which
#' is a positive result. What matters is that every divergence has a reason.
#'
#' @section What the file side is:
#' The files at the time of a run are not recorded, but they do not need to be:
#' the previous run wrote its resolved state into them, so `qcTs` of version N-1
#' is both the baseline and the file view. The replay therefore exercises the
#' rules that decide between a curator edit and an unchanged cell, which is where
#' the losses happened.
#'
#' @name replay
NULL

#' Version directories that can be replayed
#'
#' @param compilation Compilation name.
#' @param root Directory of published compilation websites.
#' @return A tibble of `version` and `path`, oldest first.
#' @export
lv_replay_versions <- function(compilation, root = lv_replay_root()) {
  d <- fs::path(root, compilation)
  if (!fs::dir_exists(d)) return(tibble::tibble(version = character(), path = character()))
  dirs <- fs::dir_ls(d, type = "directory")
  keep <- vapply(dirs, function(p) {
    fs::file_exists(fs::path(p, "qcTs.csv")) && fs::file_exists(fs::path(p, "qcGoog.csv"))
  }, logical(1))
  dirs <- dirs[keep]
  # `current_version` is a copy of the newest release, not a release of its own;
  # replaying it would compare the last version against itself.
  dirs <- dirs[grepl("^[0-9]+_[0-9]+_[0-9]+$", fs::path_file(dirs))]
  if (!length(dirs)) return(tibble::tibble(version = character(), path = character()))
  v <- fs::path_file(dirs)
  # A_B_C sorts numerically, not lexically: 0_2_10 follows 0_2_9.
  ord <- order(numeric_version(gsub("_", ".", v), strict = FALSE))
  tibble::tibble(version = v[ord], path = as.character(dirs[ord]))
}

#' @rdname lv_replay_versions
#' @export
lv_replay_root <- function() {
  path.expand(Sys.getenv("LIPDVERSE_HTML", "~/Dropbox/lipdverse/html"))
}

#' Read a recorded wide QC table into cells
#'
#' `qcTs.csv` and `qcGoog.csv` are the wide sheet layout, so they melt the same
#' way the live sheet does and resolve through the same registry.
#'
#' @param path CSV path.
#' @param registry Field registry.
#' @return A cell table.
#' @export
lv_replay_cells <- function(path, registry = lv_qc_fields()) {
  raw <- suppressWarnings(readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                                          progress = FALSE))
  raw <- raw[, !duplicated(names(raw)) & nzchar(names(raw)) & !is.na(names(raw)), drop = FALSE]
  if (!"TSid" %in% names(raw)) return(qc_cells_empty())
  raw <- raw[!is.na(raw$TSid) & nzchar(raw$TSid), , drop = FALSE]
  value_cols <- setdiff(names(raw), c("TSid", "datasetId"))
  if (!length(value_cols)) return(qc_cells_empty())

  long <- tidyr::pivot_longer(raw[, c("TSid", value_cols), drop = FALSE],
                              dplyr::all_of(value_cols),
                              names_to = "field", values_to = "value")
  long$field <- lv_canonical_field(long$field, registry)
  long <- long[!is.na(long$value) & nzchar(long$value), , drop = FALSE]
  # One row per cell: a wide table can repeat a display name that resolves to
  # the same canonical field.
  long <- long[!duplicated(paste(long$TSid, long$field)), , drop = FALSE]
  tibble::tibble(tsid = long$TSid, field = long$field, value = long$value,
                 present = TRUE, dataset_id = NA_character_,
                 updated_at = NA_character_, source = "replay", actor = NA_character_)
}

#' Replay one version against the one before it
#'
#' @param from,to Version directories, older and newer.
#' @param registry Field registry.
#' @return A list of `summary` (one row) and `divergences` (a cell table).
#' @export
lv_replay_step <- function(from, to, registry = lv_qc_fields()) {
  base <- lv_replay_cells(fs::path(from, "qcTs.csv"), registry)
  sheet <- lv_replay_cells(fs::path(to, "qcGoog.csv"), registry)
  expected <- lv_replay_cells(fs::path(to, "qcTs.csv"), registry)

  # The previous run wrote its state into the files, so it is the file view too.
  plan <- qc_merge(base, sheet, base, registry = registry,
                   policy = qc_merge_policy(strict = FALSE))
  got <- qc_plan_state(plan)

  key <- function(x) paste(x$tsid, x$field, sep = "\r")
  cmp <- dplyr::full_join(
    tibble::tibble(k = key(got), tsid_a = got$tsid, field_a = got$field, ours = got$value),
    tibble::tibble(k = key(expected), tsid_b = expected$tsid, field_b = expected$field,
                   theirs = expected$value),
    by = "k")
  # Identify the cell from whichever side has it. Taking tsid and field from the
  # `got` side alone left them NA for every cell only the legacy run had, and
  # those then classified as "field not in the registry" -- 636,099 of them on
  # hydroclimate2k, which is a bug in the comparison rather than a finding.
  cmp$tsid <- dplyr::coalesce(cmp$tsid_a, cmp$tsid_b)
  cmp$field <- dplyr::coalesce(cmp$field_a, cmp$field_b)
  cmp <- cmp[!(is.na(cmp$ours) & is.na(cmp$theirs)), , drop = FALSE]

  # Scope to the timeseries the target version actually holds. The store carries
  # every cell it has ever seen, by design, while qcTs holds only the current
  # membership -- so a dataset dropped from the compilation appears as a value
  # the legacy run "lost", which is the replay comparing two different questions.
  keep_ts <- unique(expected$tsid)
  if (length(keep_ts)) cmp <- cmp[cmp$tsid %in% keep_ts, , drop = FALSE]
  cmp$agree <- values_equal(cmp$ours, cmp$theirs)

  # A cell the legacy run has that neither the baseline nor the sheet carries can
  # only have come from the LiPD files, and the replay has no file view: qcTs of
  # the previous version stands in for it, which by construction cannot contain a
  # dataset that version did not hold. So every dataset added in a run appears
  # this way. Naming it keeps the output honest -- these are not disagreements.
  had <- unique(c(paste(base$tsid, base$field, sep = "\r"),
                  paste(sheet$tsid, sheet$field, sep = "\r")))
  cmp$only_in_files <- !cmp$k %in% had & is.na(cmp$ours)

  rule <- lv_field_rule(cmp$field, registry)
  cmp$ownership <- rule$ownership
  cmp$known <- rule$known
  cmp$class <- dplyr::case_when(
    cmp$agree                              ~ "agree",
    !cmp$known %in% TRUE                   ~ "field not in the registry",
    is.na(cmp$theirs)                      ~ "we keep a value the legacy run dropped",
    cmp$only_in_files                      ~ "arrived via the files, which the replay cannot see",
    is.na(cmp$ours)                        ~ "legacy has a value we do not",
    # Inherent to the replay rather than a disagreement: a machine field is
    # derived from the data each run, and the replay has only the recorded
    # tables, so ours keeps the previous value where the legacy run recomputed
    # one. lv_calculate() is what closes this on a live run.
    cmp$ownership %in% "machine"           ~ "machine-owned, recomputed by the legacy run",
    TRUE                                   ~ "different value")

  list(
    summary = tibble::tibble(
      from = fs::path_file(from), to = fs::path_file(to),
      n_base = nrow(base), n_sheet = nrow(sheet), n_expected = nrow(expected),
      n_compared = nrow(cmp), n_agree = sum(cmp$agree, na.rm = TRUE),
      n_differ = sum(!cmp$agree, na.rm = TRUE),
      pct_agree = if (nrow(cmp)) round(100 * sum(cmp$agree, na.rm = TRUE) / nrow(cmp), 2) else NA_real_,
      n_conflicts = nrow(plan$conflicts)),
    divergences = cmp[!cmp$agree %in% TRUE, c("tsid", "field", "ours", "theirs", "ownership", "class"),
                      drop = FALSE])
}

#' Replay a compilation's whole recorded history
#'
#' @param compilation Compilation name.
#' @param root Directory of published compilation websites.
#' @param registry Field registry.
#' @param progress Show progress.
#' @return A list of `summary` (one row per version-run) and `divergences`.
#' @export
lv_replay <- function(compilation, root = lv_replay_root(), registry = lv_qc_fields(),
                      progress = TRUE) {
  v <- lv_replay_versions(compilation, root)
  if (nrow(v) < 2) {
    return(list(summary = tibble::tibble(), divergences = tibble::tibble()))
  }
  if (progress) {
    cli::cli_alert_info("Replaying {nrow(v) - 1} version-run{?s} of {compilation}")
  }
  steps <- lapply(seq_len(nrow(v))[-1], function(i) {
    s <- lv_replay_step(v$path[i - 1], v$path[i], registry)
    s$divergences$from <- v$version[i - 1]
    s$divergences$to <- v$version[i]
    s
  })
  list(summary = dplyr::bind_rows(lapply(steps, `[[`, "summary")),
       divergences = dplyr::bind_rows(lapply(steps, `[[`, "divergences")))
}
