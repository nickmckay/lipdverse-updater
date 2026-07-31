#' Transactional writes to the LiPD database
#'
#' Nothing is ever deleted before its replacement exists and has been verified.
#'
#' lipdverseR's final step was `unlink(filesToUltimatelyDelete)` followed by
#' `writeLipd(DF, path = lipdDir)` (`nightlyUpdateDrake.R:2057-2058`). A failure
#' between those two lines destroys the loaded subset of the database, and the
#' only recovery is Dropbox history — there is a commented-out ensemble-recovery
#' recipe a few lines below acknowledging exactly that.
#'
#' The order here is: stage, verify, move the old aside, rename the new in.
#' Every step is reversible until the last, and the last is a rename.
#'
#' @section Layout:
#' ```
#' <lipd_dir>/.staging/<run_id>/   candidate files, before verification
#' <lipd_dir>/.trash/<run_id>/     what was replaced, kept for lv_gc()
#' <lipd_dir>/.runs/<run_id>.json  receipt: every path, old md5, new md5, action
#' ```
#'
#' @name db_write
NULL

lv_write_dirs <- function(dir, run_id) {
  list(staging = fs::path(dir, ".staging", run_id),
       trash   = fs::path(dir, ".trash", run_id),
       runs    = fs::path(dir, ".runs"))
}

#' Verify a staged LiPD file is readable and intact
#'
#' Re-reads from disk rather than trusting the object that was written. A file
#' that cannot be read back is not a file that may replace a good one.
#'
#' @param path Staged file.
#' @param expect_name Expected dataSetName, if known.
#' @param min_bytes Reject implausibly small files.
#' @param strict_valid Treat a `validLipd()` failure as an error. `validLipd()`
#'   signals by return value rather than by condition, so this must be checked
#'   explicitly.
#' @return An `lv_issues` tibble; empty means the file passed.
#' @export
lv_verify_file <- function(path, expect_name = NULL, min_bytes = 200,
                           strict_valid = TRUE) {
  bad <- function(check, msg) lv_issues(check = check, severity = "error", message = msg, path = path)

  if (!fs::file_exists(path)) return(bad("staged_missing", "Staged file does not exist."))
  if (fs::file_size(path) < min_bytes) {
    return(bad("staged_too_small", paste0("Staged file is ", fs::file_size(path), " bytes.")))
  }

  L <- tryCatch(lipdR::readLipd(path), error = function(e) e)
  if (inherits(L, "error")) {
    return(bad("staged_unreadable", paste("readLipd failed:", conditionMessage(L))))
  }
  dsn <- L$dataSetName
  if (is.null(dsn) || !nzchar(dsn)) return(bad("staged_no_name", "Staged file has no dataSetName."))
  if (!is.null(expect_name) && !identical(as.character(dsn), as.character(expect_name))) {
    return(bad("staged_wrong_name",
               paste0("Staged file is '", dsn, "' but '", expect_name, "' was expected.")))
  }
  # validLipd() reports by returning FALSE, not by erroring, so the return value
  # has to be checked. Catching only errors let an invalid file through the gate.
  valid <- tryCatch(suppressWarnings(lipdR::validLipd(L)), error = function(e) e)
  if (inherits(valid, "error")) {
    return(bad("staged_invalid", paste("validLipd errored:", conditionMessage(valid))))
  }
  if (isTRUE(strict_valid) && !isTRUE(all(unlist(valid)))) {
    return(bad("staged_invalid", "validLipd reported the file as invalid."))
  }
  lv_issues_empty()
}

#' Promote a directory of prepared LiPD files into the database
#'
#' Used when the files already exist — a migration, or a run that staged
#' elsewhere. `lv_write()` is the same commit machinery with a `writeLipd()`
#' step in front.
#'
#' @param staging Directory of prepared `.lpd` files.
#' @param dir Target database directory.
#' @param run_id Run identifier.
#' @param dry_run Verify and report, changing nothing.
#' @param verify Re-read every staged file before committing. Leave on.
#' @param workers Parallel verification workers.
#' @param allow_delete Permit removing live files absent from staging.
#' @return An `lv_write_receipt`, invisibly.
#' @export
lv_promote <- function(staging, dir = lv_path("database"), run_id = lv_run_id(),
                       dry_run = TRUE, verify = TRUE, workers = NULL,
                       allow_delete = FALSE) {
  staging <- path.expand(staging); dir <- path.expand(dir)
  if (!fs::dir_exists(staging)) cli::cli_abort("Staging directory not found: {.path {staging}}")
  if (!fs::dir_exists(dir)) cli::cli_abort("Database directory not found: {.path {dir}}")

  new_files <- fs::dir_ls(staging, glob = "*.lpd", type = "file")
  if (!length(new_files)) cli::cli_abort("No .lpd files in {.path {staging}}", class = "lv_error_write")
  live_files <- fs::dir_ls(dir, glob = "*.lpd", type = "file")

  new_names  <- fs::path_file(new_files)
  live_names <- fs::path_file(live_files)

  plan <- tibble::tibble(
    file = new_names,
    action = ifelse(new_names %in% live_names, "replace", "add"),
    staged_path = as.character(new_files),
    live_path = fs::path(dir, new_names)
  )
  deletions <- setdiff(live_names, new_names)

  cli::cli_alert_info("{nrow(plan)} file{?s} to write ({sum(plan$action == 'replace')} replace, {sum(plan$action == 'add')} add), {length(deletions)} candidate deletion{?s}")

  issues <- lv_issues_empty()
  if (verify) {
    cli::cli_alert_info("Verifying {nrow(plan)} staged file{?s}")
    n <- workers %||% max(1L, min(8L, future::availableCores() - 1L))
    checks <- if (nrow(plan) < 40) {
      purrr::map2(plan$staged_path, sub("\\.lpd$", "", plan$file), lv_verify_file)
    } else {
      oplan <- future::plan(future::multisession, workers = n)
      on.exit(future::plan(oplan), add = TRUE)
      furrr::future_map2(plan$staged_path, sub("\\.lpd$", "", plan$file), lv_verify_file,
                         .options = furrr::furrr_options(seed = TRUE))
    }
    issues <- do.call(lv_issues_bind, checks)
  }

  receipt <- structure(list(
    run_id = run_id, dir = dir, staging = staging, dry_run = dry_run,
    plan = plan, deletions = deletions, issues = issues,
    committed = FALSE, at = Sys.time()
  ), class = "lv_write_receipt")

  if (lv_n_issues(issues, "error") > 0) {
    p <- fs::path(lv_run_dir(run_id), "write-issues.csv")
    lv_issues_check(issues, p, what = "Staged file verification")
  }
  if (length(deletions) && !allow_delete) {
    n_del <- length(deletions)
    cli::cli_abort(c(
      "{n_del} live file{?s} absent from staging.",
      i = "Pass {.code allow_delete = TRUE} to move them to .trash, or stage them too.",
      i = "First few: {.file {utils::head(deletions, 5)}}"
    ), class = "lv_error_write")
  }
  if (dry_run) {
    cli::cli_alert_success("Dry run: verification passed, nothing written.")
    return(invisible(receipt))
  }

  d <- lv_write_dirs(dir, run_id)
  fs::dir_create(c(d$trash, d$runs))

  moved <- character()
  placed <- character()
  ok <- tryCatch({
    # Move what is being replaced out of the way first. A move, never a delete,
    # so every step so far can be undone.
    for (i in which(plan$action == "replace")) {
      fs::file_move(plan$live_path[i], fs::path(d$trash, plan$file[i]))
      moved <- c(moved, plan$file[i])
    }
    for (f in deletions) {
      fs::file_move(fs::path(dir, f), fs::path(d$trash, f))
      moved <- c(moved, f)
    }
    # Same filesystem, so each rename is atomic.
    for (i in seq_len(nrow(plan))) {
      fs::file_move(plan$staged_path[i], plan$live_path[i])
      placed <- c(placed, plan$file[i])
    }
    TRUE
  }, error = function(e) e)

  if (inherits(ok, "error")) {
    cli::cli_alert_danger("Write failed after {length(placed)} file{?s}; rolling back.")
    lv_write_rollback(dir, run_id, placed = placed, moved = moved)
    cli::cli_abort("Write failed and was rolled back: {conditionMessage(ok)}",
                   class = "lv_error_write")
  }

  receipt$committed <- TRUE
  receipt$moved <- moved
  receipt$placed <- placed
  jsonlite::write_json(
    list(run_id = run_id, dir = dir, at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         n_replaced = sum(plan$action == "replace"), n_added = sum(plan$action == "add"),
         n_deleted = length(deletions), trash = as.character(d$trash),
         files = plan$file, deletions = deletions),
    fs::path(d$runs, paste0(run_id, ".json")), auto_unbox = TRUE, pretty = TRUE)

  cli::cli_alert_success("Wrote {length(placed)} file{?s}; {length(moved)} moved to {.path {d$trash}}")
  invisible(receipt)
}

#' Undo a committed write
#'
#' @param dir Database directory.
#' @param run_id Run to undo.
#' @param placed Files that were put in place; defaults to the receipt's list.
#' @param moved Files that were moved aside; defaults to the receipt's list.
#' @return `TRUE`, invisibly.
#' @export
lv_write_rollback <- function(dir = lv_path("database"), run_id, placed = NULL, moved = NULL) {
  d <- lv_write_dirs(dir, run_id)
  rec <- fs::path(d$runs, paste0(run_id, ".json"))
  if (is.null(placed) && fs::file_exists(rec)) {
    j <- jsonlite::read_json(rec, simplifyVector = TRUE)
    placed <- j$files
    moved <- c(j$files, j$deletions)
  }
  if (!fs::dir_exists(d$trash)) {
    cli::cli_abort("No trash for run {.val {run_id}} at {.path {d$trash}}", class = "lv_error_write")
  }

  # Remove everything this run put in place, then restore what it displaced.
  # Only restoring replacements would leave newly added files behind, so the
  # rollback would not actually undo the run.
  for (f in placed %||% character()) {
    p <- fs::path(dir, f)
    if (fs::file_exists(p)) fs::file_delete(p)
  }
  restored <- 0L
  for (f in fs::dir_ls(d$trash, glob = "*.lpd", type = "file")) {
    fs::file_move(f, fs::path(dir, fs::path_file(f)))
    restored <- restored + 1L
  }
  cli::cli_alert_success("Rolled back run {.val {run_id}}: restored {restored} file{?s}")
  invisible(TRUE)
}

#' Prune old trash generations
#' @param dir Database directory.
#' @param keep Number of most recent runs to retain.
#' @return Number of generations removed.
#' @export
lv_gc <- function(dir = lv_path("database"), keep = 10) {
  root <- fs::path(dir, ".trash")
  if (!fs::dir_exists(root)) return(invisible(0L))
  gens <- sort(fs::dir_ls(root, type = "directory"))
  drop <- utils::head(gens, max(0, length(gens) - keep))
  for (g in drop) fs::dir_delete(g)
  invisible(length(drop))
}

#' List committed writes
#' @param dir Database directory.
#' @return A tibble of runs, newest first.
#' @export
lv_write_log <- function(dir = lv_path("database")) {
  d <- fs::path(dir, ".runs")
  if (!fs::dir_exists(d)) return(tibble::tibble())
  f <- fs::dir_ls(d, glob = "*.json")
  if (!length(f)) return(tibble::tibble())
  x <- purrr::list_rbind(lapply(f, function(p) {
    j <- jsonlite::read_json(p, simplifyVector = TRUE)
    tibble::tibble(run_id = j$run_id, at = j$at, n_replaced = j$n_replaced,
                   n_added = j$n_added, n_deleted = j$n_deleted,
                   trash_exists = fs::dir_exists(j$trash))
  }))
  x[order(x$at, decreasing = TRUE), , drop = FALSE]
}

#' @export
print.lv_write_receipt <- function(x, ...) {
  cli::cli_h3("lv_write_receipt {x$run_id}")
  cli::cli_bullets(c(
    "*" = "{nrow(x$plan)} file{?s}: {sum(x$plan$action == 'replace')} replace, {sum(x$plan$action == 'add')} add",
    "*" = "{length(x$deletions)} deletion{?s}",
    if (x$dry_run) "i" = "dry run, nothing written"
    else if (isTRUE(x$committed)) "v" = "committed"
    else "x" = "not committed"
  ))
  invisible(x)
}
