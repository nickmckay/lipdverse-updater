#' Run lipdverseR's pipeline against an isolated copy of the database
#'
#' Runs in a separate R session via `callr`, because lipdverseR calls
#' `setwd()`, assigns into the global environment with `<<-`, and caches Google
#' credentials relative to its working directory. None of that may leak into
#' the session driving the comparison.
#'
#' **This writes to Google Sheets** for the compilation it is given: the legacy
#' pipeline pushes merged QC back as part of a normal run. Use a throwaway QC
#' sheet, or accept that the real one will be written.
#'
#' Not exercised by the test suite — it needs a full lipdverseR install, a live
#' Google session and hours of runtime. Treat the first use as exploratory.
#'
#' @param compilation Compilation name.
#' @param lipd_dir Database copy to operate on. Must not be the live database.
#' @param lipdverse_r Path to the lipdverseR source.
#' @param web_dir Output directory for the legacy web stages.
#' @param timeout Seconds before the child session is killed.
#' @param allow_live Set `TRUE` to permit running against the real database.
#' @return A list with the child's status and log path.
#' @export
shadow_run_legacy <- function(compilation,
                              lipd_dir,
                              lipdverse_r = "~/GitHub/lipdverseR",
                              web_dir = fs::path(tempdir(), "shadow-web"),
                              timeout = 6 * 3600,
                              allow_live = FALSE) {
  lipd_dir <- path.expand(lipd_dir)
  live <- path.expand(lv_path("database"))
  if (!allow_live && fs::path_norm(lipd_dir) == fs::path_norm(live)) {
    cli::cli_abort(c(
      "Refusing to run the legacy pipeline against the live database.",
      i = "Copy it first with {.fn shadow_snapshot}, or pass {.code allow_live = TRUE}."
    ), class = "lv_error_shadow")
  }
  if (!fs::dir_exists(lipd_dir)) {
    cli::cli_abort("{.path {lipd_dir}} does not exist.", class = "lv_error_shadow")
  }
  lipdverse_r <- path.expand(lipdverse_r)
  if (!fs::dir_exists(lipdverse_r)) {
    cli::cli_abort("lipdverseR source not found at {.path {lipdverse_r}}.", class = "lv_error_shadow")
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg callr} is required for shadow runs.")
  }

  reg <- lv_compilations()
  row <- reg[reg$compilation == compilation, ]
  if (nrow(row) != 1) cli::cli_abort("Unknown compilation {.val {compilation}}.")

  fs::dir_create(web_dir)
  log <- fs::path(web_dir, paste0("legacy-", compilation, ".log"))

  cli::cli_alert_info("Legacy run: {compilation} on {.path {lipd_dir}} (log {.path {log}})")

  res <- callr::r_safe(
    function(pkg_dir, compilation, lipd_dir, web_dir, qc_id, last_update_id) {
      setwd(pkg_dir)
      devtools::load_all(pkg_dir, quiet = TRUE)
      params <- buildParams(compilation, lipd_dir, web_dir,
                            qcId = qc_id, lastUpdateId = last_update_id,
                            googEmail = Sys.getenv("LIPDVERSE_GOOG_EMAIL", "nick.mckay2@gmail.com"),
                            updateWebpages = FALSE, standardizeTerms = FALSE, serialize = FALSE)
      d <- loadInUpdatedData(params)
      d <- getQcInfo(params, d)
      d <- standardizeQCInfo(params, d)
      d <- createQcFromFile(params, d)
      d <- mergeQcSheets(params, d)
      d <- updateTsFromMergedQc(params, d)
      "ok"
    },
    args = list(pkg_dir = lipdverse_r, compilation = compilation, lipd_dir = lipd_dir,
                web_dir = as.character(web_dir), qc_id = row$qc_sheet_id,
                last_update_id = row$last_update_id),
    stdout = log, stderr = "2>&1", timeout = timeout * 1000, error = "stack"
  )

  list(status = res, log = log, lipd_dir = lipd_dir)
}

#' Compare two database directories end to end
#'
#' @param old,new Directories to compare.
#' @param out Optional CSV path for the full diff.
#' @param ignore Ignore patterns, see [shadow_ignore()].
#' @return The diff tibble, invisibly.
#' @export
shadow_compare <- function(old, new, out = NULL, ignore = shadow_ignore()) {
  a <- shadow_normalize(old, ignore = ignore)
  b <- shadow_normalize(new, ignore = ignore)
  d <- shadow_diff(a, b)
  shadow_report(d, out)
  invisible(d)
}
