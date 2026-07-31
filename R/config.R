#' The compilation registry
#'
#' Identifiers and the database directory for every compilation, transcribed
#' from lipdverseR's `drakePlan.R`. That file defined ~19 near-identical
#' `drake_plan`s differing only in these values.
#'
#' @return A tibble, one row per compilation.
#' @export
lv_compilations <- function() {
  p <- lv_extdata("compilations.tsv")
  readr::read_tsv(p, col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE)
}

lv_extdata <- function(...) {
  p <- system.file("extdata", ..., package = "lipdverseUpdater")
  # devtools::load_all() does not always populate system.file() for inst/.
  if (!nzchar(p)) p <- file.path("inst", "extdata", ...)
  if (!file.exists(p)) cli::cli_abort("Missing package data file: {.path {paste(c(...), collapse='/')}}")
  p
}

#' Build the configuration for one compilation
#'
#' Layers, later winning: package defaults, the registry row, an optional
#' per-compilation YAML, then any `...` overrides.
#'
#' Replaces `buildParams()`, which captured its own arguments with
#' `ls()` + `map(eval(parse(text = .x)))` into an untyped list that every stage
#' then splatted into its local frame with `assign()`. Nothing validated the
#' contents and nothing declared what a stage required.
#'
#' @param compilation Compilation name.
#' @param ... Overrides applied last.
#' @return An `lv_config` object.
#' @export
lv_config <- function(compilation, ...) {
  reg <- lv_compilations()
  row <- reg[reg$compilation == compilation, ]
  if (nrow(row) == 0) {
    cli::cli_abort(c("Unknown compilation {.val {compilation}}.",
                     i = "Known: {.val {reg$compilation}}"), class = "lv_error_config")
  }
  if (nrow(row) > 1) {
    cli::cli_abort("Compilation {.val {compilation}} appears {nrow(row)} times in the registry.",
                   class = "lv_error_config")
  }

  cfg <- yaml::read_yaml(lv_extdata("defaults.yml"))
  cfg <- utils::modifyList(cfg, as.list(row)[!is.na(unlist(row))])

  per <- file.path(dirname(lv_extdata("defaults.yml")), "compilations", paste0(compilation, ".yml"))
  if (file.exists(per)) cfg <- utils::modifyList(cfg, yaml::read_yaml(per))

  cfg <- utils::modifyList(cfg, list(...))

  # lipd_dir is recorded relative to the lipdverse root because it is
  # per-compilation: GBRCD has its own database, and CoralHydro2k used to.
  cfg$lipd_dir <- if (grepl("^[/~]", cfg$lipd_dir)) {
    path.expand(cfg$lipd_dir)
  } else {
    file.path(dirname(lv_path("database")), cfg$lipd_dir)
  }

  cfg$run_id <- lv_run_id()
  validate_lv_config(structure(cfg, class = "lv_config"))
}

validate_lv_config <- function(cfg) {
  required <- c("compilation", "qc_sheet_id", "lipd_dir", "age_or_year",
                "membership", "cutover", "strict")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) {
    cli::cli_abort("Config for {.val {cfg$compilation}} is missing: {.field {missing}}",
                   class = "lv_error_config")
  }
  check_enum(cfg, "age_or_year", c("age", "year"))
  check_enum(cfg, "membership",  c("from_sheet", "from_qc"))
  check_enum(cfg, "cutover",     c("legacy", "shadow", "live"))
  if (!is.logical(cfg$strict) || length(cfg$strict) != 1) {
    cli::cli_abort("{.field strict} must be a single logical.", class = "lv_error_config")
  }
  if (!grepl("^[A-Za-z0-9_-]{20,}$", cfg$qc_sheet_id)) {
    cli::cli_abort("{.field qc_sheet_id} for {.val {cfg$compilation}} does not look like a Google Sheet id.",
                   class = "lv_error_config")
  }
  cfg
}

check_enum <- function(cfg, field, allowed) {
  v <- cfg[[field]]
  if (length(v) != 1 || !v %in% allowed) {
    cli::cli_abort("{.field {field}} must be one of {.val {allowed}}, not {.val {v}}.",
                   class = "lv_error_config")
  }
}

#' @export
print.lv_config <- function(x, ...) {
  cli::cli_h3("lv_config: {x$compilation}")
  cli::cli_bullets(c(
    "*" = "database  {.path {x$lipd_dir}}",
    "*" = "QC sheet  {.val {x$qc_sheet_id}}",
    "*" = "time      {x$age_or_year}",
    "*" = "cutover   {.strong {x$cutover}}{if (isTRUE(x$strict)) ' (strict)' else ''}",
    "*" = "run       {.val {x$run_id}}"
  ))
  invisible(x)
}

#' Configuration for every compilation
#'
#' Useful as a preflight check: it validates all of them at once.
#'
#' @param ... Passed to [lv_config()].
#' @return A named list of `lv_config`.
#' @export
lv_config_all <- function(...) {
  nms <- lv_compilations()$compilation
  stats::setNames(lapply(nms, lv_config, ...), nms)
}
