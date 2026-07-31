#' The QC field registry
#'
#' One row per QC sheet column, recording how the merge engine should treat it.
#' Built from the reviewed decisions in `review/` by
#' `data-raw/build_qc_fields.R`; edit those and rebuild rather than editing the
#' registry directly.
#'
#' `role` says what kind of field it is:
#'
#' \describe{
#'   \item{`merged`}{Participates in the three-way merge under `ownership`.}
#'   \item{`csm`}{Compilation-specific; lives under `csm` in the matching
#'     `inCompilation` entry, not in the shared namespace.}
#'   \item{`csm_pending`}{Destined for `csm`, target compilation not yet decided.}
#'   \item{`key`}{Identifier. Never merged; disagreement is an error.}
#'   \item{`synonym`}{An alias; `canonical` names the field it resolves to.}
#'   \item{`control`}{An instruction column consumed by the pipeline.}
#'   \item{`unused`}{Present in a sheet but never populated.}
#'   \item{`delete`}{To be removed from the files.}
#' }
#'
#' For `merged` fields, `ownership` decides who wins a disagreement:
#' `curator` (the sheet), `machine` (the file), `shared` (a real conflict,
#' retain base and report), or `key`.
#'
#' @param validate Check the registry before returning it.
#' @return A tibble.
#' @export
lv_qc_fields <- function(validate = TRUE) {
  p <- lv_extdata("qc_fields.csv")
  x <- readr::read_csv(p, col_types = readr::cols(
    .default = readr::col_character(),
    deprecated = readr::col_logical(),
    n_compilations = readr::col_integer(),
    n_filled = readr::col_integer()
  ), progress = FALSE)
  if (validate) validate_qc_fields(x)
  x
}

#' Validate the field registry
#'
#' Errors rather than warns: the merge engine cannot behave correctly against a
#' malformed registry, and a wrong `ownership` silently changes which side wins
#' a disagreement.
#'
#' @param x A registry tibble.
#' @return `x`, invisibly.
#' @export
validate_qc_fields <- function(x) {
  req <- c("qc_name", "ts_name", "family", "role", "ownership", "nullable_by_curator",
           "cardinality", "type", "vocab_key", "canonical", "csm_compilation",
           "csm_field", "csm_flat_key", "deprecated", "n_compilations", "n_filled")
  miss <- setdiff(req, names(x))
  if (length(miss)) {
    cli::cli_abort("Registry is missing column{?s}: {.field {miss}}", class = "lv_error_registry")
  }

  dup <- x$qc_name[duplicated(x$qc_name)]
  if (length(dup)) {
    cli::cli_abort("Duplicate qc_name in registry: {.val {unique(dup)}}", class = "lv_error_registry")
  }

  roles <- c("merged", "csm", "csm_pending", "key", "synonym", "control", "unused", "delete")
  bad <- setdiff(unique(stats::na.omit(x$role)), roles)
  if (length(bad)) {
    cli::cli_abort("Unknown role{?s}: {.val {bad}}", class = "lv_error_registry")
  }

  merged <- x[x$role == "merged", ]
  ok <- c("curator", "machine", "shared", "key")
  bad_own <- merged$qc_name[!merged$ownership %in% ok]
  if (length(bad_own)) {
    cli::cli_abort(c(
      "{length(bad_own)} merged field{?s} without a usable ownership rule.",
      i = "{.val {utils::head(bad_own, 8)}}",
      i = "Set it in {.path review/qc-field-ownership.csv} and rebuild."
    ), class = "lv_error_registry")
  }

  bad_null <- merged$qc_name[!merged$nullable_by_curator %in% c("TRUE", "FALSE")]
  if (length(bad_null)) {
    cli::cli_abort("{length(bad_null)} merged field{?s} without nullable_by_curator: {.val {utils::head(bad_null, 8)}}",
                   class = "lv_error_registry")
  }

  # Only curator-owned fields may be cleared by a blank cell. Anywhere else a
  # blank must mean "unchanged", which is what prevents the NA-as-deletion loss.
  wrong <- merged$qc_name[merged$nullable_by_curator == "TRUE" & merged$ownership != "curator"]
  if (length(wrong)) {
    cli::cli_abort(c("Only curator-owned fields may be nullable: {.val {wrong}}"),
                   class = "lv_error_registry")
  }

  syn <- x[x$role == "synonym", ]
  orphan <- syn$qc_name[!is.na(syn$canonical) & !syn$canonical %in% x$qc_name]
  if (length(orphan)) {
    cli::cli_warn("Synonym{?s} whose canonical is absent from the registry: {.val {utils::head(orphan, 8)}}")
  }

  csm <- x[x$role == "csm", ]
  bad_csm <- csm$qc_name[is.na(csm$csm_flat_key) | !grepl("^[A-Za-z0-9]+_csm_", csm$csm_flat_key)]
  if (length(bad_csm)) {
    cli::cli_abort("csm field{?s} with a malformed flat key: {.val {bad_csm}}", class = "lv_error_registry")
  }

  invisible(x)
}

#' Look up the merge rule for a field
#'
#' @param field One or more QC column names.
#' @param registry The registry; read once and pass in for repeated lookups.
#' @return A tibble of `qc_name`, `role`, `ownership`, `nullable_by_curator`.
#' @export
lv_field_rule <- function(field, registry = lv_qc_fields()) {
  i <- match(field, registry$qc_name)
  tibble::tibble(
    qc_name = field,
    role = registry$role[i],
    ownership = registry$ownership[i],
    nullable_by_curator = registry$nullable_by_curator[i] == "TRUE",
    known = !is.na(i)
  )
}

#' Resolve field names to their canonical form
#'
#' The QC sheet uses display names (`lat`, `basis`, `QC Certification`) while
#' the files use canonical ones (`geo_latitude`,
#' `climateInterpretation1_basis`, `paleoData_QCCertification`). Both sides are
#' normalised to canonical before merging, so a rename in a sheet header cannot
#' look like a different field.
#'
#' @param field Field names as they appear in a sheet or file.
#' @param registry The registry.
#' @return Canonical names; unknown fields are returned unchanged.
#' @export
lv_canonical_field <- function(field, registry = lv_qc_fields()) {
  i <- match(field, registry$qc_name)
  out <- field
  syn <- !is.na(i) & registry$role[i] == "synonym" & !is.na(registry$canonical[i])
  out[syn] <- registry$canonical[i][syn]
  out
}

#' The display name a canonical field is written back as
#'
#' The inverse of [lv_canonical_field()], for pushing state to a QC sheet.
#' Where several display names map to one canonical field, the most widely used
#' one wins.
#'
#' @param field Canonical field names.
#' @param registry The registry.
#' @return Display names; fields with no alias are returned unchanged.
#' @export
lv_display_field <- function(field, registry = lv_qc_fields()) {
  syn <- registry[registry$role == "synonym" & !is.na(registry$canonical), ]
  if (nrow(syn) == 0) return(field)
  syn <- syn[order(-dplyr::coalesce(syn$n_filled, 0L)), ]
  syn <- syn[!duplicated(syn$canonical), ]
  i <- match(field, syn$canonical)
  ifelse(is.na(i), field, syn$qc_name[i])
}
