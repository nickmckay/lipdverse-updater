# Capture every certification-like value in the database before the shared QC
# field is split per compilation.
#
# For a decade all compilations shared one QC Certification field. The csm
# migration split it by copying the shared value into EVERY compilation's csm
# slot, which invents attribution rather than recovering it:
#
#   wNAm_csm_QCCertification                 = CR
#   HoloceneAbruptChange_csm_QCCertification = CR
#   HoloceneHydroclimate_csm_QCCertification = CR
#   hydroclimate2k_csm_QCCertification       = GF; CR
#
# CR certified that record once. GF is the hydroclimate2k lead curator and CR
# never curated hydroclimate2k, so h2k's true value is GF -- which is exactly
# what the h2k QC sheet says, unchanged since before this project's first run.
#
# Rebuilding each compilation's csm field from its own QC sheet therefore
# overwrites these strings. Nick's call (2026-08-12) is that the shared field's
# history is disposable, but the capture is cheap and it is the only way back if
# the per-compilation split turns out wrong somewhere. Written to the qcstore,
# which is private and git-tracked -- never a scratchpad.
#
#   Rscript scripts/snapshot-certification-history.R
suppressMessages(devtools::load_all(quiet = TRUE))
suppressPackageStartupMessages({library(dplyr); library(parallel)})

DIR   <- lv_path("database")
OUT   <- fs::path(lv_path("qcstore"), "snapshots/_shared/pre-split-certification")
CORES <- max(1L, detectCores() - 2L)

# Anything that could carry a curation credit: the csm fields the migration
# wrote, the legacy per-compilation flat fields, and the shared originals.
PAT <- "_csm_|Certification|certification|QCnotes|QCcomment|QCRemainingIssues"

paths <- fs::dir_ls(DIR, glob = "*.lpd", type = "file")
cat("files:", length(paths), "| cores:", CORES, "\n")

one <- function(p) {
  L <- try(suppressMessages(lipdR::readLipd(as.character(p))), silent = TRUE)
  if (inherits(L, "try-error")) return(NULL)
  ts <- try(suppressWarnings(lipdR::extractTs(L)), silent = TRUE)
  if (inherits(ts, "try-error") || !length(ts)) return(NULL)
  out <- list()
  for (e in ts) {
    k <- grep(PAT, names(e), value = TRUE)
    if (!length(k)) next
    v <- vapply(k, function(kk) paste(as.character(e[[kk]]), collapse = "|"), character(1))
    keep <- !is.na(v) & nzchar(v) & v != "NA"
    if (!any(keep)) next
    out[[length(out) + 1L]] <- tibble::tibble(
      dataSetName = as.character(e[["dataSetName"]] %||% NA),
      tsid = as.character(e[["paleoData_TSid"]] %||% NA),
      field = k[keep], value = unname(v[keep]))
  }
  if (!length(out)) NULL else dplyr::bind_rows(out)
}

t0 <- Sys.time()
res <- mclapply(paths, one, mc.cores = CORES)
# mclapply returns the error condition instead of throwing, so a dead worker
# would otherwise look like a file with nothing to capture.
bad <- !vapply(res, function(x) is.null(x) || is.data.frame(x), logical(1))
if (any(bad)) { print(fs::path_file(paths[bad])); stop("workers failed") }
d <- bind_rows(res)
cat("captured in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n\n")

cat("rows:", nrow(d), "| timeseries:", n_distinct(d$tsid),
    "| datasets:", n_distinct(d$dataSetName), "\n\n")
cat("by field family:\n")
fam <- d |> mutate(family = case_when(
  grepl("_csm_", field) ~ sub("_csm_.*", "", field),
  TRUE ~ "(legacy flat / shared)")) |> count(family, sort = TRUE)
print(as.data.frame(fam), right = FALSE)

cat("\nvalues carrying more than one curator (a ';' or ','):",
    sum(grepl("[;,]", d$value)), "\n")

fs::dir_create(OUT)
p <- fs::path(OUT, "certification-values.csv.gz")
readr::write_csv(d, p)
jsonlite::write_json(list(
  captured_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  database = as.character(DIR),
  db_fingerprint = lv_scan(DIR)$fingerprint,
  n_rows = nrow(d), n_timeseries = n_distinct(d$tsid),
  n_datasets = n_distinct(d$dataSetName),
  pattern = PAT,
  why = paste("Pre-split capture of the shared QC certification field.",
              "The csm migration copied one shared value into every compilation's",
              "csm slot; rebuilding each from its own QC sheet overwrites these.")),
  fs::path(OUT, "_meta.json"), auto_unbox = TRUE, pretty = TRUE)
cat("\nwritten:", as.character(p), "\n")
cat("commit it in", as.character(lv_path("qcstore")), "\n")
