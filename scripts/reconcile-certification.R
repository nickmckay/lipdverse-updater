#!/usr/bin/env Rscript
#
# Reconcile a compilation's certification across its three records, and write
# review/<compilation>-certification.csv.
#
#   ./scripts/reconcile-certification.R iso2k
#   ./scripts/reconcile-certification.R iso2k --force    # overwrite an edited file
#
# Reads only; writes one CSV. Generalised from the hydroclimate2k version, which
# is issue #11: the same reconciliation is owed to every compilation whose
# certification the csm migration touched.
#
# Three records of the same judgement, which disagree:
#
#   sheet   the compilation's QC tab -- the authority, since it is where the
#           certifying was done
#   csm     inCompilation[<compilation>].csm.QCCertification in the files
#   flat    the legacy private key still on a column, where the migration could
#           not place it
#
# The migration copied the shared paleoData_QCCertification into every member
# compilation, appending it to whatever private value was already there. Its
# signature is a value that begins with what the sheet says and continues past a
# separator: "AO (01/16/2019); AO (01/15/2019)" where the sheet says
# "AO (01/16/2019)".
#
# How badly a compilation is affected varies enormously, so the numbers matter
# more than the recipe. hydroclimate2k's files shared one string of 32 with its
# sheet and carried 361 values the sheet had never seen; iso2k shares 54 of 79
# and carries 34. The suggestions below are evidence-led per row rather than a
# blanket rule.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args  <- commandArgs(trailingOnly = TRUE)
comp  <- args[!grepl("^--", args)][1]
force <- "--force" %in% args
if (is.na(comp)) stop("usage: reconcile-certification.R <compilation> [--force]")

out <- file.path("review", paste0(comp, "-certification.csv"))
if (file.exists(out) && !force) {
  stop(out, " exists. Re-running would overwrite decisions already in it; pass --force if that is what you want.")
}

reg <- lv_qc_fields()
fields <- lv_csm_fields(comp, reg)
FIELD <- fields$qc_name[fields$csm_field == "QCCertification"]
if (!length(FIELD)) stop(comp, " has no csm certification field in the registry")
FLAT <- sub("^paleoData_", "", FIELD)
cat(sprintf("compilation : %s\nfield       : %s (flat key: %s)\n\n", comp, FIELD, FLAT))

cfg <- lv_config(comp)
db  <- lv_path("database")
idx <- lv_db_index(lv_scan(db), cache = TRUE)
bk  <- sheet_backend_google()

sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
sh <- sheet |> filter(field == FIELD, !is.na(value), nzchar(value)) |>
  transmute(tsid, sheet_value = trimws(value))
inc <- sheet |> filter(field == "inThisCompilation") |> transmute(tsid, sheet_membership = value)
sheet_vocab <- unique(sh$sheet_value)

members <- lv_compilation_timeseries(idx, comp)
ts2ds <- setNames(idx$timeseries$dataSetName, idx$timeseries$TSid)
want_ds <- unique(na.omit(unname(ts2ds[union(members, sh$tsid)])))
paths <- setNames(idx$datasets$path, idx$datasets$fileDataSetName)[want_ds]
paths <- paths[!is.na(paths)]
cat(sprintf("reading %d file%s\n", length(paths), if (length(paths) == 1) "" else "s"))

read_one <- function(p) {
  nms <- utils::unzip(p, list = TRUE)$Name
  j <- grep("\\.jsonld$", nms, value = TRUE)
  if (!length(j)) return(NULL)
  con <- unz(p, j[1]); on.exit(close(con), add = TRUE)
  m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                   simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  rows <- list()
  for (pd in m$paleoData) for (tb in pd$measurementTable) {
    if (!is.list(tb)) next
    cols <- if (!is.null(tb$columns)) tb$columns else tb
    for (col in cols) {
      if (!is.list(col) || is.null(col$TSid)) next
      i <- lv_csm_entry_index(col$inCompilation, comp)
      v <- if (!is.na(i) && is.list(col$inCompilation[[i]]$csm))
        col$inCompilation[[i]]$csm$QCCertification else NULL
      rows[[length(rows) + 1L]] <- tibble::tibble(
        tsid = as.character(col$TSid)[1],
        dataSetName = as.character(m$dataSetName %||% basename(p))[1],
        is_member = !is.na(i),
        csm_value = if (is.null(v)) NA_character_ else trimws(as.character(v)[1]),
        flat_value = if (is.null(col[[FLAT]])) NA_character_ else trimws(as.character(col[[FLAT]])[1]))
    }
  }
  if (!length(rows)) NULL else bind_rows(rows)
}
files <- bind_rows(lapply(unname(paths), read_one))

nz <- function(v) !is.na(v) & nzchar(v)
x <- full_join(files, sh, by = "tsid") |>
  left_join(inc, by = "tsid") |>
  mutate(dataSetName = coalesce(dataSetName, unname(ts2ds[tsid])),
         is_member = coalesce(is_member, tsid %in% members),
         # The migration appended rather than replaced, so a contaminated value
         # still begins with the one a curator typed.
         is_concatenation = nz(csm_value) & nz(sheet_value) & csm_value != sheet_value &
           startsWith(csm_value, sheet_value),
         appended = ifelse(is_concatenation, substring(csm_value, nchar(sheet_value) + 1), NA_character_),
         # Whether a file value is a string this compilation's own certifiers
         # use. That single fact separated hydroclimate2k's 361 foreign values
         # from iso2k's largely genuine ones.
         csm_in_sheet_vocabulary = nz(csm_value) & csm_value %in% sheet_vocab)

x <- x |> mutate(
  class = case_when(
    nz(sheet_value) & nz(csm_value) & sheet_value == csm_value ~ "sheet and file agree",
    is_concatenation                        ~ "csm carries the sheet value plus an appended one",
    nz(sheet_value) & nz(csm_value)         ~ "csm differs, no migration signature",
    nz(sheet_value) & is_member             ~ "sheet only, member",
    nz(sheet_value) & !is_member            ~ "sheet only, excluded timeseries",
    nz(csm_value) & csm_in_sheet_vocabulary ~ "csm only, but a string this compilation uses",
    nz(csm_value)                           ~ "csm only, and not this compilation's vocabulary",
    nz(flat_value)                          ~ "legacy flat key only",
    TRUE                                    ~ "empty everywhere"),
  suggested_action = case_when(
    class == "sheet and file agree" ~ "nothing to do",
    class == "csm carries the sheet value plus an appended one" ~
      "strip the appended value; the sheet and the baseline agree on what it should be",
    class == "csm differs, no migration signature" ~
      "REVIEW: file and sheet disagree and neither looks appended",
    class == "sheet only, member" ~ "write the sheet value into csm",
    class == "sheet only, excluded timeseries" ~ "leave; timeseries is excluded from the compilation",
    class == "csm only, but a string this compilation uses" ~
      "REVIEW: probably this compilation's own, recorded before the sheet column existed",
    class == "csm only, and not this compilation's vocabulary" ~
      "remove from csm; not this compilation's value",
    class == "legacy flat key only" ~ "delete the legacy flat key",
    TRUE ~ "nothing to do"),
  suggested_value = case_when(
    class == "csm carries the sheet value plus an appended one" ~ sheet_value,
    class == "sheet only, member" ~ sheet_value,
    TRUE ~ NA_character_),
  decision = NA_character_)

x <- filter(x, class != "empty everywhere")
order_key <- c("csm differs, no migration signature",
               "csm only, but a string this compilation uses",
               "csm only, and not this compilation's vocabulary",
               "csm carries the sheet value plus an appended one",
               "legacy flat key only", "sheet only, member",
               "sheet only, excluded timeseries", "sheet and file agree")
x <- x |>
  arrange(match(class, order_key), dataSetName, tsid) |>
  transmute(tsid, dataSetName, class, suggested_action, suggested_value, decision,
            sheet_value, csm_value, appended, flat_value,
            is_member, sheet_membership, csm_in_sheet_vocabulary)

dir.create("review", showWarnings = FALSE)
readr::write_csv(x, out, na = "")
cat("\n", out, ": ", nrow(x), " rows\n\n", sep = "")
print(as.data.frame(count(x, class, suggested_action)), right = FALSE)
cat(sprintf("\nsheet vocabulary: %d distinct string%s; file values matching it: %d of %d\n",
            length(sheet_vocab), if (length(sheet_vocab) == 1) "" else "s",
            sum(x$csm_in_sheet_vocabulary), sum(nz(x$csm_value))))
