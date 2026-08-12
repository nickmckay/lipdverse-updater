#!/usr/bin/env Rscript
#
# Reconcile hydroclimate2k's certification across its three records, and write
# review/hydroclimate2k-certification.csv for review.
#
#   ./scripts/reconcile-h2k-certification.R            # write the review file
#   ./scripts/reconcile-h2k-certification.R --force    # overwrite an edited one
#
# Reads only; writes one CSV. The seeding it prepares is a separate script.
#
# Three records of the same judgement, which disagree:
#
#   sheet   the compilation's QC tab. 142 values in 10 distinct strings.
#   csm     inCompilation[hydroclimate2k].csm.QCCertification, 430 values in
#           33 strings, of which exactly one is also in the sheet.
#   flat    paleoData_hydroclimate2kCertification still on a column, where the
#           migration could not place it because the column is not a member.
#
# The sheet is the authority. csm carries the migration's copy of the shared
# paleoData_QCCertification into every member compilation, so most of what a
# file says about hydroclimate2k was written by somebody else -- visible in the
# "; " concatenations the migration's collision rule produced, where the file
# reads "ET; GF" and the sheet reads "GF".

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args  <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args
comp  <- "hydroclimate2k"
FIELD <- "paleoData_hydroclimate2kCertification"
out   <- file.path("review", "hydroclimate2k-certification.csv")

# Never overwrite a file Nick may have decided on: these are edited in place and
# handed back, and a regenerate silently discards the decisions.
if (file.exists(out) && !force) {
  stop(out, " exists. Re-running would overwrite decisions already in it; pass --force if that is what you want.")
}

cfg <- lv_config(comp)
db  <- lv_path("database")
idx <- lv_db_index(lv_scan(db), cache = TRUE)

sheet <- qc_sheet_pull(sheet_backend_google(), cfg$qc_sheet_id, cfg$qc_tabs$qc)
sh <- sheet |> filter(field == FIELD, !is.na(value), nzchar(value)) |>
  transmute(tsid, sheet_value = value)
inc <- sheet |> filter(field == "inThisCompilation") |>
  transmute(tsid, sheet_membership = value)
sheet_vocab <- unique(trimws(sh$sheet_value))

base <- qc_state_current(qc_store(), comp) |> filter(field == FIELD)

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
  for (pd in m$paleoData) {
    for (tb in pd$measurementTable) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      for (col in cols) {
        if (!is.list(col) || is.null(col$TSid)) next
        i <- lv_csm_entry_index(col$inCompilation, comp)
        csm_v <- if (!is.na(i) && is.list(col$inCompilation[[i]]$csm))
          col$inCompilation[[i]]$csm$QCCertification else NULL
        one <- function(k) if (is.null(col[[k]])) NA_character_ else as.character(col[[k]])[1]
        rows[[length(rows) + 1L]] <- tibble::tibble(
          tsid = as.character(col$TSid)[1],
          dataSetName = as.character(m$dataSetName %||% basename(p))[1],
          is_member = !is.na(i),
          csm_value = if (is.null(csm_v)) NA_character_ else as.character(csm_v)[1],
          flat_private = one("hydroclimate2kCertification"),
          flat_shared = one("QCCertification"))
      }
    }
  }
  if (!length(rows)) NULL else bind_rows(rows)
}

files <- bind_rows(lapply(unname(paths), read_one))

nz <- function(v) !is.na(v) & nzchar(v)
same <- function(a, b) nz(a) & nz(b) & trimws(a) == trimws(b)

x <- full_join(files, sh, by = "tsid") |>
  left_join(inc, by = "tsid") |>
  mutate(dataSetName = coalesce(dataSetName, unname(ts2ds[tsid])),
         is_member = coalesce(is_member, tsid %in% members),
         on_qc_tab = tsid %in% sheet$tsid,
         # The evidence behind every suggestion below: hydroclimate2k's own
         # curators wrote 10 distinct strings, and a file value outside that set
         # was written by somebody else and copied here by the migration.
         file_value_in_sheet_vocabulary = nz(csm_value) &
           trimws(csm_value) %in% sheet_vocab)

x <- x |> mutate(
  class = case_when(
    nz(sheet_value) & same(sheet_value, csm_value)  ~ "sheet and file agree",
    nz(sheet_value) & nz(csm_value)                 ~ "csm contaminated",
    nz(sheet_value) & nz(flat_private)              ~ "legacy flat key, excluded timeseries",
    nz(sheet_value) &  is_member                    ~ "sheet only, member",
    nz(sheet_value) & !is_member                    ~ "sheet only, excluded timeseries",
    nz(csm_value)                                   ~ "csm only, sheet never had it",
    nz(flat_private)                                ~ "legacy flat key, sheet blank",
    nz(flat_shared)                                 ~ "shared flat key, another compilation's field",
    TRUE                                            ~ "empty everywhere"),
  suggested_action = case_when(
    class == "sheet and file agree"       ~ "seed baseline from sheet; file already agrees",
    class == "csm contaminated"           ~ "seed baseline from sheet; overwrite csm with the sheet value",
    class == "sheet only, member"         ~ "seed baseline from sheet; write the sheet value into csm",
    class == "csm only, sheet never had it" ~ "remove from hydroclimate2k csm; not this compilation's value",
    grepl("^legacy flat key", class)      ~ "delete the legacy flat key; timeseries is excluded, so csm cannot hold it",
    class == "sheet only, excluded timeseries" ~ "leave; timeseries is excluded from the compilation",
    class == "shared flat key, another compilation's field" ~ "leave; paleoData_QCCertification belongs to another compilation",
    TRUE                                  ~ "nothing to do"),
  suggested_value = if_else(grepl("seed baseline", suggested_action),
                            trimws(sheet_value), NA_character_),
  decision = NA_character_)

# Out of scope, and misleadingly so if left in: paleoData_QCCertification is
# HoloceneAbruptChange's csm field, and the files read here are the ones
# hydroclimate2k touches, so any count of it here is a sample rather than a
# census. It is reported at the end instead.
n_shared_flat <- sum(x$class == "shared flat key, another compilation's field")
x <- filter(x, !class %in% c("empty everywhere",
                             "shared flat key, another compilation's field"))

# Least certain first: the 361 removals are the judgement being asked for, the
# agreements at the bottom are there to be skimmed.
order_key <- c("csm only, sheet never had it", "legacy flat key, excluded timeseries",
               "legacy flat key, sheet blank", "csm contaminated", "sheet only, member",
               "sheet only, excluded timeseries", "sheet and file agree")
x <- x |>
  # Within the removals, the ones whose value is a string hydroclimate2k's own
  # curators do use come first. Those are the only removals where "written by
  # somebody else" is a judgement rather than a reading of the vocabulary.
  arrange(match(class, order_key), desc(file_value_in_sheet_vocabulary),
          dataSetName, tsid) |>
  transmute(tsid, dataSetName, class, suggested_action, suggested_value, decision,
            sheet_value, csm_value, flat_private, flat_shared,
            is_member, sheet_membership, on_qc_tab, file_value_in_sheet_vocabulary)

dir.create("review", showWarnings = FALSE)
readr::write_csv(x, out, na = "")

cat("\n", out, ": ", nrow(x), " rows\n\n", sep = "")
print(as.data.frame(count(x, class, suggested_action)), right = FALSE)
cat(sprintf("\nstore baseline for %s: %d cell%s\n", FIELD, nrow(base),
            if (nrow(base) == 1) "" else "s"))
cat(sprintf("sheet vocabulary: %d distinct string%s; file values matching it: %d of %d\n",
            length(sheet_vocab), if (length(sheet_vocab) == 1) "" else "s",
            sum(x$file_value_in_sheet_vocabulary), sum(nz(x$csm_value))))
cat(sprintf("not in this file: %d column%s still carrying paleoData_QCCertification, which is %s\n",
            n_shared_flat, if (n_shared_flat == 1) "" else "s",
            "HoloceneAbruptChange's csm field, not hydroclimate2k's"))
