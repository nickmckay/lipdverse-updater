#!/usr/bin/env Rscript
#
# Prove a curator edit reaches the files, and only the cells edited.
#
#   ./scripts/test-curator-edit.R lipdverseTest --apply
#   ./scripts/test-curator-edit.R lipdverseTest --revert
#
# Everything up to here has run with the sheet agreeing with the files, so the
# merge has never actually had to move a value. This writes real edits into the
# QC sheet -- one on a nullable curator field, one on a non-nullable one, and
# one clear -- so the path a compilation lead exercises daily is the path being
# tested.
#
# --revert puts the original values back, so the compilation returns to a
# clean state and the run can be repeated.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
comp <- args[!grepl("^--", args)][1]
if (is.na(comp)) comp <- "lipdverseTest"
mode <- if ("--revert" %in% args) "revert" else if ("--apply" %in% args) "apply" else "show"

cfg <- lv_config(comp); bk <- sheet_backend_google()
# Sheet columns carry display names, not canonical ones, so pick them from the
# tab itself rather than hardcoding a name the registry happens to use.
field_edit  <- "paleoDataNotes"                       # curator, nullable
field_fixed <- "environmentInterpretation1_variable"  # curator, not nullable
marker <- "edited by test-curator-edit"

raw <- sheet_read(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
raw <- raw[, !duplicated(names(raw)), drop = FALSE]
missing <- setdiff(c(field_edit, field_fixed), names(raw))
if (length(missing)) stop("QC tab has no column: ", paste(missing, collapse = ", "))
canon <- function(f) lv_canonical_field(f, lv_qc_fields())

# Rows to touch: one blank note to fill, one populated note to clear, one
# interpretation to change. Deterministic, so --revert can find them again.
fill  <- which(is.na(raw[[field_edit]]))[1]
clear <- which(!is.na(raw[[field_edit]]))[1]
chg   <- which(!is.na(raw[[field_fixed]]))[1]
stopifnot(!is.na(fill), !is.na(clear), !is.na(chg))

cat(sprintf("compilation : %s\nmode        : %s\n\n", comp, mode))
show <- function(lbl, i, f) cat(sprintf("%-6s row %-5d %-32s %s = [%s]\n",
                                        lbl, i, raw$TSid[i], f, raw[[f]][i]))
show("fill",  fill,  field_edit)
show("clear", clear, field_edit)
show("change", chg,  field_fixed)

if (mode == "show") { cat("\npass --apply to write the edits.\n"); quit(save = "no") }

col_letter <- function(nm) {
  j <- match(nm, names(raw))
  # Sheets columns past Z are two letters; the QC tab has 44 of them.
  if (j <= 26) LETTERS[j] else paste0(LETTERS[(j - 1) %/% 26], LETTERS[(j - 1) %% 26 + 1])
}
put <- function(i, nm, v) {
  googlesheets4::range_write(
    cfg$qc_sheet_id, data.frame(x = v), sheet = cfg$qc_tabs$qc,
    range = paste0(col_letter(nm), i + 1L), col_names = FALSE, reformat = FALSE)
}

if (mode == "apply") {
  put(fill,  field_edit,  marker)
  put(clear, field_edit,  "")
  put(chg,   field_fixed, marker)
  cat("\nedits written. Now run: ./scripts/run-compilation.R", comp, "--commit\n")
} else {
  # The store holds what was there before the edits, which is exactly what
  # revert needs -- no separate backup file to go stale.
  st <- qc_store()
  base <- qc_state_current(st, comp)
  orig <- function(tsid, f) {
    v <- base$value[base$tsid == tsid & base$field == canon(f)]
    if (!length(v) || is.na(v)) "" else v
  }
  put(fill,  field_edit,  orig(raw$TSid[fill],  field_edit))
  put(clear, field_edit,  orig(raw$TSid[clear], field_edit))
  put(chg,   field_fixed, orig(raw$TSid[chg],   field_fixed))
  cat("\nreverted to the store's values. Re-run the pipeline to settle.\n")
}
