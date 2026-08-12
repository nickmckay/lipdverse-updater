#!/usr/bin/env Rscript
#
# Replay the recorded version history of one compilation (or all of them)
# through qc_merge, and write the divergences for review.
#
#   ./scripts/replay-history.R hydroclimate2k
#   ./scripts/replay-history.R --all
#
# Reads only: the published websites under ~/Dropbox/lipdverse/html and the
# field registry. Writes review/replay-<compilation>.csv.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
root <- lv_replay_root()
comps <- if ("--all" %in% args) {
  basename(list.dirs(root, recursive = FALSE))
} else args[!grepl("^--", args)]
if (!length(comps)) stop("usage: replay-history.R <compilation> | --all")

all_sum <- list(); all_div <- list()
for (comp in comps) {
  v <- lv_replay_versions(comp, root)
  if (nrow(v) < 2) next
  r <- lv_replay(comp, root, progress = FALSE)
  r$summary$compilation <- comp
  r$divergences$compilation <- comp
  all_sum[[comp]] <- r$summary; all_div[[comp]] <- r$divergences
  cat(sprintf("%-24s %2d runs  %7d cells  %6.2f%% agree\n", comp, nrow(r$summary),
              sum(r$summary$n_compared),
              100 * sum(r$summary$n_agree) / max(1, sum(r$summary$n_compared))))
}
s <- bind_rows(all_sum); d <- bind_rows(all_div)
if (!nrow(s)) stop("nothing to replay")

cat(sprintf("\n%d version-run%s over %d compilation%s\n", nrow(s), if (nrow(s) == 1) "" else "s",
            dplyr::n_distinct(s$compilation), if (dplyr::n_distinct(s$compilation) == 1) "" else "s"))
cat(sprintf("cells compared: %d   agree: %d (%.2f%%)   differ: %d\n",
            sum(s$n_compared), sum(s$n_agree),
            100 * sum(s$n_agree) / max(1, sum(s$n_compared)), sum(s$n_differ)))
cat("\ndivergence by class:\n")
print(as.data.frame(count(d, class, sort = TRUE)), right = FALSE)
cat("\nfields diverging most:\n")
print(as.data.frame(head(count(d, field, class, sort = TRUE), 12)), right = FALSE)

dir.create("review", showWarnings = FALSE)
out <- if (length(comps) == 1) file.path("review", paste0("replay-", comps, ".csv")) else
  file.path("review", "replay-all.csv")
readr::write_csv(d, out, na = "")
readr::write_csv(s, sub("\\.csv$", "-summary.csv", out), na = "")
cat("\nwrote", out, "and its summary\n")
