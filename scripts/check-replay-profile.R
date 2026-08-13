#!/usr/bin/env Rscript
#
# Regression check over the recorded version history.
#
#   ./scripts/check-replay-profile.R            # compare against the baseline
#   ./scripts/check-replay-profile.R --update   # rewrite the baseline
#
# Replays every recorded version-run through qc_merge() and compares the shape
# of the result against inst/extdata/replay-profile.csv. Exits 1 on drift.
#
# This is a regression check, not an oracle. The legacy pipeline's flaws mean a
# disagreement with it is at least as likely to be the rewrite being right, so
# the baseline records what the current code *does*, and the check is that it
# has not changed without someone deciding it should. What it genuinely proves
# is that qc_merge() runs over 178 real-world inputs without error and resolves
# them the same way it did yesterday.
#
# The corpus is on Nick's machine, not in CI: ~/Dropbox/lipdverse/html. Set
# LIPDVERSE_HTML to point elsewhere.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
update <- "--update" %in% args
baseline_path <- "inst/extdata/replay-profile.csv"
root <- lv_replay_root()

if (!dir.exists(root)) {
  cat("No replay corpus at", root, "-- skipping.\n")
  quit(save = "no", status = 0)
}

comps <- sort(basename(list.dirs(root, recursive = FALSE)))
profile <- list()
for (comp in comps) {
  v <- lv_replay_versions(comp, root)
  if (nrow(v) < 2) next
  r <- lv_replay(comp, root, progress = FALSE)
  profile[[comp]] <- r$divergences |>
    count(class, name = "n") |>
    mutate(compilation = comp,
           runs = nrow(r$summary),
           compared = sum(r$summary$n_compared),
           .before = 1)
  cat(sprintf("%-24s %2d runs  %8d cells  %6.2f%% agree\n", comp, nrow(r$summary),
              sum(r$summary$n_compared),
              100 * sum(r$summary$n_agree) / max(1, sum(r$summary$n_compared))))
}
now <- bind_rows(profile) |> arrange(compilation, class)
if (!nrow(now)) { cat("Nothing to replay.\n"); quit(save = "no", status = 0) }

if (update) {
  readr::write_csv(now, baseline_path, na = "")
  cat("\nWrote", baseline_path, "--", nrow(now), "rows over",
      dplyr::n_distinct(now$compilation), "compilations\n")
  quit(save = "no", status = 0)
}

if (!file.exists(baseline_path)) {
  cat("\nNo baseline at", baseline_path, "-- run with --update to record one.\n")
  quit(save = "no", status = 1)
}
was <- readr::read_csv(baseline_path, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE) |>
  mutate(across(c(n, runs, compared), as.integer))

drift <- full_join(was, now, by = c("compilation", "class"), suffix = c("_baseline", "_now")) |>
  filter(is.na(n_baseline) | is.na(n_now) | n_baseline != n_now)

if (!nrow(drift)) {
  cat(sprintf("\nThe replay profile is unchanged: %d compilations, %d cells compared.\n",
              dplyr::n_distinct(now$compilation), sum(unique(now[, c("compilation", "compared")])$compared))
  )
  quit(save = "no", status = 0)
}
cat("\nThe replay profile has drifted:\n\n")
print(as.data.frame(drift[, c("compilation", "class", "n_baseline", "n_now")]), right = FALSE)
cat("\nIf this is intended, re-record with --update and say why in the commit.\n")
quit(save = "no", status = 1)
