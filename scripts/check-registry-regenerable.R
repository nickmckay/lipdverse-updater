# Is inst/extdata/qc_fields.csv still what build_qc_fields.R would produce?
#
# The registry is meant to be derived from the review/ files, so that every
# ownership and nullability decision has a recorded rationale rather than living
# only in a commit message. That only holds if the two are kept in step, and
# twice now they have not been: hand edits landed on the registry and never made
# it back into the review files, so re-running the builder silently reverted
# them. #15 fixed that once without pinning it, and it drifted again within four
# commits.
#
# This reports the difference rather than fixing it, and never writes the
# registry. Run it after touching either side.
#
#   Rscript scripts/check-registry-regenerable.R
#
# Exit 0 when they agree, 1 when they do not.
suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

# The builder reads a snapshot from the QC store, so this cannot run without
# one. Said plainly here rather than surfacing as "the builder failed".
qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
if (!dir.exists(qcstore)) {
  cat("No QC store at", qcstore, "-- the registry builder needs one. Skipping.\n")
  quit(save = "no", status = 0)
}

# The package root, found rather than assumed: this script has to run from a CI
# checkout as well as from Nick's home directory, and hardcoding the latter is
# what lv_path() exists to stop.
repo <- pkgload::pkg_path()
live_path <- fs::path(repo, "inst/extdata/qc_fields.csv")

# Everything runs inside a function so on.exit() actually fires. At top level it
# does not: quit() bypasses it, and the first version of this script left the
# rebuilt registry sitting in the tree -- a check that damaged the thing it was
# checking, which is the exact failure it exists to prevent.
main <- function() {

  # The builder writes in place, so it runs against a copy of the tree's registry
  # which is restored afterwards no matter how this exits. A check that can damage
  # what it is checking is worse than no check.
  backup <- fs::file_temp(ext = "csv")
  fs::file_copy(live_path, backup)
  on.exit({fs::file_copy(backup, live_path, overwrite = TRUE); fs::file_delete(backup)}, add = TRUE)

  live <- readr::read_csv(live_path, col_types = readr::cols(.default = readr::col_character()),
                          progress = FALSE)

  out <- system2("Rscript", fs::path(repo, "data-raw/build_qc_fields.R"),
                 stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    cat(paste(out, collapse = "\n"), "\n"); stop("the builder failed")
  }
  rebuilt <- readr::read_csv(live_path, col_types = readr::cols(.default = readr::col_character()),
                             progress = FALSE)

  cat("committed:", nrow(live), "rows | rebuilt:", nrow(rebuilt), "rows\n\n")

  only_live <- setdiff(live$qc_name, rebuilt$qc_name)
  only_new  <- setdiff(rebuilt$qc_name, live$qc_name)

  k <- intersect(live$qc_name, rebuilt$qc_name)
  A <- live[match(k, live$qc_name), ]; B <- rebuilt[match(k, rebuilt$qc_name), ]
  cols <- intersect(names(A), names(B))
  ne <- function(x, y) { x[is.na(x)] <- "\001"; y[is.na(y)] <- "\001"; x != y }
  M <- vapply(cols, function(cc) ne(A[[cc]], B[[cc]]), logical(length(k)))
  per_col <- colSums(M); per_col <- per_col[per_col > 0]

  ok <- !length(only_live) && !length(only_new) && !sum(per_col)
  if (ok) { cat("The builder reproduces the committed registry exactly.\n"); return(0L) }

  if (length(only_live)) {
    cat("Rows the rebuild would DROP (", length(only_live), "):\n", sep = "")
    cat(paste0("  ", only_live, collapse = "\n"), "\n\n")
  }
  if (length(only_new)) {
    cat("Rows the rebuild would ADD (", length(only_new), "):\n", sep = "")
    cat(paste0("  ", only_new, collapse = "\n"), "\n\n")
  }
  if (length(per_col)) {
    cat("Cells the rebuild would change:\n"); print(per_col)
    for (cc in names(per_col)) {
      i <- which(M[, cc])
      cat("\n ", cc, ":\n", sep = "")
      print(as.data.frame(tibble(qc_name = A$qc_name[i], committed = A[[cc]][i],
                                 rebuilt = B[[cc]][i]) |> head(10)), right = FALSE)
    }
    cat("\n")
  }
  cat("The registry and the review files disagree. Either the review files are\n",
      "missing a decision that was made on the registry, or the builder is wrong.\n",
      "The committed registry has NOT been modified.\n", sep = "")
    return(1L)
}

quit(status = main())
