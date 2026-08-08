# Extracted from test-db-write.R:119

# prequel ----------------------------------------------------------------------
local_db <- function(specs, envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  for (s in specs) do.call(write_lpd, c(list(dir = d), s))
  d
}
fingerprint <- function(dir) {
  f <- sort(fs::dir_ls(dir, glob = "*.lpd", type = "file"))
  paste(fs::path_file(f), vapply(f, function(p) digest::digest(file = p), character(1)),
        collapse = "|")
}

# test -------------------------------------------------------------------------
withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9")),
                         list(dataSetName = "B.Author.2002", tsids = c("T2", "T8")),
                         list(dataSetName = "C.Author.2003")))
before <- fingerprint(live)
lv_promote(stage, live, run_id = "R1", dry_run = FALSE)
expect_false(identical(fingerprint(live), before))
lv_write_rollback(live, "R1")
