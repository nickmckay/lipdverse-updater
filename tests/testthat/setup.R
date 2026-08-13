# Point every configured path at a scratch directory for the whole suite.
#
# Two tests have already been caught reaching past their fixtures into the
# operator's machine: test-created-by.R copied a file out of the live database,
# and three bibliography tests resolved against the real 4,838-entry reference
# store because lv_export_bib()'s default reads it. Both passed here and only
# here -- the first failed in CI with ENOENT on ~/Dropbox, the second would have
# started failing the day somebody edited a reference.
#
# A test that needs real data should say so and skip when it is absent. This
# makes the default direction safe: reach for the real database and you get an
# empty directory, not Nick's.
#
# Individual tests still override these with withr::local_envvar(); a local
# setting wins, which is what the isolated tests already do.
local({
  root <- fs::path(tempdir(), "lipdverse-test-paths")
  fs::dir_create(fs::path(root, c("database", "snapshots", "qcstore", "state",
                                  "export", "holding-tank")))
  Sys.setenv(
    LIPDVERSE_DATABASE     = fs::path(root, "database"),
    LIPDVERSE_SNAPSHOTS    = fs::path(root, "snapshots"),
    LIPDVERSE_QCSTORE      = fs::path(root, "qcstore"),
    LIPDVERSE_STATE        = fs::path(root, "state"),
    LIPDVERSE_EXPORT       = fs::path(root, "export"),
    LIPDVERSE_HOLDING_TANK = fs::path(root, "holding-tank"))
})
