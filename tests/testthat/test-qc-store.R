local_store <- function(envir = parent.frame()) {
  qc_store(withr::local_tempdir(.local_envir = envir))
}

ev <- function(tsid, field, old = NA_character_, new, old_p = !is.na(old), new_p = !is.na(new),
               source = "sheet", ts = NA_character_) {
  qc_events_empty() |>
    dplyr::add_row(run_id = NA_character_, event_seq = NA_integer_, ts = ts,
                   compilation = NA_character_, tsid = tsid, dataset_id = "DS1",
                   field = field, old_value = old, old_present = old_p,
                   new_value = new, new_present = new_p,
                   source = source, actor = "test", reason = NA_character_)
}

test_that("an empty store yields empty state", {
  s <- local_store()
  expect_equal(nrow(qc_state_current(s, "demo")), 0)
  expect_equal(nrow(qc_store_events(s, "demo")), 0)
})

test_that("events round trip through the log", {
  s <- local_store()
  qc_store_append(s, "demo", ev("T1", "archiveType", new = "coral"))
  e <- qc_store_events(s, "demo")

  expect_equal(nrow(e), 1)
  expect_equal(e$tsid, "T1")
  expect_true(e$new_present)
  expect_false(e$old_present)
  expect_equal(e$compilation, "demo")
})

test_that("state is the latest event per cell", {
  s <- local_store()
  qc_store_append(s, "demo", ev("T1", "archiveType", new = "coral", ts = "2026-01-01T00:00:00Z"))
  qc_store_append(s, "demo", ev("T1", "archiveType", old = "coral", new = "Coral",
                                ts = "2026-02-01T00:00:00Z"))
  st <- qc_state_current(s, "demo")

  expect_equal(nrow(st), 1)
  expect_equal(st$value, "Coral")
})

# The property that replaces backupQCId and checkUpdate.R.
test_that("state at a past time is recoverable", {
  s <- local_store()
  qc_store_append(s, "demo", ev("T1", "QC Certification", new = "A", ts = "2026-01-01T00:00:00Z"))
  qc_store_append(s, "demo", ev("T1", "QC Certification", old = "A", new = "B",
                                ts = "2026-06-01T00:00:00Z"))

  expect_equal(qc_state_at(s, "demo", "2026-03-01T00:00:00Z")$value, "A")
  expect_equal(qc_state_at(s, "demo", "2026-12-01T00:00:00Z")$value, "B")
  expect_equal(nrow(qc_state_at(s, "demo", "2025-01-01T00:00:00Z")), 0)
})

# A tombstone is an explicit record that a cell was cleared, which is precisely
# what a plain NA in a wide sheet cannot express.
test_that("a tombstone removes a cell from state but stays in history", {
  s <- local_store()
  qc_store_append(s, "demo", ev("T1", "QC comments", new = "check", ts = "2026-01-01T00:00:00Z"))
  qc_store_append(s, "demo", ev("T1", "QC comments", old = "check", new = NA_character_,
                                new_p = FALSE, ts = "2026-02-01T00:00:00Z"))

  expect_equal(nrow(qc_state_current(s, "demo")), 0)
  expect_equal(nrow(qc_history(s, "demo", "T1", "QC comments")), 2)
  # And the value before the clear is still recoverable.
  expect_equal(qc_state_at(s, "demo", "2026-01-15T00:00:00Z")$value, "check")
})

# Regression: two appends inside the same second carry identical timestamps.
# Ordering by anything non-monotonic (run_id is random) let the later event sort
# first, so "latest wins" could resolve to the superseded value. This surfaced
# only intermittently, which is exactly why it is pinned here.
test_that("appends in the same second stay in order", {
  s <- local_store()
  fixed <- "2026-01-01T00:00:00Z"
  for (v in c("v1", "v2", "v3", "v4", "v5")) {
    qc_store_append(s, "demo", ev("T1", "archiveType", new = v, ts = fixed))
  }
  st <- qc_state_current(s, "demo")
  expect_equal(nrow(st), 1)
  expect_equal(st$value, "v5")

  e <- qc_store_events(s, "demo")
  expect_equal(e$new_value, c("v1", "v2", "v3", "v4", "v5"))
})

test_that("a tombstone written in the same second as its value still clears", {
  s <- local_store()
  fixed <- "2026-01-01T00:00:00Z"
  qc_store_append(s, "demo", ev("T1", "QC comments", new = "check", ts = fixed))
  qc_store_append(s, "demo", ev("T1", "QC comments", old = "check", new = NA_character_,
                                new_p = FALSE, ts = fixed))
  expect_equal(nrow(qc_state_current(s, "demo")), 0)
})

test_that("compilations are independent", {
  s <- local_store()
  qc_store_append(s, "a", ev("T1", "archiveType", new = "coral"))
  qc_store_append(s, "b", ev("T1", "archiveType", new = "Coral"))
  expect_equal(qc_state_current(s, "a")$value, "coral")
  expect_equal(qc_state_current(s, "b")$value, "Coral")
})

test_that("malformed events are rejected", {
  s <- local_store()
  expect_error(qc_store_append(s, "demo", ev("T1", "f", new = "x", source = "telepathy")),
               class = "lv_error_store")
  expect_error(qc_store_append(s, "demo", ev(NA_character_, "f", new = "x")),
               class = "lv_error_store")
  # A present cell must carry a value, and a tombstone must not.
  expect_error(qc_store_append(s, "demo", ev("T1", "f", new = NA_character_, new_p = TRUE)),
               class = "lv_error_store")
  expect_error(qc_store_append(s, "demo", ev("T1", "f", new = "x", new_p = FALSE)),
               class = "lv_error_store")
})

test_that("qc_diff_to_events records additions, changes and removals", {
  before <- tibble::tibble(tsid = c("T1", "T2"), field = "archiveType",
                           value = c("coral", "lake"), present = TRUE, dataset_id = "DS1")
  after <- tibble::tibble(tsid = c("T1", "T3"), field = "archiveType",
                          value = c("Coral", "ice"), present = TRUE, dataset_id = "DS1")
  e <- qc_diff_to_events(before, after)

  expect_equal(nrow(e), 3)
  chg <- e[e$tsid == "T1", ]
  expect_equal(chg$old_value, "coral"); expect_equal(chg$new_value, "Coral")
  rem <- e[e$tsid == "T2", ]
  expect_true(rem$old_present); expect_false(rem$new_present); expect_true(is.na(rem$new_value))
  add <- e[e$tsid == "T3", ]
  expect_false(add$old_present); expect_true(add$new_present)
})

test_that("an unchanged state produces no events", {
  x <- tibble::tibble(tsid = "T1", field = "archiveType", value = "coral",
                      present = TRUE, dataset_id = "DS1")
  expect_equal(nrow(qc_diff_to_events(x, x)), 0)
})

# Replaying the log must reproduce the state it was derived from, or the store
# is not a faithful record of what happened.
test_that("applying the emitted events reproduces the target state", {
  s <- local_store()
  before <- qc_cells_empty()
  after <- tibble::tibble(tsid = c("T1", "T2"), field = "archiveType",
                          value = c("coral", "lake"), present = TRUE, dataset_id = "DS1")

  qc_store_append(s, "demo", qc_diff_to_events(before, after))
  st <- qc_state_current(s, "demo")
  expect_setequal(paste(st$tsid, st$value), paste(after$tsid, after$value))

  after2 <- tibble::tibble(tsid = "T1", field = "archiveType", value = "Coral",
                           present = TRUE, dataset_id = "DS1")
  qc_store_append(s, "demo", qc_diff_to_events(st, after2))
  st2 <- qc_state_current(s, "demo")
  expect_equal(nrow(st2), 1)
  expect_equal(st2$value, "Coral")
})

test_that("a merge plan can be committed to the store and read back", {
  s <- local_store()
  reg <- tibble::tibble(qc_name = "archiveType", role = "merged", ownership = "shared",
                        nullable_by_curator = "FALSE", ts_name = "archiveType",
                        family = "archiveType", cardinality = "timeseries", type = "character",
                        vocab_key = NA_character_, canonical = NA_character_,
                        csm_compilation = NA_character_, csm_field = NA_character_,
                        csm_flat_key = NA_character_, deprecated = FALSE,
                        n_compilations = 1L, n_filled = 1L)
  base <- tibble::tibble(tsid = "T1", field = "archiveType", value = "coral",
                         present = TRUE, dataset_id = "DS1")
  sheet <- dplyr::mutate(base, value = "Coral")

  plan <- qc_merge(base, sheet, base, registry = reg, policy = qc_merge_policy(strict = FALSE))
  qc_store_append(s, "demo", qc_diff_to_events(base, qc_plan_state(plan)))

  expect_equal(qc_state_current(s, "demo")$value, "Coral")
})
