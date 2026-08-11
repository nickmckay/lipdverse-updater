test_that("a dataset already in the database is found", {
  # The regression: the `existing` column shadowed the `existing` argument
  # inside tibble(), so the hash column held dataset names and nothing ever
  # matched. Every ingest screened clean, including one that should not have.
  inc <- list(A = c("h1", "h2", "h3"))
  ex  <- list(A = c("h1", "h2", "h3"), B = c("zz", "yy"))
  d <- lv_duplicate_screen(inc, ex, NULL)

  expect_equal(nrow(d), 1)
  expect_equal(d$new, "A")
  expect_equal(d$existing, "A")
  expect_equal(d$shared, 3L)
  expect_equal(d$containment, 1)
  expect_equal(d$disposition, "already_present")
})

test_that("the same record under a different name is found", {
  # The case the screen exists for: metadata cannot tell this from a different
  # record at the same site, but the measured values can.
  inc <- list(NewName = c("h1", "h2"))
  ex  <- list(OldName = c("h1", "h2"), Other = c("q1"))
  d <- lv_duplicate_screen(inc, ex, NULL)

  expect_equal(d$existing, "OldName")
  expect_false(d$same_name)
  expect_true(grepl("under a different name", d$recommendation))
})

test_that("an incoming file with more columns is reported as an update", {
  inc <- list(A = c("h1", "h2", "h3", "h4"))
  ex  <- list(A = c("h1", "h2", "h3"))
  d <- lv_duplicate_screen(inc, ex, NULL)
  expect_equal(d$disposition, "already_present_incoming_has_more")
})

test_that("partial overlap is reported as partial, not as a duplicate", {
  inc <- list(A = c("h1", "h2", "h3", "h4"))
  ex  <- list(A = c("h1", "zz", "yy"))
  d <- lv_duplicate_screen(inc, ex, NULL)
  expect_equal(d$disposition, "partial_overlap")
  expect_lt(d$containment, 1)
})

test_that("genuinely new data screens clean", {
  # The result that must stay trustworthy, and only is now that the positive
  # cases above are covered.
  d <- lv_duplicate_screen(list(A = c("n1", "n2")), list(B = c("h1", "h2")), NULL)
  expect_equal(nrow(d), 0)
})
