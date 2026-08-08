test_that("the same number written two ways is one value", {
  expect_true(values_equal("1.0", "1"))
  expect_true(values_equal("1.50", "1.5"))
  expect_true(values_equal("1e3", "1000"))
  expect_true(values_equal(" 2.5 ", "2.5"))
  # Float representation noise is not an edit.
  expect_true(values_equal("60.070000000000004", "60.07"))
  # Nor is a value the sheet has rounded, when it still agrees to the precision
  # the two have in common. Treating this as an edit would write the rounded
  # value back to the file on every run.
  expect_true(values_equal("1770.7917", "1770.79167"))
})

test_that("a difference beyond the shorter spelling is still a difference", {
  # The regression that lost coordinate precision on four datasets. These used to
  # compare equal, because both were rounded to the LESSER of the two precisions
  # before comparing, so 60.07 became 60.1 and matched.
  expect_false(values_equal("60.07", "60.1"))
  expect_false(values_equal("60.1", "60.07"))
  expect_false(values_equal("-133.81", "-133.8"))
  expect_false(values_equal("39.764", "39.8"))
  expect_false(values_equal("39.652", "39.7"))
  # The line is the shared precision, not the size of the gap: two decimals in
  # common is enough to trust a rounding, one is not.
  expect_false(values_equal("60.07", "60.08"))
  expect_false(values_equal("1770.7917", "1770.7918"))
})

test_that("ordinary comparisons still behave", {
  expect_false(values_equal("2.5", "2.6"))
  expect_true(values_equal("abc", "abc"))
  expect_false(values_equal("abc", "abd"))
  expect_true(values_equal(NA_character_, NA_character_))
  expect_false(values_equal(NA_character_, "1"))
  expect_false(values_equal("1", NA_character_))
})

test_that("numeric comparison can be switched off", {
  expect_false(values_equal("1.0", "1", numeric_ok = FALSE))
  expect_true(values_equal("1.0", "1.0", numeric_ok = FALSE))
})

test_that("it is vectorised and order does not matter", {
  x <- c("1.0", "60.07", "abc", NA)
  y <- c("1",   "60.1",  "abc", NA)
  expect_equal(values_equal(x, y), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(values_equal(x, y), values_equal(y, x))
})
