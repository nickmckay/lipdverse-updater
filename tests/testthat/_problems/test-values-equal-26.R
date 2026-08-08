# Extracted from test-values-equal.R:26

# test -------------------------------------------------------------------------
expect_false(values_equal("60.07", "60.1"))
expect_false(values_equal("60.1", "60.07"))
expect_false(values_equal("-133.81", "-133.8"))
expect_false(values_equal("39.764", "39.8"))
expect_false(values_equal("39.652", "39.7"))
expect_false(values_equal("60.07", "60.08"))
expect_true(values_equal("1770.79170", "1770.79167"))
