test_that("HP is calculated correctly", {
  # Compare function output with known values
  expect_equal(stat_value(base =  70, level =  50, iv = 31, ev = 252, is_hp = TRUE), 177)
  expect_equal(stat_value(base =  70, level =  50, iv =  0, ev =   0, is_hp = TRUE), 130)
  expect_equal(stat_value(base = 255, level = 100, iv = 31, ev = 252, is_hp = TRUE), 714)

  # Check that HP is unaffected by nature
  expect_length(unique(c(
    stat_value(base = 70, level = 50, iv = 31, ev = 252, is_hp = TRUE, nature = "positive"),
    stat_value(base = 70, level = 50, iv = 31, ev = 252, is_hp = TRUE, nature =  "neutral"),
    stat_value(base = 70, level = 50, iv = 31, ev = 252, is_hp = TRUE, nature = "negative")
  )), 1L)
})

test_that("other stats are calculated correctly", {
  # Compare function output with known values
  expect_equal(stat_value(base = 142, level =  50, iv = 31, ev = 252, nature = "positive", is_hp = FALSE), 213)
  expect_equal(stat_value(base =  20, level =  50, iv =  0, ev =   0, nature = "negative", is_hp = FALSE),  22)
  expect_equal(stat_value(base = 100, level = 100, iv = 31, ev =   0, nature =  "neutral", is_hp = FALSE), 236)
})
