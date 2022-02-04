# # context("lts_input") #commented because deprecated
# library(devtools)
# library(lifeTimes)

test_that("lts_input returns a list", {
  expect_true(is.list(eval(lts_input())))
  # expect_equal(str_length("ab"), 2)
  # expect_equal(str_length("abc"), 3)
})


