test_that("month rate tests", {
  expect_is(month_rate(1), "numeric")
  expect_error(month_rate())
  expect_error(month_rate(1, 2))
  expect_length(month_rate(1), 1)
  expect_length(month_rate(c(1, 2)), 2)
})



