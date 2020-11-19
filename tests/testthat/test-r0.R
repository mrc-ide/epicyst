test_that("R0 tests", {
  a <- r0(set_up()[[1]])
  expect_is(a, "numeric")
  expect_length(a, 1)
})