test_that("Setup basic tests", {
  expect_length(set_up(), 2)
  expect_is(set_up(), "list")
  expect_is(unlist(set_up()), "numeric")
  expect_error(set_up(TPrev = 1, PCPrev = 0.00001))
})
