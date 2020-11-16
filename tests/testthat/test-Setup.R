test_that("Setup basic tests", {
  expect_length(Set_up(), 2)
  expect_is(Set_up(), 'list')
  expect_is(unlist(Set_up()), 'numeric')
  expect_error(Set_up(TPrev=1, PCPrev=0.00001))
})