test_that("Setup basic tests", {
  expect_length(set_up(), 2)
  expect_is(set_up(), "list")
  expect_is(unlist(set_up()), "numeric")
  expect_error(set_up(TPrev = 1, PCPrev = 0.00001))
})

test_that("Setup: prevalence adjustments", {
  s1 <- set_up(PCPrev_true = 0.2)
  expect_length(s1[[1]], 36)
  expect_length(s1[[2]], 13)
  expect_equal(s1[[1]]$PCPrev_new, PCPrev_true = 0.2, s1[[1]]$PCPrev_new)
  expect_error(PCPrev_obs = 0.3, PC_sens = 1, PC_spec = 0.5)
})

