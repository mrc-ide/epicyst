test_that("Model run (non age-structured pig model) test", {
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  # s1 <- set_up(number_age_classes_pig = 1)
  # expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
  mi_bi <- run_model(burn_in = 50, time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi_bi, "data.frame")
  expect_is(mi_bi[, 1], "numeric")
  
})


test_that("Model run (non age-structured human model) test", {
  s1 <- set_up(number_age_classes_human = 1)
  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  # s1 <- set_up(number_age_classes_pig = 1)
  # expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
})


