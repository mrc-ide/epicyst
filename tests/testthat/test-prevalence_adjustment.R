test_that("Prevalence adjustment function #1 works (PCPrev)", {
  s1 <- set_up(PCPrev_obs = 0.3, PC_sens = 0.75, PC_spec = 0.75)
  PCPrev <- prev_adjustment_TP_func(sens = 0.75, spec = 0.75, positive = 600, total = 2000)
  expect_is(PCPrev, "numeric")
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
})

test_that("Prevalence adjustment function #2 works (PCPrev)", {
  s1 <- set_up(PCPrev_true = 0.25, PC_sens = 0.8, PC_spec = 0.8)
  PCPrev <- prev_adjustment_AP_func(sens = 0.75, spec = 0.75, TP = 0.25)
  expect_is(PCPrev, "numeric")
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
})

test_that("Prevalence adjustment function #3 works (PCPrev)", {
  s1 <- set_up(PCPrev_true = 0.25, PC_sens = 0.8, PC_spec = 0.8)
  apparent_prev_out <- apparent_prevalence_packaging_func(sens = 0.75, spec = 0.75, TP = 0.25)
  expect_is(apparent_prev_out, "data.frame")
  expect_is(apparent_prev_out[, 1], "numeric")
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")

})

test_that("Prevalence adjustment function (TPrev)", {
  s1 <- set_up(TPrev_obs = 0.3, T_sens = 0.75, T_spec = 0.75)
  TPrev <- prev_adjustment_TP_func(sens = 0.75, spec = 0.75, positive = 600, total = 2000)
  expect_is(TPrev, "numeric")
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
})

test_that("Prevalence adjustment function (CPrev)", {
  s1 <- set_up(CPrev_obs = 0.3, C_sens = 0.75, C_spec = 0.75)
  CPrev <- prev_adjustment_TP_func(sens = 0.75, spec = 0.75, positive = 600, total = 2000)
  expect_is(CPrev, "numeric")
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
})



