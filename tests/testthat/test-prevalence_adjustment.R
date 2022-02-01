test_that("Prevalence adjustment function #1 works", {
  s1 <- set_up(PCPrev_obs = 0.3, PC_sens = 0.75, PC_spec = 0.75)
  PCPrev <- prev_adjustment_TP_func(sens = 0.75, spec = 0.75, positive = 600, total = 2000)
  expect_is(PCPrev, "numeric")
  
})

test_that("Prevalence adjustment function #2 works", {
  s1 <- set_up(PCPrev_true = 0.25, PC_sens = 0.8, PC_spec = 0.8)
  PCPrev <- prev_adjustment_AP_func(sens = 0.75, spec = 0.75, TP = 0.25)
  expect_is(PCPrev, "numeric")
  
})

test_that("Prevalence adjustment function #2 works", {
  s1 <- set_up(PCPrev_true = 0.25, PC_sens = 0.8, PC_spec = 0.8)
  apparent_prev_out <- apparent_prevalence_packaging_func(sens = 0.75, spec = 0.75, TP = 0.25)
  expect_is(apparent_prev_out, "data.frame")
  expect_is(apparent_prev_out[, 1], "numeric")

})