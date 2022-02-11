test_that("Prevalence adjustment function (PCPrev; true prev to apparent prev)", {
  s1 <- set_up(PCPrev = 0.3, PC_sens = 0.75, PC_spec = 0.75)

  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  expect_is(mi$Pig_cysticercosis_apparent_prev, "numeric")
  
  mi_i <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi_i, "data.frame")
  expect_is(mi_i[, 1], "numeric")
  expect_is(mi$Pig_cysticercosis_apparent_prev, "numeric")
  
  mi_i2 <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]],
                     num_intervention_rounds = 3)
  expect_is(mi_i2, "data.frame")
  expect_is(mi_i2[, 1], "numeric")
  expect_is(mi$Pig_cysticercosis_apparent_prev, "numeric")
  
  mi_i3 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                      intervention_stage2 = c('Pig_MDA'),
                      intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                      num_intervention_rounds_stage1 = 3, 
                      intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                      pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                      age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i3, "data.frame")
  expect_is(mi_i3[, 1], "numeric")
  expect_is(mi$Pig_cysticercosis_apparent_prev, "numeric")
  
  mi_i4 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, num_intervention_rounds_stage2 = 3,
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i4, "data.frame")
  expect_is(mi_i4[, 1], "numeric")
  expect_is(mi$Pig_cysticercosis_apparent_prev, "numeric")
  
})

test_that("Prevalence adjustment function (TPrev; true prev to apparent prev)", {
  s1 <- set_up(TPrev = 0.05, T_sens = 0.75, T_spec = 0.75)
  
  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  expect_is(mi$Human_taeniasis_apparent_prev, "numeric")
  
  mi_i <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi_i, "data.frame")
  expect_is(mi_i[, 1], "numeric")
  expect_is(mi$Human_taeniasis_apparent_prev, "numeric")
  
  mi_i2 <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]],
                     num_intervention_rounds = 3)
  expect_is(mi_i2, "data.frame")
  expect_is(mi_i2[, 1], "numeric")
  expect_is(mi$Human_taeniasis_apparent_prev, "numeric")
  
  mi_i3 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i3, "data.frame")
  expect_is(mi_i3[, 1], "numeric")
  expect_is(mi$Human_taeniasis_apparent_prev, "numeric")
  
  mi_i4 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, num_intervention_rounds_stage2 = 3,
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i4, "data.frame")
  expect_is(mi_i4[, 1], "numeric")
  expect_is(mi$Human_taeniasis_apparent_prev, "numeric")
  
})

test_that("Prevalence adjustment function (CPrev; true prev to apparent prev)", {
  s1 <- set_up(CPrev = 0.1, C_sens = 0.75, C_spec = 0.75)

  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  expect_is(mi$Human_cysticercosis_apparent_prev, "numeric")
  
  mi_i <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi_i, "data.frame")
  expect_is(mi_i[, 1], "numeric")
  expect_is(mi$Human_cysticercosis_apparent_prev, "numeric")
  
  mi_i2 <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]],
                     num_intervention_rounds = 3)
  expect_is(mi_i2, "data.frame")
  expect_is(mi_i2[, 1], "numeric")
  expect_is(mi$Human_cysticercosis_apparent_prev, "numeric")
  
  mi_i3 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i3, "data.frame")
  expect_is(mi_i3[, 1], "numeric")
  expect_is(mi$Human_cysticercosis_apparent_prev, "numeric")
  
  mi_i4 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, num_intervention_rounds_stage2 = 3,
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                     params = s1[[1]], initial_states = s1[[2]],)
  expect_is(mi_i4, "data.frame")
  expect_is(mi_i4[, 1], "numeric")
  expect_is(mi$Human_cysticercosis_apparent_prev, "numeric")
  
})



