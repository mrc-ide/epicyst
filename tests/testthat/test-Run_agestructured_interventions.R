test_that("Model run (with intervention; age-structured pig intervention) tests - pig MDA", {
  mi <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_pig = 1)
  expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
})

test_that("Model run (with intervention; age-structured pig intervention) tests - pig vaccine", {
  mi <- run_model(time = 10, intervention = "Pig_vaccine", age_target_pig_vaccine = c(4:150))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = "Pig_vaccine", age_target_pig_vaccine = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
})

test_that("Model run (with intervention; age-structured pig intervention) tests - pig MDA (two-stage)", {
  mi <- run_model(time = 10, intervention = "Pig_MDA", 
                  age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:7))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = "Pig_MDA", params = s1[[1]], initial_states = s1[[2]], 
                         age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:7)))
})

test_that("Model run (with intervention; age-structured pig intervention) tests - pig vaccine (two-stage)", {
  mi <- run_model(time = 10, intervention = "Pig_vaccine", 
                  age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = "Pig_vaccine", params = s1[[1]], initial_states = s1[[2]],
               age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7)))
})

test_that("Model run (with intervention; age-structured human intervention) tests - human MDA with praziquantel", {
  mi <- run_model(time = 10, intervention = "Human_MDA_pzq", age_target_human_MDA = c(2:3))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic"), age_target_human_MDA = c(2:3)))
  s1 <- set_up(number_age_classes_human = 1)
  expect_error(run_model(time = 10, intervention = "Human_MDA_pzq", age_target_human_MDA = c(2:3), params = s1[[1]], initial_states = s1[[2]]))
})

test_that("Model run (with intervention; age-structured human intervention) tests - human MDA with niclosamide", {
  mi <- run_model(time = 10, intervention = "Human_MDA_nic", age_target_human_MDA = c(2:3))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  expect_error(run_model(time = 10, intervention = c("Human_MDA_nic", "Human_MDA_pzq"), age_target_human_MDA = c(2:3)))
  s1 <- set_up(number_age_classes_human = 1)
  expect_error(run_model(time = 10, intervention = "Human_MDA_nic", age_target_human_MDA = c(2:3), params = s1[[1]], initial_states = s1[[2]]))
})

test_that("Model run (with intervention; age-structured human intervention) tests - human test and treat", {
  mi <- run_model(time = 10, intervention = "Human_test_and_treat", age_target_human_test_and_treat = c(2:3))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_human = 1)
  expect_error(run_model(time = 10, intervention = "Human_test_and_treat", age_target_human_test_and_treat = c(2:3), params = s1[[1]], initial_states = s1[[2]]))
})

test_that("Model run (with combined interventions; age-structured pig interventions) tests - pig MDA & vaccine", {
  mi <- run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_MDA = c(4:150), age_target_pig_vaccine = c(4:150))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_MDA = c(4:150), age_target_pig_vaccine = c(4:150), params = s1[[1]], initial_states=s1[[2]]))
})

# HumanMDASAC_test <- run_model(time = 11, burn_in=50, intervention='Human_MDA_pzq', intervention_time=1, 
#                               intervention_frequency = 6, human_MDApzq_cov = 0.75, age_target_human_MDA = c(2:3),
#                               params = S1[[1]], initial_states = S1[[2]]) 