test_that("Model run (with intervention; non-age strcutured & age-structured pig intervention) tests - pig MDA", {
  mi_1 <- run_model(time = 10, intervention = "Pig_MDA")
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_2 <- run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
  s1 <- set_up(number_age_classes_pig = 1)
  expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
})

test_that("Model run (with intervention; non-age strcutured & age-structured pig intervention) tests - pig vaccine", {
  mi_1 <- run_model(time = 10, intervention = "Pig_vaccine")
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_2 <- run_model(time = 10, intervention = "Pig_vaccine", age_target_pig_vaccine = c(4:150))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
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

test_that("Model run (with combined interventions; age-structured pig interventions) tests - pig MDA & vaccine", {
  
  mi <- run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
  mi_1 <- run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_MDA = c(4:150), age_target_pig_vaccine = c(4:150))
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_2 <- run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_MDA = c(4:150))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
  mi_3 <- run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_vaccine = c(4:150))
  expect_is(mi_3, "data.frame")
  expect_is(mi_3[, 1], "numeric")
  
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = c('Pig_MDA','Pig_vaccine'), age_target_pig_MDA = c(4:150), age_target_pig_vaccine = c(4:150), params = s1[[1]], initial_states=s1[[2]]))
})


test_that("Model run (with intervention; age-structured human intervention) tests - human MDA with praziquantel", {
  mi <- run_model(time = 10, intervention = "Human_MDA_pzq", age_target_human_MDA = c(2:3))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  s1 <- set_up(number_age_classes_human = 1)
  expect_error(run_model(time = 10, intervention = "Human_MDA_pzq", age_target_human_MDA = c(2:3), params = s1[[1]], initial_states = s1[[2]]))
})

test_that("Model run (with intervention; age-structured human intervention) tests - human MDA with niclosamide", {
  mi <- run_model(time = 10, intervention = "Human_MDA_nic", age_target_human_MDA = c(2:3))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
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

test_that("Model run (with intervention; multiple age-structured human intervention) tests", {
  
  mi <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Human_test_and_treat"))
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  
  mi_1 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Human_test_and_treat"), age_target_human_MDA = c(2:3), age_target_human_test_and_treat = c(2:3))
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_2 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Human_test_and_treat"), age_target_human_MDA = c(2:3))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
  mi_3 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Human_test_and_treat"), age_target_human_test_and_treat = c(2:3))
  expect_is(mi_3, "data.frame")
  expect_is(mi_3[, 1], "numeric")
  
  mi_4 <- run_model(time = 10, intervention = c("Human_MDA_nic", "Human_test_and_treat"), age_target_human_MDA = c(2:3), age_target_human_test_and_treat = c(2:3))
  expect_is(mi_4, "data.frame")
  expect_is(mi_4[, 1], "numeric")
  
  mi_5 <- run_model(time = 10, intervention = c("Human_MDA_nic", "Human_test_and_treat"), age_target_human_MDA = c(2:3))
  expect_is(mi_5, "data.frame")
  expect_is(mi_5[, 1], "numeric")
  
  mi_6 <- run_model(time = 10, intervention = c("Human_MDA_nic", "Human_test_and_treat"), age_target_human_test_and_treat = c(2:3))
  expect_is(mi_6, "data.frame")
  expect_is(mi_6[, 1], "numeric")
})

test_that("Model run (with intervention; multiple age-structured human intervention) tests generating expected errors", {
  
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic")))
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic"), age_target_human_MDA = c(2:3)))
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic", "Human_test_and_treat"), 
                         age_target_human_MDA = c(2:3), age_target_human_test_and_treat = c(2:3)))
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic", "Human_test_and_treat"), 
                         age_target_human_MDA = c(2:3)))
  expect_error(run_model(time = 10, intervention = c("Human_MDA_pzq","Human_MDA_nic", "Human_test_and_treat"), 
                         age_target_human_test_and_treat = c(2:3)))
  
})
  
test_that("Model run (with pig and human combined interventions) tests", {
  
  mi_1 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Pig_MDA"))
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_2 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Pig_MDA"), age_target_pig_MDA = c(4:150))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
  mi_3 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Pig_vaccine"))
  expect_is(mi_3, "data.frame")
  expect_is(mi_3[, 1], "numeric")
  
  mi_4 <- run_model(time = 10, intervention = c("Human_MDA_pzq", "Pig_vaccine"), age_target_pig_vaccine = c(4:150))
  expect_is(mi_4, "data.frame")
  expect_is(mi_4[, 1], "numeric")
  
})


