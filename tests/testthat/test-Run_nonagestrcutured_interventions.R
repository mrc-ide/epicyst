test_that("Model run (non age-structured pig model, with pig MDA) test", {
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  mi_1 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]],  intervention = 'Pig_MDA', intervention_time = 1)
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], 
                         initial_states = s1[[2]], intervention_time = 1))
  
  mi_2 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]],  intervention = "Pig_vaccine", intervention_time = 1)
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  expect_error(run_model(time = 10, intervention = "Pig_vaccine", age_target_pig_vaccine = c(4:150), params = s1[[1]], 
                         initial_states = s1[[2]], intervention_time = 1))
  
  mi_3 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]],  intervention = c('Pig_MDA', 'Pig_vaccine'), intervention_time = 1)
  expect_is(mi_3, "data.frame")
  expect_is(mi_3[, 1], "numeric")
  
  expect_error(time = 10, intervention = "Pig_MDA", age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:7), 
               params = s1[[1]], initial_states = s1[[2]])
  expect_error(time = 10, intervention = "Pig_vaccine", age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7), 
               params = s1[[1]], initial_states = s1[[2]])
 
})


 test_that("Model run (non age-structured human model, with human MDA - praziquantel) test", {
  s1 <- set_up(number_age_classes_human = 1)
  mi_1 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]], intervention = 'Human_MDA_pzq', intervention_time = 1)
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  expect_error(run_model(time = 10, intervention = 'Human_MDA_pzq',  age_target_human_MDA = c(2:3), intervention_time = 1, 
                         params = s1[[1]], initial_states = s1[[2]]))
  
  expect_error(run_model(time = 10, intervention = c('Human_MDA_pzq','human_MDA_nic'), intervention_time = 1, 
                         params = s1[[1]], initial_states = s1[[2]]))

})


 test_that("Model run (non age-structured human model, with human MDA - niclosamide) test", {
   s1 <- set_up(number_age_classes_human = 1)
   mi_1 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]], intervention = 'Human_MDA_nic', intervention_time = 1)
   expect_is(mi_1, "data.frame")
   expect_is(mi_1[, 1], "numeric")
   expect_error(run_model(time = 10, intervention = 'Human_MDA_nic',  age_target_human_MDA = c(2:3), intervention_time = 1, 
                          params = s1[[1]], initial_states = s1[[2]]))
   
 })
 
 test_that("Model run (non age-structured human model, with human test & treat) test", {
   s1 <- set_up(number_age_classes_human = 1)
   mi_1 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]], intervention = 'Human_test_and_treat', intervention_time = 1)
   expect_is(mi_1, "data.frame")
   expect_is(mi_1[, 1], "numeric")
   expect_error(run_model(time = 10, intervention = 'Human_test_and_treat',  age_target_human_test_and_treat = c(2:3), intervention_time = 1, 
                          params = s1[[1]], initial_states = s1[[2]]))
   
 })
 
 test_that("Model run (non age-structured human model, with human MDA - praziquantel or niclosamide & test and treat) test", {
   s1 <- set_up(number_age_classes_human = 1)
   mi_1 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]], intervention = c('Human_MDA_pzq', 'Human_test_and_treat'), intervention_time = 1)
   expect_is(mi_1, "data.frame")
   expect_is(mi_1[, 1], "numeric")
   
   mi_2 <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]], intervention = c('Human_MDA_nic', 'Human_test_and_treat'), intervention_time = 1)
   expect_is(mi_2, "data.frame")
   expect_is(mi_2[, 1], "numeric")
   
   expect_error(run_model(time = 10, intervention = c('Human_MDA_nic', 'Human_MDA_pzq', 'Human_test_and_treat'),  intervention_time = 1, 
                          params = s1[[1]], initial_states = s1[[2]]))
 })
 

 
