test_that("Model run (non age-structured pig model) test", {
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  # s1 <- set_up(number_age_classes_pig = 1)
  # expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
})


test_that("Model run (non age-structured human model) test", {
  s1 <- set_up(number_age_classes_human = 1)
  mi <- run_model(time = 10, params = s1[[1]], initial_states = s1[[2]])
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
  # s1 <- set_up(number_age_classes_pig = 1)
  # expect_error(run_model(time = 10, intervention = "Pig_MDA", age_target_pig_MDA = c(4:150), params = s1[[1]], initial_states = s1[[2]]))
  
})


# S1_nonage <-  set_up(
#   number_age_classes_pig = 1,
#   slaughter_age_min = 0,
#   PCPrev_true = 0.2581, 
#   CPrev_true = 0.0611, 
#   TPrev_true= 0.03055,
#   HPS = 12000,
#   PPS = 1000,
#   LEP = 15,
#   slgEP = 1, 
#   psi = psi_input_test
# ) # for 25% PCC prev
# 
# # S1_nonage[[1]]
# # S1_nonage[[2]]
# 
# #  note with above: object 'age_rate_pig' not found - look in set_up func
# 
# m1_test <- run_model(time = 100, burn_in = 100, params = S1_nonage[[1]], initial_states = S1_nonage[[2]]) #