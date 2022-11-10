
test_that("check pre_pig_MDA function", {
  
  s1 <- set_up(PCPrev = 0.3)
  tt1 <- seq(0, (11/2 * 12) - 1 / 30, 1 / 30)
  # set-up tail states
  bl <- single_run(tt1, params = s1[[1]], states = s1[[2]])
  runs <- list()
  runs[[1]] <- bl
  tail_states <- inter_run_setup(
    model_output = runs[[1]], na_pig = s1[[1]]$na_pig, na_human = s1[[1]]$na_human
  )
  
  # run function to test
  p <- pre_pig_MDA(age_target = c(4:150), tail_states = tail_states)
  
  # tests
  expect_is(p, "list")
  expect_length(p, 5)
  #expect_error(p <- pre_pig_MDA(age_target = c(4:151), tail_states = tail_states))
  
})


test_that("check pre_pig_vaccine function works", {
  
  s1 <- set_up(PCPrev = 0.3)
  tt1 <- seq(0, (11/2 * 12) - 1 / 30, 1 / 30)
  
  # set-up tail states
  bl <- single_run(tt1, params = s1[[1]], states = s1[[2]])
  runs <- list()
  runs[[1]] <- bl
  tail_states <- inter_run_setup(
    model_output = runs[[1]], na_pig = s1[[1]]$na_pig, na_human = s1[[1]]$na_human
  )
  
  # run function to test
  p <- pre_pig_vaccine(age_target = c(4:150), tail_states = tail_states)
  
  # tests
  expect_is(p, "list")
  expect_length(p, 2)
  
})


test_that("check update_states function works", {
  
  # set up initial params and timings
  s1 <- set_up(PCPrev = 0.3)
  tt1 <- seq(0, (11/2 * 12) - 1 / 30, 1 / 30)
  
  # set-up tail states
  bl <- single_run(tt1, params = s1[[1]], states = s1[[2]])
  runs <- list()
  runs[[1]] <- bl
  tail_states <- inter_run_setup(
    model_output = runs[[1]], na_pig = s1[[1]]$na_pig, na_human = s1[[1]]$na_human
  )
  
  # run function to test
  p <- pre_pig_vaccine(age_target = c(4:150), tail_states = tail_states)
  
  # need intervention effect size object first (from intervention_effects.R)
  int_effect_size_list <- intervention_effect_size_set_up(
    pig_MDA_cov = 0.9,
    pig_vaccine_ds1_cov = 0.9,
    pig_vaccine_ds2_cov = 1.0,
    human_testtreat_cov = 0.9,
    human_MDAnic_cov = 0.75,
    human_MDApzq_cov = 0.75,
    pig_MDA_prop_noimmunity = 0.1
  )
  
  # final steps in model_wrappers
  states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
  states_test <- update_states(states_move = states_move_age_pig_vaccine, tail_states = tail_states)
  
  # tests 
  expect_is(states_test, "list")
  
})


test_that("check age_struc_pig_vacc_func works", {
  
  s1 <- set_up(PCPrev = 0.3)
  age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = s1[[1]]$na_pig, intervention_frequency = 12)
  expect_vector(age_target_pig_vaccine)
  intervention_frequency_test <- 12+2
  length_expect_test <- length(c(intervention_frequency_test:s1[[1]]$na_pig))
  expect_length(age_target_pig_vaccine, length_expect_test)
  
})

test_that("check pre_human_MDA function", {
  
  s1 <- set_up(PCPrev = 0.2)
  tt1 <- seq(0, (11/2 * 12) - 1 / 30, 1 / 30)
  # set-up tail states
  bl <- single_run(tt1, params = s1[[1]], states = s1[[2]])
  runs <- list()
  runs[[1]] <- bl
  tail_states <- inter_run_setup(
    model_output = runs[[1]], na_pig = s1[[1]]$na_pig, na_human = s1[[1]]$na_human
  )
  
  # run function to test
  p <- pre_human_MDA(age_target = c(2:4), tail_states = tail_states)
  
  # tests
  expect_is(p, "list")
  expect_length(p, 4)
  
})

test_that("check pre_human_test_and_treat function", {
  
  s1 <- set_up(PCPrev = 0.3)
  tt1 <- seq(0, (11/2 * 12) - 1 / 30, 1 / 30)
  # set-up tail states
  bl <- single_run(tt1, params = s1[[1]], states = s1[[2]])
  runs <- list()
  runs[[1]] <- bl
  tail_states <- inter_run_setup(
    model_output = runs[[1]], na_pig = s1[[1]]$na_pig, na_human = s1[[1]]$na_human
  )
  
  # run function to test
  p <- pre_human_test_and_treat(age_target = c(2:4), tail_states = tail_states)
  
  # tests
  expect_is(p, "list")
  expect_length(p, 2)
  
})


