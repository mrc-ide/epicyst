test_that("Model run (with intervention; number of intervention rounds specified)", {
  mi <- run_model(time = 10, intervention = "Pig_MDA", intervention_time = 1, num_intervention_rounds = 6)
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
})

test_that("Model run (with intervention; human MDA with praziquantel (PZQ))", {
  mi <- run_model(time = 10, intervention = "Pig_MDA", intervention_time = 1, num_intervention_rounds = 6)
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
})


# run_model <-
#   function(params = NULL,
#            initial_states = NULL,
#            time,
#            intervention = NULL,
#            intervention_time = time / 2,
#            intervention_effect = intervention_effect_size(),
#            intervention_frequency = 12,
#            step = 1 / 30,
#            burn_in = 0,
#            age_target_pig_MDA = NULL,
#            age_target_pig_vaccine = NULL,
#            num_intervention_rounds = NULL,
#            pig_MDA_cov = NULL,
#            pig_vaccine_ds1_cov = NULL,
#            pig_vaccine_ds2_cov = NULL,
#            pig_MDA_prop_noimmunity = NULL,
#            human_testtreat_cov = NULL,
#            human_MDAnic_cov = NULL,
#            human_MDApzq_cov = NULL,
#            age_target_human_MDA = NULL,
#            age_target_human_test_and_treat = NULL,
#            intervention_stage1 = NULL,
#            intervention_stage2 = NULL,
#            intervention_time_stage1 = NULL,
#            intervention_time_stage2 = NULL,
#            intervention_frequency_stage1 = NULL,
#            intervention_frequency_stage2 = NULL,
#            num_intervention_rounds_stage1 = NULL,
#            num_intervention_rounds_stage2 = NULL,
#            age_target_pig_MDA_stage1 = NULL,
#            age_target_pig_MDA_stage2 = NULL,
#            age_target_pig_vaccine_stage1 = NULL,
#            age_target_pig_vaccine_stage2 = NULL,
#            pig_MDA_cov_stage1 = NULL,
#            pig_MDA_cov_stage2 = NULL,
#            pig_vaccine_ds1_cov_stage1 = NULL,
#            pig_vaccine_ds1_cov_stage2 = NULL,
#            pig_vaccine_ds2_cov_stage1 = NULL,
#            pig_vaccine_ds2_cov_stage2 = NULL) 