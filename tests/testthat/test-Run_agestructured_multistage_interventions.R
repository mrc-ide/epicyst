test_that("Model run (with intervention; age-structured pig intervention) tests - pig vaccine (two-stage)", {
  
  mi_1 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                  intervention_stage2 = c('Pig_MDA'),
                  intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                  num_intervention_rounds_stage1 = 3, 
                  intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                  pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                  age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150))
  expect_is(mi_1, "data.frame")
  expect_is(mi_1[, 1], "numeric")
  
  mi_1a <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                    intervention_stage2 = c('Pig_MDA'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75)
  expect_is(mi_1a, "data.frame")
  expect_is(mi_1a[, 1], "numeric")
  
  mi_1b <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                    intervention_stage2 = c('Pig_MDA'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    age_target_pig_MDA_stage1 = c(4:7))
  expect_is(mi_1b, "data.frame")
  expect_is(mi_1b[, 1], "numeric")
  
  mi_1c <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage2 = c(4:7))
  expect_is(mi_1c, "data.frame")
  expect_is(mi_1c[, 1], "numeric")
  
  mi_2 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
            intervention_stage2 = c('Pig_vaccine'),
            intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
            num_intervention_rounds_stage1 = 3, 
            intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
            pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
            pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
            age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_2, "data.frame")
  expect_is(mi_2[, 1], "numeric")
  
  mi_2a <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                      intervention_stage2 = c('Pig_vaccine'),
                      intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                      num_intervention_rounds_stage1 = 3, 
                      intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                      pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                      pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9) 
  
  expect_is(mi_2a, "data.frame")
  expect_is(mi_2a[, 1], "numeric")
  
  mi_2b <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                   intervention_stage2 = c('Pig_vaccine'),
                   intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                   num_intervention_rounds_stage1 = 3, 
                   intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                   pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                   pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                   age_target_pig_vaccine_stage1 = c(4:7))
  expect_is(mi_2b, "data.frame")
  expect_is(mi_2b[, 1], "numeric")
  
  mi_2c <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_2c, "data.frame")
  expect_is(mi_2c[, 1], "numeric")
  
  mi_3 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150),
                    age_target_pig_vaccine_stage2 = c(4:150))
  expect_is(mi_3, "data.frame")
  expect_is(mi_3[, 1], "numeric")
  
  mi_3a <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75)
  expect_is(mi_3a, "data.frame")
  expect_is(mi_3a[, 1], "numeric")
  
  mi_3b <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    age_target_pig_MDA_stage1 = c(4:7),
                    age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_3b, "data.frame")
  expect_is(mi_3b[, 1], "numeric")
  
  mi_3c <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                     age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_3c, "data.frame")
  expect_is(mi_3c[, 1], "numeric")
  
  mi_3d <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage1 = c(4:7))
  expect_is(mi_3d, "data.frame")
  expect_is(mi_3d[, 1], "numeric")
  
  mi_3e <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
                     intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                     age_target_pig_MDA_stage2 = c(4:7))
  expect_is(mi_3e, "data.frame")
  expect_is(mi_3e[, 1], "numeric")
  
  
  mi_4 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_MDA_stage2 = c(4:150),
                    age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:150))
  expect_is(mi_4, "data.frame")
  expect_is(mi_4[, 1], "numeric")
  
  mi_4a <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9)
  expect_is(mi_4a, "data.frame")
  expect_is(mi_4a[, 1], "numeric")
  
  mi_4b <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_MDA_stage2 = c(4:7))
  expect_is(mi_4b, "data.frame")
  expect_is(mi_4b[, 1], "numeric")
  
  mi_4c <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_vaccine_stage1 = c(4:7), age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_4c, "data.frame")
  expect_is(mi_4c[, 1], "numeric")
  
  mi_4d <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9,
                    age_target_pig_vaccine_stage1 = c(4:7))
  expect_is(mi_4d, "data.frame")
  expect_is(mi_4d[, 1], "numeric")
  
  mi_4e <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
                     intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9,
                     age_target_pig_vaccine_stage2 = c(4:7))
  expect_is(mi_4e, "data.frame")
  expect_is(mi_4e[, 1], "numeric")
  
  mi_5 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150), 
                    pig_MDA_prop_noimmunity = 1.0, 
                    age_target_pig_vaccine_stage1 = c(4:150))
  expect_is(mi_5, "data.frame")
  expect_is(mi_5[, 1], "numeric")
  
  mi_5a <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                    intervention_stage2 = c('Pig_MDA'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150))
  expect_is(mi_5a, "data.frame")
  expect_is(mi_5a[, 1], "numeric")
  
  mi_5b <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                     intervention_stage2 = c('Pig_MDA'),
                     intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                     num_intervention_rounds_stage1 = 3, 
                     intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                     pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                     pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                     age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150))
  expect_is(mi_5b, "data.frame")
  expect_is(mi_5b[, 1], "numeric")
  
  
  mi_6 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                    intervention_stage2 = c('Pig_vaccine'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9, 
                    pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
                    pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
                    age_target_pig_MDA_stage1 = c(4:150), 
                    pig_MDA_prop_noimmunity = 1.0, 
                    age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:150))
  expect_is(mi_6, "data.frame")
  expect_is(mi_6[, 1], "numeric")
  
  
  mi_7 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
            intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
            intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
            num_intervention_rounds_stage1 = 3, 
            intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
            pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
            pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
            pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9, 
            age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150), 
            pig_MDA_prop_noimmunity = 1.0, 
            age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:150))
  expect_is(mi_7, "data.frame")
  expect_is(mi_7[, 1], "numeric")
  
  
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = "Pig_vaccine", params = s1[[1]], initial_states = s1[[2]],
                         age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7)))
  
  expect_error(run_model(time = 10, intervention_stage1 = c('Pig_MDA'),  intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                         num_intervention_rounds_stage1 = 3, intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                         pig_MDA_cov_stage1 = 0.9))
  expect_error(run_model(time = 10, intervention_stage1 = c('Pig_vaccine'),  intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                         num_intervention_rounds_stage1 = 3, intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                         pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage1 = 0.75))
  
  mi_8 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Sanitation'), 
                    intervention_stage2 = c('Sanitation'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6)
  expect_is(mi_8, "data.frame")
  expect_is(mi_8[, 1], "numeric")
  
  mi_8 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA', 'Sanitation'), 
                    intervention_stage2 = c('Pig_MDA'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150))
  expect_is(mi_8, "data.frame")
  expect_is(mi_8[, 1], "numeric")
  
  mi_9 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
            intervention_stage2 = c('Pig_MDA', 'Sanitation'),
            intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
            num_intervention_rounds_stage1 = 3, 
            intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
            pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
            age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150)) 
  expect_is(mi_9, "data.frame")
  expect_is(mi_9[, 1], "numeric")
  
  mi_10 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Sanitation'), 
                    intervention_stage2 = c('Pig_MDA', 'Sanitation'),
                    intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                    num_intervention_rounds_stage1 = 3, 
                    intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                    pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75, 
                    age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:150)) 
  expect_is(mi_10, "data.frame")
  expect_is(mi_10[, 1], "numeric")
  
  mi_11 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_vaccine'), 
            intervention_stage2 = c('Pig_vaccine'),
            intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
            num_intervention_rounds_stage1 = 3, 
            intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
            pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75, 
            pig_vaccine_ds1_cov_stage1 = 0.9, pig_vaccine_ds2_cov_stage1 = 0.9)
  expect_is(mi_11, "data.frame")
  expect_is(mi_11[, 1], "numeric")
  
  mi_12 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA'), 
            intervention_stage2 = c('Pig_MDA'),
            intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
            num_intervention_rounds_stage1 = 3, 
            intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
            pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75) 
  expect_is(mi_11, "data.frame")
  expect_is(mi_11[, 1], "numeric")
  
  
})

test_that("Model run (multi-staged - expected error messages) test", {
  
  s1 <- set_up(number_age_classes_pig = 1, slaughter_age_min = 0)
  expect_error(run_model(time = 10, intervention = "Pig_vaccine", params = s1[[1]], initial_states = s1[[2]],
                         age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7)))
  
  
  expect_error(run_model(time = 11, burn_in = 50, intervention_stage1 = c('Human_MDA_pzq'), 
                      intervention_stage2 = c('Human_MDA_pzq'),
                      intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, 
                      num_intervention_rounds_stage1 = 3, 
                      intervention_time_stage2 = 1.75, intervention_frequency_stage2 = 6, 
                      pig_MDA_cov_stage1 = 0.9,  pig_MDA_cov_stage2 = 0.75)) 
  
  
})