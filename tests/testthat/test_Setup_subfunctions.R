test_that("check age_parameters_pig_func works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes_pig = 100, pig_age_class_width = 1) 
  expect_is(pig_age_parameters, "list")
  expect_error(pig_age_parameters <- age_parameters_pig_func(number_age_classes_pig = 0, pig_age_class_width = 1) )
  
})


test_that("check life_tables_pigs_func works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes_pig = 100, pig_age_class_width = 1) 
  pig_lifetable_output <- life_tables_pigs_func(number_age_classes_pig = 100, slaughter_age_min = 6, slgtage = 7, 
                                                slgtage_bfr = 6, dP = 0.008333333, dPslg = 0.08333333, na_pig = pig_age_parameters[[2]])
  expect_is(pig_lifetable_output, "list")
 
})


test_that("check pig_ageclass_proportions works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes_pig = 100, pig_age_class_width = 1) 
  pig_lifetable_output <- life_tables_pigs_func(number_age_classes_pig = 100, slaughter_age_min = 6, slgtage = 7, 
                                                slgtage_bfr = 6, dP = 0.008333333, dPslg = 0.08333333, na_pig = pig_age_parameters[[2]])
  pig_ageclass_proportions <- Pig_age_class_proportions_func(PPS = 2000, pig_demography = pig_lifetable_output, na_pig = pig_age_parameters[[2]], 
                                                             IPL0_total = 80, IPH0_total = 320)
  expect_is(pig_ageclass_proportions, "list")
})


test_that("check human_ageclass_proportions works", {
  
  s1 <- set_up(PCPrev = 0.2)
  
  #======================#
  # Set-up test age-rate #
  # vector of specified no. of months in each age compartment 
  age_width_human_test <- c()
  age_width_human_test[1] <- 5 * 12 # 0 - 4.99 yrs (in months)
  age_width_human_test[2] <- 5 * 12 # 5 - 9.99 yrs
  age_width_human_test[3] <- 5 * 12 # 10- 14.99 yrs
  age_width_human_test[4] <- 15 * 12 # 15 - 29.99 yrs
  age_width_human_test[5] <- 20 * 12 # 30 - 49.99 yrs
  age_width_human_test[6] <- 20 * 12 # 50 - 69.99 yrs
  age_width_human_test[7] <- 20 * 12 # 70 - 89.99 yrs
  
  
  # create vector of length n age classes for ODIN (& create dimensions for other variables)
  na_human_test <- as.integer(length(age_width_human_test)) 
  
  # calculate age rate (function of age width)
  age_rate_human_test <- c()
  
  for (i in na_human_test) {
    age_rate_human_test[1:(na_human_test - 1)] <- 1 / age_width_human_test[1:i - 1]
    age_rate_human_test[na_human_test] <- 0
  }
  #=======================#
  
  human_ageclass_proportions <- human_age_class_proportions_func(age_rate = age_rate_human_test, na_human = na_human_test, 
                                                                 dH = s1[[1]]$dH, HPS = s1[[1]]$HPS, SHC0_total = 686, 
                                                                 IH0_total = 186, IHC0_total = 14)
  expect_is(human_ageclass_proportions, "list")
  expect_error(human_ageclass_proportions <- human_age_class_proportions_func(age_rate = age_rate_human_test, na_human = 1, 
                                                                              dH = s1[[1]]$dH, HPS = s1[[1]]$HPS, SHC0_total = 686, 
                                                                              IH0_total = 186, IHC0_total = 14))
  expect_length(human_ageclass_proportions, 4)
  
  
})