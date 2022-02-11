test_that("check age_parameters_pig_func works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes = 100, pig_age_class_width = 1) 
  expect_is(pig_age_parameters, "list")
  expect_error(pig_age_parameters <- age_parameters_pig_func(number_age_classes = 0, pig_age_class_width = 1) )
  
})


test_that("check life_tables_pigs_func works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes = 100, pig_age_class_width = 1) 
  pig_lifetable_output <- life_tables_pigs_func(number_age_classes = 100, slgt_age_min = 6, slgtage = 7, 
                                                slgtage_bfr = 6, dP = 0.008333333, dPslg = 0.08333333, na_pig = pig_age_parameters[[2]])
  expect_is(pig_lifetable_output, "list")
  expect_error(pig_lifetable_output <- life_tables_pigs_func(number_age_classes = 1, slgt_age_min = 6, slgtage = 7, 
                                                               slgtage_bfr = 6, dP = 0.008333333, dPslg = 0.08333333, na_pig = 1))
  
})


test_that("check pig_ageclass_proportions works", {
  s1 <- set_up(PCPrev = 0.2)
  pig_age_parameters <- age_parameters_pig_func(number_age_classes = 100, pig_age_class_width = 1) 
  pig_lifetable_output <- life_tables_pigs_func(number_age_classes = 100, slgt_age_min = 6, slgtage = 7, 
                                                slgtage_bfr = 6, dP = 0.008333333, dPslg = 0.08333333, na_pig = pig_age_parameters[[2]])
  pig_ageclass_proportions <- Pig_age_class_proportions_func(PPS = 2000, pig_demography = pig_lifetable_output, na_pig = pig_age_parameters[[2]], 
                                                             IPL0_total = 80, IPH0_total = 320)
  expect_is(pig_ageclass_proportions, "list")
  expect_error(pig_ageclass_proportions <- Pig_age_class_proportions_func(PPS = 2000, pig_demography = pig_lifetable_output, na_pig = 1, 
                                                                          IPL0_total = 80, IPH0_total = 320))
  
})


test_that("check age_parameters_human_func works", {
  s1 <- set_up(PCPrev = 0.2)
  human_age_parameters <- age_parameters_human_func(number_age_classes = 7) 
  expect_is(human_age_parameters, "list")
  expect_error(human_age_parameters <- age_parameters_human_func(number_age_classes = 0))
  
})

test_that("check human_ageclass_proportions works", {
  s1 <- set_up(PCPrev = 0.2)
  human_age_parameters <- age_parameters_human_func(number_age_classes = 7) 
  human_ageclass_proportions <- human_age_class_proportions_func(age_rate = human_age_parameters[[3]], na_human = human_age_parameters[[2]], 
                                                                 dH = s1[[1]]$dH, HPS = s1[[1]]$HPS, SHC0_total = 686, 
                                                                 IH0_total = 186, IHC0_total = 14)
  expect_is(human_ageclass_proportions, "list")
  expect_error(human_ageclass_proportions <- human_age_class_proportions_func(age_rate = human_age_parameters[[3]], na_human = 1, 
                                                                              dH = s1[[1]]$dH, HPS = s1[[1]]$HPS, SHC0_total = 686, 
                                                                              IH0_total = 186, IHC0_total = 14))
  expect_length(human_ageclass_proportions, 4)
  
  
})