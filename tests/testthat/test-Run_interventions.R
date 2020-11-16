test_that("Model run (with intervention) tests", {
  mi <- Run_model(Time = 10, Intervention = 'Sanitation')
  expect_is(mi, 'data.frame') 
  expect_is(mi[,1], 'numeric')
})

test_that("Intervention checks tests", {
  expect_null(Check_interventions('Sanitation'))
  expect_error(Check_interventions('Wrong'), 'not recognised. Possible options')
  ie<-Intervention_effect_size()
  expect_null(Check_effect(ie))
  names(ie)[1]<-'wrong'
  expect_error(Check_effect(ie))
})

test_that("Intervention events tests", {
  expect_error(Replace_param('A','B',2))
  s1<-Set_up()
  eff<-Intervention_effect_size()
  expect_is(Replace_param(params=s1[[1]], param_name='tau', effect_size=0.5), 'list')
  expect_length(Replace_param(params=s1[[1]], param_name='tau', effect_size=0.5), length(s1[[1]]))
  
  expect_is(Intervention_event_param(Params=s1[[1]], Intervention='Husbandry', Intervention_effect=eff), 'list')
  expect_length(Intervention_event_param(Params=s1[[1]], Intervention='Husbandry', Intervention_effect=eff), length(s1[[1]]))
  expect_is(Intervention_event_param(Params=s1[[1]], Intervention='Sanitation', Intervention_effect=eff), 'list')
  expect_length(Intervention_event_param(Params=s1[[1]], Intervention='Sanitation', Intervention_effect=eff), length(s1[[1]]))
  expect_is(Intervention_event_param(Params=s1[[1]], Intervention='Inspection', Intervention_effect=eff), 'list')
  expect_length(Intervention_event_param(Params=s1[[1]], Intervention='Inspection', Intervention_effect=eff), length(s1[[1]]))
  
})

test_that("Move states tests", {
  s1<-Set_up()
  eff<-Intervention_effect_size()
  expect_is(Move_state(States=s1[[2]], 'IH0', 'SH0', 0.1), 'list')
  expect_length(Move_state(States=s1[[2]], 'IH0', 'IH0', 0.1), length(s1[[2]]))
  
  expect_is(Intervention_event_state(States=s1[[2]], Intervention = 'Pig_MDA', Intervention_effect = eff), 'list')
  expect_length(Intervention_event_state(States=s1[[2]], Intervention = 'Pig_MDA', Intervention_effect = eff), length(s1[[2]]))
  expect_is(Intervention_event_state(States=s1[[2]], Intervention = 'Pig_vaccine', Intervention_effect = eff), 'list')
  expect_length(Intervention_event_state(States=s1[[2]], Intervention = 'Pig_vaccine', Intervention_effect = eff), length(s1[[2]]))
  expect_is(Intervention_event_state(States=s1[[2]], Intervention = 'Human_test_and_treat', Intervention_effect = eff), 'list')
  expect_length(Intervention_event_state(States=s1[[2]], Intervention = 'Human_test_and_treat', Intervention_effect = eff), length(s1[[2]]))
  
})