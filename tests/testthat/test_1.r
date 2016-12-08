### Tests for EPICYST Package ###

test_that("Setup basic tests", {
  expect_length(Set_up(), 2)
  expect_is(Set_up(), 'list')
  expect_is(unlist(Set_up()), 'numeric')
  expect_error(Set_up(TPrev=1, PCPrev=0.00001))
})

test_that("Sensitivity params basic tests", {
  expect_is(Sensitivity_params(2), 'list')
  expect_is(unlist(Sensitivity_params(2)), 'numeric')
  expect_error(Sensitivity_params())
  expect_length(Sensitivity_params(1), 1)
  expect_length(Sensitivity_params(2), 2)
})

test_that("Sensitivity interventions basic tests", {
  expect_is(Sensitivity_intervention(2), 'list')
  expect_is(unlist(Sensitivity_intervention(2)), 'numeric')
  expect_error(Sensitivity_intervention())
  expect_length(Sensitivity_intervention(1), 1)
  expect_length(Sensitivity_intervention(2), 2)
})

test_that("month rate tests", {
  expect_is(month_rate(1), 'numeric')
  expect_error(month_rate())
  expect_error(month_rate(1,2))
  expect_length(month_rate(1), 1)
  expect_length(month_rate(c(1,2)), 2)
})

test_that("R0 tests", {
  a<-R0(Set_up()[[1]])
  expect_is(a, 'numeric')
  expect_length(a, 1)
})

test_that("Single run tests", {
  s1<-Set_up()
  r1<-Single_run(tt=seq(0,10,0.1), params=s1[[1]], states=s1[[2]])
  expect_is(r1, 'deSolve')
  expect_is(r1[,1], 'numeric')
  expect_error(Single_run())
})

test_that("Model run (no intervention) tests", {
  s1<-Set_up()
  m1<-Run_model(Time=10)
  expect_is(m1, 'data.frame') 
  expect_is(m1[,1], 'numeric')
  expect_error(Run_model(), 'argument "Time" is missing')
  m2<-Run_model(Time=10, Params=s1[[1]], Initial_states=s1[[2]])
  expect_is(m2, 'data.frame') 
  expect_is(m2[,1], 'numeric')
})

test_that("Model run (with intervention) tests", {
  mi<-Run_model(Time=10, Intervention='Sanitation')
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

test_that("PRCC tests", {
  # Create a set of sensitivity parameters
  sens_params<-Sensitivity_params(2)
  # Run the model for each draw, recording the cumulative enumebr of cysticercosis cases in humans
  out<-rep(NA, length(sens_params))
  for(i in 1:length(sens_params)){
    print(paste(i/length(sens_params)*100, '%'))
    t1<-Run_model(Params=sens_params[[i]], Time=50)
    out[i]<-tail(t1$CCC,1)
  }
  
  sens_params_df<-as.data.frame(t(matrix(unlist(sens_params), nrow=length(unlist(sens_params[1])))))
  colnames(sens_params_df)<-names(sens_params[[1]])
  
  expect_is(PRCC(out, sens_params_df), 'numeric')
  expect_length(PRCC(out, sens_params_df), length(sens_params[[1]]))
  expect_is(PRCC_sig(prcc, 2, 15), 'numeric')
  expect_length(PRCC_sig(prcc, 2, 15), length(sens_params[[1]]))
})
  