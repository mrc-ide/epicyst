### Tests for EPICYST Package ###

test_that("Setup basic tests", {
  expect_length(Set_up(), 2)
  expect_is(Set_up(), 'list')
  expect_is(unlist(Set_up()), 'numeric')
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