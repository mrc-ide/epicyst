### Tests for EPICYST Package ###

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






# test_that("PRCC tests", {
#   # Create a set of sensitivity parameters
#   sens_params<-Sensitivity_params(50)
#   # Run the model for each draw, recording the cumulative enumebr of cysticercosis cases in humans
#   out<-rep(NA, length(sens_params))
#   for(i in 1:length(sens_params)){
#     print(paste(i/length(sens_params)*100, '%'))
#     t1<-Run_model(Params=sens_params[[i]], Time=50)
#     out[i]<-tail(t1$CCC,1)
#   }
#   
#   sens_params_df<-as.data.frame(t(matrix(unlist(sens_params), nrow=length(unlist(sens_params[1])))))
#   colnames(sens_params_df)<-names(sens_params[[1]])
#   
#   prcc<-PRCC(out, sens_params_df)
#   expect_is(prcc, 'numeric')
#   expect_length(prcc, length(sens_params[[1]]))
#   prcc_sig<-PRCC_sig(prcc, 50, 22)
#   expect_is(prcc_sig, 'numeric')
#   expect_length(prcc_sig, length(sens_params[[1]]))
# })
  