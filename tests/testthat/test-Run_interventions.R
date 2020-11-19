test_that("Model run (with intervention) tests", {
  mi <- run_model(time = 10, intervention = "Sanitation")
  expect_is(mi, "data.frame")
  expect_is(mi[, 1], "numeric")
})

test_that("intervention checks tests", {
  expect_null(check_interventions("Sanitation"))
  expect_error(check_interventions("Wrong"), "not recognised. Possible options")
  ie <- intervention_effect_size()
  expect_null(check_effect(ie))
  names(ie)[1] <- "wrong"
  expect_error(check_effect(ie))
})

test_that("intervention events tests", {
  expect_error(replace_param("A", "B", 2))
  s1 <- set_up()
  eff <- intervention_effect_size()
  expect_is(replace_param(params = s1[[1]], param_name = "tau", effect_size = 0.5), "list")
  expect_length(replace_param(params = s1[[1]], param_name = "tau", effect_size = 0.5), length(s1[[1]]))

  expect_is(intervention_event_param(params = s1[[1]], intervention = "Husbandry", intervention_effect = eff), "list")
  expect_length(intervention_event_param(params = s1[[1]], intervention = "Husbandry", intervention_effect = eff), length(s1[[1]]))
  expect_is(intervention_event_param(params = s1[[1]], intervention = "Sanitation", intervention_effect = eff), "list")
  expect_length(intervention_event_param(params = s1[[1]], intervention = "Sanitation", intervention_effect = eff), length(s1[[1]]))
  expect_is(intervention_event_param(params = s1[[1]], intervention = "Inspection", intervention_effect = eff), "list")
  expect_length(intervention_event_param(params = s1[[1]], intervention = "Inspection", intervention_effect = eff), length(s1[[1]]))
})

test_that("Move states tests", {
  s1 <- set_up()
  eff <- intervention_effect_size()
  expect_is(move_state(states = s1[[2]], "IH0", "SH0", 0.1), "list")
  expect_length(move_state(states = s1[[2]], "IH0", "IH0", 0.1), length(s1[[2]]))

  expect_is(intervention_event_state(states = s1[[2]], intervention = "Pig_MDA", intervention_effect = eff), "list")
  expect_length(intervention_event_state(states = s1[[2]], intervention = "Pig_MDA", intervention_effect = eff), length(s1[[2]]))
  expect_is(intervention_event_state(states = s1[[2]], intervention = "Pig_vaccine", intervention_effect = eff), "list")
  expect_length(intervention_event_state(states = s1[[2]], intervention = "Pig_vaccine", intervention_effect = eff), length(s1[[2]]))
  expect_is(intervention_event_state(states = s1[[2]], intervention = "Human_test_and_treat", intervention_effect = eff), "list")
  expect_length(intervention_event_state(states = s1[[2]], intervention = "Human_test_and_treat", intervention_effect = eff), length(s1[[2]]))
})
