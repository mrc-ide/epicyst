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

test_that("Model run (with intervention; number of intervention rounds, timing & frequency (two stages) specified)", {
  expect_error(mi <- run_model(time = 10, intervention_stage1 = c('Human_MDA_pzq'), intervention_stage2 = c('Human_MDA_pzq'), intervention_time = 1,
                  num_intervention_rounds_stage1 = 3, num_intervention_rounds_stage2 = 6,
                  intervention_time_stage2 = 1.25, intervention_frequency_stage2 = 6,
                  intervention_time_stage1 =1, intervention_frequency_stage1 = 3))

})

