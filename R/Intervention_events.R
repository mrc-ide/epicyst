
#' @title
#' Check intervention input
#' @description
#' Checks that the interventions input are correct
#'
#' @param intervention Vector of one or more parameter interventions
check_interventions <- function(intervention) {
  param_interventions <- c("Husbandry", "Sanitation", "Inspection")
  state_interventions <- c("Pig_MDA", "Pig_vaccine", "Human_test_and_treat")

  present <- intervention %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if (!all(present)) {
    stop(
      intervention[!present], " not recognised. Possible options are: ",
      paste(c(param_interventions, state_interventions), collapse = ", "), "."
    )
  }
}

#' @title
#' Check intervention effect input
#' @description
#' Checks that the intervention effect inputs are correct
#'
#' @param intervention_effect Vector of one or more parameter interventions
check_effect <- function(intervention_effect) {
  param_interventions <- c("Husbandry", "Sanitation", "Inspection")
  state_interventions <- c("Pig_MDA", "Pig_vaccine", "Human_test_and_treat")

  present <- names(intervention_effect) %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if (!all(present)) {
    stop(
      names(intervention_effect)[!present], " not recognised. Possible options are: ",
      paste(c(param_interventions, state_interventions), collapse = ", "), "."
    )
  }
}

#' @title
#' Change parameter
#' @description
#' Replaces a paramter with an aletred values after a parameter event
#'
#' @param params List of model parameters
#' @param param_name The name of the parameter to be changed
#' @param effect_size The intervention effect size. Where the new parameter value = old value multiplied by the effect size.
replace_param <- function(params, param_name, effect_size) {
  if (!param_name %in% names(params)) {
    stop("Parameter ", param_name, " to change not found")
  }
  params[[which(names(params) == param_name)]] <- params[[which(names(params) == param_name)]] * effect_size

  return(params)
}

#' @title
#' Implement parameter intervention
#' @description
#' Implements one or more interventions that involve a parameter value being altered
#'
#' @param params List of model parameters
#' @param intervention Vector of one or more parameter interventions
#' @param intervention_effect a list of intervention effect sizes
intervention_event_param <- function(params, intervention, intervention_effect) {
  # Check_interventions(intervention)
  # Check_effect(intervention_effect)

  if ("Husbandry" %in% intervention) {
    effect <- intervention_effect[["Husbandry"]]
    params <- replace_param(params = params, param_name = "tau", effect_size = effect)
  }

  if ("Sanitation" %in% intervention) {
    effect <- intervention_effect[["Sanitation"]]
    params <- replace_param(params = params, param_name = "delta", effect_size = effect)
  }

  if ("Inspection" %in% intervention) {
    effect <- intervention_effect[["Inspection"]]
    params <- replace_param(params = params, param_name = "pil", effect_size = effect[1])
    params <- replace_param(params = params, param_name = "pih", effect_size = effect[2])
  }

  return(params)
}

#' @title
#' Move individuals between state variables
#' @description
#' Moves a propotion of individuals from one state compartemnt to another
#'
#' @param states List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to The name of the state variable that individuals will be moved in to
#' @param proportion The propotion of individuals in from that get moved into to
move_state <- function(states, from, to, proportion) {
  prior_state_from <- states[[from]]
  states[[from]] <- states[[from]] * (1 - proportion)
  states[[to]] <- states[[to]] + prior_state_from * proportion

  return(states)
}

#' @title
#' Move individuals between state variables two flows
#' @description
#' Moves a propotions of individuals from one state compartemnt to two others
#'
#' @param states List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to_1 The name of the state variable that individuals will be moved in to
#' @param to_2 The name of the state variable that individuals will be moved in to
#' @param proportion_1 The propotion of individuals in from that get moved into to
#' @param proportion_2 The propotion of individuals in from that get moved into to
move_state_double <- function(states, from, to_1, to_2, proportion_1, proportion_2) {
  prior_state_from <- states[[from]]
  states_reduce <- prior_state_from * (proportion_1) + prior_state_from * (proportion_2)
  states[[from]] <- states[[from]] - states_reduce
  states[[to_1]] <- states[[to_1]] + prior_state_from * proportion_1
  states[[to_2]] <- states[[to_2]] + prior_state_from * proportion_2

  return(states)
}


#' @title
#' Implement parameter intervention
#' @description
#' Implements one or more interventions that involve a parameter value being altered
#'
#' @param states List of model states
#' @param intervention Vector of one or more parameter interventions
#' @param intervention_effect a list of intervention effect sizes
intervention_event_state <- function(states, intervention, intervention_effect) {
  if ("Pig_MDA" %in% intervention) {
    proportion <- intervention_effect[["Pig_MDA"]]
    states <- move_state_double(states,
      from = "IPL0", to_1 = "SP0", to_2 = "RP0",
      proportion_1 = proportion[1] * proportion[2],
      proportion_2 = proportion[1] * (1 - proportion[2])
    )
    states <- move_state_double(states,
      from = "IPH0", to_1 = "SP0", to_2 = "RP0",
      proportion_1 = proportion[1] * proportion[2],
      proportion_2 = proportion[1] * (1 - proportion[2])
    )
  }

  if ("Pig_vaccine" %in% intervention) {
    proportion <- intervention_effect[["Pig_vaccine"]]
    states <- move_state(states, from = "SP0", to = "VP0", proportion = proportion)
  }

  if ("Human_test_and_treat" %in% intervention) {
    proportion <- intervention_effect[["Human_test_and_treat"]]
    states <- move_state(states, from = "IH0", to = "SH0", proportion = proportion)
  }

  return(states)
}

#' @title
#' Pre-set intervention effects
#' @description
#' Provides preset intervention effect
#'
#' @return A list intervention effects
#' @export
intervention_effect_size <- function() {
  pars <- set_up()[[1]]
  states <- set_up()[[2]]
  list(
    Husbandry = 0.8,
    Sanitation = 0.8,
    Inspection = c(Proportion_low_burden_mean = 0.8, Proportion_high_burden_meat = 0.6),
    Pig_MDA = c(Proportion_sucess_treated = 0.9 * 0.99, Proportion_no_immunity = 0.1), # Succesfully treated = the assumed therapeutic coverage (0.9) × the anthelmintic efficacy (0.99).
    Pig_vaccine = 0.9 * (0.99 - (0.99 * pars$tau * states$E0 * 3)), # Assumed coverage (0.9) * vaccine efficacy (0.99-(0.99*tau*E*3)
    Human_test_and_treat = 0.9 * 0.97 * 0.98 * 0.99 # Proportion of people tested that are T+ and C- (Assumed therapeutic coverage (0.9) * Taeniasis sensitivity (0.97) * Cysticercosis specificity (0.98)) * drug efficacy (0.99)
  )
}
