#' @title
#' Check intervention input
#' @description
#' Checks that the interventions input are correct
#'
#' @param intervention Vector of one or more parameter interventions
check_interventions <- function(intervention) {
  param_interventions <- c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions <- c('Pig_MDA','Pig_vaccine','Human_test_and_treat','Human_MDA_nic','Human_MDA_pzq')
  present <- intervention %in% c(param_interventions, state_interventions)
  
  # Check the interventions supplied are proper and correct
  if (!all(present)) {
    stop(
      intervention[!present],
      ' not recognised. Possible options are: ',
      paste(c(
        param_interventions, state_interventions
      ), collapse = ', '),
      '.'
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
  param_interventions <- c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions <- c('Pig_MDA','Pig_vaccine','Human_test_and_treat','Human_MDA_nic','Human_MDA_pzq')
  present <- names(intervention_effect) %in% c(param_interventions, state_interventions)
  
  # Check the intervcentions supplied are proper and correct
  if (!all(present)) {
    stop(
      names(intervention_effect)[!present],
      ' not recognised. Possible options are: ',
      paste(c(
        param_interventions, state_interventions
      ), collapse = ', '),
      '.'
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
    stop('Parameter ', param_name, ' to change not found')
  }
  params[[which(names(params) == param_name)]] <-
    params[[which(names(params) == param_name)]] * effect_size
  
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
    
    if ('Husbandry' %in% intervention) {
      effect <- intervention_effect[['Husbandry']]
      params <- replace_param(params = params, param_name = 'tau', effect_size = effect)
    }
    
    if ('Sanitation' %in% intervention) {
      effect <- intervention_effect[['Sanitation']]
      params <- replace_param(params = params, param_name = 'delta', effect_size = effect)
    }
    
    if ('Inspection' %in% intervention) {
      effect <- intervention_effect[['Inspection']]
      params <- replace_param(params = params, param_name = 'pil', effect_size = effect[1])
      params <- replace_param(params = params, param_name = 'pih', effect_size = effect[2])
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
#' Moves a proportions of individuals from one state compartemnt to two others
#'
#' @param states List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to_1 The name of the state variable that individuals will be moved in to (first step)
#' @param to_2 The name of the state variable that individuals will be moved in to (second step)
#' @param proportion_1 The proportion of individuals in from that get moved into to (step 1)
#' @param proportion_2 The proportion of individuals in from that get moved into to (step 2)
move_state_double <- function(states, from, to_1, to_2, proportion_1, proportion_2) {
  prior_state_from <- states[[from]]
  states_reduce <- prior_state_from * (proportion_1) + prior_state_from * (proportion_2)
  states[[from]] <- states[[from]] - states_reduce
  states[[to_1]] <- states[[to_1]] + prior_state_from * proportion_1
  states[[to_2]] <- states[[to_2]] + prior_state_from * proportion_2
  
  return(states)
}

#' @title
#' Move individuals between state variables three flows
#' @description
#' Moves a propotions of individuals from one state compartemnt to three others
#'
#' @param states List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to_1 The name of the state variable that individuals will be moved in to (first step)
#' @param to_2 The name of the state variable that individuals will be moved in to (second step)
#' @param to_3 The name of the state variable that individuals will be moved in to (third step)
#' @param proportion_1 The proportion of individuals in from that get moved into to (step 1)
#' @param proportion_2 The proportion of individuals in from that get moved into to (step 2)
#' @param proportion_3 The proportion of individuals in from that get moved into to (step 3)
move_state_triple <- function(states, from, to_1, to_2, to_3, proportion_1, proportion_2, proportion_3) {
  prior_state_from <- states[[from]]
  states_reduce <- prior_state_from * (proportion_1) + prior_state_from * (proportion_2) + prior_state_from * (proportion_3)
  states[[from]] <- states[[from]] - states_reduce
  states[[to_1]] <- states[[to_1]] + prior_state_from * proportion_1
  states[[to_2]] <- states[[to_2]] + prior_state_from * proportion_2
  states[[to_3]] <- states[[to_3]] + prior_state_from * proportion_3
  
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
  
  if('Pig_MDA' %in% intervention) {
    proportion <- intervention_effect[['Pig_MDA']]
    states <- move_state_double(states, from = 'IPL0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
    states <- move_state_double(states, from = 'IPH0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
    states <- move_state_double(states, from = 'PP0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
  }
  
  if('Pig_vaccine' %in% intervention) {
    proportion <- intervention_effect[['Pig_vaccine']]
    states <- move_state(states, from ='SP0', to = 'VP0', proportion = proportion)
  }
  
  if('Human_test_and_treat' %in% intervention) {
    proportion <- intervention_effect[['Human_test_and_treat']]
    states <- move_state(states, from = 'IH0', to = 'SH0', proportion = proportion)
  }
  
  if('Human_MDA_nic' %in% intervention) {
    proportion <- intervention_effect[['Human_MDA_nic']]
    states <- move_state(states, from = 'IH0', to = 'SH0', proportion = proportion)
    states <- move_state(states, from = 'IHC0', to = 'SHC0', proportion = proportion)
  }
  
  if('Human_MDA_pzq' %in% intervention) {
    proportion <- intervention_effect[['Human_MDA_pzq']]
    states <- move_state_triple(states, from ='IHC0', to_1 = "SH0", to_2 ="SHC0", to_3 ="IH0",
                               proportion_1 = proportion[1],
                               proportion_2 = proportion[2],
                               proportion_3 = proportion[3])
    states <- move_state(states, from = 'IH0', to ='SH0', proportion = proportion[4])
    states <- move_state(states, from = 'SHC0', to = 'SH0', proportion = proportion[5])
  }
  
  return(states)
 
}

#' @title
#' Pre-set intervention effects
#' @description
#' Provides preset intervention effect
#'
#' @return A list Intervention effects
#' @export
intervention_effect_size <- function() {
  pars <- set_up()[[1]]
  states <- set_up()[[2]]
  list(
    Husbandry = 0.8,
    Sanitation = 0.8,
    Inspection = c(Proportion_low_burden_mean = 0.8, 
                   Proportion_high_burden_meat = 0.6),
    Pig_MDA = c(Proportion_sucess_treated = 0.9 * 0.99, 
                Proportion_no_immunity = 0.1), # Successfully treated = the assumed therapeutic coverage (0.9) × the anthelmintic efficacy (0.99).
    Pig_vaccine = 0.9 * 1.0 * (0.99), # Assumed coverage (0.9) dose 1 * assumed coverage round2 * vaccine efficacy (0.99) WITHOUT ADJUSTMENT
    Human_test_and_treat = 0.9 * 0.97 * 0.98 * 0.99, # Proportion of people tested that are T+ and C- (Assumed therapeutic coverage (0.9) * Taeniasis sensitivity (0.97) * Cysticercosis specificity (0.98)) * drug efficacy (0.99)
    Human_MDA_nic = c(Proportion_sucess_treated = 0.75 * 0.779), # Successfully treated = the assumed therapeutic coverage (0.75) from literature × the anthelmintic efficacy with niclosamide - efficacy estimate from Bustos et al, 2012 (0.779).
    Human_MDA_pzq = c(Proportion_sucess_treated_pzq_taenneg_cystneg = 0.75 * 0.7 * 0.8,
                      Proportion_success_treated_pzq_taenneg_cystpos = 0.75 * 0.7 * (1 - 0.8),
                      Proportion_success_treated_pzq_taenpos_cystneg = (0.75 * (1 - 0.7) * 0.8),
                      Proportion_success_treated_pzq_taenneg = 0.75 * 0.7, 
                      Proportion_success_treated_pzq_cystneg = 0.75 * 0.8) # proportions 1-4 are treatment of those with cysticercosis +, taeniasis + 
    # to different states, proportion 5 is treatment of those with just taeniasis +, proportion 6 is treatment of those with just cysticercosis +
  )
  
}

#' @title
#' Coverage user defined intervention effect size
#' @description
#' enables user specified coverage to create list of proportions for state intervention moves
#' @param pig_MDA_cov pig treatment coverage
#' @param pig_vaccine_ds1_cov pig vaccine coverage (dose 1)
#' @param pig_vaccine_ds2_cov pig vaccine coverage (dose 2)
#' @param human_testtreat_cov human test and treat coverage
#' @param human_MDAnic_cov human MDA (with niclosamide) coverage
#' @param human_MDApzq_cov human MDA (with praziquantel) coverage
#' @param pig_MDA_prop_noimmunity proportion of pigs without immunity following treatment
#' 
#' @return A list Intervention effects
#' @export

intervention_effect_size_set_up <- function(pig_MDA_cov, pig_vaccine_ds1_cov, pig_vaccine_ds2_cov,
                                          human_testtreat_cov, human_MDAnic_cov, human_MDApzq_cov, 
                                          pig_MDA_prop_noimmunity){
  
  # set up default coverage values if non specified #
  if (is.null(pig_MDA_cov)) {
    pig_MDA_cov = 0.9
  }

  if (is.null(pig_vaccine_ds1_cov)) {
    pig_vaccine_ds1_cov = 0.9
  }

  if (is.null(pig_vaccine_ds2_cov)) {
    pig_vaccine_ds2_cov = 1.0
  }

  if (is.null(human_testtreat_cov)) {
    human_testtreat_cov = 0.9
  }

  if (is.null(human_MDAnic_cov)) {
    human_MDAnic_cov = 0.75
  }

  if (is.null(human_MDApzq_cov)) {
    human_MDApzq_cov = 0.75
  }

  if (is.null(pig_MDA_prop_noimmunity)) {
    pig_MDA_prop_noimmunity = 0.1
  }
  
  # produce list for proportion moved (efficacy x coverage) #
  list <- list(
    Husbandry = 0.8,
    Sanitation = 0.8,
    Inspection = c(Proportion_low_burden_mean = 0.8, 
                   Proportion_high_burden_meat = 0.6),
    Pig_MDA = c(Proportion_sucess_treated = pig_MDA_cov * 0.99,
                Proportion_no_immunity = pig_MDA_prop_noimmunity), # Successfully treated = the assumed therapeutic coverage (0.9) × the anthelmintic efficacy (0.99).
    Pig_vaccine = pig_vaccine_ds1_cov * pig_vaccine_ds2_cov * (0.99), # Assumed coverage (0.9) dose 1 * assumed coverage round2 * vaccine efficacy (0.99) WITHOUT ADJUSTMENT
    Human_test_and_treat = human_testtreat_cov * 0.97 * 0.98 * 0.99, # Proportion of people tested that are T+ and C- (Assumed therapeutic coverage (0.9) * Taeniasis sensitivity (0.97) * Cysticercosis specificity (0.98)) * drug efficacy (0.99)
    Human_MDA_nic = c(Proportion_sucess_treated = human_MDAnic_cov * 0.779), # Successfully treated = the assumed therapeutic coverage (0.75) from literature × the anthelmintic efficacy with niclosamide - efficacy estimate from Bustos et al, 2012 (0.779).
    Human_MDA_pzq = c(Proportion_sucess_treated_pzq_taenneg_cystneg = human_MDApzq_cov * 0.7 * 0.8, 
                      Proportion_success_treated_pzq_taenneg_cystpos = human_MDApzq_cov * 0.7 * (1 - 0.8), 
                      Proportion_success_treated_pzq_taenpos_cystneg = (human_MDApzq_cov * (1 - 0.7) * 0.8),
                      Proportion_success_treated_pzq_taenneg = human_MDApzq_cov * 0.7, 
                      Proportion_success_treated_pzq_cystneg = human_MDApzq_cov * 0.8) # proportions 1-4 are treatment of those with cysticercosis +, taeniasis + 
    # to different states, proportion 5 is treatment of those with just taeniasis +, proportion 6 is treatment of those with just cysticercosis +
  )
  return(list)
}

#=================================================================#
#    Function for multi-stage interventions (stage 1 and stage 2) #                                                                        


#' @title
#' Check intervention input (stage 1 of multi-stage intervention)
#' @description
#' Checks that the interventions input are correct
#'
#' @param intervention_stage1 Vector of one or more parameter interventions (stage 1)
check_interventions_stg1 <- function(intervention_stage1) {
  param_interventions <- c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions <- c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  present <- intervention_stage1 %in% c(param_interventions, state_interventions)
  
  # Check the intervcentions supplied are proper and correct
  if(!all(present)) {
    stop(intervention_stage1[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}

#' @title
#' Check intervention input (stage 2 of multi-stage intervention)
#' @description
#' Checks that the interventions input are correct
#'
#' @param intervention_stage2 Vector of one or more parameter interventions (stage 2)
check_interventions_stg2 <- function(intervention_stage2) {
  param_interventions <- c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions <- c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  present <- intervention_stage2 %in% c(param_interventions, state_interventions)
  
  # Check the intervcentions supplied are proper and correct
  if(!all(present)) {
    stop(intervention_stage2[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}


