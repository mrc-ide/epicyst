
#' @title
#' Run Cysticercosis model ODE
#' @description
#' Runs a single implementation of the ODE Cysticercosis model
#'
#' @param tt vector of times
#' @param params list of parameters
#' @param states list of states
#' @export
single_run <- function(tt, params, states) {
  mod <- cyst_generator$new(user = c(params[c(1:26)], states))
  y <- mod$run(tt)
  return(y)
}

#' @title
#' Run Cysticercosis model with interventions
#' @description
#' Runs the ODE Cysticercosis model
#'
#' @param params List of model parameters
#' @param initial_states List of intitial state values
#' @param time The number of years to run the model for (from equilibrium). Default is at the halfway point.
#' @param intervention A vector of interventions to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param intervention_time Specify when intervention begins (year)
#' @param intervention_effect A list of intervention effect sizes, see \code{Intervention_effect_size} for details
#' @param intervention_frequency Frequency of intervention implementation (i.e. 12 = every year)
#' @param step Time step (months)
#' @param burn_in A burn in period run before model run (years)
#' @param age_target_pig_MDA Vector of age classes to target with pig MDA intervention (months)
#' @param age_target_pig_vaccine Vector of age classes to target with pig vaccine intervention (months)
#' @param num_intervention_rounds Specify number of rounds of intervention
#' @param pig_MDA_cov Specify pig MDA coverage
#' @param pig_vaccine_ds1_cov Specify pig vaccine (dose 1) coverage
#' @param pig_vaccine_ds2_cov Specify pig vaccine (dose 2) coverage
#' @param human_testtreat_cov Specify human test and treat coverage
#' @param human_MDAnic_cov Specify human MDA with niclosamide coverage
#' @param human_MDApzq_cov Specify human MDA with praziquantel coverage
#' @param intervention_stage1 A vector of interventions (multistage stage 1) to include from: Husbandry, Sanitation, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param intervention_stage2 A vector of interventions (multistage stage 2)to include from: Husbandry, Sanitation, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param intervention_frequency_stage1 Frequency of intervention (multistage stage 1) implementation (i.e. 12 = every year)
#' @param intervention_frequency_stage2 Frequency of intervention (multistage stage 2) implementation (i.e. 12 = every year)
#' @param intervention_time_stage1 Specify when intervention stage 1 begins (years)
#' @param intervention_time_stage2 Specify when intervention stage 2 begins (years)
#' @param age_target_pig_MDA_stage1 Vector of age classes to target with pig MDA (multistage stage 1) intervention (months)
#' @param age_target_pig_MDA_stage2 Vector of age classes to target with pig MDA (multistage stage 2) intervention (months)
#' @param age_target_pig_vaccine_stage1 Vector of age classes to target with pig vaccine (multistage stage 1)intervention (months)
#' @param age_target_pig_vaccine_stage2 Vector of age classes to target with pig vaccine (multistage stage 2)intervention (months)
#' @param num_intervention_rounds_stage1 Specify number of rounds of intervention (multistage stage 1)
#' @param num_intervention_rounds_stage2 Specify number of rounds of intervention (multistage stage 2)
#' @param pig_MDA_cov_stage1 Specify pig MDA coverage (multistage stage 1)
#' @param pig_MDA_cov_stage2 Specify pig MDA coverage (multistage stage 2)
#' @param pig_vaccine_ds1_cov_stage1 Specify pig vaccine (dose 1) coverage (multistage stage 1)
#' @param pig_vaccine_ds1_cov_stage2 Specify pig vaccine (dose 1) coverage (multistage stage 2)
#' @param pig_vaccine_ds2_cov_stage1 Specify pig vaccine (dose 2) coverage (multistage stage 1)
#' @param pig_vaccine_ds2_cov_stage2 Specify pig vaccine (dose 2) coverage (multistage stage 2)
#' @param pig_MDA_prop_noimmunity proportion of pigs without immunity following treatment
#' @param age_target_human_MDA specify age groups for human treatment
#' @param age_target_human_test_and_treat specify age group for human test and treat
#' 
#' @examples
#' # Run the baseline model:
#' M1 <- run_model(time=50, burn_in=50)
#' plot(M1$t / 12, M1$Humans_Cysticercosis, t = 'l', ylim = c(0 , 1000), 
#' ylab = 'Humans with Cysticercosis', xlab='Time (years)')
#' 
#' # Run the model with a single intervention:
#' M2 <- run_model(time = 50, intervention = 'Sanitation', intervention_time = 20, burn_in = 50)
#' lines(M2$t / 12, M2$Humans_Cysticercosis, col ='deeppink')
#' 
#' # Run the model with multiple interventions:
#' M3 <- run_model(time = 50, intervention = c('Human_test_and_treat', 'Pig_MDA'), 
#' intervention_time = 20, burn_in = 50)
#' lines(M3$t / 12, M3$Humans_Cysticercosis, col ='dodgerblue')
#' legend('topright', c('Baseline','Sanitation','Human test & treat and Pig MDA'), lty = c(1,1,1), 
#' col = c('black','deeppink', 'dodgerblue'))
#' 
#' @export
run_model <-
  function(params = NULL,
           initial_states = NULL,
           time,
           intervention = NULL,
           intervention_time = time / 2,
           intervention_effect = intervention_effect_size(),
           intervention_frequency = 12,
           step = 1 / 30,
           burn_in = 0,
           age_target_pig_MDA = NULL,
           age_target_pig_vaccine = NULL,
           num_intervention_rounds = NULL,
           pig_MDA_cov = NULL,
           pig_vaccine_ds1_cov = NULL,
           pig_vaccine_ds2_cov = NULL,
           pig_MDA_prop_noimmunity = NULL,
           human_testtreat_cov = NULL,
           human_MDAnic_cov = NULL,
           human_MDApzq_cov = NULL,
           age_target_human_MDA = NULL,
           age_target_human_test_and_treat = NULL,
           intervention_stage1 = NULL,
           intervention_stage2 = NULL,
           intervention_time_stage1 = NULL,
           intervention_time_stage2 = NULL,
           intervention_frequency_stage1 = NULL,
           intervention_frequency_stage2 = NULL,
           num_intervention_rounds_stage1 = NULL,
           num_intervention_rounds_stage2 = NULL,
           age_target_pig_MDA_stage1 = NULL,
           age_target_pig_MDA_stage2 = NULL,
           age_target_pig_vaccine_stage1 = NULL,
           age_target_pig_vaccine_stage2 = NULL,
           pig_MDA_cov_stage1 = NULL,
           pig_MDA_cov_stage2 = NULL,
           pig_vaccine_ds1_cov_stage1 = NULL,
           pig_vaccine_ds1_cov_stage2 = NULL,
           pig_vaccine_ds2_cov_stage1 = NULL,
           pig_vaccine_ds2_cov_stage2 = NULL) {
    
  # Calculate parameters and initial state variables (if not provided)
  initialise <- set_up()
  if (is.null(params)) {
    params <- initialise[[1]]
  }
  if (is.null(initial_states)) {
    initial_states <- initialise[[2]]
  }
  
  # run burn in period
  if(burn_in>0) {
    tt_burn <- seq(0, (burn_in * 12), step)
    burn <- single_run(tt_burn, params = params, states = initial_states)
    burn_out <- as.data.frame(burn)
    initial_states <- inter_run_setup(model_output = burn, na_pig = params$na_pig, na_human = params$na_human)
  }
  
  # Run with no interventions (if none specified)
  if(is.null(intervention) &&
     is.null(intervention_stage1)) {
    run <- single_run(seq(0, time * 12, step), params = params, initial_states)
    run <- as.data.frame(run)
    
    # To plot 'apparent' prevalence if argument selected (from true underlying model predicted prevalence)
    if (!is.null(params$PC_sens)) {
      pig_cysticercosis_apparent_prev <-
        apparent_prevalence_packaging_func(
          sens = params$PC_sens,
          spec = params$PC_spec,
          TP = run$Pig_Cysticercosis_prev
        )
      run <- cbind(run, pig_cysticercosis_apparent_prev)
      colnames(run)[colnames(run) == "apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
    }
    
    if (!is.null(params$C_sens)) {
      human_cysticercosis_apparent_prev <-
        apparent_prevalence_packaging_func(
          sens = params$C_sens,
          spec = params$C_spec,
          TP = run$Human_Cysticercosis_prev
        )
      run <- cbind(run, human_cysticercosis_apparent_prev)
      colnames(run)[colnames(run) == "apparent_prev"] <- "Human_cysticercosis_apparent_prev"
    }
    
    if (!is.null(params$T_sens)) {
      human_taeniasis_apparent_prev <-
        apparent_prevalence_packaging_func(
          sens = params$T_sens,
          spec = params$T_spec,
          TP = run$Human_Taeniasis_prev
        )
      run <- cbind(run, human_taeniasis_apparent_prev)
      colnames(run)[colnames(run) == "apparent_prev"] <- "Human_taeniasis_apparent_prev"
    }
    
    return(run)
    
  }
  
  #==============================================================================================#
  #       Standard interventions (Same intervention applied over model run) i.e. same dataframe  #
  #==============================================================================================#
  
  # If interventions DO NOT change over time (in model run), proceed
  if (is.null(intervention_stage1)) {

    # User specified coverage values for age-structured model
    int_effect_size_list <- intervention_effect_size_set_up(
      pig_MDA_cov = pig_MDA_cov,
      pig_vaccine_ds1_cov = pig_vaccine_ds1_cov,
      pig_vaccine_ds2_cov = pig_vaccine_ds2_cov,
      human_testtreat_cov = human_testtreat_cov,
      human_MDAnic_cov = human_MDAnic_cov,
      human_MDApzq_cov = human_MDApzq_cov,
      pig_MDA_prop_noimmunity = pig_MDA_prop_noimmunity
    )
    # check on inputs
    check_interventions(intervention)
    stopifnot(
      is.numeric(time),
      is.numeric(intervention_time),
      is.numeric(step),
      length(time) == 1,
      length(intervention_time) == 1,
      length(step) == 1,
      time > 0,
      intervention_time <= time,
      intervention_time > 0,
      is.list(params),
      is.list(initial_states),
      is.numeric(burn_in),
      burn_in >= 0,
      is.numeric(intervention_frequency),
      length(intervention_frequency) == 1,
      intervention_frequency > 0
    )
    
    # if non-age structured model specified, but age_target input provided, throw an error message
    
    if('Pig_MDA' %in% intervention && is.numeric(age_target_pig_MDA) && params$na_pig == 1) {
      stop('Cannot specify age target for MDA in non age-structured pig model')
    }
    
    if('Pig_vaccine' %in% intervention && is.numeric(age_target_pig_vaccine) && params$na_pig == 1) {
      stop('Cannot specify age target for vaccine in non age-structured pig model')
    }
    
    if('Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && params$na_human == 1) {
      stop('Cannot specify age target for MDA (praziquantel) in non age-structured human model')
    }
    
    if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && params$na_human == 1) {
      stop('Cannot specify age target for MDA (niclosamide) in non age-structured human model')
    }
    
    if('Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) && params$na_human == 1) {
      stop('Cannot specify age target for test & treat in non age-structured human model')
    }
    
    if('Pig_MDA' %in% intervention && params$na_pig == 1 && is.numeric(age_target_pig_MDA_stage1)) {
      stop('Cannot specify age target for MDA in non age-structured pig model')
    }
    
    if('Pig_vaccine' %in% intervention && params$na_pig == 1 && is.numeric(age_target_pig_vaccine_stage1)) {
      stop('Cannot specify age target for vaccine in non age-structured pig model')
    }
    
    # Set time vectors for pre- intervention
    tt1 <- seq(0, (intervention_time * 12) - step, step)
    
    # Set yearly times for interention period (post first intervention round)
    splits <- seq((intervention_time * 12), time * 12, intervention_frequency) # previously frequency set to 12 i.e 1 year
    tt2<-list()
    
    # Specify vector for interventions in absence of number of int round argument
    if(is.null(num_intervention_rounds)) {
      if (length(splits) > 1) {
        for (i in 1:(length(splits) - 1)) {
          tt2[[i]] <- seq(splits[i] + step, splits[i + 1], step)
        }
      }
      
      if (length(splits) == 1) {
        for (i in 1:(length(splits))) {
          tt2[[i]] <- seq(splits[i] + step, time * 12, step)
        }
      }
    }
    
    # Specify vector for number of intervention rounds (if number of intervention round argument used)
    if(!is.null(num_intervention_rounds)) {
      num_intervention_rounds_split <- num_intervention_rounds + 1
      
      splits1 <- splits[1:num_intervention_rounds_split]
      
      if (length(splits) >= 1) {
        for (i in 1:(length(splits1) - 1)) {
          tt2[[i]] <- seq(splits1[i] + step, splits1[i + 1], step)
        }
      }
    }
    
    # Run the pre-intervention period
    bl <- single_run(tt1, params = params, states = initial_states)
    
    runs <- list()
    runs[[1]] <- bl
    
    for (i in 1:length(tt2)) {
      # Pull the 'end' state values from previous run
      tail_states <- inter_run_setup(
          model_output = runs[[i]], na_pig = params$na_pig, na_human = params$na_human
        )
      
      # Alter states/params for single NPI interventions (during first year of intervention)
      if (i == 1 && !'Pig_vaccine' %in% intervention && !'Pig_MDA' %in% intervention &&
          !'Human_MDA_nic' %in% intervention && !'Human_MDA_pzq' %in% intervention &&
          !'Human_test_and_treat' %in% intervention) {
        params <- intervention_event_param(params = params, intervention, intervention_effect)
        states <- intervention_event_state(states = tail_states, intervention, intervention_effect)
      }
      
      # Alter states/params for single NPI interventions (subsequent years of intervention if continuously applied)
      if (i > 1 && !'Pig_vaccine' %in% intervention && !'Pig_MDA' %in% intervention &&
          !'Human_MDA_nic' %in% intervention && !'Human_MDA_pzq' %in% intervention &&
          !'Human_test_and_treat' %in% intervention) {
        states <- intervention_event_state(states = tail_states, intervention, intervention_effect)
      }
      
      #=========================================================================================#
      # IF statements for human interventions (either before or in absence of pig interventions #
      if('Human_MDA_nic' %in% intervention || 'Human_MDA_pzq' %in% intervention || 'Human_test_and_treat' %in% intervention) {
        # first param changes (non-biomedical interventions)
        if (i == 1) {
          params <- intervention_event_param(params = params, intervention, intervention_effect)
        }
        
        # A) Non age-structured human interventions
        # 1) Options with only 1 human intervention selected
        if('Human_MDA_nic' %in% intervention || 'Human_MDA_pzq' %in% intervention || 
           'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_MDA) &&
           !is.numeric(age_target_human_test_and_treat)) {
          
          if ('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) &&
              !'Human_MDA_pzq' %in% intervention && !'Human_test_and_treat' %in% intervention) {
            states <- intervention_event_state(states = tail_states, intervention = 'Human_MDA_nic', intervention_effect = int_effect_size_list)
          }
          
          if ('Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) 
              && !'Human_MDA_nic' %in% intervention && !'Human_test_and_treat' %in% intervention) {
            states <- intervention_event_state(states = tail_states, intervention = 'Human_MDA_pzq', intervention_effect = int_effect_size_list)
          }
          
          if ('Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_MDA) &&
              !'Human_MDA_nic' %in% intervention && !'Human_MDA_pzq' %in% intervention) {
            states <- intervention_event_state(states = tail_states, intervention = 'Human_test_and_treat', intervention_effect = int_effect_size_list)
          }
          
          # 2) Options with 2 human intervention selected
          if('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) &&
             'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat) &&
             ! 'Human_MDA_pzq' %in% intervention) {
            states <- intervention_event_state(states = tail_states, intervention, intervention_effect = int_effect_size_list)
          }
          
          if ('Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) &&
              'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat) &&
              !'Human_MDA_nic' %in% intervention) {
            states <- intervention_event_state(states = tail_states, intervention, intervention_effect = int_effect_size_list)
          }
          
          if('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) && 
             !'Human_test_and_treat' %in% intervention) {
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          # 3) Options with 3 human intervention selected
          if('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat)) {
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
        } 
        
        # B) Age-structured human interventions (user specified)
        if('Human_MDA_nic' %in% intervention || 'Human_MDA_pzq' %in% intervention || 
           'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_MDA) || 
           is.numeric(age_target_human_test_and_treat)) {
          
          # 1) Options with only 1 human intervention selected
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && 
             !'Human_MDA_pzq' %in% intervention && !'Human_test_and_treat' %in% intervention) {
            
            # takes processed tail states and selects specific age groups to implement intervention
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states) 
            # apply intervention effect to specific selected age groups 
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention = 'Human_MDA_nic', intervention_effect = int_effect_size_list)
            # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states) 
          }
          
          if('Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && 
             !'Human_MDA_nic' %in% intervention && !'Human_test_and_treat' %in% intervention) {
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention ='Human_MDA_pzq', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
          }
          
          if('Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) && 
             !'Human_MDA_nic' %in% intervention && !'Human_MDA_pzq' %in% intervention) {
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention ='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          # 2) Options with 2 human intervention selected (all combination thereof)
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) &&
             !'Human_MDA_pzq' %in% intervention) {
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention='Human_MDA_nic', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states) 
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat) &&
             !'Human_MDA_pzq' %in% intervention) {
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention = 'Human_MDA_nic', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            age_target_human_test_and_treat <- c(1:params$na_human) # make age vector (all human ages)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          if('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) &&
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) &&
             !'Human_MDA_pzq' %in% intervention) {
            age_target_human_MDA <- c(1:params$na_human) # make age vector (all human ages)
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention='Human_MDA_nic', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          if('Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) && 
             !'Human_MDA_nic' %in% intervention) {
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention='Human_MDA_pzq', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          if('Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat) && 
             !'Human_MDA_nic' %in% intervention) {
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention='Human_MDA_pzq', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            age_target_human_test_and_treat <- c(1:params$na_human) # make age vector (all human ages)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention='Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          if('Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat) &&
             !'Human_MDA_nic' %in% intervention) {
            age_target_human_MDA <- c(1:params$na_human) # make age vector (all human ages)
            p <- pre_human_MDA(age_target = age_target_human_MDA, tail_states = tail_states)
            states_move_age_human_MDA <- intervention_event_state(states = p, intervention='Human_MDA_pzq', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_MDA, tail_states = tail_states)
            p <- pre_human_test_and_treat(age_target = age_target_human_test_and_treat, tail_states = tail_states)
            states_move_age_human_test_and_treat <- intervention_event_state(states = p, intervention = 'Human_test_and_treat', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_human_test_and_treat, tail_states = tail_states)
          }
          
          # Throw error messages with specific combinations (i.e. where combinations are non-feasible)
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && 
             !'Human_test_and_treat' %in% intervention){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          # 3) Non-feasible options with 3 human intervention selected (as human MDA with both PZQ and NIC not appropriate) - throw error message
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat)) {
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          if('Human_MDA_nic' %in% intervention && is.numeric(age_target_human_MDA) &&
             'Human_MDA_pzq' %in% intervention && is.numeric(age_target_human_MDA) &&
             'Human_test_and_treat' %in% intervention && !is.numeric(age_target_human_test_and_treat)) {
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          if('Human_MDA_nic' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_MDA_pzq' %in% intervention && !is.numeric(age_target_human_MDA) && 
             'Human_test_and_treat' %in% intervention && is.numeric(age_target_human_test_and_treat)) {
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          }
      
      } 
      
      #=========================================================================================#
      # IF statements for pig interventions (either after or in absence of human interventions) #
      
      # IF statements for pig interventions (including age-structured interventions)
      if('Pig_MDA' %in% intervention || 'Pig_vaccine' %in% intervention) {
        
        if(i == 1) {
          params <- intervention_event_param(params = params, intervention, intervention_effect)
        }
        
        # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN 
        if('Human_MDA_nic' %in% intervention || 'Human_MDA_pzq' %in% intervention || 'Human_test_and_treat' %in% intervention) {
          tail_states <- states # set-up 
        }
        
        # A) Non age-structured pig interventions combinations
        # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% intervention && !is.numeric(age_target_pig_vaccine)) {
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = params$na_pig, intervention_frequency = intervention_frequency)
        } 
        
        if('Pig_MDA' %in% intervention && !is.numeric(age_target_pig_MDA) && !'Pig_vaccine' %in% intervention) {
          age_target_pig_MDA <- c(4:params$na_pig)
        }
        
        if('Pig_vaccine' %in% intervention && !is.numeric(age_target_pig_vaccine) && !'Pig_MDA' %in% intervention) {
          states <- intervention_event_state(states = tail_states, intervention, intervention_effect = int_effect_size_list)
        }
        
        if('Pig_MDA' %in% intervention && !is.numeric(age_target_pig_MDA) &&
           'Pig_vaccine' %in% intervention && !is.numeric(age_target_pig_vaccine)) {
          age_target_pig_MDA <- c(4:params$na_pig)
        }
        
        # B) Age-structured pig interventions (user specified) combinations
        if('Pig_MDA' %in% intervention && is.numeric(age_target_pig_MDA)) {
          # takes processed tail states and selects specific age groups to implement intervention
          p <- pre_pig_MDA(age_target = age_target_pig_MDA, tail_states = tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention='Pig_MDA', intervention_effect = int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          
          if('Pig_MDA' %in% intervention && is.numeric(age_target_pig_MDA) && is.numeric(age_target_pig_vaccine)) {
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
          
          if('Pig_MDA' %in% intervention && is.numeric(age_target_pig_MDA) && 
             'Pig_vaccine' %in% intervention) {
            age_target_pig_vaccine <- c(1:params$na_pig)
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
        }
        
        if('Pig_MDA' %in% intervention && !is.numeric(age_target_pig_MDA) && is.numeric(age_target_pig_vaccine)) {
          age_target_pig_MDA <- c(1:params$na_pig)
          p <- pre_pig_MDA(age_target = age_target_pig_MDA, tail_states = tail_states)
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention ='Pig_MDA', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine, tail_states = states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          
        }
        
        if('Pig_vaccine' %in% intervention && is.numeric(age_target_pig_vaccine) && !'Pig_MDA' %in% intervention) {
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine, tail_states = tail_states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = tail_states)
        }
      }
      
      # Do the next run
      runs[[i+1]] <- single_run(tt2[[i]], params, states = states)
    }
    
    # create model run (data frame) output
    runs <- do.call('rbind', runs)
    runs <- as.data.frame(runs)
    
    
    # If number of intervention rounds NOT specified #
    if((is.null(num_intervention_rounds))) {
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(params$PC_sens)) {
        pig_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$PC_sens,
            spec = params$PC_spec,
            TP = runs$Pig_Cysticercosis_prev
          )
        runs <- cbind(runs, pig_cysticercosis_apparent_prev)
        colnames(runs)[colnames(runs) =="apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$C_sens)) {
        human_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$C_sens,
            spec = params$C_spec,
            TP = runs$Human_Cysticercosis_prev
          )
        runs <- cbind(runs, human_cysticercosis_apparent_prev)
        colnames(runs)[colnames(runs) == "apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$T_sens)) {
        human_taeniasis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$T_sens,
            spec = params$T_spec,
            TP = runs$Human_Taeniasis_prev
          )
        Runs <- cbind(runs, human_taeniasis_apparent_prev)
        colnames(runs)[colnames(runs) == "apparent_prev"] <- "Human_taeniasis_apparent_prev"
      }
      
      return(runs)
    }
    
    #================================================================================================#
    # If number of interventions specified: compute rest of model run from end of last intervention  # 
    if((num_intervention_rounds >= 1)) {
      num_intervention_rounds_split <- num_intervention_rounds + 1
      splits1 <- splits[1:num_intervention_rounds_split]
      last_value <- utils::tail(splits1, n = 1) 
      initial_states_post_intervention <- inter_run_setup(model_output = runs, na_pig = params$na_pig, na_human = params$na_human)
      run_post_last_round <- single_run(seq(last_value, time*12, step), params, initial_states_post_intervention)
      run_post_last_round <- as.data.frame(run_post_last_round)
      runs_final <- rbind(runs, run_post_last_round)
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(params$PC_sens)) {
        pig_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$PC_sens,
            spec = params$PC_spec,
            TP = runs_final$Pig_Cysticercosis_prev
          )
        runs_final <- cbind(runs_final, pig_cysticercosis_apparent_prev)
        colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$C_sens)) {
        human_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$C_sens,
            spec = params$C_spec,
            TP = runs_final$Human_Cysticercosis_prev
          )
        runs_final <- cbind(runs_final, human_cysticercosis_apparent_prev)
        colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$T_sens)) {
        human_taeniasis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$T_sens,
            spec = params$T_spec,
            TP = runs_final$Human_Taeniasis_prev
          )
        runs_final <- cbind(runs_final, human_taeniasis_apparent_prev)
        colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <- "Human_taeniasis_apparent_prev"
      }
      
      return(runs_final)
    }
    
  }
  #=======================================================================================================#
  #       Multi-stage interventions (Diff intervention applied over model run) i.e. different dataframes  #
  #=======================================================================================================#
  # Note only pig intervention can currently be structured with two different stages
  
  #======================================================================================#
  #                             Prepare pre-STAGE 1 and STAGE 1intervention period       #
  
  # If interventions DO change over time (i.e. in model run), proceed
  if(!is.null(intervention_stage1)) {
 
    # User specified coverage values for age-structured model
    int_effect_size_list <-
      intervention_effect_size_set_up(
        pig_MDA_cov = pig_MDA_cov_stage1,
        pig_vaccine_ds1_cov = pig_vaccine_ds1_cov_stage1,
        pig_vaccine_ds2_cov = pig_vaccine_ds2_cov_stage1,
        pig_MDA_prop_noimmunity = pig_MDA_prop_noimmunity,
        human_testtreat_cov = human_testtreat_cov,
        human_MDAnic_cov = human_MDAnic_cov,
        human_MDApzq_cov = human_MDApzq_cov
      )
    
    # check on inputs
    check_interventions_stg1(intervention_stage1)
    check_effect(intervention_effect = int_effect_size_list)
    stopifnot("CHECK: 1) intervention_time_stage1 & intervention_time_stage2; 2) intervention_frequency_stage1 & intervention_frequency_stage2; 3)
              age_target_stage1 & age_target_stage2; 4) num_intervention_rounds_stage1 & num_intervention_rounds_stage2; 5) intervention_time_stage1 &
              intervention_time_stage2; 6) intervention_frequency_stage1 & intervention_frequency_stage2" =
              is.numeric(time),
              is.numeric(intervention_time_stage1),
              is.numeric(step),
              length(time) == 1,length(intervention_time_stage1) == 1,
              length(step) == 1,
              time > 0,
              intervention_time_stage1 <= time,
              intervention_time_stage1 > 0,
              is.list(params),
              is.list(initial_states),
              is.numeric(burn_in),
              burn_in >= 0,
              is.numeric(intervention_frequency_stage1),
              length(intervention_frequency_stage1) == 1,
              intervention_frequency_stage1 > 0)

     # if non-age structured model specified, but age_target input provided, throw an error message
    
    if('Pig_MDA' %in% intervention_stage1 && is.numeric(age_target_pig_MDA_stage1) && 
       'Pig_MDA' %in% intervention_stage2 && is.numeric(age_target_pig_MDA_stage2) && 
       params$na_pig == 1) {
      stop('Cannot specify age target for MDA in non age-structured pig model')
    }
    
    
    if('Pig_vaccine' %in% intervention_stage1 && is.numeric(age_target_pig_vaccine_stage1) && 
       'Pig_vaccine' %in% intervention_stage2 && is.numeric(age_target_pig_vaccine_stage2) && 
       params$na_pig == 1) {
      stop('Cannot specify age target for vaccine in non age-structured pig model')
    }
    
    # ========================================================================================== #
    # throw error messages for following model specifications - lacking all neccessary inputs    #
    
    if('Pig_MDA' %in% intervention_stage1 &&  
       !isTRUE('Pig_MDA' %in% intervention_stage2) && !isTRUE('Pig_vaccine' %in% intervention_stage2)) {
      stop('need to specify interventions for stage 2')
    }
    
    if('Pig_vaccine' %in% intervention_stage1 &&  
       !isTRUE('Pig_MDA' %in% intervention_stage2) && !isTRUE('Pig_vaccine' %in% intervention_stage2)) {
      stop('need to specify interventions for stage 2')
    }

    # Set time vectors for pre- intervention
    tt1 <- seq(0, (intervention_time_stage1 * 12) - step, step)
    
    # Set yearly times for intervention period (post first intervention round)
    splits <- seq((intervention_time_stage1 * 12), time * 12, intervention_frequency_stage1) # previously frequency set to 12 i.e 1 year
    tt2 <- list()
    
    # Setting up vectors for different stages (interventions changing) of model run
    
    # Specify vector for interventions (STAGE 1) using number of intervention round argument
    if(!is.null(num_intervention_rounds_stage1)) {
      num_intervention_rounds_stage1_split <- num_intervention_rounds_stage1 + 1
      splits_stage1 <- splits[1:num_intervention_rounds_stage1_split]
      
      if(length(splits) >= 1) {
        for(i in 1:(length(splits_stage1) - 1)) {
          tt2[[i]] <- seq(splits_stage1[i] + step, splits_stage1[i + 1], step)
        }
      }
    }
    
    # Run the pre-stage 1 period 
    bl <- single_run(tt1, params = params, states = initial_states)
    
    # Prepare stage 1 intervention period data structure
    runs_stage1 <- list()
    runs_stage1[[1]] <- bl
    
    #======================================================================================#
    #                             Implement STAGE 1 interventions                          #
    
    for(i in 1:length(tt2)) {
      
      # Pull the 'end' state values from previous run
      tail_states <- inter_run_setup(model_output = runs_stage1[[i]], na_pig = params$na_pig, na_human = params$na_human)
      # Alter states/params for single interventions (non-biomedical)
      if(i == 1 && !'Pig_vaccine' %in% intervention_stage1 && !'Pig_MDA' %in% intervention_stage1 &&
         !'Human_MDA_nic' %in% intervention_stage1 && !'Human_MDA_pzq' %in% intervention_stage1 &&
         !'Human_test_and_treat' %in% intervention_stage1) {
        params <- intervention_event_param(params = params, intervention = intervention_stage1, intervention_effect)
        states <- intervention_event_state(states = tail_states, intervention = intervention_stage1, intervention_effect)
      }
      
      # Alter states/params for single NPI interventions (subsequent years of intervention if continuous)
      if (i > 1 && !'Pig_vaccine' %in% intervention_stage1 && !'Pig_MDA' %in% intervention_stage1 &&
          !'Human_MDA_nic' %in% intervention_stage1 && !'Human_MDA_pzq' %in% intervention_stage1 &&
          !'Human_test_and_treat' %in% intervention_stage1) {
        states <- intervention_event_state(states = tail_states, intervention, intervention_effect)
      }
      
      if('Human_MDA_nic' %in% intervention_stage1 || 'Human_MDA_pzq' %in% intervention_stage1 ||
         'Human_test_and_treat' %in% intervention_stage1) {
        stop('model cannot yet run multi-stage human interventions - this funciton is coming soon!')
        #tail_states <- states # set-up 
      }
      
      #==========================================================#
      # IF statements for pig interventions (non age-structured) #
      if('Pig_MDA' %in% intervention_stage1 || 'Pig_vaccine' %in% intervention_stage1) {
        
        # first param changes (non-biomedical interventions)
        if(i == 1) {
          params <- intervention_event_param(params = params, intervention = intervention_stage1, intervention_effect)
          states <- intervention_event_state(states = tail_states, intervention = intervention_stage1, intervention_effect)
        }

        # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN
        if('Human_MDA_nic' %in% intervention_stage1 || 'Human_MDA_pzq' %in% intervention_stage1 ||
           'Human_test_and_treat' %in% intervention_stage1) {
          stop('model cannot yet run multi-stage human interventions - this funciton is coming soon!')
          #tail_states <- states # set-up 
        }
        
      # A) Non age-structured pig intervention combinations
      # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% intervention_stage1 && !is.numeric(age_target_pig_vaccine_stage1)) {
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = params$na_pig, intervention_frequency = intervention_frequency_stage1)
        } 
        
        if('Pig_MDA' %in% intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && !'Pig_vaccine' %in% intervention_stage1) {
          age_target_pig_MDA_stage1 <- c(4:params$na_pig)
        }
        
        if('Pig_vaccine' %in% intervention_stage1 && !is.numeric(age_target_pig_vaccine_stage1) && !'Pig_MDA' %in% intervention_stage1) {
          states <- intervention_event_state(states = tail_states, intervention = intervention_stage1, intervention_effect = int_effect_size_list)
        }
        
        if('Pig_MDA' %in% intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && 'Pig_vaccine' %in% intervention_stage1 &&
           !is.numeric(age_target_pig_vaccine_stage1)) {
          age_target_pig_MDA_stage1 <- c(4:params$na_pig)
        }
        
        # B) Age-structured pig interventions (user specified) combinations
        if('Pig_MDA' %in% intervention_stage1 && is.numeric(age_target_pig_MDA_stage1)) {
          # takes processed tail states and selects specific age groups to implement intervention
          p <- pre_pig_MDA(age_target = age_target_pig_MDA_stage1, tail_states = tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention = 'Pig_MDA', intervention_effect = int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          
          if('Pig_MDA' %in% intervention_stage1 && is.numeric(age_target_pig_MDA_stage1) && is.numeric(age_target_pig_vaccine_stage1)) {
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
          
          if('Pig_MDA' %in% intervention_stage1 && is.numeric(age_target_pig_MDA_stage1) && 
             'Pig_vaccine' %in% intervention_stage1 && !is.numeric(age_target_pig_vaccine_stage1)) {
            age_target_pig_vaccine <- c(1:params$na_pig)
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention = 'Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
        }
        
        if('Pig_MDA' %in% intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && is.numeric(age_target_pig_vaccine_stage1)) {
          age_target_pig_MDA_stage1 <- c(1:params$na_pig)
          p <- pre_pig_MDA(age_target = age_target_pig_MDA_stage1, tail_states = tail_states)
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention = 'Pig_MDA', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, tail_states = states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention='Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
        }
        
        if('Pig_vaccine' %in% intervention_stage1 && is.numeric(age_target_pig_vaccine_stage1) && !'Pig_MDA' %in% intervention_stage1) {
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, tail_states = tail_states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention='Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = tail_states)
        }
      }
      
      # Do the next run
      runs_stage1[[i + 1]] <- single_run(tt2[[i]], params, states = states)
    }
    
    runs_stage1 <- do.call('rbind', runs_stage1)
    runs_stage1 <- as.data.frame(runs_stage1)
    
    #======================================================================================#
    #                           Prepare STAGE 2 intervention period                        #
    
    # User specified coverage values for age-structured model 
    int_effect_size_list <-
      intervention_effect_size_set_up(
        pig_MDA_cov = pig_MDA_cov_stage2,
        pig_vaccine_ds1_cov = pig_vaccine_ds1_cov_stage2,
        pig_vaccine_ds2_cov = pig_vaccine_ds2_cov_stage2,
        pig_MDA_prop_noimmunity = pig_MDA_prop_noimmunity,
        human_testtreat_cov = human_testtreat_cov,
        human_MDAnic_cov = human_MDAnic_cov,
        human_MDApzq_cov = human_MDApzq_cov
      )
    
    # check on inputs
    check_interventions_stg2(intervention_stage2)
    check_effect(intervention_effect = int_effect_size_list)
    stopifnot(
      is.numeric(time),
      is.numeric(intervention_time_stage1),
      is.numeric(step),
      length(time) == 1,
      length(intervention_time_stage1) == 1,
      length(step) == 1,
      time > 0,
      intervention_time_stage1 <= time,
      intervention_time_stage1 > 0,
      is.list(params),
      is.list(initial_states),
      is.numeric(burn_in),
      burn_in >= 0,
      is.numeric(intervention_frequency_stage1),
      length(intervention_frequency_stage1) == 1,
      intervention_frequency_stage1 > 0
    )
    
    if (is.null(intervention_time_stage2) ||
        !is.null(intervention_time_stage2)) {
      end_int_stage1 <- (intervention_time_stage1 * 12) + (intervention_frequency_stage1 * num_intervention_rounds_stage1)
      intervention_time_stage2 <- end_int_stage1 / 12
    }
    
    # Set yearly times for intervention period (stage 2)
    splits2 <- seq((intervention_time_stage2 * 12), time * 12, intervention_frequency_stage2) # previously frequency set to 12 i.e 1 year
    tt3 <- list()
    
    # Specify vector for interventions in absence of number of int round argument (stage 2)
    if(is.null(num_intervention_rounds_stage2)) {
      if (length(splits2) > 1) {
        for (i in 1:(length(splits2) - 1)) {
          tt3[[i]] <- seq(splits2[i] + step, splits2[i + 1], step)
        }
      }
      
      if (length(splits2) == 1) {
        for (i in 1:(length(splits2))) {
          tt3[[i]] <- seq(splits2[i] + step, time * 12, step)
        }
      }
    }
    
    # Specify vector for number of intervention rounds (stage 2)
    if(!is.null(num_intervention_rounds_stage2)) {
      num_intervention_rounds_stage2_split <- num_intervention_rounds_stage2 + 1
      
      splits_stage2 <-
        splits2[1:num_intervention_rounds_stage2_split]
      
      if (length(splits2) >= 1) {
        for (i in 1:(length(splits_stage2) - 1)) {
          tt3[[i]] <- seq(splits_stage2[i] + step, splits_stage2[i + 1], step)
        }
      }
    }
    
    # Prepare stage 2 intervention period data structure
    runs_stage2 <- list()
    runs_stage2[[1]] <- runs_stage1
    
    #======================================================================================#
    #                             Implement STAGE 2 interventions                          #
    
    for(i in 1:length(tt3)) {
      
      # Pull the 'end' state values from previous run
      tail_states <- inter_run_setup(model_output = runs_stage2[[i]], na_pig = params$na_pig, na_human = params$na_human)
      
      # Alter states/params for single interventions
      if (i == 1 &&
          !'Pig_vaccine' %in% intervention_stage2 &&
          !'Pig_MDA' %in% intervention_stage2 &&
          !'Human_MDA_nic' %in% intervention_stage2 &&
          !'Human_MDA_pzq' %in% intervention_stage2 &&
          !'Human_test_and_treat' %in% intervention_stage2) {
        params <- intervention_event_param(params = params, intervention = intervention_stage2, intervention_effect = int_effect_size_list)
        states <- intervention_event_state(states = tail_states, intervention = intervention_stage2, intervention_effect)
      }
      
      if (i > 1 &&
          !'Pig_vaccine' %in% intervention_stage2 &&
          !'Pig_MDA' %in% intervention_stage2 &&
          !'Human_MDA_nic' %in% intervention_stage2 &&
          !'Human_MDA_pzq' %in% intervention_stage2 &&
          !'Human_test_and_treat' %in% intervention_stage2) {
        states <- intervention_event_state(states = tail_states, intervention, intervention_effect)
      }
      
      
      # IF statements for pig interventions 
      if('Pig_MDA' %in% intervention_stage2 || 'Pig_vaccine' %in% intervention_stage2) {
        
        # first param changes (non-biomedical interventions)
        if(i == 1) {
          params <- intervention_event_param(params = params, intervention = intervention_stage2, intervention_effect)
          states <- intervention_event_state(states = tail_states, intervention = intervention_stage2, intervention_effect)
        }
        
         # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN (?)
        if('Human_MDA_nic' %in% intervention_stage2 || 'Human_MDA_pzq' %in% intervention_stage2 || 'Human_test_and_treat' %in% intervention_stage2) {
          tail_states <- states # set-up 
        }
        
        # A) Non age-structured pig interventions combinations
        # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2)) {
          
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = params$na_pig, intervention_frequency = intervention_frequency_stage2)
        } 
        
        if('Pig_MDA' %in% intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && !'Pig_vaccine' %in% intervention_stage2) {
          age_target_pig_MDA_stage2 <- c(4:params$na_pig)
        }
        
        if('Pig_vaccine' %in% intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2) && !'Pig_MDA' %in% intervention_stage2) {
          states <- intervention_event_state(states = tail_states, intervention = intervention_stage2, intervention_effect = int_effect_size_list)
        }
        
        if('Pig_MDA' %in% intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && 'Pig_vaccine' %in% intervention_stage2 &&!is.numeric(age_target_pig_vaccine_stage2)) {
          age_target_pig_MDA_stage2 <- c(4:params$na_pig)
        }
        
        # B) Age-structured pig interventions (user specified) combinations
        if('Pig_MDA' %in% intervention_stage2 && is.numeric(age_target_pig_MDA_stage2)) {
          
          # takes processed tail states and selects specific age groups to implement intervention
          p <- pre_pig_MDA(age_target = age_target_pig_MDA_stage2, tail_states = tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention='Pig_MDA', intervention_effect = int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          
          if('Pig_MDA' %in% intervention_stage2 && is.numeric(age_target_pig_MDA_stage2) && is.numeric(age_target_pig_vaccine_stage2)) {
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention ='Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
          
          if('Pig_MDA' %in% intervention_stage2 && is.numeric(age_target_pig_MDA_stage2) && 'Pig_vaccine' %in% intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2)) {
            age_target_pig_vaccine_stage2 <- c(1:params$na_pig)
            p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, tail_states = states)
            states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention = 'Pig_vaccine', intervention_effect = int_effect_size_list)
            states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
          }
        }
        
        if('Pig_MDA' %in% intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && is.numeric(age_target_pig_vaccine_stage2)) {
          age_target_pig_MDA_stage2 <- c(1:params$na_pig)
          p <- pre_pig_MDA(age_target = age_target_pig_MDA_stage2, tail_states = tail_states)
          states_move_age_pig_MDA <- intervention_event_state(states = p, intervention='Pig_MDA', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_MDA, tail_states = tail_states)
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, tail_states = states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention = 'Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = states)
      }
        
        if('Pig_vaccine' %in% intervention_stage2 && is.numeric(age_target_pig_vaccine_stage2) && !'Pig_MDA' %in% intervention_stage2) {
          p <- pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, tail_states = tail_states)
          states_move_age_pig_vaccine <- intervention_event_state(states = p, intervention = 'Pig_vaccine', intervention_effect = int_effect_size_list)
          states <- update_states(states_move = states_move_age_pig_vaccine, tail_states = tail_states)
        }
      }
      
      # Do the next run
      runs_stage2[[i+1]] <- single_run(tt3[[i]], params, states = states)
    }
    
    # create model run (data frame) output
    runs_stage2 <- do.call('rbind', runs_stage2)
    runs_stage2 <- as.data.frame(runs_stage2)
    
    # If no number of intervention rounds specified
    if((is.null(num_intervention_rounds_stage2))) {
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(params$PC_sens)) {
        pig_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$PC_sens,
            spec = params$PC_spec,
            TP = runs_stage2$Pig_Cysticercosis_prev
          )
        runs_stage2 <- cbind(runs_stage2, pig_cysticercosis_apparent_prev)
        colnames(runs_stage2)[colnames(runs_stage2) == "apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$C_sens)) {
        human_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$C_sens,
            spec = params$C_spec,
            TP = runs_stage2$Human_Cysticercosis_prev
          )
        runs_stage2 <- cbind(runs_stage2, human_cysticercosis_apparent_prev)
        colnames(runs_stage2)[colnames(runs_stage2) =="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$T_sens)) {
        human_taeniasis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$T_sens,
            spec = params$T_spec,
            TP = runs_stage2$Human_Taeniasis_prev
          )
        runs_stage2 <- cbind(runs_stage2, human_taeniasis_apparent_prev)
        colnames(runs_stage2)[colnames(runs_stage2) =="apparent_prev"] <- "Human_taeniasis_apparent_prev"
      }
      
      return(runs_stage2)
    }
    
    
    # If number of interventions specified for STAGE 2 interventions 
    if((num_intervention_rounds_stage2 >= 1)) {
      
      num_intervention_rounds_stage2_split <- num_intervention_rounds_stage2 + 1
      splits_end <- splits2[1:num_intervention_rounds_stage2_split]
      last_value <- utils::tail(splits_end, n = 1) 
      
      # prepare and run from end of stage 2 to the end of model run (i.e. no further interventions in final stage)
      initial_states_post_intervention <- inter_run_setup(model_output = runs_stage2, na_pig = params$na_pig, na_human = params$na_human)
      run_post_last_round <- single_run(seq(last_value, time * 12, step), params, initial_states_post_intervention)
      run_post_last_round <- as.data.frame(run_post_last_round)
      runs_stage2_final <- rbind(runs_stage2, run_post_last_round) # final dataframe output
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(params$PC_sens)) {
        pig_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$PC_sens,
            spec = params$PC_spec,
            TP = runs_stage2_final$Pig_Cysticercosis_prev
          )
        runs_stage2_final <- cbind(runs_stage2_final, pig_cysticercosis_apparent_prev)
        colnames(runs_stage2_final)[colnames(runs_stage2_final) == "apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$C_sens)) {
        human_cysticercosis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$C_sens,
            spec = params$C_spec,
            TP = runs_stage2_final$Human_Cysticercosis_prev
          )
        runs_stage2_final <- cbind(runs_stage2_final, human_cysticercosis_apparent_prev)
        colnames(runs_stage2_final)[colnames(runs_stage2_final) == "apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(params$T_sens)) {
        human_taeniasis_apparent_prev <-
          apparent_prevalence_packaging_func(
            sens = params$T_sens,
            spec = params$T_spec,
            TP = runs_stage2_final$Human_Taeniasis_prev
          )
        runs_stage2_final <- cbind(runs_stage2_final, human_taeniasis_apparent_prev)
        colnames(runs_stage2_final)[colnames(runs_stage2_final) == "apparent_prev"] <- "Human_taeniasis_apparent_prev"
      }
      
      return(runs_stage2_final)
    }
    
  }
  
}
