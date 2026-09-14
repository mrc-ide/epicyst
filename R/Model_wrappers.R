
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
#' @param pig_ofz_efficacy efficacy for oxfendazole for pig MDA
#' @param pig_vaccine_ds1_cov Specify pig vaccine (dose 1) coverage
#' @param pig_vaccine_ds2_cov Specify pig vaccine (dose 2) coverage
#' @param human_testtreat_cov Specify human test and treat coverage
#' @param human_MDAnic_cov Specify human MDA with niclosamide coverage
#' @param human_MDApzq_cov Specify human MDA with praziquantel coverage
#' @param human_pzq_efficacy efficacy for praziqunatel for human MDA
#' @param human_nic_efficacy efficacy for niclosamide for human MDA
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
#' @param human_MDA_offset month from which human MDA should start (counting from round 1) in multi-stage interventions
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
           pig_ofz_efficacy = NULL,
           pig_vaccine_ds1_cov = NULL,
           pig_vaccine_ds2_cov = NULL,
           pig_MDA_prop_noimmunity = NULL,
           human_testtreat_cov = NULL,
           human_MDAnic_cov = NULL,
           human_MDApzq_cov = NULL,
           human_pzq_efficacy = NULL,
           human_nic_efficacy = NULL,
           human_MDA_offset = NULL,
           age_target_human_MDA = NULL,
           age_target_human_test_and_treat = NULL,
           age_target_human_MDA_multistage = NULL,
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

    # User specified coverage and efficacy values for age-structured model
    int_effect_size_list <- intervention_effect_size_set_up(
      pig_MDA_cov = pig_MDA_cov,
      pig_vaccine_ds1_cov = pig_vaccine_ds1_cov,
      pig_vaccine_ds2_cov = pig_vaccine_ds2_cov,
      human_testtreat_cov = human_testtreat_cov,
      human_MDAnic_cov = human_MDAnic_cov,
      human_MDApzq_cov = human_MDApzq_cov,
      pig_ofz_efficacy = pig_ofz_efficacy, 
      human_pzq_efficacy = human_pzq_efficacy, 
      human_nic_efficacy = human_nic_efficacy,
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
             'Pig_vaccine' %in% intervention && !is.numeric(age_target_pig_vaccine)) {
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
  
  #=======================================================================================================#
  #       Multi-stage interventions (Diff intervention applied over model run) i.e. different dataframes  #
  #=======================================================================================================#
  # Note only pig intervention can currently be structured with two different stages
  
  
  #=======================================================================================================#
  #       Multi-stage interventions (different intervention sets applied over the model run)               #
  #=======================================================================================================#
  # Pig interventions can be structured across two stages.
  #
  # A SINGLE round of human MDA can now be layered on at an arbitrary offset from the first
  # intervention round, via the new `human_MDA_offset` argument (months). The round is routed
  # automatically into whichever stage contains that time point, and composes with any pig round
  # falling at the same time. Where it falls between pig rounds, an extra time-step boundary is
  # inserted so the model can stop, apply it, and carry on.
  #
  # NEW FORMAL to add to run_model() (and any wrapper that forwards ... to it):
  #   human_MDA_offset = NULL
  
  if (!is.null(intervention_stage1)) {
    
    # ================================================================================ #
    #  0. Split human from non-human (pig / NPI) interventions                          #
    # ================================================================================ #
    # The pig / NPI machinery must never see a human intervention name, otherwise the
    # human round would be re-applied at every pig round.
    
    human_int_names <- c("Human_MDA_pzq", "Human_MDA_nic", "Human_test_and_treat")
    
    human_stage1    <- intersect(intervention_stage1, human_int_names)
    nonhuman_stage1 <- setdiff(intervention_stage1, human_int_names)
    human_stage2    <- intersect(intervention_stage2, human_int_names)
    nonhuman_stage2 <- setdiff(intervention_stage2, human_int_names)
    
    # The single human round. Declaring it in either stage vector is fine - the offset,
    # not the vector it sits in, decides which stage it lands in.
    human_round_ints <- unique(c(human_stage1, human_stage2))
    
    if (length(human_round_ints) > 1) {
      stop("Only one human intervention can currently be scheduled as a single round: ",
           paste(human_round_ints, collapse = ", "))
    }
    if (length(human_round_ints) == 1 && is.null(human_MDA_offset)) {
      stop("A human intervention was requested but `human_MDA_offset` was not supplied. ",
           "Give it in months after the first intervention round.")
    }
    if (length(human_round_ints) == 0 && !is.null(human_MDA_offset)) {
      stop("`human_MDA_offset` was supplied but no human intervention appears in ",
           "intervention_stage1 or intervention_stage2")
    }
    
    # Absolute time (months) of the single human round, snapped to the model time grid
    human_MDA_time <- NULL
    if (length(human_round_ints) == 1) {
      human_MDA_time <- (intervention_time_stage1 * 12) + human_MDA_offset
      human_MDA_time <- round(human_MDA_time / step) * step
      stopifnot("human_MDA_offset must be >= 0" = human_MDA_offset >= 0,
                "human round falls beyond the end of the model run" = human_MDA_time <= time * 12)
    }
    
    human_applied <- FALSE   # set TRUE once the round has actually been applied
    
    # ================================================================================ #
    #  1. Internal helper: apply one round of pig / NPI interventions                   #
    # ================================================================================ #
    # Pulled out of the two loops so stage 1 and stage 2 cannot drift apart.
    # Returns the (possibly updated) params and states, plus any age targets that were
    # filled in with defaults, so the caller can hold on to them.
    
    apply_pig_round <- function(tail_states, params, intervention_vec,
                                age_target_MDA, age_target_vaccine,
                                int_effect_size_list, intervention_frequency,
                                update_params) {
      
      states <- tail_states
      
      if (length(intervention_vec) == 0) {
        return(list(params = params, states = states,
                    age_target_MDA = age_target_MDA,
                    age_target_vaccine = age_target_vaccine))
      }
      
      # Parameter changes (non-biomedical interventions) - applied once per stage
      if (isTRUE(update_params)) {
        params <- intervention_event_param(params = params,
                                           intervention = intervention_vec,
                                           intervention_effect = int_effect_size_list)
      }
      
      has_MDA  <- "Pig_MDA"     %in% intervention_vec
      has_vacc <- "Pig_vaccine" %in% intervention_vec
      
      # ---- NPI-only round: apply to the whole population -------------------------- #
      if (!has_MDA && !has_vacc) {
        states <- intervention_event_state(states = states,
                                           intervention = intervention_vec,
                                           intervention_effect = int_effect_size_list)
        return(list(params = params, states = states,
                    age_target_MDA = age_target_MDA,
                    age_target_vaccine = age_target_vaccine))
      }
      
      # ---- Non age-structured pig model: no age selection possible ----------------- #
      if (params$na_pig == 1) {
        states <- intervention_event_state(states = states,
                                           intervention = intervention_vec,
                                           intervention_effect = int_effect_size_list)
        return(list(params = params, states = states,
                    age_target_MDA = age_target_MDA,
                    age_target_vaccine = age_target_vaccine))
      }
      
      # ---- A) Fill in age targets where the user did not supply them --------------- #
      # Vaccine: accounts for vaccination from 2 months and the < 4 month interval
      # between first and second dose.
      if (has_vacc && !is.numeric(age_target_vaccine)) {
        age_target_vaccine <- age_struc_pig_vacc_func(
          oldest_age = params$na_pig,
          intervention_frequency = intervention_frequency)
      }
      if (has_MDA && !is.numeric(age_target_MDA)) {
        age_target_MDA <- c(4:params$na_pig)
      }
      
      # ---- B) Apply to the targeted age groups ------------------------------------- #
      # pre_*  : select the targeted age groups out of the tail states
      # intervention_event_state : apply the effect to those groups
      # update_states : write them back into the full state vector
      if (has_MDA) {
        p  <- pre_pig_MDA(age_target = age_target_MDA, tail_states = states)
        sm <- intervention_event_state(states = p, intervention = "Pig_MDA",
                                       intervention_effect = int_effect_size_list)
        states <- update_states(states_move = sm, tail_states = states)
      }
      
      if (has_vacc) {
        p  <- pre_pig_vaccine(age_target = age_target_vaccine, tail_states = states)
        sm <- intervention_event_state(states = p, intervention = "Pig_vaccine",
                                       intervention_effect = int_effect_size_list)
        states <- update_states(states_move = sm, tail_states = states)
      }
      
      list(params = params, states = states,
           age_target_MDA = age_target_MDA,
           age_target_vaccine = age_target_vaccine)
    }
    
    # ================================================================================ #
    #  2. STAGE 1 set-up                                                                #
    # ================================================================================ #
    
    int_effect_size_list <-
      intervention_effect_size_set_up(
        pig_MDA_cov             = pig_MDA_cov_stage1,
        pig_vaccine_ds1_cov     = pig_vaccine_ds1_cov_stage1,
        pig_vaccine_ds2_cov     = pig_vaccine_ds2_cov_stage1,
        pig_MDA_prop_noimmunity = pig_MDA_prop_noimmunity,
        human_testtreat_cov     = human_testtreat_cov,
        human_MDAnic_cov        = human_MDAnic_cov,
        human_MDApzq_cov        = human_MDApzq_cov,
        pig_ofz_efficacy        = pig_ofz_efficacy,
        human_pzq_efficacy      = human_pzq_efficacy,
        human_nic_efficacy      = human_nic_efficacy
      )
    
    # Input checks (pig / NPI names only - human names are validated above)
    if (length(nonhuman_stage1) > 0) check_interventions_stg1(nonhuman_stage1)
    check_effect(intervention_effect = int_effect_size_list)
    
    stopifnot(
      "time must be a single positive number"                   = is.numeric(time) && length(time) == 1 && time > 0,
      "step must be a single number"                            = is.numeric(step) && length(step) == 1,
      "intervention_time_stage1 must be a single number in (0, time]" =
        is.numeric(intervention_time_stage1) && length(intervention_time_stage1) == 1 &&
        intervention_time_stage1 > 0 && intervention_time_stage1 <= time,
      "params must be a list"                                   = is.list(params),
      "initial_states must be a list"                           = is.list(initial_states),
      "burn_in must be a non-negative number"                   = is.numeric(burn_in) && burn_in >= 0,
      "intervention_frequency_stage1 must be a single positive number" =
        is.numeric(intervention_frequency_stage1) && length(intervention_frequency_stage1) == 1 &&
        intervention_frequency_stage1 > 0,
      "num_intervention_rounds_stage1 must be supplied for a multi-stage run" =
        !is.null(num_intervention_rounds_stage1)
    )
    
    # Age targets are meaningless in a non age-structured pig model
    if (params$na_pig == 1 &&
        (is.numeric(age_target_pig_MDA_stage1) || is.numeric(age_target_pig_MDA_stage2))) {
      stop("Cannot specify age target for MDA in non age-structured pig model")
    }
    if (params$na_pig == 1 &&
        (is.numeric(age_target_pig_vaccine_stage1) || is.numeric(age_target_pig_vaccine_stage2))) {
      stop("Cannot specify age target for vaccine in non age-structured pig model")
    }
    if (params$na_human == 1 && is.numeric(age_target_human_MDA_multistage)) {
      stop("Cannot specify age target for human MDA in non age-structured human model")
    }
    
    # Stage 2 must contain something if stage 1 used the biomedical pig interventions
    if (any(c("Pig_MDA", "Pig_vaccine") %in% nonhuman_stage1) &&
        !any(c("Pig_MDA", "Pig_vaccine") %in% nonhuman_stage2)) {
      stop("need to specify interventions for stage 2")
    }
    
    # ---- Pre-intervention (burn-in / baseline) time vector ------------------------- #
    tt1 <- seq(0, (intervention_time_stage1 * 12) - step, step)
    
    # ---- Stage 1 event schedule ---------------------------------------------------- #
    splits <- seq((intervention_time_stage1 * 12), time * 12, intervention_frequency_stage1)
    
    # Pig round times are counted independently of the human round, so the stage 1 ->
    # stage 2 hand-off below is unaffected by inserting an off-cycle human event.
    pig_round_times_stage1 <- splits[1:(num_intervention_rounds_stage1 + 1)]
    end_int_stage1         <- max(pig_round_times_stage1)
    
    # Does the human round fall inside stage 1?
    human_time_stage1 <- NULL
    if (!is.null(human_MDA_time) && human_MDA_time < end_int_stage1) {
      human_time_stage1 <- human_MDA_time
    }
    
    event_times_stage1 <- sort(unique(c(pig_round_times_stage1, human_time_stage1)))
    event_pig_stage1   <- event_times_stage1 %in% pig_round_times_stage1
    event_hum_stage1   <- event_times_stage1 %in% human_time_stage1
    
    # Segments run from one boundary to the next; the final boundary closes the last
    # segment and carries no event.
    tt2 <- vector("list", length(event_times_stage1) - 1)
    for (i in seq_len(length(event_times_stage1) - 1)) {
      tt2[[i]] <- seq(event_times_stage1[i] + step, event_times_stage1[i + 1], step)
    }
    
    first_pig_stage1 <- which(event_pig_stage1)[1]
    
    # ---- Run baseline -------------------------------------------------------------- #
    bl <- single_run(tt1, params = params, states = initial_states)
    
    runs_stage1 <- list()
    runs_stage1[[1]] <- bl
    
    # ================================================================================ #
    #  3. Implement STAGE 1 interventions                                               #
    # ================================================================================ #
    
    for (i in seq_along(tt2)) {
      
      # End state values from the previous segment
      tail_states <- inter_run_setup(model_output = runs_stage1[[i]],
                                     na_pig = params$na_pig, na_human = params$na_human)
      
      # Default: carry states forward untouched. Essential - at a human-only boundary
      # nothing in the pig block runs, and without this a stale `states` object from the
      # previous round would silently re-apply the last pig intervention.
      states <- tail_states
      
      # ---- Pig / NPI round ---------------------------------------------------------- #
      if (event_pig_stage1[i] && length(nonhuman_stage1) > 0) {
        out <- apply_pig_round(
          tail_states            = tail_states,
          params                 = params,
          intervention_vec       = nonhuman_stage1,
          age_target_MDA         = age_target_pig_MDA_stage1,
          age_target_vaccine     = age_target_pig_vaccine_stage1,
          int_effect_size_list   = int_effect_size_list,
          intervention_frequency = intervention_frequency_stage1,
          update_params          = (i == first_pig_stage1)
        )
        params                        <- out$params
        states                        <- out$states
        age_target_pig_MDA_stage1     <- out$age_target_MDA
        age_target_pig_vaccine_stage1 <- out$age_target_vaccine
      }
      
      # ---- Human round (applied after the pig round so the two compose) ------------- #
      if (event_hum_stage1[i]) {
        if (is.numeric(age_target_human_MDA_multistage)) {
          p  <- pre_human_MDA(age_target = age_target_human_MDA_multistage,
                              tail_states = states)
          sm <- intervention_event_state(states = p,
                                         intervention = human_round_ints,
                                         intervention_effect = int_effect_size_list)
          states <- update_states(states_move = sm, tail_states = states)
        } else {
          states <- intervention_event_state(states = states,
                                             intervention = human_round_ints,
                                             intervention_effect = int_effect_size_list)
        }
        human_applied <- TRUE
        message("Human round (", human_round_ints, ") applied at month ",
                event_times_stage1[i], " [stage 1], ",
                if (is.numeric(age_target_human_MDA_multistage))
                  sprintf("ages %d-%d", min(age_target_human_MDA_multistage),
                          max(age_target_human_MDA_multistage)) else "all ages")
      }
      
      runs_stage1[[i + 1]] <- single_run(tt2[[i]], params, states = states)
    }
    
    runs_stage1 <- do.call("rbind", runs_stage1)
    runs_stage1 <- as.data.frame(runs_stage1)
    
    # ================================================================================ #
    #  4. STAGE 2 set-up                                                                #
    # ================================================================================ #
    
    int_effect_size_list <-
      intervention_effect_size_set_up(
        pig_MDA_cov             = pig_MDA_cov_stage2,
        pig_vaccine_ds1_cov     = pig_vaccine_ds1_cov_stage2,
        pig_vaccine_ds2_cov     = pig_vaccine_ds2_cov_stage2,
        pig_MDA_prop_noimmunity = pig_MDA_prop_noimmunity,
        human_testtreat_cov     = human_testtreat_cov,
        human_MDAnic_cov        = human_MDAnic_cov,
        human_MDApzq_cov        = human_MDApzq_cov,
        pig_ofz_efficacy        = pig_ofz_efficacy,
        human_pzq_efficacy      = human_pzq_efficacy,
        human_nic_efficacy      = human_nic_efficacy
      )
    
    if (length(nonhuman_stage2) > 0) check_interventions_stg2(nonhuman_stage2)
    check_effect(intervention_effect = int_effect_size_list)
    
    stopifnot(
      "intervention_frequency_stage2 must be a single positive number" =
        is.numeric(intervention_frequency_stage2) &&
        length(intervention_frequency_stage2) == 1 &&
        intervention_frequency_stage2 > 0
    )
    
    # Stage 2 starts where stage 1 finished, unless the user pinned it explicitly.
    # (The original condition here was `is.null(x) || !is.null(x)`, i.e. always TRUE,
    #  so a user-supplied intervention_time_stage2 was silently overwritten.)
    if (is.null(intervention_time_stage2)) {
      intervention_time_stage2 <- end_int_stage1 / 12
    } else if (!isTRUE(all.equal(intervention_time_stage2 * 12, end_int_stage1))) {
      warning("intervention_time_stage2 (", intervention_time_stage2 * 12,
              " months) does not match the end of stage 1 (", end_int_stage1,
              " months); using the supplied value.")
    }
    
    # ---- Stage 2 event schedule ----------------------------------------------------- #
    splits2 <- seq((intervention_time_stage2 * 12), time * 12, intervention_frequency_stage2)
    
    if (!is.null(num_intervention_rounds_stage2)) {
      pig_round_times_stage2 <- splits2[1:(num_intervention_rounds_stage2 + 1)]
    } else {
      pig_round_times_stage2 <- splits2
      if (length(splits2) == 1) pig_round_times_stage2 <- c(splits2, time * 12)
    }
    
    human_time_stage2 <- NULL
    if (!is.null(human_MDA_time) && !human_applied &&
        human_MDA_time >= min(pig_round_times_stage2) &&
        human_MDA_time <  max(pig_round_times_stage2)) {
      human_time_stage2 <- human_MDA_time
    }
    
    event_times_stage2 <- sort(unique(c(pig_round_times_stage2, human_time_stage2)))
    event_pig_stage2   <- event_times_stage2 %in% pig_round_times_stage2
    event_hum_stage2   <- event_times_stage2 %in% human_time_stage2
    
    tt3 <- vector("list", length(event_times_stage2) - 1)
    for (i in seq_len(length(event_times_stage2) - 1)) {
      tt3[[i]] <- seq(event_times_stage2[i] + step, event_times_stage2[i + 1], step)
    }
    
    first_pig_stage2 <- which(event_pig_stage2)[1]
    
    runs_stage2 <- list()
    runs_stage2[[1]] <- runs_stage1
    
    # ================================================================================ #
    #  5. Implement STAGE 2 interventions                                               #
    # ================================================================================ #
    
    for (i in seq_along(tt3)) {
      
      tail_states <- inter_run_setup(model_output = runs_stage2[[i]],
                                     na_pig = params$na_pig, na_human = params$na_human)
      states <- tail_states
      
      if (event_pig_stage2[i] && length(nonhuman_stage2) > 0) {
        out <- apply_pig_round(
          tail_states            = tail_states,
          params                 = params,
          intervention_vec       = nonhuman_stage2,
          age_target_MDA         = age_target_pig_MDA_stage2,
          age_target_vaccine     = age_target_pig_vaccine_stage2,
          int_effect_size_list   = int_effect_size_list,
          intervention_frequency = intervention_frequency_stage2,
          update_params          = (i == first_pig_stage2)
        )
        params                        <- out$params
        states                        <- out$states
        age_target_pig_MDA_stage2     <- out$age_target_MDA
        age_target_pig_vaccine_stage2 <- out$age_target_vaccine
      }
      
      if (event_hum_stage2[i]) {
        if (is.numeric(age_target_human_MDA_multistage)) {
          p  <- pre_human_MDA(age_target = age_target_human_MDA_multistage,
                              tail_states = states)
          sm <- intervention_event_state(states = p,
                                         intervention = human_round_ints,
                                         intervention_effect = int_effect_size_list)
          states <- update_states(states_move = sm, tail_states = states)
        } else {
          states <- intervention_event_state(states = states,
                                             intervention = human_round_ints,
                                             intervention_effect = int_effect_size_list)
        }
        human_applied <- TRUE
        message("Human round (", human_round_ints, ") applied at month ",
                event_times_stage2[i], " [stage 2], ",
                if (is.numeric(age_target_human_MDA_multistage))
                  sprintf("ages %d-%d", min(age_target_human_MDA_multistage),
                          max(age_target_human_MDA_multistage)) else "all ages")
      }
      
      runs_stage2[[i + 1]] <- single_run(tt3[[i]], params, states = states)
    }
    
    runs_stage2 <- do.call("rbind", runs_stage2)
    runs_stage2 <- as.data.frame(runs_stage2)
    
    if (!is.null(human_MDA_time) && !human_applied) {
      warning("The human round scheduled for month ", human_MDA_time,
              " fell outside both intervention stages and was NOT applied. ",
              "Stage 1 covers [", min(pig_round_times_stage1), ", ", end_int_stage1,
              "), stage 2 covers [", min(pig_round_times_stage2), ", ",
              max(pig_round_times_stage2), ").")
    }
    
    # ================================================================================ #
    #  6. Tail of the run, and true -> apparent prevalence adjustment                   #
    # ================================================================================ #
    
    # No fixed number of stage 2 rounds: the run already reaches the end
    if (is.null(num_intervention_rounds_stage2)) {
      runs_final <- runs_stage2
    } else {
      # Run on from the last stage 2 boundary to the end of the model run, no further
      # interventions applied.
      last_value <- max(pig_round_times_stage2)
      initial_states_post_intervention <-
        inter_run_setup(model_output = runs_stage2,
                        na_pig = params$na_pig, na_human = params$na_human)
      run_post_last_round <- single_run(seq(last_value + step, time * 12, step),
                                        params, initial_states_post_intervention)
      run_post_last_round <- as.data.frame(run_post_last_round)
      runs_final <- rbind(runs_stage2, run_post_last_round)
    }
    
    if (!is.null(params$PC_sens)) {
      pig_cysticercosis_apparent_prev <-
        apparent_prevalence_packaging_func(sens = params$PC_sens,
                                           spec = params$PC_spec,
                                           TP   = runs_final$Pig_Cysticercosis_prev)
      runs_final <- cbind(runs_final, pig_cysticercosis_apparent_prev)
      colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <-
        "Pig_cysticercosis_apparent_prev"
    }
    
    if (!is.null(params$C_sens)) {
      human_cysticercosis_apparent_prev <-
        apparent_prevalence_packaging_func(sens = params$C_sens,
                                           spec = params$C_spec,
                                           TP   = runs_final$Human_Cysticercosis_prev)
      runs_final <- cbind(runs_final, human_cysticercosis_apparent_prev)
      colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <-
        "Human_cysticercosis_apparent_prev"
    }
    
    if (!is.null(params$T_sens)) {
      human_taeniasis_apparent_prev <-
        apparent_prevalence_packaging_func(sens = params$T_sens,
                                           spec = params$T_spec,
                                           TP   = runs_final$Human_Taeniasis_prev)
      runs_final <- cbind(runs_final, human_taeniasis_apparent_prev)
      colnames(runs_final)[colnames(runs_final) == "apparent_prev"] <-
        "Human_taeniasis_apparent_prev"
    }
    
    return(runs_final)
  }
  
}


