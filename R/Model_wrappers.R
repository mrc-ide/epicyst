
#' @title
#' Run Cysticercosis model ODE
#' @description
#' Runs a single implementation of the ODE Cysticercosis model
#'
#' @param tt vector of times
#' @param params list of parameters
#' @param states list of states
#' @export
Single_run<-function(tt, params, states){
  
  Mod<-cyst_generator(user=c(params, states))
  
  y<-Mod$run(tt)
  
  return(y)
}


#' @title
#' Run Cysticercosis model with interventions
#' @description
#' Runs the ODE Cysticercosis model
#'
#' @param Params List of model parameters
#' @param Initial_states List of intitial state values
#' @param Time The number of years to run the model for (from equilibrium). Default is at the halfway point.
#' @param Intervention A vector of interventions to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param Intervention_effect A list of intervention effect sizes, see \code{Intervention_effect_size} for details
#' @param Intervention_frequency Frequency of intervention implementation (i.e. 12 = every year)
#' @param step Time step (months)
#' @param Burn_in A burn in period run before model run (years)
#' @param age_target_pig_MDA Vector of age classes to target with pig MDA intervention (months)
#' @param age_target_pig_vaccine Vector of age classes to target with pig vaccine intervention (months)
#' @param Num_intervention_rounds Specify number of rounds of intervention
#' @param pig_MDA_cov Specify pig MDA coverage
#' @param pig_vaccine_ds1_cov Specify pig vaccine (dose 1) coverage
#' @param pig_vaccine_ds2_cov Specify pig vaccine (dose 2) coverage
#' @param human_testtreat_cov Specify human test and treat coverage
#' @param human_MDAnic_cov Specify human MDA with niclosamide coverage
#' @param human_MDApzq_cov Specify human MDA with praziquantel coverage
#' @param Intervention_stage1 A vector of interventions (multistage stage 1) to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param Intervention_stage2 A vector of interventions (multistage stage 2)to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param Intervention_frequency_stage1 Frequency of intervention (multistage stage 1) implementation (i.e. 12 = every year)
#' @param Intervention_frequency_stage2 Frequency of intervention (multistage stage 2) implementation (i.e. 12 = every year)
#' @param age_target_pig_MDA_stage1 Vector of age classes to target with pig MDA (multistage stage 1) intervention (months)
#' @param age_target_pig_MDA_stage2 Vector of age classes to target with pig MDA (multistage stage 2) intervention (months)
#' @param age_target_pig_vaccine_stage2 Vector of age classes to target with pig vaccine (multistage stage 1)intervention (months)
#' @param age_target_pig_vaccine_stage2 Vector of age classes to target with pig vaccine (multistage stage 2)intervention (months)
#' @param Num_intervention_rounds_stage1 Specify number of rounds of intervention (multistage stage 1)
#' @param Num_intervention_rounds_stage2 Specify number of rounds of intervention (multistage stage 2)
#' @param pig_MDA_cov_stage1 Specify pig MDA coverage (multistage stage 1)
#' @param pig_MDA_cov_stage2 Specify pig MDA coverage (multistage stage 2)
#' @param pig_vaccine_ds1_cov_stage1 Specify pig vaccine (dose 1) coverage (multistage stage 1)
#' @param pig_vaccine_ds1_cov_stage2 Specify pig vaccine (dose 1) coverage (multistage stage 2)
#' @param pig_vaccine_ds2_cov_stage1 Specify pig vaccine (dose 2) coverage (multistage stage 1)
#' @param pig_vaccine_ds2_cov_stage2 Specify pig vaccine (dose 2) coverage (multistage stage 2)
#' 
#' @examples
#' # Run the baseline model:
#' M1<-Run_model(Time=50, Burn_in=50)
#' plot(M1$t/12, M1$Humans_Cysticercosis, t='l', ylim=c(0,1000), ylab='Humans with Cysticercosis', xlab='Time (years)')
#' 
#' # Run the model with a single intervention:
#' M2<-Run_model(Time=50, Intervention='Sanitation', Intervention_time=20, Burn_in=50)
#' lines(M2$t/12, M2$Humans_Cysticercosis, col='deeppink')
#' 
#' # Run the model with multiple interventions:
#' M3<-Run_model(Time=50, Intervention=c('Human_test_and_treat', 'Pig_MDA'), Intervention_time=20, Burn_in=50)
#' lines(M3$t/12, M3$Humans_Cysticercosis, col='dodgerblue')
#' legend('topright', c('Baseline','Sanitation','Human test & treat and Pig MDA'), lty=c(1,1,1), col=c('black','deeppink', 'dodgerblue'))
#' 
#' @export
Run_model<-function(Params=NULL, Initial_states=NULL, Time, Intervention=NULL, Intervention_time=Time/2,
                    Intervention_effect=Intervention_effect_size(), Intervention_frequency, step=1/30, Burn_in=0,
                    age_target_pig_MDA = NULL, age_target_pig_vaccine = NULL, Num_intervention_rounds= NULL,
                    pig_MDA_cov = NULL, pig_vaccine_ds1_cov=NULL, pig_vaccine_ds2_cov=NULL,
                    human_testtreat_cov = NULL, human_MDAnic_cov = NULL, human_MDApzq_cov = NULL,
                    age_target_human_MDA = NULL, age_target_human_test_and_treat = NULL,
                    Pig_MDA_prop_noimmunity = NULL,
                    Intervention_stage1=NULL, Intervention_stage2= NULL, 
                    Intervention_time_stage1=NULL, Intervention_time_stage2=NULL,
                    Intervention_frequency_stage1=NULL, Intervention_frequency_stage2=NULL,
                    Num_intervention_rounds_stage1=NULL, Num_intervention_rounds_stage2=NULL,
                    age_target_pig_MDA_stage1 = NULL, age_target_pig_MDA_stage2 = NULL, 
                    age_target_pig_vaccine_stage1 = NULL, age_target_pig_vaccine_stage2 = NULL,
                    pig_MDA_cov_stage1 = NULL, pig_MDA_cov_stage2 = NULL,
                    pig_vaccine_ds1_cov_stage1=NULL, pig_vaccine_ds1_cov_stage2=NULL,
                    pig_vaccine_ds2_cov_stage1=NULL, pig_vaccine_ds2_cov_stage2=NULL){
  
  # Calculate parmaters and initial state variables (if not provided)
  Initialise<-Set_up()
  if(is.null(Params)){
    Params<-Initialise[[1]]
  }
  if(is.null(Initial_states)){
    Initial_states<-Initialise[[2]]
  }
  
  # Run burn in period
  if(Burn_in>0){
    tt_burn<-seq(0, (Burn_in*12), step)
    Burn<-Single_run(tt_burn, params=Params, states=Initial_states)
    Burn_out <- as.data.frame(Burn)
    Initial_states<- Inter_run_setup(model_output=Burn, na_pig=Params$na_pig, na_human=Params$na_human)
  }
  
  # Run with no interventions (if none specified)
  if(is.null(Intervention) && is.null(Intervention_stage1)){
    Run<-Single_run(seq(0, Time*12, step), params=Params, Initial_states)
    Run<-as.data.frame(Run)
    
    # To plot 'apparent' prevalence if argument selected (from true underlying model predicted prevalence)
    if(!is.null(Params$PC_sens)){
      Pig_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$PC_sens, spec=Params$PC_spec, TP=Run$Pig_Cysticercosis_prev)
      Run <- cbind(Run, Pig_cysticercosis_apparent_prev)
      colnames(Run)[colnames(Run)=="apparent_prev"] <- "Pig_cysticercosis_apparent_prev"
    }
    
    if(!is.null(Params$C_sens)){
      Human_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$C_sens, spec=Params$C_spec, TP=Run$Human_Cysticercosis_prev)
      Run <- cbind(Run, Human_cysticercosis_apparent_prev)
      colnames(Run)[colnames(Run)=="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
    }
    
    if(!is.null(Params$T_sens)){
      Human_Taeniasis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$T_sens, spec=Params$T_spec, TP=Run$Human_Taeniasis_prev)
      Run <- cbind(Run, Human_Taeniasis_apparent_prev)
      colnames(Run)[colnames(Run)=="apparent_prev"] <- "Human_Taeniasis_apparent_prev"
    }
    
    return(Run)
    
  }
  
  ##==============================================================================================##
  ##       Standard interventions (Same intervention applied over model run) i.e. same dataframe  ##
  ##==============================================================================================##
  
  # If interventions DO NOT change over time (in model run), proceed
  if(is.null(Intervention_stage1) || is.null(Intervention_time_stage1) || is.null(Intervention_frequency_stage1) || is.null(Num_intervention_rounds_stage1))  {
    
    # User specified coverage values for age-structured model 
    Int_effect_size_list <- Intervention_effect_size_set_up(pig_MDA_cov = pig_MDA_cov, pig_vaccine_ds1_cov = pig_vaccine_ds1_cov,
                                                            pig_vaccine_ds2_cov = pig_vaccine_ds2_cov, human_testtreat_cov = 
                                                              human_testtreat_cov, human_MDAnic_cov = human_MDAnic_cov,
                                                            human_MDApzq_cov= human_MDApzq_cov, Pig_MDA_prop_noimmunity=Pig_MDA_prop_noimmunity)
    # check on inputs
    Check_interventions(Intervention)
    Check_effect(Intervention_effect = Int_effect_size_list)
    stopifnot(is.numeric(Time), is.numeric(Intervention_time), is.numeric(step),
              length(Time)==1, length(Intervention_time)==1, length(step)==1,
              Time>0, Intervention_time<=Time, Intervention_time>0, is.list(Params),
              is.list(Initial_states), is.numeric(Burn_in), Burn_in>=0, is.numeric(Intervention_frequency),
              length(Intervention_frequency)==1, Intervention_frequency>0)
    
    # additional checks on new intervention arguements (TO DO: incorporate into above?)
    if(is.numeric(age_target_pig_MDA)){
      stopifnot(is.numeric(age_target_pig_MDA))
    }
    
    if(is.numeric(age_target_pig_vaccine)){
      stopifnot(is.numeric(age_target_pig_vaccine))
    }
    
    if(is.numeric(age_target_pig_MDA) && !is.numeric(age_target_pig_vaccine)){
      stopifnot(is.numeric(age_target_pig_MDA))
    }
    
    if(is.numeric(age_target_human_MDA)){
      stopifnot(is.numeric(age_target_human_MDA))
    }
    
    if(is.numeric(age_target_human_test_and_treat)){
      stopifnot(is.numeric(age_target_human_test_and_treat))
    }
    
    # Set time vectors for pre- intervention
    tt1<-seq(0, (Intervention_time*12)-step, step)
    
    # Set yearly times for interention period (post first intervention round)
    splits<-seq((Intervention_time*12), Time*12, Intervention_frequency) # previously frequency set to 12 i.e 1 year
    tt2<-list()
    
    # Specify vector for interventions in absence of number of int round argument
    if(is.null(Num_intervention_rounds)){
      
      if(length(splits)>1){
        for(i in 1:(length(splits)-1)){
          tt2[[i]]<-seq(splits[i]+step, splits[i+1], step)
        }
      }
      
      if(length(splits)==1){
        for(i in 1:(length(splits))){
          tt2[[i]]<-seq(splits[i]+step, Time*12, step)
        }
      }
    }
    
    
    # Specify vector for number of intervention rounds (if number of intervention round argument used)
    if(!is.null(Num_intervention_rounds)){
      
      Num_intervention_rounds_split <- Num_intervention_rounds+1
      
      splits1 <- splits[1:Num_intervention_rounds_split]
      
      
      if(length(splits)>=1){
        for(i in 1:(length(splits1)-1)){
          tt2[[i]]<-seq(splits1[i]+step, splits1[i+1], step)
        }
      }
    }
    
    
    # Run the pre-intervention period
    BL<-Single_run(tt1, params=Params, states=Initial_states)
    
    Runs<-list()
    Runs[[1]]<-BL
    
    for(i in 1:length(tt2)){
      
      # Pull the 'end' state values from previous run
      Tail_states<- Inter_run_setup(model_output=Runs[[i]], na_pig=Params$na_pig, na_human=Params$na_human)
      
      
      # Alter states/params for single interventions
      if(i==1 && !'Pig_vaccine' %in% Intervention && !'Pig_MDA' %in% Intervention && !'Human_MDA_nic' %in% Intervention && ! 'Human_MDA_pzq' %in% Intervention && ! 'Human_test_and_treat' %in% Intervention){
        Params<-Intervention_event_param(Params=Params, Intervention, Intervention_effect = Int_effect_size_list)
        States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect)
      }
      
      
      #=========================================================================================#
      # IF statements for human interventions (either before or in absence of pig interventions #
      if('Human_MDA_nic' %in% Intervention || 'Human_MDA_pzq' %in% Intervention || 'Human_test_and_treat' %in% Intervention){
        
        # first param changes (non-biomedical interventions)
        if(i==1){
          Params<-Intervention_event_param(Params=Params, Intervention, Intervention_effect = Int_effect_size_list)
        }
        
        # A) Non age-structured human interventions
        
        # 1) Options with only 1 human intervention selected
        if('Human_MDA_nic' %in% Intervention || 'Human_MDA_pzq' %in% Intervention || 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_MDA) && !is.numeric(age_target_human_test_and_treat)){
          
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && !'Human_MDA_pzq' %in% Intervention && !'Human_test_and_treat' %in% Intervention){
            States<-Intervention_event_state(States=Tail_states, Intervention= 'Human_MDA_nic', Intervention_effect = Int_effect_size_list)
          }
          
          if('Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && !'Human_MDA_nic' %in% Intervention && !'Human_test_and_treat' %in% Intervention){
            States<-Intervention_event_state(States=Tail_states, Intervention= 'Human_MDA_pzq', Intervention_effect = Int_effect_size_list)
          }
          
          if('Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_MDA) && !'Human_MDA_nic' %in% Intervention && !'Human_MDA_pzq' %in% Intervention){
            States<-Intervention_event_state(States=Tail_states, Intervention= 'Human_test_and_treat', Intervention_effect = Int_effect_size_list)
          }
          
          # 2) Options with 2 human intervention selected
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_pzq' %in% Intervention){
            States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect = Int_effect_size_list)
          }
          
          if('Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_nic' %in% Intervention){
            States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect = Int_effect_size_list)
          }
          
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && !'Human_test_and_treat' %in% Intervention){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          # 3) Options with 3 human intervention selected
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
        } 
        
        # B) Age-structured human interventions (user specified)
        if('Human_MDA_nic' %in% Intervention || 'Human_MDA_pzq' %in% Intervention || 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_MDA) || is.numeric(age_target_human_test_and_treat)){
          
          # 1) Options with only 1 human intervention selected
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && !'Human_MDA_pzq' %in% Intervention && !'Human_test_and_treat' %in% Intervention){
            
            # takes processed tail states and selects specific age groups to implement intervention
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states) 
            # apply intervention effect to specific selected age groups 
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_nic', Intervention_effect = Int_effect_size_list)
            # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states) 
          }
          
          if('Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && !'Human_MDA_nic' %in% Intervention && !'Human_test_and_treat' %in% Intervention){
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_pzq', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
          }
          
          if('Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat) && !'Human_MDA_nic' %in% Intervention && !'Human_MDA_pzq' %in% Intervention){
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
          }
          
          # 2) Options with 2 human intervention selected (all combination thereof)
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_pzq' %in% Intervention){
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_nic', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
          }
          
          
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_pzq' %in% Intervention){
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_nic', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            age_target_human_test_and_treat <- c(1:Params$na_human) # make age vector (all human ages)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
            
          }
          
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_pzq' %in% Intervention){
            
            age_target_human_MDA <- c(1:Params$na_human) # make age vector (all human ages)
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_nic', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
            
          }
          
          if('Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_nic' %in% Intervention){
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_pzq', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
          }
          
          if('Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_nic' %in% Intervention){
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_pzq', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            age_target_human_test_and_treat <- c(1:Params$na_human) # make age vector (all human ages)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
            
          }
          
          if('Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat) && ! 'Human_MDA_pzq' %in% Intervention){
            
            age_target_human_MDA <- c(1:Params$na_human) # make age vector (all human ages)
            
            p <- Pre_human_MDA(age_target = age_target_human_MDA, Tail_states = Tail_states)
            
            states_move_age_human_MDA <-Intervention_event_state(States=p, Intervention='Human_MDA_pzq', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_MDA, tail_states = Tail_states)
            
            p <- Pre_human_test_and_treat(age_target = age_target_human_test_and_treat, Tail_states = Tail_states)
            
            states_move_age_human_test_and_treat <-Intervention_event_state(States=p, Intervention='Human_test_and_treat', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_human_test_and_treat, tail_states = Tail_states)
            
          }
          
          # Throw error messages with specific combinations (non-appropriate)
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && !'Human_test_and_treat' %in% Intervention){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && !'Human_test_and_treat' %in% Intervention){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && !'Human_test_and_treat' %in% Intervention){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          
          # 3) Non-feasible options with 3 human intervention selected (as human MDA with both PZQ and NIC not appropriate) - throw error message
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          if('Human_MDA_nic' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && !is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
          if('Human_MDA_nic' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_MDA_pzq' %in% Intervention && !is.numeric(age_target_human_MDA) && 'Human_test_and_treat' %in% Intervention && is.numeric(age_target_human_test_and_treat)){
            stop('MDA with PZQ and NICLOSAMIDE NOT POSSIBLE')
          }
        }
        
        
      } 
      
      #========================================================================================#
      # IF statements for pig interventions (either after or in absence of human interventions #
      
      # IF statements for pig interventions (including age-structured interventions)
      if('Pig_MDA' %in% Intervention || 'Pig_vaccine' %in% Intervention){
        
        if(i==1){
          Params<-Intervention_event_param(Params=Params, Intervention, Intervention_effect = Int_effect_size_list)
        }
        
        
        
        # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN 
        if('Human_MDA_nic' %in% Intervention || 'Human_MDA_pzq' %in% Intervention || 'Human_test_and_treat' %in% Intervention){
          Tail_states <- States # set-up 
        }
        
        # A) Non age-structured pig interventions combinations
        
        # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% Intervention && !is.numeric(age_target_pig_vaccine)){
          
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = Params$na_pig, Intervention_frequency = Intervention_frequency)
        } 
        
        
        if('Pig_MDA' %in% Intervention && !is.numeric(age_target_pig_MDA) && !'Pig_vaccine' %in% Intervention){
          States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect = Int_effect_size_list)
        }
        
        if('Pig_vaccine' %in% Intervention && !is.numeric(age_target_pig_vaccine) && !'Pig_MDA' %in% Intervention){
          States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect = Int_effect_size_list)
        }
        
        
        if('Pig_MDA' %in% Intervention && !is.numeric(age_target_pig_MDA) && 'Pig_vaccine' %in% Intervention &&!is.numeric(age_target_pig_vaccine)){
          States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect = Int_effect_size_list)
        }
        
        
        # B) Age-structured pig interventions (user specified) combinations
        if('Pig_MDA' %in% Intervention && is.numeric(age_target_pig_MDA)){
          
          # takes processed tail states and selects specific age groups to implement intervention
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA, Tail_states = Tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          if('Pig_MDA' %in% Intervention && is.numeric(age_target_pig_MDA) && is.numeric(age_target_pig_vaccine)){
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
          
          if('Pig_MDA' %in% Intervention && is.numeric(age_target_pig_MDA) && 'Pig_vaccine' %in% Intervention && !is.numeric(age_target_pig_vaccine)){
            
            age_target_pig_vaccine <- c(1:Params$na_pig)
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
        }
        
        if('Pig_MDA' %in% Intervention && !is.numeric(age_target_pig_MDA) && is.numeric(age_target_pig_vaccine)){
          
          age_target_pig_MDA <- c(1:Params$na_pig)
          
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA, Tail_states = Tail_states)
          
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine, Tail_states = States)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          
        }
        
        if('Pig_vaccine' %in% Intervention && is.numeric(age_target_pig_vaccine) && !'Pig_MDA' %in% Intervention){
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine, Tail_states = Tail_states)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = Tail_states)
        }
        
      }
      
      
      # Do the next run
      Runs[[i+1]]<-Single_run(tt2[[i]], Params, states=States)
    }
    
    # create model run (data frame) output
    Runs<-do.call('rbind', Runs)
    Runs<-as.data.frame(Runs)
    
    # If no number of intervention rounds specified 
    if((is.null(Num_intervention_rounds))){
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(Params$PC_sens)){
        pig_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$PC_sens, spec=Params$PC_spec, TP=Runs$Pig_Cysticercosis_prev)
        Runs <- cbind(Runs, pig_cysticercosis_apparent_prev)
        colnames(Runs)[colnames(Runs)=="apparent_prev"] <- "pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$C_sens)){
        Human_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$C_sens, spec=Params$C_spec, TP=Runs$Human_Cysticercosis_prev)
        Runs <- cbind(Runs, Human_cysticercosis_apparent_prev)
        colnames(Runs)[colnames(Runs)=="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$T_sens)){
        Human_Taeniasis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$T_sens, spec=Params$T_spec, TP=Runs$Human_Taeniasis_prev)
        Runs <- cbind(Runs, Human_Taeniasis_apparent_prev)
        colnames(Runs)[colnames(Runs)=="apparent_prev"] <- "Human_Taeniasis_apparent_prev"
      }
      
      return(Runs)
    }
    
    # If number of interventions specified: compute rest of model run from end of last intervention 
    if((Num_intervention_rounds >= 1)){
      
      Num_intervention_rounds_split <- Num_intervention_rounds+1
      
      splits1 <- splits[1:Num_intervention_rounds_split]
      
      last_value <- tail(splits1, n=1) 
      
      Initial_states_post_intervention<- Inter_run_setup(model_output=Runs, na_pig=Params$na_pig, na_human=Params$na_human)
      Run_post_last_round<-Single_run(seq(last_value, Time*12, step), Params, Initial_states_post_intervention)
      Run_post_last_round<-as.data.frame(Run_post_last_round)
      Runs_final <- rbind(Runs, Run_post_last_round)
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(Params$PC_sens)){
        pig_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$PC_sens, spec=Params$PC_spec, TP=Runs_final$Pig_Cysticercosis_prev)
        Runs_final <- cbind(Runs_final, pig_cysticercosis_apparent_prev)
        colnames(Runs_final)[colnames(Runs_final)=="apparent_prev"] <- "pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$C_sens)){
        Human_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$C_sens, spec=Params$C_spec, TP=Runs_final$Human_Cysticercosis_prev)
        Runs_final <- cbind(Runs_final, Human_cysticercosis_apparent_prev)
        colnames(Runs_final)[colnames(Runs_final)=="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$T_sens)){
        Human_Taeniasis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$T_sens, spec=Params$T_spec, TP=Runs_final$Human_Taeniasis_prev)
        Runs_final <- cbind(Runs_final, Human_Taeniasis_apparent_prev)
        colnames(Runs_final)[colnames(Runs_final)=="apparent_prev"] <- "Human_Taeniasis_apparent_prev"
      }
      
      return(Runs_final)
    }
    
  }
  #=======================================================================================================#
  #       Multi-stage interventions (Diff intervention applied over model run) i.e. different dataframes  #
  #=======================================================================================================#
  # Note only pig intervention can currently be structured with two different stages
  # TO DO: Repeat for human interventions (need for human MDA to SAC then human MDA to all)
  
  
  #======================================================================================#
  #                             Prepare pre-STAGE 1 and STAGE 1intervention period       #
  
  # If interventions DO change over time (in model run), proceed
  if(!is.null(Intervention_stage1) || !is.null(Intervention_time_stage1) || !is.null(Intervention_frequency_stage1) || !is.null(Num_intervention_rounds_stage1))  {
    
    # User specified coverage values for age-structured model
    Int_effect_size_list <- Intervention_effect_size_set_up(pig_MDA_cov = pig_MDA_cov_stage1, pig_vaccine_ds1_cov = pig_vaccine_ds1_cov_stage1,
                                                            pig_vaccine_ds2_cov = pig_vaccine_ds2_cov_stage1, Pig_MDA_prop_noimmunity=Pig_MDA_prop_noimmunity,
                                                            human_testtreat_cov = human_testtreat_cov, human_MDAnic_cov = human_MDAnic_cov,
                                                            human_MDApzq_cov= human_MDApzq_cov)
    
    # check on inputs
    Check_interventions(Intervention_stage1)
    Check_effect(Intervention_effect = Int_effect_size_list)
    stopifnot(is.numeric(Time), is.numeric(Intervention_time_stage1), is.numeric(step),
              length(Time)==1, length(Intervention_time_stage1)==1, length(step)==1,
              Time>0, Intervention_time_stage1<=Time, Intervention_time_stage1>0, is.list(Params),
              is.list(Initial_states), is.numeric(Burn_in), Burn_in>=0, is.numeric(Intervention_frequency_stage1),
              length(Intervention_frequency_stage1)==1, Intervention_frequency_stage1>0)
    
    # additional checks on new intervention arguements (TO DO: incorporate into above?)
    if(is.numeric(age_target_pig_MDA_stage1)){
      stopifnot(is.numeric(age_target_pig_MDA_stage1))
    }
    
    if(is.numeric(age_target_pig_vaccine_stage1)){
      stopifnot(is.numeric(age_target_pig_vaccine_stage1))
    }
    
    if(is.numeric(age_target_pig_MDA_stage1) && !is.numeric(age_target_pig_vaccine_stage1)){
      stopifnot(is.numeric(age_target_pig_MDA_stage1))
    }
    
    if(is.numeric(age_target_human_MDA)){
      stopifnot(is.numeric(age_target_human_MDA))
    }
    
    if(is.numeric(age_target_human_test_and_treat)){
      stopifnot(is.numeric(age_target_human_test_and_treat))
    }
    
    # Set time vectors for pre- intervention
    tt1<-seq(0, (Intervention_time_stage1*12)-step, step)
    
    # Set yearly times for intervention period (post first intervention round)
    splits<-seq((Intervention_time_stage1*12), Time*12, Intervention_frequency_stage1) # previously frequency set to 12 i.e 1 year
    tt2<-list()
    
    
    # Setting up vectors for different stages (interventions changing) of model run
    
    # Specify vector for interventions (STAGE 1) using number of intervention round argument
    if(!is.null(Num_intervention_rounds_stage1)){
      
      Num_intervention_rounds_stage1_split <- Num_intervention_rounds_stage1+1
      
      splits_stage1 <- splits[1:Num_intervention_rounds_stage1_split]
      
      
      if(length(splits)>=1){
        for(i in 1:(length(splits_stage1)-1)){
          tt2[[i]]<-seq(splits_stage1[i]+step, splits_stage1[i+1], step)
        }
      }
    }
    
    
    # Run the pre-stage 1 period 
    BL<-Single_run(tt1, params=Params, states=Initial_states)
    
    # Prepare stage 1 intervention period data structure
    Runs_stage1<-list()
    Runs_stage1[[1]]<-BL
    
    #======================================================================================#
    #                             Implement STAGE 1 interventions                          #
    
    for(i in 1:length(tt2)){
      
      # Pull the 'end' state values from previous run
      Tail_states<- Inter_run_setup(model_output=Runs_stage1[[i]], na_pig=Params$na_pig, na_human=Params$na_human)
      
      
      # Alter states/params for single interventions
      if(i==1 && !'Pig_vaccine' %in% Intervention_stage1 && !'Pig_MDA' %in% Intervention_stage1 && !'Human_MDA_nic' %in% Intervention_stage1 && ! 'Human_MDA_pzq' %in% Intervention_stage1 && ! 'Human_test_and_treat' %in% Intervention_stage1){
        Params<-Intervention_event_param(Params=Params, Intervention = Intervention_stage1, Intervention_effect = Int_effect_size_list)
        States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage1, Intervention_effect)
      }
      
      
      #=========================================================#
      # IF statements for pig interventions (non age-structured)#
      if('Pig_MDA' %in% Intervention_stage1 || 'Pig_vaccine' %in% Intervention_stage1){
        
        # first param changes (non-biomedical interventions)
        if(i==1){
          Params<-Intervention_event_param(Params=Params, Intervention = Intervention_stage1, Intervention_effect = Int_effect_size_list)
        }
        
        
        
        # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN (?)
        if('Human_MDA_nic' %in% Intervention_stage1 || 'Human_MDA_pzq' %in% Intervention_stage1 || 'Human_test_and_treat' %in% Intervention_stage1){
          Tail_states <- States # set-up 
        }
        
        
        # A) Non age-structured pig intervention combinations
        
        # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% Intervention_stage1 && !is.numeric(age_target_pig_vaccine_stage1)){
          
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = Params$na_pig, Intervention_frequency = Intervention_frequency_stage1)
        } 
        
        
        if('Pig_MDA' %in% Intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && !'Pig_vaccine' %in% Intervention_stage1){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage1, Intervention_effect = Int_effect_size_list)
        }
        
        if('Pig_vaccine' %in% Intervention_stage1 && !is.numeric(age_target_pig_vaccine_stage1) && !'Pig_MDA' %in% Intervention_stage1){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage1, Intervention_effect = Int_effect_size_list)
        }
        
        
        if('Pig_MDA' %in% Intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && 'Pig_vaccine' %in% Intervention_stage1 &&!is.numeric(age_target_pig_vaccine_stage1)){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage1, Intervention_effect = Int_effect_size_list)
        }
        
        
        # B) Age-structured pig interventions (user specified) combinations
        
        if('Pig_MDA' %in% Intervention_stage1 && is.numeric(age_target_pig_MDA_stage1)){
          
          # takes processed tail states and selects specific age groups to implement intervention
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA_stage1, Tail_states = Tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          if('Pig_MDA' %in% Intervention_stage1 && is.numeric(age_target_pig_MDA_stage1) && is.numeric(age_target_pig_vaccine_stage1)){
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
          
          if('Pig_MDA' %in% Intervention_stage1 && is.numeric(age_target_pig_MDA_stage1) && 'Pig_vaccine' %in% Intervention_stage1&& !is.numeric(age_target_pig_vaccine_stage1)){
            
            age_target_pig_vaccine <- c(1:Params$na_pig)
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
        }
        
        if('Pig_MDA' %in% Intervention_stage1 && !is.numeric(age_target_pig_MDA_stage1) && is.numeric(age_target_pig_vaccine_stage1)){
          
          age_target_pig_MDA_stage1 <- c(1:Params$na_pig)
          
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA_stage1, Tail_states = Tail_states)
          
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, Tail_states = States)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          
        }
        
        if('Pig_vaccine' %in% Intervention_stage1 && is.numeric(age_target_pig_vaccine_stage1) && !'Pig_MDA' %in% Intervention_stage1){
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage1, Tail_states = Tail_states)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = Tail_states)
        }
        
      }
      
      
      # Do the next run
      Runs_stage1[[i+1]]<-Single_run(tt2[[i]], Params, states=States)
    }
    
    Runs_stage1<-do.call('rbind', Runs_stage1)
    Runs_stage1<-as.data.frame(Runs_stage1)
    
    #======================================================================================#
    #                           Prepare STAGE 2 intervention period                        #
    
    # User specified coverage values for age-structured model 
    Int_effect_size_list <- Intervention_effect_size_set_up(pig_MDA_cov = pig_MDA_cov_stage2, pig_vaccine_ds1_cov = pig_vaccine_ds1_cov_stage2,
                                                            pig_vaccine_ds2_cov = pig_vaccine_ds2_cov_stage2, Pig_MDA_prop_noimmunity=Pig_MDA_prop_noimmunity, 
                                                            human_testtreat_cov = human_testtreat_cov, human_MDAnic_cov = human_MDAnic_cov,
                                                            human_MDApzq_cov= human_MDApzq_cov)
    
    # check on inputs
    Check_interventions(Intervention_stage2)
    Check_effect(Intervention_effect = Int_effect_size_list)
    stopifnot(is.numeric(Time), is.numeric(Intervention_time_stage1), is.numeric(step),
              length(Time)==1, length(Intervention_time_stage1)==1, length(step)==1,
              Time>0, Intervention_time_stage1<=Time, Intervention_time_stage1>0, is.list(Params),
              is.list(Initial_states), is.numeric(Burn_in), Burn_in>=0, is.numeric(Intervention_frequency_stage1),
              length(Intervention_frequency_stage1)==1, Intervention_frequency_stage1>0)
    
    # additional checks on new intervention arguements (TO DO: incorporate into above?)
    if(is.numeric(age_target_pig_MDA_stage2)){
      stopifnot(is.numeric(age_target_pig_MDA_stage2))
    }
    
    if(is.numeric(age_target_pig_vaccine_stage2)){
      stopifnot(is.numeric(age_target_pig_vaccine_stage2))
    }
    
    if(is.numeric(age_target_pig_MDA_stage2) && !is.numeric(age_target_pig_vaccine_stage2)){
      stopifnot(is.numeric(age_target_pig_MDA_stage1))
    }
    
    if(is.numeric(age_target_human_MDA)){
      stopifnot(is.numeric(age_target_human_MDA))
    }
    
    if(is.numeric(age_target_human_test_and_treat)){
      stopifnot(is.numeric(age_target_human_test_and_treat))
    }
    
    if(is.null(Intervention_time_stage2) || !is.null(Intervention_time_stage2)){
      end_int_stage1 <- (Intervention_time_stage1*12) + (Intervention_frequency_stage1 * Num_intervention_rounds_stage1) 
      Intervention_time_stage2 <- end_int_stage1/12
    }
    
    # Set yearly times for intervention period (stage 2)
    splits2<-seq((Intervention_time_stage2*12), Time*12, Intervention_frequency_stage2) # previously frequency set to 12 i.e 1 year
    tt3<-list()
    
    # Specify vector for interventions in absence of number of int round argument (stage 2)
    if(is.null(Num_intervention_rounds_stage2)){
      
      if(length(splits2)>1){
        for(i in 1:(length(splits2)-1)){
          tt3[[i]]<-seq(splits2[i]+step, splits2[i+1], step)
        }
      }
      
      if(length(splits2)==1){
        for(i in 1:(length(splits2))){
          tt3[[i]]<-seq(splits2[i]+step, Time*12, step)
        }
      }
    }
    
    # Specify vector for number of intervention rounds (stage 2)
    if(!is.null(Num_intervention_rounds_stage2)){
      
      Num_intervention_rounds_stage2_split <- Num_intervention_rounds_stage2+1
      
      splits_stage2 <- splits2[1:Num_intervention_rounds_stage2_split]
      
      
      if(length(splits2)>=1){
        for(i in 1:(length(splits_stage2)-1)){
          tt3[[i]]<-seq(splits_stage2[i]+step, splits_stage2[i+1], step)
        }
      }
    }
    
    
    # Prepare stage 2 intervention period data structure
    Runs_stage2<-list()
    Runs_stage2[[1]]<-Runs_stage1
    
    #======================================================================================#
    #                             Implement STAGE 2 interventions                          #
    
    for(i in 1:length(tt3)){
      
      # Pull the 'end' state values from previous run
      Tail_states<- Inter_run_setup(model_output=Runs_stage2[[i]], na_pig=Params$na_pig, na_human=Params$na_human)
      
      
      # Alter states/params for single interventions
      if(i==1 && !'Pig_vaccine' %in% Intervention_stage2 && !'Pig_MDA' %in% Intervention_stage2 && !'Human_MDA_nic' %in% Intervention_stage2 && ! 'Human_MDA_pzq' %in% Intervention_stage2 && ! 'Human_test_and_treat' %in% Intervention_stage2){
        Params<-Intervention_event_param(Params=Params, Intervention = Intervention_stage2, Intervention_effect = Int_effect_size_list)
        States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage2, Intervention_effect)
      }
      
      
      # IF statements for pig interventions 
      if('Pig_MDA' %in% Intervention_stage2 || 'Pig_vaccine' %in% Intervention_stage2){
        
        # first param changes (non-biomedical interventions)
        if(i==1){
          Params<-Intervention_event_param(Params=Params, Intervention = Intervention_stage2, Intervention_effect = Int_effect_size_list)
        }
        
        
        
        # IF statements for pig interventions WITH HUMAN INTERVENTIONS ALREADY RUN (?)
        if('Human_MDA_nic' %in% Intervention_stage2 || 'Human_MDA_pzq' %in% Intervention_stage2 || 'Human_test_and_treat' %in% Intervention_stage2){
          Tail_states <- States # set-up 
        }
        
        # A) Non age-structured pig interventions combinations
        
        # Define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
        if('Pig_vaccine' %in% Intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2)){
          
          age_target_pig_vaccine <- age_struc_pig_vacc_func(oldest_age = Params$na_pig, Intervention_frequency = Intervention_frequency_stage2)
        } 
        
        if('Pig_MDA' %in% Intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && !'Pig_vaccine' %in% Intervention_stage2){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage2, Intervention_effect = Int_effect_size_list)
        }
        
        if('Pig_vaccine' %in% Intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2) && !'Pig_MDA' %in% Intervention_stage2){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage2, Intervention_effect = Int_effect_size_list)
        }
        
        
        if('Pig_MDA' %in% Intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && 'Pig_vaccine' %in% Intervention_stage2 &&!is.numeric(age_target_pig_vaccine_stage2)){
          States<-Intervention_event_state(States=Tail_states, Intervention = Intervention_stage2, Intervention_effect = Int_effect_size_list)
        }
        
        
        # B) Age-structured pig interventions (user specified) combinations
        
        if('Pig_MDA' %in% Intervention_stage2 && is.numeric(age_target_pig_MDA_stage2)){
          
          # takes processed tail states and selects specific age groups to implement intervention
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA_stage2, Tail_states = Tail_states)
          # apply intervention effect to specific selected age groups 
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          # identifies age targeted states and updates these specific states in the overall tail states (from the initial model run)
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          if('Pig_MDA' %in% Intervention_stage2 && is.numeric(age_target_pig_MDA_stage2) && is.numeric(age_target_pig_vaccine_stage2)){
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
          
          if('Pig_MDA' %in% Intervention_stage2 && is.numeric(age_target_pig_MDA_stage2) && 'Pig_vaccine' %in% Intervention_stage2 && !is.numeric(age_target_pig_vaccine_stage2)){
            
            age_target_pig_vaccine_stage2 <- c(1:Params$na_pig)
            
            p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, Tail_states = States)
            
            states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
            
            States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          }
        }
        
        if('Pig_MDA' %in% Intervention_stage2 && !is.numeric(age_target_pig_MDA_stage2) && is.numeric(age_target_pig_vaccine_stage2)){
          
          age_target_pig_MDA_stage2 <- c(1:Params$na_pig)
          
          p <- Pre_pig_MDA(age_target = age_target_pig_MDA_stage2, Tail_states = Tail_states)
          
          states_move_age_pig_MDA <-Intervention_event_state(States=p, Intervention='Pig_MDA', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_MDA, tail_states = Tail_states)
          
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, Tail_states = States)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = States)
          
        }
        
        if('Pig_vaccine' %in% Intervention_stage2 && is.numeric(age_target_pig_vaccine_stage2) && !'Pig_MDA' %in% Intervention_stage2){
          p <- Pre_pig_vaccine(age_target = age_target_pig_vaccine_stage2, Tail_states = Tail_states)
          
          states_move_age_pig_vaccine <-Intervention_event_state(States=p, Intervention='Pig_vaccine', Intervention_effect = Int_effect_size_list)
          
          States <- Update_states(states_move = states_move_age_pig_vaccine, tail_states = Tail_states)
        }
        
      }
      
      
      # Do the next run
      Runs_stage2[[i+1]]<-Single_run(tt3[[i]], Params, states=States)
    }
    
    # create model run (data frame) output
    Runs_stage2<-do.call('rbind', Runs_stage2)
    Runs_stage2<-as.data.frame(Runs_stage2)
    
    # If no number of intervention rounds specified
    if((is.null(Num_intervention_rounds_stage2))){
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(Params$PC_sens)){
        pig_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$PC_sens, spec=Params$PC_spec, TP=Runs_stage2$Pig_Cysticercosis_prev)
        Runs_stage2 <- cbind(Runs_stage2, pig_cysticercosis_apparent_prev)
        colnames(Runs_stage2)[colnames(Runs_stage2)=="apparent_prev"] <- "pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$C_sens)){
        Human_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$C_sens, spec=Params$C_spec, TP=Runs_stage2$Human_Cysticercosis_prev)
        Runs_stage2 <- cbind(Runs_stage2, Human_cysticercosis_apparent_prev)
        colnames(Runs_stage2)[colnames(Runs_stage2)=="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$T_sens)){
        Human_Taeniasis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$T_sens, spec=Params$T_spec, TP=Runs_stage2$Human_Taeniasis_prev)
        Runs_stage2 <- cbind(Runs_stage2, Human_Taeniasis_apparent_prev)
        colnames(Runs_stage2)[colnames(Runs_stage2)=="apparent_prev"] <- "Human_Taeniasis_apparent_prev"
      }
      
      return(Runs_stage2)
    }
    
    
    # If number of interventions specified for STAGE 2 interventions 
    if((Num_intervention_rounds_stage2 >= 1)){
      
      Num_intervention_rounds_stage2_split <- Num_intervention_rounds_stage2+1
      
      splits_end <- splits2[1:Num_intervention_rounds_stage2_split]
      
      last_value <- tail(splits_end, n=1) 
      
      # prepare and run from end of stage 2 to the end of model run (i.e. no further interventions in final stage)
      Initial_states_post_intervention<- Inter_run_setup(model_output=Runs_stage2, na_pig=Params$na_pig, na_human=Params$na_human)
      Run_post_last_round<-Single_run(seq(last_value, Time*12, step), Params, Initial_states_post_intervention)
      Run_post_last_round<-as.data.frame(Run_post_last_round)
      Runs_stage2_final <- rbind(Runs_stage2, Run_post_last_round) # final dataframe output
      
      # proceed to true prevalence to apparent prevalence adjustment (if specified)
      if(!is.null(Params$PC_sens)){
        pig_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$PC_sens, spec=Params$PC_spec, TP=Runs_stage2_final$Pig_Cysticercosis_prev)
        Runs_stage2_final <- cbind(Runs_stage2_final, pig_cysticercosis_apparent_prev)
        colnames(Runs_stage2_final)[colnames(Runs_stage2_final)=="apparent_prev"] <- "pig_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$C_sens)){
        Human_cysticercosis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$C_sens, spec=Params$C_spec, TP=Runs_stage2_final$Human_Cysticercosis_prev)
        Runs_stage2_final <- cbind(Runs_stage2_final, Human_cysticercosis_apparent_prev)
        colnames(Runs_stage2_final)[colnames(Runs_stage2_final)=="apparent_prev"] <- "Human_cysticercosis_apparent_prev"
      }
      
      if(!is.null(Params$T_sens)){
        Human_Taeniasis_apparent_prev <- Apparent_prevalence_packaging_func(sens=Params$T_sens, spec=Params$T_spec, TP=Runs_stage2_final$Human_Taeniasis_prev)
        Runs_stage2_final <- cbind(Runs_stage2_final, Human_Taeniasis_apparent_prev)
        colnames(Runs_stage2_final)[colnames(Runs_stage2_final)=="apparent_prev"] <- "Human_Taeniasis_apparent_prev"
      }
      
      return(Runs_stage2_final)
    }
    
  }
  
}



