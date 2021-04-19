#' @title
#' Check intervention input
#' @description
#' Checks that the interventions input are correct
#'
#' @param Intevention Vector of one or more parameter interventions
Check_interventions<-function(intervention){
  param_interventions<-c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions<-c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  
  present<-Intervention %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if(!all(present)){
    stop(Intervention[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}

#' @title
#' Check intervention effect input
#' @description
#' Checks that the intervention effect inputs are correct
#'
#' @param Intevention Vector of one or more parameter interventions
Check_effect<-function(Intervention_effect){
  param_interventions<-c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions<-c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  
  present<-names(Intervention_effect) %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if(!all(present)){
    stop(names(Intervention_effect)[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}

#' @title
#' Change parameter
#' @description
#' Replaces a paramter with an aletred values after a parameter event
#'
#' @param Params List of model parameters
#' @param param_name The name of the parameter to be changed
#' @param effect_size The intervention effect size. Where the new parameter value = old value multiplied by the effect size.
Replace_param<-function(params, param_name, effect_size){
  if(!param_name %in% names(params)){
    stop('Parameter ', param_name, ' to change not found')
  }
  params[[which(names(params)==param_name)]]<-params[[which(names(params)==param_name)]]*effect_size
  
  return(params)
}

#' @title
#' Implement parameter intervention
#' @description
#' Implements one or more interventions that involve a parameter value being altered
#'
#' @param Params List of model parameters
#' @param Intevention Vector of one or more parameter interventions
#' @param Intervention_effect a list of intervention effect sizes
Intervention_event_param<-function(Params, Intervention, Intervention_effect){
  #Check_interventions(Intervention)
  #Check_effect(Intervention_effect)
  
  if('Husbandry' %in% Intervention){
    effect<-Intervention_effect[['Husbandry']]
    Params<-Replace_param(params=Params, param_name='tau', effect_size=effect)
  }
  
  if('Sanitation' %in% Intervention){
    effect<-Intervention_effect[['Sanitation']]
    Params<-Replace_param(params=Params, param_name='delta', effect_size=effect)
  }
  
  if('Inspection' %in% Intervention){
    effect<-Intervention_effect[['Inspection']]
    Params<-Replace_param(params=Params, param_name='pil', effect_size=effect[1])
    Params<-Replace_param(params=Params, param_name='pih', effect_size=effect[2])
  }
  
  return(Params)
  
}

#' @title
#' Move individuals between state variables
#' @description
#' Moves a propotion of individuals from one state compartemnt to another
#'
#' @param States List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to The name of the state variable that individuals will be moved in to
#' @param proportion The propotion of individuals in from that get moved into to
Move_state<-function(States, from, to, proportion){
  prior_state_from<-States[[from]]
  States[[from]]<-States[[from]]*(1-proportion)
  States[[to]]<-States[[to]] + prior_state_from*proportion
  
  return(States)
}

#' @title
#' Move individuals between state variables two flows
#' @description
#' Moves a propotions of individuals from one state compartemnt to two others
#'
#' @param States List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to The name of the state variable that individuals will be moved in to
#' @param proportion The propotion of individuals in from that get moved into to
Move_state_double<-function(States, from, to_1, to_2, proportion_1, proportion_2){
  prior_state_from <- States[[from]]
  States_reduce <- prior_state_from * (proportion_1) + prior_state_from * (proportion_2)
  States[[from]] <- States[[from]] - States_reduce
  States[[to_1]] <- States[[to_1]] + prior_state_from * proportion_1
  States[[to_2]] <- States[[to_2]] + prior_state_from * proportion_2
  
  return(States)
}

#Move individuals between state variables three flows
#' @description
#' Moves a propotions of individuals from one state compartemnt to three others
#'
#' @param States List of state variable
#' @param from The name of the state variabe individuals will be moved out of
#' @param to The name of the state variable that individuals will be moved in to
#' @param proportion The propotion of individuals in from that get moved into to

Move_state_triple<-function(States, from, to_1, to_2, to_3, proportion_1, proportion_2, proportion_3){
  prior_state_from <- States[[from]]
  States_reduce <- prior_state_from * (proportion_1) + prior_state_from * (proportion_2) + prior_state_from * (proportion_3)
  States[[from]] <- States[[from]] - States_reduce
  States[[to_1]] <- States[[to_1]] + prior_state_from * proportion_1
  States[[to_2]] <- States[[to_2]] + prior_state_from * proportion_2
  States[[to_3]] <- States[[to_3]] + prior_state_from * proportion_3
  
  return(States)
}

#' @title
#' Implement parameter intervention
#' @description
#' Implements one or more interventions that involve a parameter value being altered
#'
#' @param States List of model states
#' @param Intevention Vector of one or more parameter interventions
#' @param Intervention_effect a list of intervention effect sizes
Intervention_event_state<-function(States, Intervention, Intervention_effect){
  
  if('Pig_MDA' %in% Intervention){
    proportion<-Intervention_effect[['Pig_MDA']]
    States <- Move_state_double(States, from = 'IPL0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
    States <- Move_state_double(States, from = 'IPH0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
    States <- Move_state_double(States, from = 'PP0', to_1 = "SP0", to_2 = "RP0",
                                proportion_1 = proportion[1] * proportion[2],
                                proportion_2  = proportion[1] * (1 - proportion[2]))
  }
  
  if('Pig_vaccine' %in% Intervention){
    proportion<-Intervention_effect[['Pig_vaccine']]
    States<-Move_state(States, from='SP0', to='VP0', proportion=proportion)
  }
  
  if('Human_test_and_treat' %in% Intervention){
    proportion<-Intervention_effect[['Human_test_and_treat']]
    States<-Move_state(States, from='IH0', to='SH0', proportion=proportion)
  }
  
  if('Human_MDA_nic' %in% Intervention){
    proportion<-Intervention_effect[['Human_MDA_nic']]
    States<-Move_state(States, from='IH0', to='SH0', proportion=proportion)
    States<-Move_state(States, from='IHC0', to='SHC0', proportion=proportion)
  }
  
  if('Human_MDA_pzq' %in% Intervention){
    proportion<-Intervention_effect[['Human_MDA_pzq']]
    States<- Move_state_triple(States, from ='IHC0', to_1="SH0", to_2 ="SHC0", to_3="IH0",
                               proportion_1 = proportion[1],
                               proportion_2 = proportion[2],
                               proportion_3 = proportion[3])
    States<-Move_state(States, from='IH0', to='SH0', proportion=proportion[4])
    States<-Move_state(States, from='SHC0', to='SH0', proportion=proportion[5])
  }
  
  return(States)
  
  
}

#' @title
#' Pre-set intervention effects
#' @description
#' Provides preset intervention effect
#'
#' @return A list Intervention effects
#' @export
Intervention_effect_size<-function(){
  pars<-Set_up()[[1]]
  states<-Set_up()[[2]]
  list(
    Husbandry=0.8,
    Sanitation=0.8,
    Inspection=c(Proportion_low_burden_mean=0.8, Proportion_high_burden_meat=0.6),
    Pig_MDA=c(Proportion_sucess_treated=0.9*0.99,Proportion_no_immunity=0.1), # Succesfully treated = the assumed therapeutic coverage (0.9) × the anthelmintic efficacy (0.99).
    Pig_vaccine=0.9*1.0*(0.99), # Assumed coverage (0.9) dose 1 * assumed coverage round2 * vaccine efficacy (0.99) WITHOUT ADJUSTMENT
    Human_test_and_treat=0.9*0.97*0.98*0.99, # Proportion of people tested that are T+ and C- (Assumed therapeutic coverage (0.9) * Taeniasis sensitivity (0.97) * Cysticercosis specificity (0.98)) * drug efficacy (0.99)
    Human_MDA_nic=c(Proportion_sucess_treated=0.75*0.779), # Succesfully treated = the assumed therapeutic coverage (0.75) from literature × the anthelmintic efficacy with niclosamide - efficacy estimate from Bustos et al, 2012 (0.779).
    Human_MDA_pzq=c(Proportion_sucess_treated_pzq_taenneg_cystneg=0.75*0.7*0.8, Proportion_success_treated_pzq_taenneg_cystpos=0.75*0.7*(1-0.8),Proportion_success_treated_pzq_taenpos_cystneg=(0.75*(1-0.7)*0.8),
                    Proportion_success_treated_pzq_taenneg=0.75*0.7, Proportion_success_treated_pzq_cystneg=0.75*0.8) ## proportions 1-4 are treatment of those with cysticercosis +, taeniasis + 
    #to different states, proportion 5 is treatment of those with just taeniasis +, proportion 6 is treatment of those with just cysticercosis +
  )
  
  
}

#' @title
#' Coverage user defined intervention effect size
#' @description
#' enables user specified coverage to create list of proportions for state intervention moves
#'
#' @return A list Intervention effects
#' @export

Intervention_effect_size_set_up<-function(pig_MDA_cov, pig_vaccine_ds1_cov, pig_vaccine_ds2_cov,
                                          human_testtreat_cov, human_MDAnic_cov, human_MDApzq_cov, Pig_MDA_prop_noimmunity){
  
  ### set up default coverage values if non specified ##
  if(is.null(pig_MDA_cov)){
    pig_MDA_cov = 0.9
  }
  Pig_MDA_coverage <- pig_MDA_cov  
  
  if(is.null(pig_vaccine_ds1_cov)){
    pig_vaccine_ds1_cov = 0.9
  }
  Pig_vaccine_ds1_coverage<- pig_vaccine_ds1_cov 
  
  if(is.null(pig_vaccine_ds2_cov)){
    pig_vaccine_ds2_cov = 1.0
  }
  Pig_vaccine_ds2_coverage <- pig_vaccine_ds2_cov  
  
  if(is.null(human_testtreat_cov)){
    human_testtreat_cov = 0.9
  }
  Human_testandtreat_coverage <- human_testtreat_cov 
  
  if(is.null(human_MDAnic_cov)){
    human_MDAnic_cov = 0.75
  }
  Human_MDAnic_coverage<- human_MDAnic_cov  
  
  if(is.null(human_MDApzq_cov)){
    human_MDApzq_cov = 0.75
  }
  Human_MDApzq_coverage <- human_MDApzq_cov  
  
  if(is.null(Pig_MDA_prop_noimmunity)){
    Pig_MDA_prop_noimmunity = 0.1
  }
  
  Proportion_no_immunity <- Pig_MDA_prop_noimmunity
  
  # produce list for proportion moved (efficacy x coverage) #
  list <- list(
    Husbandry=0.8,
    Sanitation=0.8,
    Inspection=c(Proportion_low_burden_mean=0.8, Proportion_high_burden_meat=0.6),
    Pig_MDA=c(Proportion_sucess_treated=Pig_MDA_coverage*0.99,Proportion_no_immunity=Proportion_no_immunity), # Succesfully treated = the assumed therapeutic coverage (0.9) × the anthelmintic efficacy (0.99).
    Pig_vaccine=Pig_vaccine_ds1_coverage*Pig_vaccine_ds2_coverage*(0.99), # Assumed coverage (0.9) dose 1 * assumed coverage round2 * vaccine efficacy (0.99) WITHOUT ADJUSTMENT
    Human_test_and_treat=Human_testandtreat_coverage*0.97*0.98*0.99, # Proportion of people tested that are T+ and C- (Assumed therapeutic coverage (0.9) * Taeniasis sensitivity (0.97) * Cysticercosis specificity (0.98)) * drug efficacy (0.99)
    Human_MDA_nic=c(Proportion_sucess_treated=Human_MDAnic_coverage *0.779), # Succesfully treated = the assumed therapeutic coverage (0.75) from literature × the anthelmintic efficacy with niclosamide - efficacy estimate from Bustos et al, 2012 (0.779).
    Human_MDA_pzq=c(Proportion_sucess_treated_pzq_taenneg_cystneg=Human_MDApzq_coverage*0.7*0.8, Proportion_success_treated_pzq_taenneg_cystpos=Human_MDApzq_coverage*0.7*(1-0.8),Proportion_success_treated_pzq_taenpos_cystneg=(Human_MDApzq_coverage*(1-0.7)*0.8),
                    Proportion_success_treated_pzq_taenneg=Human_MDApzq_coverage*0.7, Proportion_success_treated_pzq_cystneg=Human_MDApzq_coverage*0.8) ## proportions 1-4 are treatment of those with cysticercosis +, taeniasis + 
    #to different states, proportion 5 is treatment of those with just taeniasis +, proportion 6 is treatment of those with just cysticercosis +
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
#' @param Intevention Vector of one or more parameter interventions
Check_interventions<-function(Intervention_stage1){
  param_interventions<-c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions<-c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  
  present<-Intervention_stage1 %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if(!all(present)){
    stop(Intervention_stage1[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}

#' @title
#' Check intervention input (stage 2 of multi-stage intervention)
#' @description
#' Checks that the interventions input are correct
#'
#' @param Intevention Vector of one or more parameter interventions
Check_interventions<-function(Intervention_stage2){
  param_interventions<-c('Husbandry', 'Sanitation', 'Inspection')
  state_interventions<-c('Pig_MDA', 'Pig_vaccine', 'Human_test_and_treat', 'Human_MDA_nic', 'Human_MDA_pzq')
  
  present<-Intervention_stage2 %in% c(param_interventions, state_interventions)
  # Check the intervcentions supplied are proper and correct
  if(!all(present)){
    stop(Intervention_stage2[!present], ' not recognised. Possible options are: ',
         paste(c(param_interventions, state_interventions), collapse=', '), '.')
  }
}


