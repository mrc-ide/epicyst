
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
#' @param Intervention_time Timing of intervention
#' @param step Time step (months)
#' @param Burn_in A burn in period run before model run (years)
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
Run_model<-function(Params=NULL, Initial_states=NULL, Time, Intervention=NULL, Intervention_time=Time/2, Intervention_effect=Intervention_effect_size(), step=1/30, Burn_in=0){
  
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
    Initial_states<-as.list(tail(Burn,1))
    names(Initial_states)<-paste(colnames(Burn), '0', sep='')
  }
  
  # Run with no interventions (if none specified)
  if(is.null(Intervention)){
    Run<-Single_run(seq(0, Time*12, step), Params, Initial_states)
    Run<-as.data.frame(Run)
    return(Run)
  }

  # Checks on inputs
  Check_interventions(Intervention)
  Check_effect(Intervention_effect)
  stopifnot(is.numeric(Time), is.numeric(Intervention_time), is.numeric(step),
            length(Time)==1, length(Intervention_time)==1, length(step)==1,
            Time>0, Intervention_time<=Time, Intervention_time>0, is.list(Params),
            is.list(Initial_states), is.numeric(Burn_in), Burn_in>=0)

  # Set time vectors for pre- and pos-intervention
  tt1<-seq(0, (Intervention_time*12)-step, step)
  # Set yearly times for interention period
  splits<-seq((Intervention_time*12), Time*12, 12)
  tt2<-list()
  for(i in 1:(length(splits)-1)){
    tt2[[i]]<-seq(splits[i]+step, splits[i+1], step)
  }

  # Run the pre-intervention period
  BL<-Single_run(tt1, params=Params, states=Initial_states)

  Runs<-list()
  Runs[[1]]<-BL

  for(i in 1:length(tt2)){
    # Pull the 'end' state values from previous run
    Tail_states<-as.list(tail(Runs[[i]],1))
    names(Tail_states)<-paste(colnames(Runs[[i]]), '0', sep='')
    Tail_states <- Tail_states[!names(Tail_states) %in% c("Humans_Taeniasis0", "Humans_Cysticercosis0", 
                                                          "Pigs_Cysticercosis0", "Human_Taeniasis_prev0",
                                                          "Human_Cysticercosis_prev0", "Pig_Cysticercosis_prev0",
                                                          "t0")]
    
    # Alter states/params for single interventions
    if(i==1){
      Params<-Intervention_event_param(Params=Params, Intervention, Intervention_effect)
    }

    # Alter states/params for repeat interventions
    States<-Intervention_event_state(States=Tail_states, Intervention, Intervention_effect)


    # Do the next run
    Runs[[i+1]]<-Single_run(tt2[[i]], Params, states=States)
  }

  Runs<-do.call('rbind', Runs)
  Runs<-as.data.frame(Runs)

  return(Runs)
}



