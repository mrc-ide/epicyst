
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
#' @param Time The numebr of years to run the model for (from equilibrium). Default is at the halfway point.
#' @param Intervention A vector of interventions to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param Intervention_effect A list of intervention effect sizes, see \code{Intervention_effect_size} for details
#' @param step Time step (months)
#' @examples
#' M1<-Run_model(Time=500)
#' plot(M1$Humans_Cysticercosis~M1$t, t='l', ylim=c(0,1000), ylab='Humans with Cysticercosis', xlab='Time')
#' M2<-Run_model(Time=50, Intervention='Sanitation', Intervention_time=25)
#' head(M2)
#' lines(M2$Humans_Cysticercosis~M2$t, col='red')
#' M3<-Run_model(Time=50, Intervention=c('Human_test_and_treat', 'Pig_MDA'), Intervention_time=25)
#' lines(M3$Humans_Cysticercosis~M3$t, col='green')
#' legend('topright', c('Baseline','Sanitation','Human test & treat and Pig MDA'), lty=c(1,1,1), col=c('black','red', 'green'))
#' 
#' @export
Run_model<-function(Params=NULL, Initial_states=NULL, Time, Intervention=NULL, Intervention_time=Time/2, Intervention_effect=Intervention_effect_size(), step=1/30){

  # Calculate parmaters and initial state variables (if not provided)
  Initialise<-Set_up()
  if(is.null(Params)){
    Params<-Initialise[[1]]
  }
  if(is.null(Initial_states)){
    Initial_states<-Initialise[[2]]
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
            is.list(Initial_states))

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



