
# This function will produce two lists, parameters and initial state variables
Set_up<-function(LEP=1, delta=960000, HPS=10000, PPS=2000, TPrev=0.02, CPrev=0.07, PTPrev=0.2, AEL=0.03846154,
                         ATL=2, ADI=50, LEH=54, phi=0.8, chi=0.5, RR=1, epsilon=0.01){

  # Pig mortality rate
  dP<-month_rate(LEP)
  # Egg mortality rate
  dE<-month_rate(AEL)
  # Intitial number of infected pigs
  IP0<-PPS*PTPrev
  # Intitial number of susceptible pigs
  SP0<-PPS-IP0
  # Initial egg number (at equilibrium)
  E0<-E0_equilibrium(delta, HPS, TPrev, CPrev, dE)
  # Egg to pig transmission parameter
  tau<-tau_equilibrium(dP, IP0, SP0, E0)
  # Human recovery rate from Taeniasis
  alpha<-month_rate(ATL)
  # Initial number of Human: T+ C-
  IH0<-HPS*TPrev*(1-CPrev)
  # Initial number of Human: T+ C+
  IHC0<-HPS*TPrev*CPrev
  # Human recovery rate from Cysticercosis
  eta<-month_rate(ADI)
  # Human mortality rate
  dH<-month_rate(LEH)
  # Initial number of Human: susceptible
  SH0<-HPS*(1-TPrev)*(1-CPrev)
  # Initial number of Human: T- C+
  SHC0<-HPS*(1-TPrev)*CPrev


  #### THis section has been reworked:
    # Literature review = pork consumption rate of 0.5 meals per month (1 meal every two months), therefore
    # rate of contacting pork (chi) = 0.5
    # rate of contacting low intensity infected pork (chil) = 0.5 * 0.8
    # rate of contacting high intensity infected pork (chih) = 0.5 * 0.2

  # Pork to human tranmission parameter
  beta<-Beta_equilibrium(alpha, IH0, IHC0, eta, dH, IP0, PPS, SH0, SHC0)

  pil<-pil_equilibrium(beta, chi, phi)
  pih<-2*pil
  # Low intensity infected pig -> human infection probability
  #chil<-chil_equilibrium(beta, pil, phi, pih)
  # High intensity infected pig -> human infection probability
  #chih<-2*chil

  # Number of infected pigs with low cyst burden at time 0
  IPL0<-(PPS-SP0) * phi
  # Number of infected pigs with high cyst burden at time 0
  IPH0<-(PPS-SP0) * (1-phi)
  # Recovered pigs at time 0
  RP0<-0
  # Vaccinated pigs at time 0
  VP0<-0

  # Birth of humans
  bH<-HPS*dH

  # Birth of pigs
  bP<-PPS*dP

  theta<-theta_equilibrium(bH, eta, SHC0, IHC0, dH, SH0, IH0, E0, RR)


  params<-list(tau=tau,
               #LEH=LEH,
               #LEP=LEP,
               PPS=PPS,
               HPS=HPS,
               bH=bH,
               bP=bP,
               PPS=PPS,
               dH=dH,
               dP=dP,
               #AEL=AEL,
               dE=dE,
               delta=delta,
               #TPrev=TPrev,
               #CPrev=CPrev,
               #PTPrev=PTPrev,
               phi=phi,
               #ATL=ATL,
               #ADI=ADI,
               theta=theta,
               alpha=alpha,
               eta=eta,
               chi=chi,
               pil=pil,
               pih=pih,
               epsilon=epsilon,
               RR=RR
  )

  states<-list(E0=E0,
               #IP0=IP0,
               SP0=SP0,
               SHC0=SHC0,
               IHC0=IHC0,
               SH0=SH0,
               IH0=IH0,
               IPL0=IPL0,
               IPH0=IPH0,
               RP0=RP0,
               VP0=VP0)

  return(list(params, states))
}


#' @title
#' Run Cysticercosis model ODE
#' @description
#' Runs a single implementation of the ODE Cysticercosis model
#'
#' @param tt vector of times
#' @param params list of parameters
#' @param states list of states
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
#' @param Time The numebr of years to run the model for (from equilibrium)
#' @param Intervention A vector of interventions to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param Intervention_effect A list of intervention effect sizes, see \code{Intervention_effect_size} for details
#' @param step Time step (months)
Run_model<-function(Params=NULL, Initial_states=NULL, Time, Intervention=NULL, Intervention_time=NULL, Intervention_effect=Intervention_effect_size(), step=1/30){

  # Calculate parmaters and initial state variables
  Initialise<-Set_up()
  if(is.null(Params)){
    Params<-Initialise[[1]]
  }
  if(is.null(Initial_states)){
    Initial_states<-Initialise[[2]]
  }


  if(is.null(Intervention)){
    Run<-Single_run(seq(0, Time*12, step), Params, Initial_states)
    Run<-as.data.frame(Run)
    return(Run)
  }

  # Checks
  Check_interventions(Intervention)
  Check_effect(Intervention_effect)
  #browser()
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



