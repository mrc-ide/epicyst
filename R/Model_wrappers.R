tesgit<-function(){
  1+1
}

# This function will produce two lists, parameters and initial state variables
Set_up<-function(LEP=1, delta=960000, HPS=10000, PPS=2000, TPrev=0.02, CPrev=0.07, PTPrev=0.2, AEL=0.03846154,
                         pil_bl=0.5, pih_bl=1, ATL=2, ADI=50, LEH=54, phi=0.8){


  # Pig mortality rate
  dP<-1/(LEP*12)
  # Egg mortality rate
  dE<-1/(AEL*12)
  # Intitial number of infected pigs
  IP0<-PPS*PTPrev
  # Intitial number of susceptible pigs
  SP0<-PPS-IP0
  # Egg production rate
  delta<-delta_bl
  # Initial egg number (at equilibrium)
  E0<-((delta*HPS*TPrev*(1-CPrev))+(delta*HPS*TPrev*CPrev))/dE
  # Egg to pig transmission parameter
  tau<-(dP*IP0)/(SP0*E0)
  # Human recovery rate from Taeniasis
  alpha<-1/(ATL*12)
  # Initial number of Human: T+ C-
  IH0<-HPS*TPrev*(1-CPrev)
  # Initial number of Human: T+ C+
  IHC0<-HPS*TPrev*CPrev
  # Human recovery rate from Cysticercosis
  eta<-1/(ADI*12)
  # Human mortality rate
  dH<-1/(LEH*12)
  # Initial number of Human: susceptible
  SH0<-HPS*(1-TPrev)*(1-CPrev)
  # Initial number of Human: T- C+
  SHC0<-HPS*(1-TPrev)*CPrev
  # Low intensity infected pig -> human contact rate
  pil<-pil_bl
  # High intensity infected pig -> human contact rate
  pih<-pih_bl

  # Pork to human tranmission parameter
  Beta<-(alpha*(IH0+IHC0)+eta*IHC0+dH*(IH0+IHC0))/((IP0/PPS)*(SH0+SHC0))
  # Low intensity infected pig -> human infection probability
  chil<-Beta/(pil*phi+(2*pih*(1-phi)))
  # High intensity infected pig -> human infection probability
  chih<-2*chil

  params<-list(tau=tau,
               LEH=LEH,
               LEP=LEP,
               HPS=HPS,
               PPS=PPS,
               dH=dH,
               dP=dP,
               AEL=AEL,
               dE=dE,
               delta=delta,
               TPrev=TPrev,
               CPrev=CPrev,
               PTPrev=PTPrev,
               phi=phi,
               ATL=ATL,
               ADI=ADI,
               alpha=alpha,
               eta=eta,
               pil=pil,
               chil=chil,
               pih=pih,
               chih=chih
  )

  states<-list(E0=E0,
               IP0=IP0,
               SP0=SP0,
               SHC0=SHC0,
               IHC0=IHC0,
               SH0=SH0,
               IH0=IH0,
               IPL0=IPL0
               IPH0=IPH0
               RP0=RP0
               VP0=VP0)

  return(list(params, states))
}



#' @title
#' Run Cysticercosis model ODE
#' @description
#' Runs a single implementation of the ODE Cysticercosis model
#'
#' @param tt vector of times
#' @param LEP Average life expectancy of a pig (years)
#' @param delta_bl Egg production rate (per month)
#' @param HPS Human population size
#' @param PPS Pig population size
#' @param TPrev Taeniasis prevalence in human population
#' @param CPrev Cysticercosis prevalence in human population
#' @param PTPrev Cysticercosis prevalence in pig population
#' @param AEL Average egg survival (years)
#' @param pil_bl Low intensity infected pig -> human contact rate
#' @param pih_bl High intensity infected pig -> human contact rate
#' @param ATL Average lifespan of the adult tapeworm (years)
#' @param ADI Average duration of Cysticercosis infection in human (years)
#' @param LEH Average life expectancy of a human (years)
#' @param phi Proportion of infected pigs with low-intensity cyst burden
#' @param Husbandry_effect 1-proportion reduction to egg to pig transmission parameter (tau)
#' @param Sanitation_effect 1-proportion reduction to egg production rate (delta)
#' @param Inspection_effect_low 1-proportion reduction to low intensity infected pig -> human contact rate (pil)
#' @param Inspection_effect_high 1-proportion reduction to high intensity infected pig -> human contact rate (pih)
Single_run<-function(tt, LEP=1,delta_bl=960000,HPS=10000,PPS=2000,TPrev=0.02,CPrev=0.07,PTPrev=0.2,AEL=0.03846154,
                        pil_bl=0.5, pih_bl=1, ATL=2, ADI=50, LEH=54, phi=0.8, Husbandry_effect=1, Sanitation_effect=1,
                        Inspection_effect_low=1, Inspection_effect_high=1){

  # Pig mortality rate
  dP<-1/(LEP*12)
  # Egg mortality rate
  dE<-1/(AEL*12)
  # Intitial number of infected pigs
  IP0<-PPS*PTPrev
  # Intitial number of susceptible pigs
  SP0<-PPS-IP0
  # Egg production rate
  delta<-delta_bl
  # Initial egg number (at equilibrium)
  E0<-((delta*HPS*TPrev*(1-CPrev))+(delta*HPS*TPrev*CPrev))/dE
  # Egg to pig transmission parameter
  tau<-(dP*IP0)/(SP0*E0)
  # Human recovery rate from Taeniasis
  alpha<-1/(ATL*12)
  # Initial number of Human: T+ C-
  IH0<-HPS*TPrev*(1-CPrev)
  # Initial number of Human: T+ C+
  IHC0<-HPS*TPrev*CPrev
  # Human recovery rate from Cysticercosis
  eta<-1/(ADI*12)
  # Human mortality rate
  dH<-1/(LEH*12)
  # Initial number of Human: susceptible
  SH0<-HPS*(1-TPrev)*(1-CPrev)
  # Initial number of Human: T- C+
  SHC0<-HPS*(1-TPrev)*CPrev
  # Low intensity infected pig -> human contact rate
  pil<-pil_bl
  # High intensity infected pig -> human contact rate
  pih<-pih_bl

  # Pork to human tranmission parameter
  Beta<-(alpha*(IH0+IHC0)+eta*IHC0+dH*(IH0+IHC0))/((IP0/PPS)*(SH0+SHC0))
  # Low intensity infected pig -> human infection probability
  chil<-Beta/(pil*phi+(2*pih*(1-phi)))
  # High intensity infected pig -> human infection probability
  chih<-2*chil

  # Inteventions:
  tau<-tau*Husbandry_effect
  delta<-delta*Sanitation_effect
  pil<-pil*Inspection_effect_low
  pih<-pih*Inspection_effect_high

  pars<-list(E0=E0, IP0=IP0, SP0=SP0, tau=tau, SHC0=SHC0, IHC0=IHC0, SH0=SH0,
             IH0=IH0, LEH=LEH, LEP=LEP, HPS=HPS, PPS=PPS, dH=dH, dP=dP,
             AEL=AEL, dE=dE, delta=delta, TPrev=TPrev, CPrev=CPrev, PTPrev=PTPrev,
             phi=phi, ATL=ATL, ADI=ADI, alpha=alpha, eta=eta, pil=pil, chil=chil,
             pih=pih, chih=chih)

  #Mod<-cyst_generator(user=pars)
  Mod<-cyst_generator(E0=E0, IP0=IP0, SP0=SP0, tau=tau, SHC0=SHC0, IHC0=IHC0, SH0=SH0,
                      IH0=IH0, LEH=LEH, LEP=LEP, HPS=HPS, PPS=PPS, dH=dH, dP=dP,
                      AEL=AEL, dE=dE, delta=delta, TPrev=TPrev, CPrev=CPrev, PTPrev=PTPrev,
                      phi=phi, ATL=ATL, ADI=ADI, alpha=alpha, eta=eta, pil=pil, chil=chil,
                      pih=pih, chih=chih)

  y<-Mod$run(tt)

  return(y)
}


#' @title
#' Run Cysticercosis model with interventions
#' @description
#' Runs the ODE Cysticercosis model
#'
#' @param Time The numebr of years to run the model for (from equilibrium)
#' @param LEP Average life expectancy of a pig (years)
Run_model<-function(Time, Intervention, Intervention_time, Intervention_effect, step=1/30){
  tt1<-seq(0, Intervention_time*12, step)
  tt2<-seq(Intervention_time+step, Time*12, step)


  eq_state<-calc_equilibrium(params)  # This function will calculate state starting variables (at equilibirum) given params
  BL<-Single_run(tt1, params, State=eq_state) # Single run modified to take times, params and States

  # Alter params
  if(intervention=='Sanitation'){
    delta<-delta*0.8
  }

  Post<-SIngle_run(tt2, altered_params, State=End state of BL)


}



