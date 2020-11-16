#' @title
#' Monthly rate
#' @description
#' Converts an average duration (years) into a monthly rate
#'
#' @param dur Average duration (years)
#'
#' @return Monthly rate
month_rate<-function(dur){
  1/(dur*12)
}



#' @title
#' Set up
#' @description
#' Caculates internal parameters and equilbirum staring values for state variables
#'
#' @param LEP Pig life expectancy (years)
#' @param delta Egg production rate (per month)
#' @param HPS Human population size
#' @param PPS Pig population size
#' @param TPrev Taeniasis prevalence
#' @param CPrev Cysticercosis prevalence in humans
#' @param PCPrev Cysticercosis prevalence in pigs
#' @param AEL Average life expectancy of egg in the environment (weeks)
#' @param ATL Average life expectancy of T. solium adult worm in human (years)
#' @param ADI Average duration of cycticercosis infection in humans (years)
#' @param LEH Average life expectancy of a human (years)
#' @param phi Proportion of infected pigs with low-intensity cyst burden
#' @param chi Average number of pork meals (200g portion) per person per month
#' @param RR_cysticercosis Risk multiplier for Cysticercosis if human has Taeniasis
#' @param epsilon Pig rate of loss of naturally aquired immunity
#' @param RR_infection Increased RR of infection given consumption of high cyct burden pork, compared with low cyst burden
#' @param RR_consumption Decreased RR of consumption of high cyst burden pork, compared with low cyst burden pork
#'
#' @return Two lists of parameters and state variable values
#' @export
Set_up<-function(LEP=1, delta=960000, HPS=10000, PPS=2000, TPrev=0.02, CPrev=0.07, PCPrev=0.2, AEL=2,
                 ATL=2, ADI=3, LEH=54, phi=0.8, chi=0.5, RR_cysticercosis=1, epsilon=0.01, RR_infection=1, RR_consumption=-0.25){

  # Pig mortality rate
  dP<-month_rate(LEP)
  # Egg mortality rate
  dE<-month_rate(AEL*7/365)
  # Intitial number of infected pigs
  IP0<-PPS*PCPrev
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
  # Initial number of cumulative cysticercosis cases
  CCC0<-0
  # Initial number of cumulative taeniasis cases
  CTC0<-0

  # Combined raltive risk for high-cyst burden meat (increased RR of infection * decreased RR of consumption)
  CRR<-1+RR_infection+RR_consumption

  # Pork to human tranmission parameter
  beta<-Beta_equilibrium(alpha, IH0, IHC0, eta, dH, IP0, PPS, SH0, SHC0)
  # Probability of human becoming infected given consumption of low or high cyst burden meat
  pil<-pil_equilibrium(beta, chi, phi, CRR)
  pih<-CRR*pil
  if(pil>1 | pih>1){
    stop('pil or pih are >1, equilbirum not found')
  }

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
  # Egg to human transmission paramter
  theta<-theta_equilibrium(bH, eta, SHC0, IHC0, dH, SH0, IH0, E0, RR_cysticercosis)


  params<-list(tau=tau,
               PPS=PPS,
               HPS=HPS,
               bH=bH,
               bP=bP,
               dH=dH,
               dP=dP,
               dE=dE,
               delta=delta,
               phi=phi,
               theta=theta,
               alpha=alpha,
               eta=eta,
               chi=chi,
               pil=pil,
               pih=pih,
               epsilon=epsilon,
               RR_cysticercosis=RR_cysticercosis
  )

  states<-list(E0=E0,
               SP0=SP0,
               SHC0=SHC0,
               IHC0=IHC0,
               SH0=SH0,
               IH0=IH0,
               IPL0=IPL0,
               IPH0=IPH0,
               RP0=RP0,
               VP0=VP0,
               CCC0=CCC0,
               CTC0=CTC0)

  return(list(params, states))
}
