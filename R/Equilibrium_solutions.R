
#' @title
#' Equilibrium Eggs
#' @description
#' Calculate the number of eggs in the environment at equlibrium
#'
#' @param delta Egg production rate (per month)
#' @param HPS Human population size
#' @param TPrev Taeniasis prevalence in human population
#' @param CPrev Cysticercosis prevalence in human population
#' @param dE The egg mortlality/removal rate (per month)
#'
#' @return The Equilbirum number of eggs in the environment
E0_equilibrium<-function(delta, HPS, TPrev, CPrev, dE){
  ((delta*HPS*TPrev*(1-CPrev))+(delta*HPS*TPrev*CPrev))/dE
}

#' @title
#' Egg to pig transmission paramter
#' @description
#' Calculate the transmission parameter (egg->pig) at equiliberium
#'
#' @param dP Pig mortality rate (per month)
#' @param IP0 Initial number of infected pigs
#' @param SP0 Intitial number of susceptible pigs
#' @param E0 The equilibrium number of eggs in the environment
#'
#' @return The equilibrium egg to pig transmission parameter
tau_equilibrium<-function(dP, IP0, SP0, E0){
  (dP*IP0)/(SP0*E0)
}

#' @title
#' Pig to human transmission paramter
#' @description
#' Calculate the transmission parameter (pig->human) at equiliberium
#'
#' @param alpha Human recovery rate from Taenisasis (per month)
#' @param IH0 Initial number of Human: T+ C-
#' @param IHC0 Initial number of Human: T+ C+
#' @param eta Human recovery rate from Cysticercosis (oer month)
#' @param dH Human mortality rate (per month)
#' @param IP0 Initial number of infected pigs
#' @param PPS Pig popultion size
#' @param SH0 Initial number of Human: susceptible
#' @param SHC0 Initial number of Human: T- C+
#'
#' @return The equilibrium pig to human transmission parameter
Beta_equilibrium<-function(alpha, IH0, IHC0, eta, dH, IP0, PPS, SH0, SHC0){
  (alpha*(IH0+IHC0)+eta*IHC0+dH*(IH0+IHC0))/((IP0/PPS)*(SH0+SHC0))
}

#' @title
#' Egg to human transmission paramter
#' @description
#' Calculate the transmission parameter (egg->human) at equiliberium
#'
#' @param bH Births of humans (net)
#' @param eta Human recovery rate from Cysticercosis (oer month)
#' @param SHC0 Initial number of Human: T- C+
#' @param IHC0 Initial number of Human: T+ C+
#' @param dH Human mortality rate (per month)
#' @param SH0 Initial number of Human: susceptible
#' @param IH0 Initial number of Human: T+ C-
#' @param E0 The equilibrium number of eggs in the environment
#' @param RR Risk multiplier for Cysticercosis if human has Taeniasis
#'
#' @return The equilibrium egg to human transmission parameter
theta_equilibrium<-function(bH, eta, SHC0, IHC0, dH, SH0, IH0, E0, RR) {
  (bH+eta*(SHC0+IHC0)-dH*(SH0+IH0))/((SH0*E0)+((1+RR)*IH0*E0))
}

#' @title
#' Low intensity infected pig -> human infection probability
#' @description
#' Calculate the transmission probability between a low intensity infected pork meal and human at equiliberium
#'
#' @param Beta Pork to human tranmission parameter
#' @param chi rate of aquiring a pork meal (per month)
#' @param phi Proportion of infected pigs with low-intensity cyst burden
#' @param CRR Combined reltive risk for high cyst burden pork (increased RR of infection * decreased RR of consumption)
#' @return The equilibrium transmission probability between a low intesnisty infected pig and human
pil_equilibrium<-function(beta, chi, phi, CRR){
  chil<-chi*phi
  chih<-chi*(1-phi)

  pil<-beta/(chil + CRR*chih)

  return(pil)
}






