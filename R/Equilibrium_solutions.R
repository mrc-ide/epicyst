
#' @title
#' Equilibrium Eggs
#' @description
#' Calculate the number of eggs in the environment at equlibrium
#'
#' @param delta Egg production rate (per month)
#' @param HPS Human population size
#' @param TPrev Taeniasis prevalence in human population
#' @param CPrev Cysticercosis prevalence in human population
#' @param dE The egg mortlality / removal rate (per month)
#'
#' @return The Equilibrium number of eggs in the environment
e0_equilibrium <- function(delta, HPS, TPrev, CPrev, dE) {
  ((delta * HPS * TPrev * (1 - CPrev)) + (delta * HPS * TPrev * CPrev)) / dE
}

#' @title
#' Egg to pig transmission paramter
#' @description
#' Calculate the transmission parameter (egg->pig) at equiliberium
#'
#' @param dP Pig mortality rate (per month)
#' @param dPslg Pig mortality rate due to slaughter (per month)
#' @param IP0_all Initial number of infected pigs
#' @param IP0_nonslgt Initial number of infected pigs (non slaughter age pigs)
#' @param IP0_slgt Initial number of infected pigs (slaughter age pigs)
#' @param SP0_all Initial number of susceptible pigs
#' @param PP0_all Initial number of pre-patent pigs
#' @param PP0_slgt initial number of pre-patent pigs (slaughter age)
#' @param E0 The equilibrium number of eggs in the environment
#'
#' @return The equilibrium egg to pig transmission parameter
tau_equilibrium <- function(dP, dPslg, IP0_nonslgt, IP0_slgt, IP0_all, SP0_all, PP0_all, PP0_slgt, E0) {
  ((dP * IP0_all) + (dPslg * IP0_slgt) + (dP * PP0_all) + (dPslg * PP0_slgt)) / (SP0_all * E0)
}

#' @title
#' Pig to human transmission paramter
#' @description
#' Calculate the transmission parameter (pig->human) at equiliberium
#'
#' @param alpha Human recovery rate from Taenisasis (per month)
#' @param IH0_all Initial number of Human: T +  C-
#' @param IHC0_all Initial number of Human: T +  C +
#' @param eta Human recovery rate from Cysticercosis (oer month)
#' @param dH Human mortality rate (per month)
#' @param IP0_all Initial number of infected pigs
#' @param PPS Pig popultion size
#' @param SH0_all Initial number of Human: susceptible
#' @param SHC0_all Initial number of Human: T- C +
#'
#' @return The equilibrium pig to human transmission parameter
beta_equilibrium <- function(alpha, IH0_all, IHC0_all, eta, dH, IP0_all, PPS, SH0_all, SHC0_all) {
  (alpha * (IH0_all + IHC0_all) + eta * IHC0_all + dH * (IH0_all + IHC0_all)) / ((IP0_all  / PPS) * (SH0_all + SHC0_all))
}

#' @title
#' Egg to human transmission paramter
#' @description
#' Calculate the transmission parameter (egg->human) at equiliberium
#'
#' @param bH Births of humans (net)
#' @param eta Human recovery rate from Cysticercosis (oer month)
#' @param SHC0_all Initial number of Human: T- C +
#' @param IHC0_all Initial number of Human: T +  C +
#' @param dH Human mortality rate (per month)
#' @param SH0_all Initial number of Human: susceptible
#' @param IH0_all Initial number of Human: T +  C-
#' @param E0 The equilibrium number of eggs in the environment
#' @param RR Risk multiplier for Cysticercosis if human has Taeniasis
#'
#' @return The equilibrium egg to human transmission parameter
theta_equilibrium <- function(bH, eta, SHC0_all, IHC0_all, dH, SH0_all, IH0_all, E0, RR) {
  (bH + eta * (SHC0_all + IHC0_all) - dH * (SH0_all + IH0_all)) / ((SH0_all * E0) + ((1 + RR) * IH0_all * E0))
}

#' @title
#' Low intensity infected pig -> human infection probability
#' @description
#' Calculate the transmission probability between a low intensity infected pork meal and human at equiliberium
#'
#' @param beta Pork to human tranmission parameter
#' @param chi rate of aquiring a pork meal (per month)
#' @param phi Proportion of infected pigs with low-intensity cyst burden
#' @param CRR Combined reltive risk for high cyst burden pork (increased RR of infection  *  decreased RR of consumption)
#' @return The equilibrium transmission probability between a low intesnisty infected pig and human
pil_equilibrium <- function(beta, chi, phi, CRR) {
  chil <- chi * phi
  chih <- chi * (1 - phi)
  pil <- beta / (chil + CRR * chih)
  
  return(pil)
}

