#' @title
#' R0 internal
#' @description
#' Calculate R0
#'
#' @param delta Egg production rate (per month)
#' @param tau Egg to pig transmission paramter
#' @param phi Proportion of infected pigs with low-intensity cyst burden
#' @param alpha Human recovery rate from Taenisasis (per month)
#' @param dH Human mortality rate (per month)
#' @param dE The egg mortlality/removal rate (per month)
#' @param dP Pig mortality rate (per month)
#' @param RR Risk multiplier for Cysticercosis if human has Taeniasis
#' @param theta Egg to human transmission paramter
#' @param eta Human recovery rate from Cysticercosis (oer month)
#' @param chi rate of aquiring a pork meal (per month)
#' @param pih High intensity infected pig -> human contact rate (per month)
#' @param pil Low intensity infected pig -> human contact rate (per month)
#' @param HPS Human population size
#' @export
R0_internal<-function(delta,tau,phi,alpha,dH,dE,dP,RR,theta,eta,chi,pih, pil, HPS, ...){
  betah=chi*pih
  betal=chi*pil
  ((((delta*tau)*(betah-(betah*phi)+(betal*phi)))/(dE*dP*((RR*theta)+dH+alpha)))*(1+((RR*theta)/(eta+dH+alpha)))*HPS)^(1/3)
}

#' @title
#' R0 wrapper
#' @description
#' A wrapper function to faciloitate the R0 function being called with a list of parameters
#'
#' @param Params A list of model parameters: see \code{\link{R0_internal}} for list
#' @export
R0<-function(Params){
  do.call('R0_internal', Params)
}
