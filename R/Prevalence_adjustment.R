#' @title
#' Apparent prevalence adjustment
#' @description
#' estimates apparent prevalence from true underlying model predicted prevalence 
#'
#' @param sens sensitivty of diagnostic
#' @param spec specifcity of diagnostic
#' @param TP model generated prevalence
#'
#' @return Apparent / observed prevalence 
#' @export
prev_adjustment_AP_func <- function(sens, spec, TP) {
  
  AP <- sens * TP + (1 - spec) * (1 - TP) # observed/apparent prevalence Diggle et al 2011 Epi Res Int & Speybroeck et al. 2012
  
  return(AP)
}


#' @title
#' Apparent prevalence packaging
#' @description
#' uses Prev_adjustment_AP_func to generate a column with observed/ apparent prevalence in model run dataframe
#'
#' @param sens sensitivity of diagnostic
#' @param spec specificity of diagnostic
#' @param TP model generated prevalence
#'
#' @return Apparent / observed prevalence 
#' @export
apparent_prevalence_packaging_func <- function(sens, spec, TP) {
  apparent_prev <- as.vector(apply(as.matrix(TP), 2, function(i) prev_adjustment_AP_func(sens = sens, spec = spec, TP = TP)))
  apparent_prev <- data.frame(apparent_prev)
  
  return(apparent_prev)
}


