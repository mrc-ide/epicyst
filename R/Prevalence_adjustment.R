# library(prevalence) # note need prevalence package installed or called

#' @title
#' Adjustment observed prevalence to true prevalence
#' @description
#' Uses MCMC from prevalence package to adjust observed positives/ total sampled and diagnostic se and sp to true prevalence
#' see for more info: http://prevalence.cbra.be/?main=download
#'
#' @param sens sensitivty of diagnostic
#' @param spec specifcity of diagnostic
#' @param positive number of positive observed
#' @param total total sampled
#'
#' @return True prevalence median from MCMC
#' @export
prev_adjustment_TP_func <- function(sens, spec, positive, total) {
  p <- prevalence::truePrev(x=positive, n=total, SE= sens, SP= spec)
  p_matrix <- prevalence::as.matrix(p)
  TPrev <- median(p_matrix[,1])
  remove(p)
  
  return(TPrev)
}

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
  AP <- (1-spec) + (sens+spec-1)*TP      # observed/apparent prevalence Diggle et al 2011 Epi Res Int
  
  return(AP)
}


#' @title
#' Apparent prevalence packaging
#' @description
#' uses Prev_adjustment_AP_func to generate a column with observed/ apparent prevalence in model run dataframe
#'
#' @param model_out model run dataframe as input (specific prevalence column in dataframe from PCC, HCC, HT)
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


