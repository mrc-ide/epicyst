#' @title
#' Sensitivity parameter LHS draw
#' @description
#' Simulates parameter sets from a Latin Hypercube Sample (uniform distribution)
#'
#' @param N The number of draws to produce
#'
#' @return A list of parameter sets
#' @export
Sensitivity_params<-function(N){

  p1<-Set_up()[[1]]

  x<-lhs::randomLHS(N,k=14)	# Draws N Latin Hypercube Samples from a set of (k) uniform distributions

  sens_params<-list()
  for(i in 1:N){
    sens_params[[i]]<-p1

    sens_params[[i]]$tau<-qunif(x[i,1],p1$tau*0.5,p1$tau*2)

    sens_params[[i]]$dh<-qunif(x[i,2], 0.0018, 0.013)	            # Human death rate
    sens_params[[i]]$bh<-p1$HPS*sens_params[[i]]$dH	              # Recallibrate human birth rate to ensure stable population size

    sens_params[[i]]$dP<-qunif(x[i,3], 0.042, 0.17)	              # Pig death rate
    sens_params[[i]]$bP<-p1$PPS*sens_params[[i]]$dP	              # Recallibrate pig birth rate to ensure stable population size

    sens_params[[i]]$dE<-qunif(x[i,4], 0.083, 4)	                # Egg removal rate
    sens_params[[i]]$delta<-qunif(x[i,5], 640000, 1800000)        # Egg production rate

    sens_params[[i]]$phi<-qunif(x[i,6], 0.1, 0.9)                 # Egg -> pig transmission
    sens_params[[i]]$theta<-qunif(x[i,7],p1$theta*0.5,p1$theta*2) # Egg -> human transmission

    sens_params[[i]]$alpha<-qunif(x[i,8], 0.017, 0.17)            # Human recovery rate from taeniasis
    sens_params[[i]]$eta<-qunif(x[i,9], 0.00083, 0.0083)          # Human recovery rate from cysticercosis

    sens_params[[i]]$chi<-qunif(x[i,10], 0.083, 0.680)            # Pork consumption rate
    sens_params[[i]]$pil<-qunif(x[i,11],p1$pil*0.5,p1$pil*2)      # Pork low cyst-burden -> human infection prob

    rr_infection<-qunif(x[i,12], 1, 3)
    rr_consumption<-qunif(x[i,13], 0.33, 1)
    sens_params[[i]]$pih<-sens_params[[i]]$pil*rr_infection*rr_consumption     # Pork low cyst-burden -> human infection prob

    sens_params[[i]]$epsilon<-qunif(x[i,13], 0.0075, 0.0125)      # Rate of loss of immunity
    sens_params[[i]]$RR<-qunif(x[i,14], 0, 4)                     # Rate of loss of immunity

  }

  return(sens_params)
}
