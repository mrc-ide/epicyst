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

  x<-lhs::randomLHS(N,k=15)	# Draws N Latin Hypercube Samples from a set of (k) uniform distributions

  sens_params<-list()
  for(i in 1:N){
    sens_params[[i]]<-p1

    sens_params[[i]]$tau<-qunif(x[i,1],p1$tau*0.25,p1$tau*4,sens_params[[i]]$tau)       # Egg -> pig transmission coefficient

    sens_params[[i]]$dH<-triangle::qtriangle(x[i,2], 0.0013, 0.018, sens_params[[i]]$dH)	            # Human death rate
    sens_params[[i]]$bh<-p1$HPS*sens_params[[i]]$dH	              # Recallibrate human birth rate to ensure stable population size

    sens_params[[i]]$dP<-triangle::qtriangle(x[i,3], 0.042, 0.17, sens_params[[i]]$dP)	              # Pig death rate
    sens_params[[i]]$bP<-p1$PPS*sens_params[[i]]$dP	              # Recallibrate pig birth rate to ensure stable population size

    sens_params[[i]]$dE<-qunif(x[i,4], 0.083, 4, sens_params[[i]]$dE)	                # Egg removal rate
    sens_params[[i]]$delta<-triangle::qtriangle(x[i,5], 640000, 1800000, sens_params[[i]]$delta)        # Egg production rate

    sens_params[[i]]$phi<-triangle::qtriangle(x[i,6], 0.1, 0.9,  sens_params[[i]]$phi)                 # Proportion of pigs with low cyst burden
    sens_params[[i]]$theta<-qunif(x[i,7],p1$theta*0.25,p1$theta*4, sens_params[[i]]$theta) # Egg -> human transmission

    sens_params[[i]]$alpha<-triangle::qtriangle(x[i,8], 0.017, 0.17, sens_params[[i]]$alpha)            # Human recovery rate from taeniasis
    sens_params[[i]]$eta<-triangle::qtriangle(x[i,9], 0.00083, 0.0083, sens_params[[i]]$eta)          # Human recovery rate from cysticercosis

    sens_params[[i]]$chi<-triangle::qtriangle(x[i,10], 0.083, 0.680, sens_params[[i]]$chi)            # Pork consumption rate
    sens_params[[i]]$pil<-qunif(x[i,11],p1$pil*0.25,p1$pil*4,sens_params[[i]]$pil)      # Pork low cyst-burden -> human infection prob

    sens_params[[i]]$rr_infection<-triangle::qtriangle(x[i,12], 1, 3, 2)
    sens_params[[i]]$rr_consumption<-triangle::qtriangle(x[i,13], 1/3, 1, 0.75)
    sens_params[[i]]$pih<-sens_params[[i]]$pil*
      sens_params[[i]]$rr_infection*
      sens_params[[i]]$rr_consumption     # Pork high cyst-burden -> human infection prob

    sens_params[[i]]$epsilon<-triangle::qtriangle(x[i,14], 0.0075, 0.0125, sens_params[[i]]$epsilon)      # Rate of loss of immunity
    sens_params[[i]]$RR_cysticercosis<-triangle::qtriangle(x[i,15], 0.0001, 4, sens_params[[i]]$RR_cysticercosis)                     # Risk muyltiplier cysticercosis

  }

  return(sens_params)
}

#' @title
#' Sensitivity draws for interventions
#' @description
#' Simulates interventions effect sensivities
#'
#' @param N The number of draws to produce
#'
#' @return A list of intervention effect size sets
#' @export
Sensitivity_intervention<-function(N){
  bl<-Intervention_effect_size()

  sens_int<-list()
  for(i in 1:N){
    sens_int[[i]]<-bl

    sens_int[[i]]$Husbandry<-triangle::rtriangle(1, 0.5, 0.9, sens_int[[i]]$Husbandry)
    sens_int[[i]]$Sanitation<-triangle::rtriangle(1, 0.5, 0.9, sens_int[[i]]$Sanitation)
    sens_int[[i]]$Inspection[1]<-triangle::rtriangle(1, 0.5, 0.9, sens_int[[i]]$Inspection[1])
    sens_int[[i]]$Inspection[2]<-triangle::rtriangle(1, 0.2, 0.8, sens_int[[i]]$Inspection[2])

    sens_int[[i]]$Pig_MDA[1]<-triangle::rtriangle(1, 0.375, 1, sens_int[[i]]$Pig_MDA[1])
    sens_int[[i]]$Pig_MDA[2]<-triangle::rtriangle(1, 0, 1, sens_int[[i]]$Pig_MDA[2])
    sens_int[[i]]$Pig_vaccine<-triangle::rtriangle(1, 0.375, 1, sens_int[[i]]$Pig_vaccine)
    sens_int[[i]]$Human_test_and_treat<-triangle::rtriangle(1, 0.32, 1, sens_int[[i]]$Human_test_and_treat)
  }

  return(sens_int)
}


#' @title
#' PRCC
#' @description
#' Calculates the PRCC statistic
#'
#' @param outcome a vector of output (e.g. cumulative cysticercosis cases), with one value for each set of parameter draws
#' @param covariates a dataframe of the input parameters (one row for each draw)
#'
#' @return A vector of PRCC scores, one for each inoput covariate
#' @export
PRCC<-function(outcome,covariates){
  # Rank the outcome measure (eg cumulative cysticercosis cases)
  rank_outcome<-rank(outcome)
  rank_covariates<-as.data.frame(apply(covariates, 2, rank))	# Rank parameters

  PRCC_out<-c()
  for (par in 1:ncol(covariates)){
    xx<-rank_covariates[,par]		  # Ranked parameter of interest
    xy<-rank_covariates[,-par]		# Rank of all other parameters

    xj<-xx-predict(lm(xx~.,data=xy))	# Residuals 1
    yy<-rank_outcome-predict(lm(rank_outcome~.,data=xy))	# Resdiauls 2

    PRCC_out[par]<-cov(xj,yy)/(sqrt(var(xj)*var(yy)))	# Correlation coefficient (PRCC)
  }
  names(PRCC_out)<-colnames(covariates)
  PRCC_out
}

#' @title
#' PRCC significance test
#' @description
#' Calculates the test statistic for PRCC scores
#'
#' @param prcc_values a vector of prcc scores
#' @param N The number of draws
#' @param N_par The number of covariates
#'
#' @return A vector of PRCC scores, one for each inoput covariate
#' @export
PRCC_sig<-function(prcc_values, N, N_par){
  T_value<-prcc_values*sqrt((((N-2-(N_par-1))/(1-(prcc_values^2)))))				# Calculate the T value
  2*pt(abs(T_value),df=N-2-(N_par-1),lower.tail=FALSE)			# Corresponding p-value
}

