#' @title
#' Monthly rate
#' @description
#' Converts an average duration (years) into a monthly rate
#'
#' @param dur Average duration (years)
#'
#' @return Monthly rate
month_rate <- function(dur) {
  1 / (dur * 12)
}



#' @title
#' Set up (age-structured model)
#' @description
#' Caculates internal parameters and equilbirum staring values for state variables
#'
#' @param LEP Pig life expectancy (years)
#' @param delta Egg production rate (per month)
#' @param HPS Human population size
#' @param PPS Pig population size
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
#' @param number_age_classes_pig Number of age classes for pig population structure
#' @param slaughter_age_min Minimum age (in months) from which pigs are slaughtered
#' @param PCPrev Cysticercosis prevalence in pigs (true underlying prevalence)
#' @param PC_sens Sensitivity of pig cysticercosis diagnostic
#' @param PC_spec Specificity of pig cysticercosis diagnostic
#' @param CPrev Cysticercosis prevalence in humans (true underlying prevalence)
#' @param C_sens Sensitivity of human cysticercosis diagnostic
#' @param C_spec Specificity of human cysticercosis diagnostic
#' @param TPrev Taeniasis prevalence (true underlying prevalence)
#' @param T_sens Sensitivity of human taeniasis diagnostic
#' @param T_spec Specificity of human taeniasis diagnostic
#' @param psi Pig rate of transition from prepatent to infectious (average duration for maturation of cysts)
#' @param slgEP life expectancy (years) after pigs reach slaughter age
#' @param tau_input user specified egg to pig transmission coefficient
#' @param theta_input user specified egg to human transmission coefficient
#' @param beta_input user specified pig to human transmission coefficient
#' @param pig_age_class_width duration/width of each pig age class (months) 
#' @param number_age_classes_human number of age classes for human population structure
#'
#' @return Two lists of parameters and state variable values
#' @export
set_up <- function(LEP = 10, slgEP = 1,  HPS = 10000, PPS = 2000, AEL = 2, delta = 960000,
                   tau_input = NULL, theta_input = NULL, beta_input = NULL, psi = 0.5,
                   ATL = 2, ADI = 3, LEH = 54, phi = 0.8, chi = 0.5, RR_cysticercosis = 1, epsilon = 0.01, 
                   RR_infection = 1, RR_consumption = -0.25, number_age_classes_pig = 150, 
                   slaughter_age_min = 6, pig_age_class_width = 1, number_age_classes_human = 7,
                   PCPrev = 0.2, PC_sens = NULL, PC_spec = NULL, 
                   CPrev = 0.07, C_sens = NULL, C_spec = NULL, 
                   TPrev = 0.02, T_sens = NULL, T_spec = NULL){
  
  #===============#
  #  PARAMETERS   #
  #===============#
  
  #===========================================#
  # Setting up death rate parameters for pigs #
  
  # for non-age structured model # 
  if (slaughter_age_min == 0 && number_age_classes_pig == 1) {
    slgtage_bfr <- 1
    slgtage <- 3
    slgage_foi <- 1
  }
  
  # set up age classes at which pigs slaughtered and contribute exposure to humans
  if (slaughter_age_min == 0 && number_age_classes_pig > 1) {
    # Slaughter age min for age classes (in months- age-classes in 1 month intervals)
    slgtage_bfr <- 1 # age class before slaughter rate occurs
    slgtage <- 2 # age class where slaughter rate begins from
    slgage_foi <- 1 # pig age classes contributing to expsoure to humans b/c slaughtered
  }
  

  if (slaughter_age_min == 1) {
    # Slaughter age min for age classes (in months- age-classes in 1 month intervals)
    slgtage_bfr <- 2
    slgtage <- 2
    slgage_foi <- 2
  }
  
  if (slaughter_age_min > 1) {
    slgtage_bfr <- slaughter_age_min
    slgtage <- slaughter_age_min + 1
    slgage_foi <- slgtage
  }
  
  #====================================================================#
  # specify natural and slaughter-age specific mortality rates in pigs #
  
  # If Min slaughter age from 0 (i.e. unstructured model) need to set dP to include both dP and dPslg
  if (slaughter_age_min == 0) {
    LEP <- slgEP
    # Pig Total mortality rate (LEP and SlgEP)
    dP <- month_rate(LEP)
    dPslg <- 0
  }
  
  # If Min slaughter age > 0 then use both dP and dPslg
  if(slaughter_age_min > 0){
    # Pig natural mortality rate
    dP <- month_rate(LEP)
    # Pig mortality rate (from min slaughter age class)
    dPslg <- month_rate(slgEP)
    }
  
  # Egg mortality rate
  dE <- month_rate(AEL * 7 / 365)
  # Initial egg number (at equilibrium)
  E0 <- e0_equilibrium(delta, HPS, TPrev, CPrev, dE)
  # Human recovery rate from Taeniasis
  alpha <- month_rate(ATL)
  # Initial number of Human: T+ C-
  IH0_total <- HPS * TPrev * (1 - CPrev)
  # Initial number of Human: T+ C+
  IHC0_total <- HPS * TPrev * CPrev
  # Human recovery rate from cysticercosis
  eta <- month_rate(ADI)
  # Human mortality rate
  dH <- month_rate(LEH)
  # Initial number of Human: susceptible
  SH0_total <- HPS * (1 - TPrev) * (1 - CPrev)
  # Initial number of Human: T- C+
  SHC0_total <- HPS * (1 - TPrev) * CPrev
  # Initial number of cumulative cysticercosis cases
  CCC0 <- 0
  # Initial number of cumulative taeniasis cases
  CTC0 <- 0
  # Combined relative risk for high-cyst burden meat (increased RR of infection * decreased RR of consumption)
  CRR <- 1 + RR_infection + RR_consumption
  # Birth of humans
  bH <- HPS * dH
  # Intitial number of infected pigs
  IP0_total <- PPS * PCPrev
  # Intitial number of susceptible pigs
  SP0_total <- PPS - IP0_total
  # Prepatent pigs at time 0
  PP0_total <- 0
  # Number of infected pigs with low cyst burden at time 0
  IPL0_total <- (PPS - SP0_total) * phi
  # Number of infected pigs with high cyst burden at time 0
  IPH0_total <- (PPS - SP0_total) * (1 - phi)
  # Recovered pigs at time 0
  RP0_total <- 0
  # Vaccinated pigs at time 0
  VP0_total <- 0
  
  
  #==========================#
  # Set - up Pig age classes #
  # calculate age rate and age widths
  pig_age_parameters <- age_parameters_pig_func(number_age_classes = number_age_classes_pig, pig_age_class_width = pig_age_class_width) 
  
  #============================================================================================#
  # calculate pig population number for non-age strucutured model (not using life-tables)     #
  
  if (number_age_classes_pig == 1) {
  SP_eq <- SP0_total
  PP_eq <- PP0_total
  IPL_eq <- IPL0_total
  IPH_eq <- IPH0_total
  RP_eq <- RP0_total
  VP_eq <- VP0_total
  }

  #========================================================================================#
  # Calculate life-table for stable baseline pig population (where more than 1 age-class)  # 
  if (number_age_classes_pig > 1) {
  
  pig_lifetable_output <- life_tables_pigs_func(number_age_classes = number_age_classes_pig, slgt_age_min = slaughter_age_min, slgtage = slgtage, 
                                                slgtage_bfr = slgtage_bfr, dP, dPslg, na_pig = pig_age_parameters[[2]])
  
  }
  
  
  #===================================================================================================== #
  # Calculate proportion of pigs in each age class for each state using life-tables (where >1 age class) # 
  if (number_age_classes_pig > 1) {
    pig_ageclass_proportions <- Pig_age_class_proportions_func(PPS = PPS, pig_demography = pig_lifetable_output, na_pig = pig_age_parameters[[2]], 
                                                               IPL0_total = IPL0_total, IPH0_total = IPH0_total)
   # recode proportions for next calculations
  SP_eq <- pig_ageclass_proportions[[1]]
  PP_eq <-  pig_ageclass_proportions[[2]]
  IPL_eq <- pig_ageclass_proportions[[3]]
  IPH_eq <- pig_ageclass_proportions[[4]] 
  RP_eq <- pig_ageclass_proportions[[5]]
  VP_eq <- pig_ageclass_proportions[[6]]
  
  }
  
 
  #========================================================#
  #    calculating quanitites for pig population           #
  if (number_age_classes_pig > 1) {
    
    # calculate number of pigs of slaughter age in each state
    IPL0_slgt <- sum(IPL_eq[slgage_foi:pig_age_parameters[[2]]])
    IPH0_slgt <- sum(IPH_eq[slgage_foi:pig_age_parameters[[2]]])
    SP0_slgt <- sum(SP_eq[slgage_foi:pig_age_parameters[[2]]])
    PP0_slgt <- sum(PP_eq[slgage_foi:pig_age_parameters[[2]]])
    
    # calculate number non-slaughter age pigs in each state (depending on specified min slaughter age)
    if (slaughter_age_min == 0) {
      IPL0_nonslgt <- 0
      IPH0_nonslgt <- 0
      SP0_nonslgt <- 0
      PP0_nonslgt <- 0
      PPS_nonslgt_eq <- 0
    }
    
    if (slaughter_age_min == 1) {
      IPL0_nonslgt <- sum(IPL_eq[1:1])
      IPH0_nonslgt <- sum(IPH_eq[1:1])
      SP0_nonslgt <- sum(SP_eq[1:1])
      PP0_nonslgt <- sum(PP_eq[1:1])
      PPS_nonslgt_eq <-
        sum(SP_eq[1:1]) + sum(PP_eq[1:1]) + sum(IPL_eq[1:1]) + sum(IPH_eq[1:1])
    }
    
    if (slaughter_age_min > 1) {
      IPL0_nonslgt <- sum(IPL_eq[1:slgtage_bfr])
      IPH0_nonslgt <- sum(IPH_eq[1:slgtage_bfr])
      SP0_nonslgt <- sum(SP_eq[1:slgtage_bfr])
      PP0_nonslgt <- sum(PP_eq[1:slgtage_bfr])
      PPS_nonslgt_eq <-
        sum(SP_eq[1:slgtage_bfr]) + sum(PP_eq[1:slgtage_bfr]) + sum(IPL_eq[1:slgtage_bfr]) + sum(IPH_eq[1:slgtage_bfr])
    }
    
    # calculate key quantities of pigs
    IP0_slgt <- IPL0_slgt + IPH0_slgt # total infected (patent) pigs of slaughter age at baseline
    IPL0_all <- sum(IPL_eq[1:pig_age_parameters[[2]]]) # all infected (patent) pigs low burden at baseline
    IPH0_all <- sum(IPH_eq[1:pig_age_parameters[[2]]]) # all infected (patent) pigs high burden at baseline
    PP0_all <- sum(PP_eq[1:pig_age_parameters[[2]]]) # all pre-patent pigs at baseline
    PP0_slgt <- PP0_slgt # total pre-patent pigs of slaughter age
    IP0_all <- IPL0_all + IPH0_all # total infected (patent) pigs at baseline
    IP0_nonslgt <- IPL0_nonslgt + IPH0_nonslgt # total infected (patent) pigs of non slaughter age at baseline
    SP0_all <- sum(SP_eq[1:pig_age_parameters[[2]]]) # total susceptible pigs at baseline
    PCC_prev_eq <- (IPL0_all + IPH0_all) / (IPL0_all + IPH0_all + PP0_all + SP0_all) # quantity check: (patent) PCC prevalence at baseline (all ages)
    PPS_slgt_eq <- (sum(SP_eq[slgage_foi:pig_age_parameters[[2]]]) + sum(PP_eq[slgage_foi:pig_age_parameters[[2]]]) + sum(IPL_eq[slgage_foi:pig_age_parameters[[2]]]) + sum(IPH_eq[slgage_foi:pig_age_parameters[[2]]]) + sum(RP_eq[slgage_foi:pig_age_parameters[[2]]]) + sum(VP_eq[slgage_foi:pig_age_parameters[[2]]])) # quantity check: (patent) PCC prevalence at baseline (slaughter ages)
    PPS_nonslgt_eq <- PPS - PPS_slgt_eq # quantity check: (patent) PCC prevalence at baseline (non-slaughter ages)
    
  }
  
  # calculate key quanities of pigs for non-age structured model #
  if (number_age_classes_pig == 1) {
    IP0_slgt <- 0
    PP0_slgt <- 0
    IP0_all <- IP0_total
    SP0_all <- SP0_total
    PP0_all <- PP0_total
  }
  
  
  #===========================================================#
  #    FINAL PARAMETERS TO MODIFY WITH AGE STRUCTURED NUMBERS #
  
  # Birth (rate) of pigs
  
   na_pig <- pig_age_parameters[[2]]
  
  if (number_age_classes_pig > 1) {
    bP <- (dP * sum(SP_eq) + dP * sum(PP_eq) + dP * sum(IPL_eq) + dP * sum(IPH_eq) + dP * sum(RP_eq) + 
             dP * sum(VP_eq)) + ((dPslg * sum(SP_eq[slgtage:na_pig])) + (dPslg * sum(PP_eq[slgtage:na_pig])) + (dPslg * sum(IPL_eq[slgtage:na_pig])) + (dPslg * sum(IPH_eq[slgtage:na_pig])) + (dPslg * sum(RP_eq[slgtage:na_pig])) + (dPslg * sum(VP_eq[slgtage:na_pig])))
    }
  
  if (number_age_classes_pig == 1) {
    bP <- PPS * dP
  }
  
  #================================================================#
  # Set - up human age classes: note life-tables approach not used #
  
  #================================================================#
  # Human non age-structured model set-up                          #
  
  # create vector of length n age classes for ODIN (& create dimensions for other variables)
  if (number_age_classes_human == 1) {
    
    na_human <- 1
    age_rate_human <- 0
    
    SH_eq <- SH0_total
    SHC_eq <- SHC0_total
    IH_eq <- IH0_total
    IHC_eq <- IHC0_total
    
    # quantities needed for equilibrium calcs (redefined in age-structured calculations below)
    SH0_all <- SH_eq   # all susceptible humans at baseline
    SHC0_all <- SHC_eq # all cysticercosis infected humans at baseline
    IH0_all <- IH_eq   # all taeniasis infected humans at baseline
    IHC0_all <- IHC_eq # all cysticercosis & taeniasis infected humans at baseline
    
  }
  
  #================================================================#
  # Human age-structured model set-up                              #
  
  if (number_age_classes_human > 1) {
  
    # calculate age rates and age widths
  human_age_parameters <- age_parameters_human_func(number_age_classes = number_age_classes_human)
  
  age_rate_human <- human_age_parameters[[3]]
  na_human <- human_age_parameters[[2]]
  
  # calculate proportions of humans in each age class
  human_ageclass_proportions <- human_age_class_proportions_func(age_rate = age_rate_human, na_human = na_human, dH = dH,
                                                                 HPS = HPS, SHC0_total = SHC0_total, IH0_total = IH0_total, IHC0_total = IHC0_total)
  
  SH_eq <- human_ageclass_proportions[[1]]
  SHC_eq <- human_ageclass_proportions[[2]]
  IH_eq <- human_ageclass_proportions[[3]]
  IHC_eq <- human_ageclass_proportions[[4]]
  
  # key quantities for transmission (equilibrium) parameters
  SH0_all <- sum(SH_eq)   # all susceptible humans at baseline
  SHC0_all <- sum(SHC_eq) # all cysticercosis infected humans at baseline
  IH0_all <- sum(IH_eq)   # all taeniasis infected humans at baseline
  IHC0_all <- sum(IHC_eq) # all cysticercosis & taeniasis infected humans at baseline
  
  }

  #=========================================#
  # Define other transmission parameters    #
  
  # Egg to pig transmission parameter (tau)
  if (is.null(tau_input)) {
    tau <- tau_equilibrium(dP, dPslg, IP0_nonslgt, IP0_slgt, IP0_all, SP0_all, PP0_all, PP0_slgt, E0)
  }
  
  if (!is.null(tau_input)) {
    tau <- tau_input
  }
  
  # Pork to human transmission parameter (beta)
  if (is.null(beta_input)) {
    beta <- beta_equilibrium(alpha, IH0_all, IHC0_all, eta, dH, IP0_all, PPS, SH0_all, SHC0_all)
  }
  
  if (!is.null(beta_input)) {
    beta <- beta_input
  }
  
  # Probability of human becoming infected given consumption of low or high cyst burden meat (pi)
  pil <- pil_equilibrium(beta, chi, phi, CRR)
  pih <- CRR * pil
  if (pil > 1 |
     pih > 1) {
    stop('pil or pih are >1, equilbirum not found')
  }
  
  # Egg to human transmission parameter (theta)
  if (is.null(theta_input)) {
    theta <- theta_equilibrium(bH, eta, SHC0_all, IHC0_all, dH, SH0_all, IH0_all, E0, RR_cysticercosis)
  }
  
  if (!is.null(theta_input)) {
    theta <- theta_input
  }
  
  #=============================================#
  #  Generate List of outputs for ODE's in ODIN #
  
  params <- list(
    tau = tau,
    PPS = PPS,
    HPS = HPS,
    bH = bH,
    bP = bP,
    dH = dH,
    dP = dP,
    dPslg = dPslg,
    dE = dE,
    delta = delta,
    phi = phi,
    theta = theta,
    alpha = alpha,
    eta = eta,
    chi = chi,
    pil = pil,
    pih = pih,
    psi = psi,
    epsilon = epsilon,
    RR_cysticercosis = RR_cysticercosis,
    age_rate_pig = pig_age_parameters[[3]],
    na_pig = na_pig,
    slgtage = slgtage,
    slgtage_bfr = slgtage_bfr,
    slgage_foi = slgage_foi,
    age_rate_human = age_rate_human,
    na_human = na_human,
    PC_sens = PC_sens,
    PC_spec = PC_spec,
    C_sens = C_sens,
    C_spec = C_spec,
    T_sens = T_sens,
    T_spec = T_spec,
    PCPrev_new = PCPrev,
    CPrev_new = CPrev,
    TPrev_new = TPrev
    
  )
  
  states <- list(
    E0 = E0,
    SHC0 = SHC_eq,
    IHC0 = IHC_eq,
    SH0 = SH_eq,
    IH0 = IH_eq,
    SP0 = SP_eq,
    PP0 = PP_eq,
    IPL0 = IPL_eq,
    IPH0 = IPH_eq,
    RP0 = RP_eq,
    VP0 = VP_eq,
    CCC0 = CCC0,
    CTC0 = CTC0
  )
  
  return(list(params, states))
}

