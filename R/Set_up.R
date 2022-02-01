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
#' @param PCPrev_obs Observed pig cysticercosis prevaelnce option
#' @param PCPrev_true Cysticercosis prevalence in pigs (true underlying prevalence)
#' @param PC_sens Sensitivity of pig cysticercosis diagnostic
#' @param PC_spec Specificity of pig cysticercosis diagnostic
#' @param CPrev_obs Observed human cysticercosis prevalence option
#' @param CPrev_true Cysticercosis prevalence in humans (true underlying prevalence)
#' @param C_sens Sensitivity of human cysticercosis diagnostic
#' @param C_spec Specificity of human cysticercosis diagnostic
#' @param TPrev_obs Observed human taeniasis prevalence option
#' @param TPrev_true Taeniasis prevalence (true underlying prevalence)
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
set_up <- function(LEP=10, slgEP=1,  HPS=10000, PPS=2000, AEL=2, delta=960000,
                   tau_input=NULL, theta_input=NULL, beta_input=NULL, psi=0.5,
                   ATL=2, ADI=3, LEH=54, phi=0.8, chi=0.5, RR_cysticercosis=1, epsilon=0.01, 
                   RR_infection=1, RR_consumption=-0.25, number_age_classes_pig = 100, 
                   slaughter_age_min=6, pig_age_class_width=1, number_age_classes_human = 7,
                   PCPrev_obs=NULL, PCPrev_true=0.2, PC_sens=NULL, PC_spec=NULL, 
                   CPrev_obs=NULL, CPrev_true=0.07, C_sens=NULL, C_spec=NULL, 
                   TPrev_obs=NULL, TPrev_true=0.02, T_sens=NULL, T_spec=NULL){
  
  #=============================================================================#
  # First make any adjustments to observed prevalence inputs to true prevalence #
  
  # Pig cysticercosis prevalence adjustment #
  if (is.null(PCPrev_obs)) {
    PCPrev <- PCPrev_true
  }
  
  if (!is.null(PCPrev_obs)) {
    PC_obs_positive <- round(PCPrev_obs * PPS)
    PCPrev <-
      prev_adjustment_TP_func(
        sens = PC_sens,
        spec = PC_spec,
        positive = PC_obs_positive,
        total = PPS
      )
  }
  
  # Human cysticercosis prevalence adjustment #
  if(is.null(CPrev_obs)) {
    CPrev <- CPrev_true
  }
  
  if (!is.null(CPrev_obs)) {
    C_obs_positive <- round(CPrev_obs * HPS)
    CPrev <-
      prev_adjustment_TP_func(
        sens = C_sens,
        spec = C_spec,
        positive = C_obs_positive,
        total = HPS
      )
  }
  
  # Human taeniasis prevalence adjustment #
  if(is.null(TPrev_obs)) {
    TPrev <- TPrev_true
  }
  
  if (!is.null(TPrev_obs)) {
    T_obs_positive <- round(TPrev_obs * HPS)
    TPrev <-
      prev_adjustment_TP_func(
        sens = T_sens,
        spec = T_spec,
        positive = T_obs_positive,
        total = HPS
      )
  } 
  
  PCPrev <- PCPrev
  CPrev <- CPrev
  TPrev <- TPrev
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
  if(slaughter_age_min>0){
    # Pig natural mortality rate
    dP<-month_rate(LEP)
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
  
  # vector for mean no. of months in each age compartment 
  age_width_pig <- c()
  
  #=======================================#
  # preparing age_width & age_rate params #
  
 # age_width calc: Age-structured model #
  if (number_age_classes_pig > 1) {
    for (i in number_age_classes_pig) {
      # width of 1 month for each age class (symmetrical age classes)
      age_width_pig[1:number_age_classes_pig] <- pig_age_class_width
    }
  }
  
  # create vector of length n age classes for ODIN (& create dimensions for other variables)
   if (number_age_classes_pig == 1) {
    na_pig <- 1
  }
  
  if (number_age_classes_pig > 1) {
    na_pig <- as.integer(length(age_width_pig)) 
  }
  
  # calculate age rate (function of age width) for non age-strcutured model # 
  if (number_age_classes_pig == 1) {
    age_rate_pig <- 0
  }
  
  # calculate age rate (function of age width) for age-strcutured model # 
  if (number_age_classes_pig > 1) {
    age_rate_pig <- c()
    
    for (i in na_pig) {
      # age rate leaving age classes (to next) excluding last age class (e.g. 1:2)
      age_rate_pig[1:(na_pig - 1)] <- 1 / age_width_pig[1:i - 1]
      # no age rate leaving last age class
      age_rate_pig[na_pig] <- 0
    }
  }
  
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

  #==============================================================================================#
  # To set up population distribution at baseline: LIFE TABLES APPROACH for Age-structured model #
  # Ref Verity et al: https://github.com/mrc-ide/covfefe                                         #
  # FAO paper: http://www.fao.org/tempref/docrep/fao/008/a0212e/a0212E09.pdf                     #        
  #
  
  # non-age structured 
  # if (slaughter_age_min == 0) {
  #   age1toslg <- 0
  #   ageslgtoN <- length(c(1:na_pig))
  # }
  
  if (number_age_classes_pig > 1) {
    
    if (slaughter_age_min == 0) {
      age1toslg <- 0
      ageslgtoN <- length(c(1:na_pig))
    }
    
    if (slaughter_age_min == 1) {
      age1toslg <- length(c(1:1))
      ageslgtoN <- length(c(slgtage:na_pig))
    }
    
    if (slaughter_age_min > 1) {
      age1toslg <- length(c(1:slgtage_bfr))
      ageslgtoN <- length(c(slgtage:na_pig))
    }
    
    # Assuming age groups of equal width: PROBABILITY (not rate) of death: (p=1-exp(-rt))
    # From natural cases (1/15 yrs*12)
    dp <- dP
    # From slaughter (1/1 yrs*12)
    dsl <- dPslg
    # combined
    dtot <- dp + dsl
    
    # Vector of death probability for each age group (final value is 1 to ensure closed population)
    life_table_sub1 <- rep(dp, age1toslg)  # life table for non slaughter ages
    life_table_sub2 <- rep(dtot, ageslgtoN - 1) # life table for slaughter ages
    life_table <- c(life_table_sub1, life_table_sub2, 1) # combine
    n <- length(life_table)
    age_death <- rep(0, n)
    remaining <- 1
    
    # define death probabilities
    for (i in 1:n) {
      age_death[i] <- remaining * life_table[i]
      remaining <- remaining * (1 - life_table[i])
    }  # should sum to 1
    
    # convert life table to transition matrix
    m <- matrix(0, n, n)
    m[col(m) == (row(m) + 1)] <- 1 - life_table[1:(n - 1)]
    m[, 1] <- 1 - rowSums(m)
    
    # convert to rates
    r = m - diag(n)
    
    # compute Eigenvalues of the rate matrix
    E = eigen(t(r))
    
    # there should be one Eigenvalue that is zero (up to limit of computational precision) --> find which Eigenvalue this is
    w <- which.min(abs(E$values))
    
    # the stable solution is the corresponding Eigenvector, suitably normalised
    age_stable <-
      Re(E$vectors[, w] / sum(E$vectors[, w])) # intrinsic  rate  of population increase (r)  - should sum to 1
    
    # final demography parameters
    pig_demography <- list(
      life_table = life_table,
      age_death = age_death,
      age_death_rate = (-log(1 - age_death)),
      age_stable = age_stable
    )
    
    # calculate proportion/ number of pigs in each age class
    den_pig <- c()
    
    for (i in 1:na_pig) {
      den_pig[i] <- PPS * pig_demography$age_stable[i]
    }
    
    den_pig_fraction <- den_pig / PPS
    
    # calculate numbers in each age class for each state (SP, PP, IPL, IPH, RP, VP)
    SP_eq <- c()
    for (i in 1:na_pig) {
      SP_eq[i] <-
        den_pig_fraction[i] * ((PPS) - (IPL0_total + IPH0_total + PP0_total + RP0_total + VP0_total))
    }
    
    PP_eq <- c()
    for (i in 1:na_pig) {
      PP_eq[i] <- den_pig_fraction[i] * PP0_total
    }
    
    IPL_eq <- c()
    for (i in 1:na_pig) {
      IPL_eq[i] <- den_pig_fraction[i] * IPL0_total
    }
    
    IPH_eq <- c()
    for (i in 1:na_pig) {
      IPH_eq[i] <- den_pig_fraction[i] * IPH0_total
    }
    
    RP_eq <- c()
    for (i in 1:na_pig) {
      RP_eq[i] <- den_pig_fraction[i] * RP0_total
    }
    
    VP_eq <- c()
    for (i in 1:na_pig) {
      VP_eq[i] <- den_pig_fraction[i] * VP0_total
    }
    
    # calculate number of pigs of slaughter age in each state
    IPL0_slgt <- sum(IPL_eq[slgage_foi:na_pig])
    IPH0_slgt <- sum(IPH_eq[slgage_foi:na_pig])
    SP0_slgt <- sum(SP_eq[slgage_foi:na_pig])
    PP0_slgt <- sum(PP_eq[slgage_foi:na_pig])
    
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
    IPL0_all <- sum(IPL_eq[1:na_pig]) # all infected (patent) pigs low burden at baseline
    IPH0_all <- sum(IPH_eq[1:na_pig]) # all infected (patent) pigs high burden at baseline
    PP0_all <- sum(PP_eq[1:na_pig]) # all pre-patent pigs at baseline
    PP0_slgt <- PP0_slgt # total pre-patent pigs of slaughter age
    IP0_all <- IPL0_all + IPH0_all # total infected (patent) pigs at baseline
    IP0_nonslgt <- IPL0_nonslgt + IPH0_nonslgt # total infected (patent) pigs of non slaughter age at baseline
    SP0_all <- sum(SP_eq[1:na_pig]) # total susceptible pigs at baseline
    PCC_prev_eq <- (IPL0_all + IPH0_all) / (IPL0_all + IPH0_all + PP0_all + SP0_all) # quantity check: (patent) PCC prevalence at baseline (all ages)
    PPS_slgt_eq <- (sum(SP_eq[slgage_foi:na_pig]) + sum(PP_eq[slgage_foi:na_pig]) + sum(IPL_eq[slgage_foi:na_pig]) + sum(IPH_eq[slgage_foi:na_pig]) + sum(RP_eq[slgage_foi:na_pig]) + sum(VP_eq[slgage_foi:na_pig])) # quantity check: (patent) PCC prevalence at baseline (slaughter ages)
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
  
  if (number_age_classes_pig > 1) {
    bP <- (dP * sum(SP_eq) + dP * sum(PP_eq) + dP * sum(IPL_eq) + dP * sum(IPH_eq) + dP * sum(RP_eq) + dP * sum(VP_eq)) + ((dPslg * sum(SP_eq[slgtage:na_pig])) + (dPslg * sum(PP_eq[slgtage:na_pig])) + (dPslg * sum(IPL_eq[slgtage:na_pig])) + (dPslg * sum(IPH_eq[slgtage:na_pig])) + (dPslg * sum(RP_eq[slgtage:na_pig])) + (dPslg * sum(VP_eq[slgtage:na_pig])))
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
  
  # vector of specified no. of months in each age compartment 
  if (number_age_classes_human == 7) {
    age_width_human <- c()
    age_width_human[1] <- 5 * 12 # 0 - 4.99 yrs (in months)
    age_width_human[2] <- 5 * 12 # 5 - 9.99 yrs
    age_width_human[3] <- 5 * 12 # 10- 14.99 yrs
    age_width_human[4] <- 15 * 12 # 15 - 29.99 yrs
    age_width_human[5] <- 20 * 12 # 30 - 49.99 yrs
    age_width_human[6] <- 20 * 12 # 50 - 69.99 yrs
    age_width_human[7] <- 20 * 12 # 70 - 89.99 yrs
  }
  
  # create vector of length n age classes for ODIN (& create dimensions for other variables)
  na_human <- as.integer(length(age_width_human)) 
  
  # calculate age rate (function of age width)
  age_rate_human <- c()
  
  for (i in na_human) {
    age_rate_human[1:(na_human - 1)] <- 1 / age_width_human[1:i - 1]
    age_rate_human[na_human] <- 0
  }
  
  # proportion of population in age classes 
  den_human <- c()
  den_human[1] <- 1 / (1 + age_rate_human[1] / dH)
  
  for (i in 2:na_human) {
    den_human[i] <- age_rate_human[i - 1] * den_human[i - 1] / (age_rate_human[i] + dH)
  }
  
  # calculate numbers in each age class for each state (SH, SHC, IH, IHC)
  SH_eq <- c()
  for (i in 1:na_human) {
    SH_eq[i] <- den_human[i] * ((HPS) - (SHC0_total + IH0_total + IHC0_total))
  }
  
  SHC_eq <- c()
  for (i in 1:na_human) {
    SHC_eq[i] <- den_human[i] * SHC0_total
  }
  
  IH_eq <- c()
  for (i in 1:na_human) {
    IH_eq[i] <- den_human[i] * IH0_total
  }
  
  IHC_eq <- c()
  for (i in 1:na_human) {
    IHC_eq[i] <- den_human[i] * IHC0_total
  }
  
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
    age_rate_pig = age_rate_pig,
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

