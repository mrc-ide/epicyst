#' @title
#' Calculate key age parameters to inform life tables (for pig population)
#' @description
#' Calculate age rate and age widths for pig compartments depending on number of age groups 
#'
#' @param number_age_classes the number of age classes (pigs)
#' @param pig_age_class_width vector for mean no. of months in each age compartment 
#'
#' @return age rate and age width
#' @export
age_parameters_pig_func <- function(number_age_classes, pig_age_class_width){
  
  number_age_classes_pig = number_age_classes
  
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
  
  return(list(age_width_pig, na_pig, age_rate_pig))
}


#' @title
#' Calculate life table for pig population
#' @description
#' To set up population distribution at baseline: LIFE TABLES APPROACH for Age-structured model 
#' Based on code from Verity et al: https://github.com/mrc-ide/covfefe 
#' Further reference here FAO paper: http://www.fao.org/tempref/docrep/fao/008/a0212e/a0212E09.pdf  
#' 
#' @param number_age_classes the number of age classes (pigs)
#' @param slgt_age_min the age from which pigs are slaughtered (months), user-specified
#' @param slgtage age-class where slaughter rate first applied to
#' @param slgtage_bfr age-class before slaughter age first begins
#' @param dP pig death rate (natural)
#' @param dPslg pig slaughter-age death rate
#' @param na_pig number age classes for pigs (processed)
#'
#' @return life table output
#' @export
life_tables_pigs_func <- function(number_age_classes, slgt_age_min, slgtage, slgtage_bfr,
                                  dP = dP, dPslg = dPslg, na_pig){

  number_age_classes_pig <- number_age_classes
  slaughter_age_min <- slgt_age_min
  slgtage <- slgtage
  slgtage_bfr <- slgtage_bfr
  
#==============================================================================================#
# To set up population distribution at baseline: LIFE TABLES APPROACH for Age-structured model #
#                                         #
# FAO paper: http://www.fao.org/tempref/docrep/fao/008/a0212e/a0212E09.pdf                     #        
#

# non-age structured 
# if (slaughter_age_min == 0) {
#   age1toslg <- 0
#   ageslgtoN <- length(c(1:na_pig))
# }

  if (number_age_classes_pig == 1){ 
    stop("error: do not need life tables approach for 1 age class (non-age strcutured model")
  }
  
  
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
  
  return(pig_demography)
  }
}


#' @title
#' Calculate proportions of pigs in each age class
#' @description
#' Using age stable values from life table, calculate proprtion of pigs in each age class for each state 
#' 
#' @param PPS pig population size
#' @param pig_demography pig life-table
#' @param na_pig number age classes for pigs (processed)
#' @param IPL0_total total number of infected (low burden) pigs
#' @param IPH0_total total number of infected (high burden) pigs
#'
#' @return proportions of pigs in each age class and state
#' @export
Pig_age_class_proportions_func <- function(PPS, pig_demography, na_pig, IPL0_total, IPH0_total){
  
  if (na_pig == 1){ 
    stop("error: do not calculate prooprtions across age-classes for 1 age class (non-age strcutured model)")
  }
  
  
  PP0_total <- 0
  RP0_total <- 0
  VP0_total <- 0

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

return(list(SP_eq, PP_eq, IPL_eq, IPH_eq, RP_eq, VP_eq))

}


#' @title
#' Calculate key age parameters for human population
#' @description
#' Calculate age rate and age widths for human compartments depending on number of age groups 
#'
#' @param number_age_classes the number of age classes (humans)
#'
#' @return age rate and age width
#' @export
age_parameters_human_func <- function(number_age_classes){

if (number_age_classes > 1) {
  
  # vector of specified no. of months in each age compartment 
  if (number_age_classes == 7) {
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
  
  }
  return(list(age_width_human, na_human, age_rate_human))
}

#' @title
#' Calculate proportions of humans in each age class
#' @description
#' Calculate proprtion of humans in each age class for each state using age rates
#' 
#' @param age_rate age rate for humans between age classes
#' @param na_human number of human age classes
#' @param dH natural death rate of humans
#' @param HPS human populations size
#' @param SHC0_total susceptible (taeniasis) human
#' @param IH0_total infected humans (taeniasis) at t0
#' @param IHC0_total infected humans (cysticercosis) at t0
#'
#' @return proportions of humans in each age class and state
#' @export
# proportion of population in age classes 

human_age_class_proportions_func <- function(age_rate, na_human, dH, HPS, SHC0_total, IH0_total, IHC0_total){

  # create vector to contain proportions
  den_human <- c()
  
  den_human[1] <- 1 / (1 + age_rate[1] / dH)
  
  for (i in 2:na_human) {
    den_human[i] <- age_rate[i - 1] * den_human[i - 1] / (age_rate[i] + dH)
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
  
  return(list(SH_eq, SHC_eq, IH_eq, IHC_eq))

}
