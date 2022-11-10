
#=======================================================#
#                Age-structured ODEs                    #

#====================================================== #
#               Human age-structured ODEs               #


# initial condition for human age-structured compartments 
na_human <- user()
age_rate_human[] <- user()
dim(age_rate_human) <- na_human

# initial number of Human: susceptible
SH0[] <- user() 
dim(SH0) <- na_human
initial(SH[]) <- SH0[i]
dim(SH) <- na_human

# initial number of Human: T- C+
SHC0[] <- user()
dim(SHC0) <- na_human
initial(SHC[]) <- SHC0[i]
dim(SHC) <- na_human

# initial number of Human: T+ C-
IH0[] <- user()
dim(IH0) <- na_human
initial(IH[]) <- IH0[i]
dim(IH) <- na_human

# initial number of Human: T+ C+
IHC0[] <- user()
dim(IHC0) <- na_human
initial(IHC[]) <- IHC0[i]
dim(IHC) <- na_human

# Human: susceptible ODEs
deriv(SH[1]) <- bH + alpha * IH[1] + eta * SHC[1] + eta * IHC[1] - (pil * chi) * SH[1] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - (pih * chi) * SH[1] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - theta * SH[1] * E - dH * SH[1] - (age_rate_human[1] * SH[1]) # first susceptible human age class
deriv(SH[2:na_human]) <- age_rate_human[i-1] * SH[i-1] + alpha * IH[i] + eta * SHC[i] + eta * IHC[i] - (pil * chi) * SH[i] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - (pih * chi) * SH[i] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - theta * SH[i] * E - dH * SH[i] - (age_rate_human[i] * SH[i]) # remaining susceptible human age classes

# Human: T+ C- ODEs
deriv(IH[1]) <- (pil * chi) * SH[1] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * SH[1] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - alpha * IH[1]  -theta * (1 + RR_cysticercosis) * IH[1] * E - dH * IH[1] - (age_rate_human[1] * IH[1]) # first infected (taeniasis) human age class
deriv(IH[2:na_human]) <- age_rate_human[i-1] * IH[i-1] + (pil * chi) * SH[i] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * SH[i] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - alpha * IH[i] - theta * (1 + RR_cysticercosis) * IH[i] * E - dH * IH[i] - (age_rate_human[i] * IH[i]) # remaining infected (taeniasis) human age classes

# Human: T- C+ ODEs
deriv(SHC[1]) <- theta * SH[1] * E + alpha * IHC[1] - (pil * chi) * SHC[1] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) +  sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - (pih * chi) * SHC[1] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - eta * SHC[1] - dH * SHC[1] - (age_rate_human[1] * SHC[1]) # first infected (cysticercosis) human age class
deriv(SHC[2:na_human]) <- age_rate_human[i-1] * SHC[i-1] + theta * SH[i] * E + alpha * IHC[i] - (pil * chi) * SHC[i] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - (pih * chi) * SHC[i] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) - eta * SHC[i] - dH * SHC[i] - (age_rate_human[i] * SHC[i])# remaining infected (cysticercosis) human age classes

# Human: T+ C+ ODEs
deriv(IHC[1]) <- (pil * chi) * SHC[1] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * SHC[1] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + theta * (1 + RR_cysticercosis) * IH[1] * E - alpha * IHC[1] - eta * IHC[1] - dH * IHC[1] - (age_rate_human[1] * IHC[1]) # first infected (taeniasis & cysticercosis) human age class
deriv(IHC[2:na_human]) <- age_rate_human[i - 1] * IHC[i - 1] + (pil * chi) * SHC[i] * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * SHC[i] * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + theta * (1 + RR_cysticercosis) * IH[i] * E - alpha * IHC[i] - eta * IHC[i] - dH * IHC[i] - (age_rate_human[i] * IHC[i]) # remaining infected (taeniasis & cysticercosis) human age classes

# Eggs ODEs
deriv(E)<-delta*(sum(IH))+delta*(sum(IHC))-dE*E


#============================================#
#             Pig age-structured ODEs        #

# initial condition for pig age-structured compartments 
na_pig <- user()
age_rate_pig[] <- user()
dim(age_rate_pig) <- na_pig

# initial number of pigs: susceptible
SP0[] <- user()
dim(SP0) <- na_pig
initial(SP[]) <- SP0[i]
dim(SP) <- na_pig

# initial number of pigs: pre-patent
PP0[] <- user()
dim(PP0) <- na_pig
initial(PP[]) <- PP0[i]
dim(PP) <- na_pig

# initial number of pigs: infected (patent) low burden
IPL0[] <- user()
dim(IPL0) <- na_pig
initial(IPL[]) <- IPL0[i]
dim(IPL) <- na_pig

# initial number of pigs: infected (patent) high burden
IPH0[] <- user()
dim(IPH0) <- na_pig
initial(IPH[]) <- IPH0[i]
dim(IPH) <- na_pig

# initial number of pigs: recovered (following treatment)
RP0[] <- user()
dim(RP0) <- na_pig
initial(RP[]) <- RP0[i]
dim(RP) <- na_pig

# initial number of pigs: vaccinated
VP0[] <- user()
dim(VP0) <- na_pig
initial(VP[]) <- VP0[i]
dim(VP) <- na_pig

# Pigs: susceptible (first age class + births)
deriv(SP[1]) <- bP + epsilon * RP[1] - tau * SP[1] * E - dP * SP[1] - (age_rate_pig[1] * SP[1]) 
# Pigs: susceptible (subsequent age classes to slaughter age)
deriv(SP[2:slgtage_bfr]) <- age_rate_pig[i-1] * SP[i-1] + epsilon * RP[i] - tau * SP[i] * E - dP * SP[i] - (age_rate_pig[i] * SP[i]) 
# Pigs: susceptible (subsequent age classes from slaughter age)
deriv(SP[slgtage:na_pig]) <- age_rate_pig[i-1] * SP[i-1]+ epsilon * RP[i] - tau * SP[i] * E - dPslg * SP[i] - dP * SP[i] - (age_rate_pig[i] * SP[i]) 

# Prepatent Pigs (first age class)
deriv(PP[1]) <- tau * SP[1] * E - phi * psi * PP[1] - (1 - phi) * psi * PP[1] - dP * PP[1] - (age_rate_pig[1] * PP[1])
# Prepatent Pigs (subsequent age classes to slaughter age)
deriv(PP[2:slgtage_bfr]) <- age_rate_pig[i-1] * PP[i-1] + tau * SP[i] * E - phi * psi * PP[i] - (1 - phi) * psi * PP[i] - dP * PP[i] - (age_rate_pig[i] * PP[i])
# Prepatent Pigs (subsequent age classes from slaughter age)
deriv(PP[slgtage:na_pig]) <- age_rate_pig[i-1] * PP[i-1] + tau * SP[i] * E - phi * psi * PP[i] - (1 - phi) * psi * PP[i] - dPslg * PP[i] - dP * PP[i] - (age_rate_pig[i] * PP[i])

# Pigs: C+ low (first age class)
deriv(IPL[1]) <- phi * psi * PP[1] - dP * IPL[1] - (age_rate_pig[1] * IPL[1])
# Pigs: C+ low (subsequent age classes to slaughter age)
deriv(IPL[2:slgtage_bfr]) <- age_rate_pig[i-1] * IPL[i-1] + phi * psi * PP[i] - dP * IPL[i] - (age_rate_pig[i] * IPL[i])
# Pigs: C+ low (subsequent age classes from slaughter age)
deriv(IPL[slgtage:na_pig]) <- age_rate_pig[i-1] * IPL[i-1] + phi * psi * PP[i] - dP * IPL[i] -  dPslg * IPL[i] - (age_rate_pig[i] * IPL[i])

# Pigs: C+ high (first age class)
deriv(IPH[1]) <- (1 - phi) * psi * PP[1] - dP * IPH[1] - (age_rate_pig[1] * IPH[1])
# Pigs: C+ high (subsequent age classes to slaughter age)
deriv(IPH[2:slgtage_bfr]) <- age_rate_pig[i-1] * IPH[i-1] + (1 - phi) * psi * PP[i] - dP * IPH[i] - (age_rate_pig[i] * IPH[i])
## Pigs: C+ high (subsequent age classes from slaughter age)
deriv(IPH[slgtage:na_pig]) <- age_rate_pig[i-1] * IPH[i-1] + (1 - phi) * psi * PP[i] - dP * IPH[i] - dPslg * IPH[i] - (age_rate_pig[i] * IPH[i])

#  Pigs: Recovered/Immune (first age class)
deriv(RP[1]) <- -epsilon * RP[1] - dP * RP[1] - (age_rate_pig[1] * RP[1])
# Pigs: Recovered/Immune (subsequent age classes to slaughter age) 
deriv(RP[2:slgtage_bfr]) <- age_rate_pig[i-1] * RP[i-1] -epsilon * RP[i] - dP * RP[i] - (age_rate_pig[i] * RP[i])
# Pigs: Recovered/Immune (subsequent age classes from slaughter age) 
deriv(RP[slgtage:na_pig]) <- age_rate_pig[i-1] * RP[i-1]-epsilon * RP[i] - dP * RP[i] - dPslg * RP[i] - (age_rate_pig[i] * RP[i])

# Pigs: Vaccinated (first age class) 
deriv(VP[1]) <- -dP * VP[1] - (age_rate_pig[1] * VP[1])
# Pigs: Vaccinated (subsequent age classes to slaughter age) 
deriv(VP[2:slgtage_bfr]) <- age_rate_pig[i-1] * VP[i-1] -dP * VP[i] - (age_rate_pig[i] * VP[i])
# Pigs: Vaccinated (subsequent age classes from slaughter age) 
deriv(VP[slgtage:na_pig]) <- age_rate_pig[i-1] * VP[i-1] - dP * VP[i] - dPslg * VP[i] - (age_rate_pig[i] * VP[i])

#================================#
# Other key derivatives to track #

# cumulative human cysticercosis cases
deriv(CCC)<- theta * (1 + RR_cysticercosis) * sum(IH) * E + theta * sum(SH) * E

# cumulative human taeniasis cases
deriv(CTC) <- (pil * chi) * sum(SH) * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pil * chi) * sum(SHC) * sum(IPL[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * sum(SH) * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig])) + (pih * chi) * sum(SHC) * sum(IPH[slgage_foi:na_pig]) / (sum(SP[slgage_foi:na_pig]) + sum(PP[slgage_foi:na_pig]) + sum(IPL[slgage_foi:na_pig]) + sum(IPH[slgage_foi:na_pig]) + sum(RP[slgage_foi:na_pig]) + sum(VP[slgage_foi:na_pig]))

#====================#
# Initial conditions # 

# initial eggs
initial(E) <- E0
# initial human cysticercosis cases
initial(CCC) <- CCC0
# initial human taeniasis cases
initial(CTC) <- CTC0

#=============================#
# Parameters (user specified) #

# Initial egg number (at equilibrium)
E0 <- user()
# Egg to pig transmission parameter
tau <- user()
# Initial number of cumulative cysticercosis cases
CCC0 <- user()
# Initial number of cumulative taeniasis cases
CTC0 <- user()
# Human population size
HPS <- user()
# Pig population size
PPS <- user()
# Human mortality rate
dH <- user()
# Pig mortality rate
dP <- user()
# Pig (slaughter age classes) mortality rate
dPslg <- user()
# Human births (per month):
bH <- user()
# Pig births (per month):
bP <- (dP*sum(SP)+dP*sum(PP)+dP*sum(IPL)+dP*sum(IPH)+dP*sum(RP)+dP*sum(VP))+(dPslg*sum(SP[slgage_foi:na_pig]))+(dPslg*sum(PP[slgage_foi:na_pig]))+(dPslg*sum(IPL[slgage_foi:na_pig]))+(dPslg*sum(IPH[slgage_foi:na_pig]))+(dPslg*sum(RP[slgage_foi:na_pig]))+(dPslg*sum(VP[slgage_foi:na_pig]))
# Egg mortality rate
dE <- user()
# Egg production rate
delta <- user()
# Egg to human transmission parameter
theta <- user()
# Min slaughter age (age class from where slaughter rate applied)
slgtage <- user()
# slaughter age min before
slgtage_bfr <- user()
# foi to humans based on ages from which pigs first slaughtered
slgage_foi <- user()
# Taeniasis prevalence in human population
#TPrev<-user()
# Cysticercosis prevalence in human population
#CPrev<-user()
# Cysticercosis prevalence in pig population
#PTPrev<-user()
# Proportion of infected pigs with low-intensity cyst burden
phi <- user()
# Pig rate of transition from prepatent to infectious (average duration for maturation of cysts)
psi <- user()
# Human recovery rate from Taeniasis
alpha <- user()
# Human recovery rate from Cysticercosis
eta <- user()
# Low intensity infected pig -> human contact rate
pil <- user()
# Pork consumption rate
chi <- user()
# High intensity infected pig -> human contact rate
pih <- user()
# Pig rate of loss of naturally aquired immunity
epsilon <- user()
# Risk multiplier for Cysticercosis if human has Taeniasis
RR_cysticercosis <- user()


#=============================#
#            Outputs          #

output(Humans_Taeniasis) <- sum(IH) + sum(IHC)
output(Humans_Cysticercosis) <- sum(SHC) + sum(IHC)
output(Pigs_Cysticercosis) <- sum(IPH) + sum(IPL) # only patent infection
output(Human_Taeniasis_prev) <- (sum(IH) + sum(IHC)) / HPS
output(Human_Cysticercosis_prev) <- (sum(SHC) + sum(IHC)) / HPS
output(Pig_Cysticercosis_prev) <- (sum(IPH) + sum(IPL)) / PPS
output(Human_total) <- sum(IH) + sum(IHC) + sum(SH) + sum(SHC) # track to ensure stable population
output(Pig_total) <- sum(SP) + sum(PP) + sum(IPH) + sum(IPL) + sum(RP) + sum(VP) # track to ensure stable population
output(birthrate) <- bP # track
output(pig_deaths) <- (dP*sum(SP[1:na_pig]))+(dP*sum(PP[1:na_pig]))+(dP*sum(IPL[1:na_pig]))+(dP*sum(IPH[1:na_pig]))+(dP*sum(RP[1:na_pig]))+(dP*sum(VP[1:na_pig]))+(dPslg*sum(SP[slgage_foi:na_pig]))+(dPslg*sum(PP[slgage_foi:na_pig]))+(dPslg*sum(IPL[slgage_foi:na_pig]))+(dPslg*sum(IPH[slgage_foi:na_pig]))+(dPslg*sum(RP[slgage_foi:na_pig]))+(dPslg*sum(VP[slgage_foi:na_pig]))
# output(pig_age1) <- SP[1]
# output(SP_total) <- sum(SP) # track
# output(PP_total) <- sum(PP) # track
# output(IPH_total) <- sum(IPH) # track
# output(IPL_total) <- sum(IPL) # track
# output(RP_total) <- sum(RP) # track
# output(VP_total) <- sum(VP) # track