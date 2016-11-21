
### ODEs ###
# Human: susceptible
deriv(SH)<-bH+alpha*IH+eta*SHC+eta*IHC-(pil*chi)*SH*IPL/PPS-(pih*chi)*SH*IPH/PPS-theta*SH*E-dH*SH
# Human: T+ C-
deriv(IH)<-(pil*chi)*SH*IPL/PPS+(pih*chi)*SH*IPH/PPS-alpha*IH-theta*(1+RR)*IH*E-dH*IH
# Human: T- C+
deriv(SHC)<-theta*SH*E+alpha*IHC-(pil*chi)*SHC*IPL/PPS-(pih*chi)*SHC*IPH/PPS-eta*SHC-dH*SHC
# Human: T+ C+
deriv(IHC)<-(pil*chi)*SHC*IPL/PPS+(pih*chi)*SHC*IPH/PPS+theta*(1+RR)*IH*E-alpha*IHC-eta*IHC-dH*IHC

# Eggs
deriv(E)<-delta*IH+delta*IHC-dE*E

# Pigs: susceptible
deriv(SP)<-bP+epsilon*RP-phi*tau*SP*E-(1-phi)*tau*SP*E-dP*SP
# Pigs: C+ low
deriv(IPL)<-phi*tau*SP*E-dP*IPL
# Pigs: C+ high
deriv(IPH)<-(1-phi)*tau*SP*E-dP*IPH
# Pigs: Recovered/Immune
deriv(RP)<- -epsilon*RP-dP*RP
# Pigs: Vaccinated
deriv(VP)<- -dP*VP


### Initial conditions ###
initial(SH)<-SH0
initial(IH)<-IH0
initial(SHC)<-SHC0
initial(IHC)<-IHC0
initial(E)<-E0
initial(SP)<-SP0
initial(IPL)<-IPL0
initial(IPH)<-IPH0
initial(RP)<-RP0
initial(VP)<-VP0

# Initial egg number (at equilibrium)
E0<-user()
# Intitial number of infected pigs (high cyst burden)
IPH0<-user()
# Intitial number of infected pigs (low cyst burden)
IPL0<-user()
# Intitial number of susceptible pigs
SP0<-user()
# Intial number of recovered pigs
RP0<-user()
# Initial number of vaccinated pigs
VP0<-user()
# Egg to pig transmission parameter
tau<-user()
#tau<-if(t<100) tau0 else tau0/2

# Initial number of Human: T- C+
SHC0<-user()
# Initial number of Human: T+ C+
IHC0<-user()
# Initial number of Human: susceptible
SH0<-user()
# Initial number of Human: T+ C-
IH0<-user()

### Parameters ###
# Demographic:
# Human life expectancy (years)
#LEH<-user()
# Pig life expectancy (years)
#LEP<-user()
# Human population size
HPS<-user()
# Pig population size
PPS<-user()

# Human mortality rate
dH<-user()
# Pig mortality rate
dP<-user()
# Human births (per month):
bH<-user()
# Pig births (per month):
bP<-user()

# Taenia
# Average egg survival (years)
#AEL<-user()
# Egg mortality rate
dE<-user()
# Egg production rate
delta<-user()
# Egg to human transmission parameter
theta<-user()

# Disease
# Taeniasis prevalence in human population
#TPrev<-user()
# Cysticercosis prevalence in human population
#CPrev<-user()
# Cysticercosis prevalence in pig population
#PTPrev<-user()
# Proportion of infected pigs with low-intensity cyst burden
phi<-user()
# Average lifespan of the adult tapeworm (years)
#ATL<-user()
# Average duration of Cysticercosis infection in human (years)
#ADI<-user()
# Human recovery rate from Taeniasis
alpha<-user()
# Human recovery rate from Cysticercosis
eta<-user()
# Low intensity infected pig -> human contact rate
pil<-user()
# Pork consumption rate
chi<-user()
# High intensity infected pig -> human contact rate
pih<-user()
# Pig rate of loss of naturally aquired immunity
epsilon<-user()
# Risk multiplier for Cysticercosis if human has Taeniasis
RR<-user()

output(Humans_Taeniasis)<-IH+IHC
output(Humans_Cysticercosis)<-SHC+IHC
output(Pigs_Cysticercosis)<-IPH+IPL
output(Human_Taeniasis_prev)<-(IH+IHC)/HPS
output(Human_Cysticercosis_prev)<-(SHC+IHC)/HPS
output(Pig_Cysticercosis_prev)<-(IPH+IPL)/PPS
