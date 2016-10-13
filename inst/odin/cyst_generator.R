
### ODEs ###
# Human: susceptible
deriv(SH)<-bH+alpha*IH+eta*SHC+eta*IHC-(pil*chil)*SH*IPL/PPS-(pih*chih)*SH*IPH/PPS-theta*SH*E-dH*SH
# Human: T+ C-
deriv(IH)<-(pil*chil)*SH*IPL/PPS+(pih*chih)*SH*IPH/PPS-alpha*IH-theta*(1+RR)*IH*E-dH*IH
# Human: T- C+
deriv(SHC)<-theta*SH*E+alpha*IHC-(pil*chil)*SHC*IPL/PPS-(pih*chih)*SHC*IPH/PPS-eta*SHC-dH*SHC
# Human: T+ C+
deriv(IHC)<-(pil*chil)*SHC*IPL/PPS+(pih*chih)*SHC*IPH/PPS+theta*(1+RR)*IH*E-alpha*IHC-eta*IHC-dH*IHC

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


### Initial conditions ###
initial(SH)<-SH0
initial(IH)<-IH0
initial(SHC)<-SHC0
initial(IHC)<-IHC0
initial(E)<-E0
initial(SP)<-SP0
initial(IPL)<-IP0*phi
initial(IPH)<-IP0*(1-phi)
initial(RP)<-0

# Initial egg number (at equilibrium)
E0<-user()
# Intitial number of infected pigs
IP0<-user()
# Intitial number of susceptible pigs
SP0<-user()
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
LEH<-user()
# Pig life expectancy (years)
LEP<-user()
# Human population size
HPS<-user()
# Pig population size
PPS<-user()

# Human mortality rate
dH<-user()
# Pig mortality rate
dP<-user()
# Human births (per month):
bH<-HPS*dH
# Pig births (per month):
bP<-PPS*dP

# Taenia
# Average egg survival (years)
AEL<-user()
# Egg mortality rate
dE<-user()
# Egg production rate
delta<-user()
# Egg to human transmission parameter
theta<-(bH+eta*(SHC0+IHC0)-dH*(SH0+IH0))/((SH0*E0)+((1+RR)*IH0*E0))

# Disease
# Taeniasis prevalence in human population
TPrev<-user()
# Cysticercosis prevalence in human population
CPrev<-user()
# Cysticercosis prevalence in pig population
PTPrev<-user()
# Proportion of infected pigs with low-intensity cyst burden
phi<-user()
# Average lifespan of the adult tapeworm (years)
ATL<-user()
# Average duration of Cysticercosis infection in human (years)
ADI<-user()
# Human recovery rate from Taeniasis
alpha<-user()
# Human recovery rate from Cysticercosis
eta<-user()
# Low intensity infected pig -> human contact rate
pil<-user()
# Low intensity infected pig -> human infection probability
chil<-user()
# High intensity infected pig -> human contact rate
pih<-user()
# High intensity infected pig -> human infection probability
chih<-user()
# Pig rate of loss of naturally aquired immunity
epsilon<-user(0.01)
# Risk multiplier for Cysticercosis if human has Taeniasis
RR=1

