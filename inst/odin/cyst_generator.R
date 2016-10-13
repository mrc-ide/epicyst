
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
E0<-((delta*HPS*TPrev*(1-CPrev))+(delta*HPS*TPrev*CPrev))/dE
# Intitial number of infected pigs
IP0<-PPS*PTPrev
# Intitial number of susceptible pigs
SP0<-PPS-IP0
# Egg to pig transmission parameter
tau<-(dP*IP0)/(SP0*E0)
#tau<-if(t<100) tau0 else tau0/2

# Initial number of Human: T- C+
SHC0<-HPS*(1-TPrev)*CPrev
# Initial number of Human: T+ C+
IHC0<-HPS*TPrev*CPrev
# Initial number of Human: susceptible
SH0<-HPS*(1-TPrev)*(1-CPrev)
# Initial number of Human: T+ C-
IH0<-HPS*TPrev*(1-CPrev)

### Parameters ###
# Demographic:
# Human life expectancy (years)
LEH<-user(54)
# Pig life expectancy (years)
LEP<-user(1)
# Human population size
HPS<-user(10000)
# Pig population size
PPS<-user(2000)

# Human mortality rate
dH<-1/(LEH*12)
# Pig mortality rate
dP<-1/(LEP*12)
# Human births (per month):
bH<-HPS*dH
# Pig births (per month):
bP<-PPS*dP

# Taenia
# Average egg survival (years)
AEL<-user(0.03846154)
# Egg mortality rate
dE<-1/(AEL*12)
# Egg production rate
delta<-user(960000)
# Egg to human transmission parameter
theta<-(bH+eta*(SHC0+IHC0)-dH*(SH0+IH0))/((SH0*E0)+((1+RR)*IH0*E0))

# Disease
# Taeniasis prevalence in human population
TPrev<-user(0.02)
# Cysticercosis prevalence in human population
CPrev<-user(0.07)
# Cysticercosis prevalence in pig population
PTPrev<-user(0.2)
# Proportion of infected pigs with low-intensity cyst burden
phi<-user(0.8)
# Average lifespan of the adult tapeworm (years)
ATL<-user(2)
# Average duration of Cysticercosis infection in human (years)
ADI<-user(50)
# Human recovery rate from Taeniasis
alpha<-1/(ATL*12)
# Human recovery rate from Cysticercosis
eta<-1/(ADI*12)
# Low intensity infected pig -> human contact rate
pil<-user(0.5)
# Low intensity infected pig -> human infection probability
chil<-Beta/(pil*phi+(2*pih*(1-phi)))
# High intensity infected pig -> human contact rate
pih<-user(1)
# High intensity infected pig -> human infection probability
chih<-2*chil
# Pig rate of loss of naturally aquired immunity
epsilon<-user(0.01)
# Risk multiplier for Cysticercosis if human has Taeniasis
RR=1
# Pork to human tranmission parameter
Beta<-(alpha*(IH0+IHC0)+eta*IHC0+dH*(IH0+IHC0))/((IP0/PPS)*(SH0+SHC0))
