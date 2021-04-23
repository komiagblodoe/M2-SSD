
#TP VAM


require(VAM)


#AMC Ambassador data set

data("AMC_Amb")
AMC_Amb

###Plots de l'age virtuel pour ABAO et AGAN

## plot1 ##
AMC_AGAN<-model.vam(Time & Type ~ (AGAN()|Weibull(1,3)) ,data=AMC_Amb)
plot(AMC_AGAN ,"v")

## plot2 ##
AMC_ABAO<-model.vam(Time & Type ~ (ABAO()|Weibull(1,3)) ,data=AMC_Amb)
plot(AMC_ABAO ,"v")


###Plots of failure intensity for ABAO and AGAN

## plot1 ##
plot(AMC_ABAO ,"i")

## plot2 ##
plot(AMC_ABAO ,"i")
curve(1*3*x^(3-1) ,add=TRUE ,col="red", lty="dashed" ,lwd=4)

## plot3 ##
plot(AMC_AGAN ,"i")

## plot4 ##
plot(AMC_AGAN ,"i")
curve(1*3*x^(3-1) ,add=TRUE ,col="red", lty="dashed" ,lwd=4)

## plot5 ##
AMC_ABAO_LL<-model.vam(Time & Type ~ (ABAO()|LogLinear(0.1,8e-3)) ,data=AMC_Amb)
plot(AMC_ABAO_LL ,"i")
curve(0.1*exp(8e-3*x) ,add=TRUE ,col="red", lty="dashed" ,lwd=4)

## plot6 ##
AMC_AGAN_LL<-model.vam(Time & Type ~ (AGAN()|LogLinear(0.1,8e-3)) ,data=AMC_Amb)
plot(AMC_AGAN_LL ,"i")
curve(0.1*exp(8e-3*x) ,add=TRUE ,col="red", lty="dashed" ,lwd=4)



############ SIMULATION


simARAInf<-sim.vam(Time & Type ~ (ARAInf(0.6)|Weibull(1,3)))
simData<-simulate(simARAInf,5)
simData


##Plots des données simulées

## plot1 ##
simulate(simARAInf,5)
plot(simARAInf,"i",col="blue")
simulate(simARAInf,3)
plot(simARAInf,"i",col="red",add=TRUE)


##Right censoring in simulation

simulate(simARAInf,Time>(RightCensorship=3))

##Simulation of iid trajectories

simulate(simARAInf,3,nb.system=2)

##Estimation des paramètres

AMC_mle<-mle.vam(Time & Type ~ (ARAInf(0.6)|Weibull(1,3)),data=AMC_Amb)
coef(AMC_mle)

##Plug in estimator of the cumulative intensity

## plot1 ##
plot(AMC_mle,"I")

##AIC

(AIC<-2*3-2*logLik.mle.vam(AMC_mle))

##Bayesian estimators

AMC_bayes <- bayesian.vam( Time & Type ~ (ARA1(~Beta(1.08,0.108)) | Weibull(~NonInform(),~Gamma(32,0.097))),data=AMC_Amb)
coef(AMC_bayes,profile.alpha=TRUE)
summary(AMC_bayes)

##Posterior distribution of the maintenance efficiency parameter

## plot1 ##
hist(AMC_bayes,3,main="Posterior distribution of CM effect paramter")


##Posterior estimation an d credibility band for the cumulative failure intensity

## plot1 ##
plot(AMC_bayes,"I")

##Off Road Engines data set

data(OREngines)
head(OREngines)
tail(OREngines)

##MLE for off road engines data set

ORE_mle<- mle.vam( System&Time&Type ~ (ARA1(0.5) | LogLinear(1,1e-5)) & (ARAInf(0.5)),data=OREngines)
(ORE_est<-coef(ORE_mle))

##95% confidence intervals

Hessian<-logLik.mle.vam(ORE_mle,with_value=FALSE,with_hessian=TRUE)
HesInverse<-solve(-Hessian,tol=1e-24)
B<-matrix(c(rep(-1,4),rep(1,4)),ncol=2,dimnames=list(c('alpha','beta','rhoMC','rhoMP')))
ORE_est+qnorm(1-0.05/2)*sqrt(diag(HesInverse))*B


###Plug in estimator of the failure intensity for the 24th system

## plot1 ##
plot(ORE_mle,"i",system.index = 24)

##PM policy at intensity

simCMPM<-sim.vam(  ~ (ARA1(.9) | Weibull(.001,2.5)) & (ARAInf(.4) | AtIntensity(0.23)))
simData=simulate(simCMPM,Time>(RightCensorship=270))
head(simData)
tail(simData)

##PM intensity level

## plot1 ##
plot(simCMPM,"i",to=simData$Time[20])
abline(h=0.23,col="purple",lty=3)


##Combined PM policies

simCMPM<-sim.vam(  ~(ABAO() | Weibull(.001,2.7)) & (ARAInf(.6)+ARAInf(-.2)+AGAN() | Periodic(12,prob=c(0.6,0.4))*AtIntensity(0.6)))
simulate(simCMPM,20)

##Failure intensity for combined PM policies

## plot1 ##
plot(simCMPM,"i")
abline(h=0.6,col="purple",lty=3)

##PM policy not using the simulation model

## plot1 ##
planPMmod <- model.vam(Time & Type ~ (ARA1(.87) | Weibull(.0015,2.6)) & (ARAInf(.44)))
simCMPM<-sim.vam( ~ (ARA1(.9) | Weibull(.002,2.5)) & (ARAInf(.25) | AtIntensity(0.35,planPMmod)) )
simData<-simulate(simCMPM,30)
update(planPMmod,simData)
plot(planPMmod,"i",ylim=c(0,0.5))
plot(simCMPM,"i",col="blue",add=TRUE)
abline(h=0.35,lty=3)


###Simulation de 1000 échantillons avec le nombre de maintenances égal à 30
##et  tracé de l'histogramme des mle


n<-1000
MLE<-matrix(nrow=n,ncol=3)
for(i in 1:1000){
  simData<-simulate(simARAInf,30)
  MLE[i,]<-coef(mle.vam(Time & Type ~ (ARAInf(0.6)|Weibull(1,3)),data=simData))
}

par(mfrow=c(1,3))

hist(MLE[,1],breaks=50,col="green")
hist(MLE[,2],col = "red")
hist(MLE[,3], col = "blue")
