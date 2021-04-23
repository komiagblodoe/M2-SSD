# fonction mle.weibull renvoie les estimations par maximum de vraisemblance pour un échnatillon complet
# Pour cela on se sert de la fonction optim qui va maximiser la fonction vraisemblance

mle.weibull <- function(y,theta0=c(100,2),only.param=TRUE) {
	n <- length(y)
	mL <- function(theta) {
		eta <- theta[1];beta <- theta[2]
		n*(log(eta)-log(beta))+(beta-1)*sum(log(eta/y)) + sum((y/eta)^beta) 
	}
	res <- optim(theta0,mL)
	if(only.param) res$par else res 
}

mle.weibull.bfgs <- function(y,theta0=c(100,2),only.param=TRUE) {
  n <- length(y)
  mL <- function(theta) {
    eta <- theta[1];beta <- theta[2]
    n*(log(beta)-log(eta))+(beta-1)*sum(log(y/eta)) - sum((y/eta)^beta) 
  }
  mG <- function(theta) {
    eta <- theta[1];beta <- theta[2]
    c(-beta/eta*(n - sum((y/eta)^beta)), n/beta + sum(log(y/eta)) - sum(log(y/eta)*(y/eta)^beta))
  }
  res <- optim(theta0,mL,mG,method="BFGS",control=list(fnscale=-1,maxit=100000))
  if(only.param) res$par else res 
}


# La fonction mlep.weibull renvoie les estimations par maximum de vraisemblance en se servant du gradient
# Les deux fonctions sont censées renvoyer les mêmes valeurs : une se sert de la fonction optim() et l'autre uniroot()

mlep.weibull <- function(y,intervalBeta=c(0,5)) {
	n <- length(y)
	f <- function(beta) n/beta + sum(log(y))-n*sum(y^beta*log(y))/sum(y^beta)
	betaEst <- uniroot(f,intervalBeta)$root
	c(mean(y^betaEst)^(1/betaEst),betaEst)
}

# La fonction moment.weibull renvoie les estimations par méthode des moments

moment.weibull <-function(y,intervalBeta=c(0.1,5)) {
  n<-length(y)
  f <- function(beta) mean(y)^2*(gamma(1+2/beta)-(gamma(1+1/beta))^2)-var(y)*(gamma(1+1/beta))^2
  betaEst <- uniroot(f,intervalBeta)$root
  c(mean(y)/gamma(1+1/betaEst),betaEst)
}

# La fonction tauxdef.weibull renvoie le taux de défaillance d'une loi de Weibull

tauxdef.weibull <- function(x,eta,beta){
  return(dweibull(x,shape=beta,scale=eta)/(1-pweibull(x,shape=beta,scale=eta)))  
}

N<-10000
n<-100
beta<-3
eta<-1000
mat<-replicate(N,rweibull(n,shape = beta,scale = eta))

est.mle<-apply(mat,2,mle.weibull.bfgs)
est.mm<-apply(mat,2,moment.weibull)

mse.eta.mle<-var(est.mle[1,])+(mean(est.mle[1,])-eta)^2
mse.beta.mle<-var(est.mle[2,])+(mean(est.mle[2,])-beta)^2

mse.eta.mm<-var(est.mm[1,])+(mean(est.mm[1,])-eta)^2
mse.beta.mm<-var(est.mm[2,])+(mean(est.mm[2,])-beta)^2

mse.eta.mle
mse.eta.mm

mse.beta.mle
mse.beta.mm

hist(est.mle[1,],freq=F)
summary(est.mle[1,])


mle.weibull.censure <- function(y,theta0=c(100,2),only.param=TRUE) {
  # y est une matrice avec les temps de défaillance ou censures en première colonne et 
  # les indicateurs de défaillance en 2ème colonne (1 si défaillance 0 sinon)
  n <- length(obs[,1])
  mL <- function(theta) {
    eta <- theta[1];beta <- theta[2]
    sum(obs[,2])*(log(eta)-log(beta))+(beta-1)*sum(obs[,2]*log(eta/obs[,1])) + sum((y/eta)^beta) 
  }
 
  res <- optim(theta0,mL)
  if(only.param) res$par else res 
  
}






