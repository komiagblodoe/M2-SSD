# fonction mle.weibull renvoie les estimations par maximum de vraisemblance pour un échnatillon complet
# Pour cela on se sert de la fonction optim qui va maximiser la fonction vraisemblance
# Dans cette fonction, le gradient n'est pas utilisé. 

mle.weibull <- function(y,theta0=c(100,2),only.param=TRUE) {
	n <- length(y)
	mL <- function(theta) {
		eta <- theta[1];beta <- theta[2]
		n*(log(eta)-log(beta))+(beta-1)*sum(log(eta/y)) + sum((y/eta)^beta) 
	}
	mG <- function(theta) {
		eta <- theta[1];beta <- theta[2]
		c(beta/eta*(n - sum((y/eta)^beta)), -n/beta - sum(log(y/eta)) + sum(log(y/eta)*(y/eta)^beta))
	}
	res <- optim(theta0,mL)
	if(only.param) res$par else res 
}

# fonction mle.weibull.bfgs renvoie les estimations par maximum de vraisemblance pour un échnatillon complet
# Pour cela on se sert de la fonction optim qui va maximiser la fonction vraisemblance en se servant du gradient
# et en optimisant avec la méthode BFGS.


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
  # ici on maximise la fonction (fnscale=-1) car on a rentré la vraie vraisemblance (mL) et pas son opposé, comme
  # c'était le cas dans mle.weibull.
  # En effet, soit on maximise la fonction de vraisemblance soit on minimise moins la fonction de vraisemblance
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

# ESTIMATION NON PARAMETRIQUE : KAPLAN - MEIER et NELSON
KaplanMeier <- function(x,data=donnees){
  # Attention si doublon (27/10/2017)
  # ech est un data.frame
  # trier les temps
  
  data.trie <- data[order(data[,1]),]
  
  temps<-data.trie[,1]
  ind.def<-data.trie[,2]
  n<-nrow(data.trie)
  
  # comparer x avec les temps triés et ne retenir que les défaillances
  # ind<-which(x>=temps & ind.def==1)
  
  # Nb de sujets à risque juste avant t(j)
  n.j<-n:1
  #p2<-prod(1-1/n.j[ind])
  
  return(prod(1-1/n.j[x>=temps & ind.def==1]))
}

Nelson<-function(x,ech=donnees){
  # ech est un data.frame
  # trier les temps
  
  ech.trie <- ech[order(ech[,1]),]
  
  temps<-ech.trie[,1] # les temps
  ind.def<-ech.trie[,2] # =1 si defaillance et 0 sinon
  n<-length(temps)
  
  # comparer x avec les temps triés et ne retenir que les défaillances
  ind<-(1:n)*(x>=temps)*(ind.def==1)
  ind<-ind[ind>0] # ne garde que les indices des défaillances <=x
  
  # Nb de sujets à risque juste avant t(j)
  n.j<-n:1
  Lambda<-0
  for(i in ind)    Lambda<-Lambda+1/n.j[i]
  return(exp(-Lambda))
}

# Exponentielle dans le cas censuré

mle.exp.censure <- function(x){
  return(sum(x[,2])/sum(x[,1]))
}

# Weibull dans le cas censuré
mle.weibull.censure <-function(x,intervalBeta=c(0,5)){
  # les temps sont en 1ere colonne
  # les indicateurs de défaillance en 2ème colonne
  n <- length(x[,1])
  f <- function(beta) (sum(x[,2]))/beta + x[,2]%*%log(x[,1])-sum(x[,2])*sum((x[,1])^beta*log(x[,1]))/sum((x[,1])^beta)
  betaEst <- uniroot(f,intervalBeta)$root
  c(sum((x[,1])^betaEst/sum(x[,2]))^(1/betaEst),betaEst)
}









