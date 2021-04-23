
#TP partie statistique

#Exercice1

#1

n<-20
lambda<-10^(-3)
x<-rexp(n, lambda)

#2

(moy<-mean(x))
(variance<-var(x))

#3

plot(ecdf(x))

#4



#Exercice2

#1

n<-20
beta<-1.5
eta<-1000
ech<-rweibull(n,shape = beta,scale=eta)
ech2<-sort(ech)

#2

(moy<-mean(x))
(variance<-var(x))

#3

plot(ecdf(x))

#4

ech<-rweibull(n,shape = beta,scale=eta)
ech2<-sort(ech)

#estimation de beta


f<-function(x,ech){
  eta<-x[1]
  beta<-x[2]
  n<-length(ech)
  var(ech)*(gamma(1+1/beta))**2-(mean(ech))**2*(gamma(1+2/beta)-(gamma(1+1/beta))**2)
}

uniroot(f,)


#6

N<-10000
n<-20
eta<-1000
beta<-1.5
#Fonction données complètes

lambda_hat<- function(t){
  1/mean(t)
}

#Fonction données incomplètes

lambda_hat2<-function(x,gamma){
  sum(gamma)/sum(x)
 }

#Vraissemblance

vraissemblance<-function(x,ech){
  eta<-x[1]
  beta<-x[2]
  n<-length(ech)
  n*log(beta)-n*log(eta)+(beta-1)*sum(log(ech/eta))-sum((ech/eta)^beta)
}
ech<-rweibull(n,shape = beta,scale=eta)
(vraissemblance(ech,eta,beta))

x0 <- c(1000,1.5)

solution <- optim(x0, vraissemblance,ech=ech, method = "BFGS",control = list(fnscale=-1))
solution

#7

N<-10000
n<-20
eta<-1000
beta<-1.5
ech<-c()

for(i in 1:N){
  ech<-cbind(ech,rweibull(n,shape = beta,scale=eta))
  
}

 result<-c()
for(i in 1:N){
  result<-c(result,vraissemblance(x,ech[i]))
  
}
  
