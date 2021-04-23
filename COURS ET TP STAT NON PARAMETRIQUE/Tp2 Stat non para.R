
#EXERCICE6 Feuille TP2


x<-c(0.11, 0.20, 0.69, 1.02)

y<-c(0.86, 0.99, 1.24, 1.57, 1.62)

#H0: les deux échantillons ont la même loi
#H1: F<>G (différent)

#Tracé des deux courbes des fonctions de répartition 
#empiriques (ecdf en R)

plot(ecdf(x))
lines(ecdf(y), col="red" )

#On regarde ensuite le sup de la differénce des deux courbes
#Plus il est grand on rejette H0. Sinon on conserve H0

ks.test(x,y, alternative = "two.sided")

#p-valeur=0.1429 on conserve H0 au seuil 5%.


#Test de Mann Whitney

wilcox.test(x,y, alternative = "two.sided")



#####-----------------TP2 Fonction de répartition-------------------------------####


#Exercice1

#1-générer un échantillon de lon de loi exponentielle

lambda= 3
n=100
x<-rexp(n,lambda)

#2 Estimation de F à partir de la fonction de répartition empirique
#2. Estimation de Fn

ecdf(x)
plot(ecdf(x))

#3 Représenter graphiquement F et Fn chapeau

plot(ecdf(x))
curve(pexp(x,lambda),add=TRUE, col="red")


#4 Illustrer la convergence presque sûre de Fn chapeau vers F
#4 convergence presque sur

ns<- seq(10,1000,10)
diff <- numeric()
for (n in  ns){
  
  x <- rexp(n,lambda)
  f <- ecdf(x)
  diff <- c(diff, pexp(2,lambda)-f(2) )
}

plot(ns, diff,xlab="n",ylab=expression(F[n](2)-F(2)),type='b', main="Convergence presque normale" )
abline(h=0,col="red")


#5 normalité asymptotique

asymp <- c()

for (i in 1:1000){
  
  x <- rexp(1000,lambda)
  f <- ecdf(x)
  asymp <- c(asymp, f(2))
}


hist(asymp,breaks=50,prob=TRUE)
curve( dnorm(x, pexp(2,lambda) , sqrt(pexp(2,lambda)* (1-pexp(2,lambda))/1000)),col="red"  ,add=TRUE)

#6 convergence de F et Fn 

ns<- seq(10,1000,10)
ninf <- numeric()
for (n in  ns){
  
  x <- rexp(n,lambda)
  f <- ecdf(x)
  ninf <- c(ninf, max(abs(pexp(x,lambda)-f(x))))
}

plot(ns, ninf,xlab="n",ylab="norme infinie")



###############################################################################

#TP Validation croisée
##################################################

#Objectif: influence du noyau K sur l'estimateur- influence de h sur l'échantillon 
#- choix de h empiriquement ou par validation croisée

library(MASS)
data(geyser)
str(geyser) 

help(geyser)

#1- Why attach ? why density ? what is the value of the bandwidth ? what is the kernel K?


attach(geyser) 
Kernel_density<-density(waiting) 
plot(Kernel_density)

#2- The task now is to use other kernel K in order to observe their in???uence in the density estimation.


par(mfrow=c(1,4))
plot(density(waiting, kernel="gaussian"), main="Gaussian Kernel") 
plot(density(waiting, kernel="epanechnikov"), main="Epanechnikov Kernel") 
plot(density(waiting, kernel="rectangular"), main="Rectangular Kernel") 
plot(density(waiting, kernel="triangular"), main="Triangular Kernel")

#Vu les courbes on peut penser à la superposition de deux lois normales
#On remarque aussi que le noyau K ie kernel n'a pas grande influence sur les courbes

#3. The purpose now is to observe the in???uence of the bandwidth h in the estimation of the density

#bw=h

par(mfrow=c(2,4)) 
plot(density(waiting,bw=0.5), main="Bandwidth 0.5") 
plot(density(waiting,bw=1), main="Bandwidth 1") 
plot(density(waiting,bw=2), main="Bandwidth 2")
plot(density(waiting,bw=3), main="Bandwidth 3") 
plot(density(waiting,bw=4), main="Bandwidth 4") 
plot(density(waiting,bw=6), main="Bandwidth 6") 
plot(density(waiting,bw=8), main="Bandwidth 8")
plot(density(waiting,bw=12), main="Bandwidth 12")


#Pour h petit,la courbe est très peu régulière ie très volatile
#La variance de f_hat est donc très grande
#On est dans le cas de Under smoothing

#En prenant des valeurs différentes de h, on remarque que plus h est grand
#la densité prend une allure de celle d'une loi normale ie la courbe devient lisse
#Le bias de f_hat est très grand
#On est dans le cas de Over smoothing


#Il faut donc choisir une valeur de h ni petite ni grande


#---4. Cross-Validation

# Global Error 

total_risk <- c()

# We use a grid for h with length 1/50 on the interval [0,8] 

for (i in 1:400) 
  {
  # Construction of the risk vector 
  paq_risk <- c() 
  h <- 8*i/400 
  
  # The 4 first sets are considered on the same loop 
  
  for (j in 1:4)
    { 
    # We construct the estimator over all the data except those on the jth set
    
    data <- waiting[-((60*(j-1)+1):(60*j))] 
    estim <- density(data,bw=h) 

    # We approximate the integral of the square of the estimator 
    
    long <- length(estim$y) 
    int_square <- mean( (estim$x[2:long]-estim$x[1:(long-1)]) * (estim$y[2:long]^2)) 
    
    # We approximate the second part of the risk using the data of the set j 
    
    data <- waiting[(60*(j-1)+1):(60*j)] 
    estim <- density(data,bw=h)
    nb_obs <- length(data) 

    # We create a vectorial empty variable
    
    terms <- c() 
    
    # for each observation of the set v
    
    for (k in 1:(nb_obs)) 
      {
      
      index <- which.min(abs(estim$x-data[k])) 
      terms <- c(terms,estim$y[index]-(2*pi)^{-1/2}/nb_obs/h) }
    second_term <- 2/(nb_obs-1)*sum(terms) 
    paq_risk <- c(paq_risk,int_square-second_term) 
    }
  
  # We add the last set of data 
  
  data <- waiting[-(241:299)]
  estim <- density(data,bw=h) 
  long <- length(estim$y) 
  int_square <- mean( (estim$x[2:long]-estim$x[1:(long-1)]) * (estim$y[2:long]^2)) 
  data <- waiting[241:299] 
  estim <- density(data,bw=h) 
  nb_obs <- length(data) 
  terms <- c() 
  for (k in 1:(nb_obs)) 
  { 
    index <- which.min(abs(estim$x-data[k])) 
    terms <- c(terms,estim$y[index]-(2*pi)^{-1/2}/nb_obs/h) 
  } 
  second_term <- 2/(nb_obs-1)*sum(terms) 
  paq_risk <- c(paq_risk,int_square-second_term) 
  total_risk <- c(total_risk,mean(paq_risk)) 
} 

# We extract the value of h for which the estimated risk is minimal 

par(mfrow=c(1,1))

h_CV <- 8*which.min(total_risk)/400 
h_CV
Final_estim <- density(waiting,bw=h_CV) 
plot(Final_estim)


###############################################################################################

# TP3 REGRESSION NON PARAMETRIQUE

########################################################

#Partie 1

T<-1000
t<-(1:T)/T

truef1=0.5 + (0.2*cos(4*pi*t)) + (0.1*cos(24*pi*t))
truef2=0.2 + 0.6*(t>1/3 & t<=0.75 )
truef3=4*sin(4*pi*t) - sign(t-0.3)- sign(0.72-t) + 5
truef4=sqrt(t*(1-t))*sin((2*pi*1.05)/(t+0.05)) + 0.5

#1

#Choix des points du design : n valeurs régulièrement espacées sur [0,1]

n<-100
x<-(1:n)/n

#Data1

rsnr = 5 #Rapport signal sur bruit
f1= 0.5 + (0.2*cos(4*pi*x)) + (0.1*cos(24*pi*x))
sigma1<-sd(f1)/rsnr #Niveau de bruit
Y1<-f1 + rnorm(n,mean=0,sd=sigma1)

rsnr = 5 #Rapport signal sur bruit
f2= 0.2 + 0.6*(x>1/3 & x<=0.75 )
sigma2<-sd(f2)/rsnr #Niveau de bruit
Y2<-f2 + rnorm(n,mean=0,sd=sigma2)


rsnr = 5 #Rapport signal sur bruit
f3= 4*sin(4*pi*x) - sign(x-0.3)- sign(0.72-x) + 5
sigma3<-sd(f3)/rsnr #Niveau de bruit
Y3<-f3 + rnorm(n,mean=0,sd=sigma3)

rsnr = 5 #Rapport signal sur bruit
f4= sqrt(x*(1-x))*sin((2*pi*1.05)/(x+0.05)) + 0.5
sigma4<-sd(f4)/rsnr #Niveau de bruit
Y4<-f4 + rnorm(n,mean=0,sd=sigma4)




#Visualisation des données avec la fonction de regression 

par(mfrow=c(2,2))
#dev.new()
plot(x,Y1,type="p",pch=19)
lines(t,truef1, type="l", col="red", lty="dotdash",lwd=2)

#dev.new()
plot(x,Y2,type="p",pch=19)
lines(t,truef2, type="l", col="red", lty="dotdash",lwd=2)

#dev.new()
plot(x,Y3,type="p",pch=19)
lines(t,truef3, type="l", col="red", lty="dotdash",lwd=2)

#dev.new()
plot(x,Y4,type="p",pch=19)
lines(t,truef4, type="l", col="red", lty="dotdash",lwd=2)


#2 Utiliser le code ci dessous pour ajuster un estimateur à noyau à chacun des 4 jeux de données
#Y1,..Y4 qui correspond aux fonctions f1 f2 f3 f4

#Estimation à noyau

library(KernSmooth)

par(mfrow=c(2,2))

#pr f1
h=0.1
hatf1=locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef1,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf1, type="l",col="blue",pch=19)

#pr f2
h=0.1
hatf2=locpoly(x,Y2,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y


plot(t,truef2,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf2, type="l",col="blue",pch=19)

#pr f3
h=0.1
hatf3=locpoly(x,Y3,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef3,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf3, type="l",col="blue",pch=19)

#pr f4
h=0.1
hatf4=locpoly(x,Y4,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef4,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf4, type="l",col="blue",pch=19)




#3 Faire varier h pour chacune des fonctions

#pr f1
par(mfrow=c(2,2))

h=0.01
hatf1=locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef1,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf1, type="l",col="blue",pch=19)

h=0.09
hatf1=locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef1,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf1, type="l",col="blue",pch=19)

h=0.1
hatf1=locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef1,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf1, type="l",col="blue",pch=19)

h=1
hatf1=locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0,1))$y

plot(t,truef1,type="l", col="red", lty="dotdash",lwd=2)
lines(t,hatf1, type="l",col="blue",pch=19)


#Si h est petit la courbe est volatile. la variance est trop grande
#Si h est grand la courbe devient une droite. le biais est grand


#Le meilleur h

IMSE<-numeric()
j<-0
  for(i in seq(0.01,3,0.1)){
    j<-j+1
hatf1<-locpoly(x,Y1,degree=0,bandwidth=0.01,gridsize=T,range.x=c(0,1))$y
  IMSE[j]<-sum(abs(truef1-hatf1))
  }

plot(IMSE)


#Partie 2 : Validation croisée

