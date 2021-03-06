
#TP Fiabilit�

##-------------------------------ExERCICE1-------------------------------

#X une v.a de loi exponentielle


###1 Tracer la fonction de r�partition de X pour lamba = 0.1,1,10

lamda=0.1
curve(1-exp(-lamda*x),0,40)
curve(lamda*exp(-lamda*x),0,40)

lamda=1
curve(1-exp(-lamda*x),0,40)
curve(lamda*exp(-lamda*x),0,40)

lamda=10
curve(1-exp(-lamda*x),0,40)
curve(lamda*exp(-lamda*x),0,40)

#OU BIEN(DONNE LA MEME CHOSE)
curve(pexp(x,lamda),0,40)
curve(dexp(x,lamda),0,40)

###2 Simuler un echantillon de taille 10.000 de la loi exp(0.5)

lamda=0.5
x=rexp(10000,lamda)

#a) Superposition de l'histogramme de l'�chantillon simul� 
#et de la densit� th�orique de la loi exp(0.5)

hist(x,probability = TRUE)
curve(dexp(x,lamda),add = TRUE,col="red")

#b) Calculer la moyenne empirique de l'�chantillon simul�
#et comparer avec le MTTF th�orique

moyemp=mean(x)
mttf=1/lamda

#Conclusion : la moyenne empirique est presque �gale � mttf th�orique
#donc on a une bonne estimation de la moyenne

##c) Calculer la variance empirique de l'�chantillon simul�
#et comparer avec la variance th�orique

varemp=var(x)
vartheo=1/(lamda)^2

#Conclusion:  la var empirique est presque �gale � la var th�orique, 
#donc on a une bonne estimation de la var


#d) Calculer le pourcentage d'observations inf�rieures � 2 puis � 8
#et comparer avec la fonction de r�partition th�orique

#empirique

length(x[x<2])/10000
mean(x<2)#ou bien

#th�orique
1-exp(-lamda*2)
pexp(lamda,2)

#Conclusion: la valeur empirique est proche de la val th�o. Donc bonne estimation

#empirique

length(x[x<8])/10000
mean(x<8)#ou bien

#th�orique

1-exp(-lamda*8)
pexp(lamda,8)

#e)Quantile th�orique d'ordre 0.1

-log(0.9)/lamda

#QUANTILE EMPIRIQUE

quantile(x,0.1)

#Bonne estimation
#CONCLUSION: pOUR N=10000 les quantit�s th�oriques sont bien estim�es par les quantit�s empiriques

#3) MM CHOSE AVEC N=50

#lorsqu'on superpose l'hist et la densit� c'est un peu moins bon car n est beaucoup plus petit'
#Toutes les estimations sont moins bonnes
#CONCLUSION: quand on a de petite donn�es,les estimations sont de moins bonnes qualit�s
#Mais en fiabilit�, beaucoup de donn�es=mauvais signe=beaucoup de d�faillance
#petite donn�e= marge d'erreur plus importatnte qu'il faut quantifier avec des intervalles de confiance



##------------------------EXERCICE2----------------------------------


#3) Calcul empirique de cette proba en simulalnt un �chantillon
#de loi exp(1/8) et un �chantillon de loi exp(1/2)

lambda1=1/8
lambda2=1/2

# proba th�orique

p<- lambda1/(lambda1+lambda2)

#Simulation de deux �chantillons

n=10000

x<-rexp(n,lambda1)

y<-rexp(n,lambda2)

#Pourcentage de fois o� x<y ie la proba empirique

 mean(x<y)
 
 
##------------------------EXERCICE3-------------------------------------
 
 # X une variable al�atoire de loi de Weibull(n,b)
 
#1) Pour b = {0.5,1.5,3}, donner n tel que le MTTF soit de l'ordre de 10
 
beta<-c(0.5,1.5,3)
 
MTTF<-10

eta<-MTTF/gamma(1/beta+1)

#Trac� de la fonction de fiabilit�

par(mfrow=c(1,1))

beta<-3
curve(1-pweibull(x,shape = beta,scale = eta),0,40)
curve(dweibull(x,shape = beta, scale = eta),0,40)
curve(dweibull(x,shape = beta, scale = eta)/(1-pweibull(x,shape = beta,scale = eta))


#on fait le trac� pour les diff�rentes valeurs de (n,beta)
curve(1-pweibull(x,shape = 0.5,scale = 5),0,40)
curve(dweibull(x,shape = 0.5, scale = 5),0,40)
curve(dweibull(x,shape = 0.5, scale = 5)/(1-pweibull(x,shape = 0.5,scale = 5),0,40))

curve(1-pweibull(x,shape = 1.5,scale = 11.1),0,40)
curve(dweibull(x,shape = 1.5, scale = 11.1),0,40)
curve(dweibull(x,shape = 1.5, scale = 11.1)/(1-pweibull(x,shape = 1.5,scale = 11.1),0,40))

curve(1-pweibull(x,shape = 3,scale = 11.2),0,40)
curve(dweibull(x,shape = 3, scale = 11.2),0,40)
curve(dweibull(x,shape = 3, scale = 11.2)/(1-pweibull(x,shape = 3,scale = n),0,40))


#3

eta<-11
beta<-3
x<-rweibull(10000,shape = beta,scale=eta)

hist(x,prob=T)
curve(dweibull(x,shape = beta, scale = eta),add = T)


#MTTF th�orique
eta*gamma(1/beta+1)

#MTTF empirique
mean(x)

#Variances th�o et empirique

eta^2*(gamma(2/beta+1)-gamma(1/beta+1)^2)
var(x)

#loi de x^beta

y<-x^beta

hist(y,prob=T)
curve(dexp(x,eta^(-beta)),add=T,col="red")

mean(y)
eta^beta