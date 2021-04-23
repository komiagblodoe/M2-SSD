
#TP Fiabilité

##-------------------------------ExERCICE1-------------------------------

#X une v.a de loi exponentielle


###1 Tracer la fonction de répartition de X pour lamba = 0.1,1,10

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

#a) Superposition de l'histogramme de l'échantillon simulé 
#et de la densité théorique de la loi exp(0.5)

hist(x,probability = TRUE)
curve(dexp(x,lamda),add = TRUE,col="red")

#b) Calculer la moyenne empirique de l'échantillon simulé
#et comparer avec le MTTF théorique

moyemp=mean(x)
mttf=1/lamda

#Conclusion : la moyenne empirique est presque égale à mttf théorique
#donc on a une bonne estimation de la moyenne

##c) Calculer la variance empirique de l'échantillon simulé
#et comparer avec la variance théorique

varemp=var(x)
vartheo=1/(lamda)^2

#Conclusion:  la var empirique est presque égale à la var théorique, 
#donc on a une bonne estimation de la var


#d) Calculer le pourcentage d'observations inférieures à 2 puis à 8
#et comparer avec la fonction de répartition théorique

#empirique

length(x[x<2])/10000
mean(x<2)#ou bien

#théorique
1-exp(-lamda*2)
pexp(lamda,2)

#Conclusion: la valeur empirique est proche de la val théo. Donc bonne estimation

#empirique

length(x[x<8])/10000
mean(x<8)#ou bien

#théorique

1-exp(-lamda*8)
pexp(lamda,8)

#e)Quantile théorique d'ordre 0.1

-log(0.9)/lamda

#QUANTILE EMPIRIQUE

quantile(x,0.1)

#Bonne estimation
#CONCLUSION: pOUR N=10000 les quantités théoriques sont bien estimées par les quantités empiriques

#3) MM CHOSE AVEC N=50

#lorsqu'on superpose l'hist et la densité c'est un peu moins bon car n est beaucoup plus petit'
#Toutes les estimations sont moins bonnes
#CONCLUSION: quand on a de petite données,les estimations sont de moins bonnes qualités
#Mais en fiabilité, beaucoup de données=mauvais signe=beaucoup de défaillance
#petite donnée= marge d'erreur plus importatnte qu'il faut quantifier avec des intervalles de confiance



##------------------------EXERCICE2----------------------------------


#3) Calcul empirique de cette proba en simulalnt un échantillon
#de loi exp(1/8) et un échantillon de loi exp(1/2)

lambda1=1/8
lambda2=1/2

# proba théorique

p<- lambda1/(lambda1+lambda2)

#Simulation de deux échantillons

n=10000

x<-rexp(n,lambda1)

y<-rexp(n,lambda2)

#Pourcentage de fois où x<y ie la proba empirique

 mean(x<y)
 
 
##------------------------EXERCICE3-------------------------------------
 
 # X une variable aléatoire de loi de Weibull(n,b)
 
#1) Pour b = {0.5,1.5,3}, donner n tel que le MTTF soit de l'ordre de 10
 
beta<-c(0.5,1.5,3)
 
MTTF<-10

eta<-MTTF/gamma(1/beta+1)

#Tracé de la fonction de fiabilité

par(mfrow=c(1,1))

beta<-3
curve(1-pweibull(x,shape = beta,scale = eta),0,40)
curve(dweibull(x,shape = beta, scale = eta),0,40)
curve(dweibull(x,shape = beta, scale = eta)/(1-pweibull(x,shape = beta,scale = eta))


#on fait le tracé pour les différentes valeurs de (n,beta)
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


#MTTF théorique
eta*gamma(1/beta+1)

#MTTF empirique
mean(x)

#Variances théo et empirique

eta^2*(gamma(2/beta+1)-gamma(1/beta+1)^2)
var(x)

#loi de x^beta

y<-x^beta

hist(y,prob=T)
curve(dexp(x,eta^(-beta)),add=T,col="red")

mean(y)
eta^beta