# Fiabilité des Systèmes
# TP 1 - Lois exponentielle et de Weibull
#########################################

# Exercice 1
############

# Tracé des fonction de répartition et densité 
# sur un seul graphique.

par(mfrow=c(1,2))

lambda<-0.1
curve(1-exp(-lambda*x),0,40)
curve(lambda*exp(-lambda*x),0,40)

lambda<-1
curve(1-exp(-lambda*x),0,10)
curve(lambda*exp(-lambda*x),0,10)

lambda<-10
curve(1-exp(-lambda*x),0,1)
curve(lambda*exp(-lambda*x),0,1)

# Caler la plage de valeurs du tracé en fonction de 
# l'espérance de la loi.

# Simulation d'un échantillon de taille 10 de la loi exp(0.5).

n<-10
lambda<-0.5
rexp(n,lambda)

# Simulation d'un échantillon de taille 10000 de la loi 
# exp(0.5).

n<-10000
lambda<-0.5
x<-rexp(n,lambda)

# Superposition de l'histogramme de l'échantillon simulé et de 
# la densité théorique de la loi exp(0.5).

par(mfrow=c(1,1))

hist(x,prob=T)
curve(dexp(x,lambda),add=T,col="red")

# Calcul de la moyenne empirique de l'échantillon simulé et 
# comparaison avec le MTTF théorique

mean(x)
1/lambda

# Calcul de la variance empirique de l'échantillon simulé et 
# comparaison avec la variance théorique

var(x)
1/lambda^2


# Calcul du pourcentage d'observations inférieures à 2
# puis à 8
# et comparaison avec la fonction de répartition théorique.

length(x[x<2])/n
mean(x<2)
1-exp(-lambda*2)
pexp(lambda,2)

length(x[x<8])/n
1-exp(-lambda*8)
pexp(lambda,8)

# Calcul du quantile théorique d'ordre 0.1.

-log(0.9)/lambda

# Calcul du quantile empirique d'ordre 0.1.

quantile (x,0.1)


# Même chose avec un échantillon de taille 50

n<-50
x<-rexp(n,lambda)

hist(x,prob=T,xlim=c(0,12), ylim=c(0,lambda))
curve(dexp(x,lambda),0,12,add=T,col="red")

mean(x)
1/lambda

var(x)
1/lambda^2

length(x[x<2])/n
pexp(lambda,2)

length(x[x<8])/n
pexp(lambda,8)





# Exercice 2
############

# Cas lambda1=lambda2

lambda1<-1/10
lambda2<-1/10

# Probabilité théorique
lambda1/(lambda1+lambda2)

# Simulation des deux échantillons
n<-10000
x1<-rexp(n,lambda1)
x2<-rexp(n,lambda2)

# Pourcentage de fois où x1<x2$
nb<-0
for (i in (1:n))
{if (x1[i]<x2[i]) {nb<-nb+1}
}
nb/n

# Ou bien
length(x1[x1<x2])/n
mean(x1<x2)

# Cas lambda1 différent de lambda2

lambda1<-1/8
lambda2<-1/2

# Probabilité théorique
lambda1/(lambda1+lambda2)

# Simulation des deux échantillons
n<-10000
x1<-rexp(n,lambda1)
x2<-rexp(n,lambda2)

# Pourcentage de fois où x1<x2$
mean(x1<x2)





# Exercice 3
############

10/gamma(1/0.5+1)
10/gamma(1/1.5+1)
10/gamma(1/3+1)

# Tracé des fonction de fiabilité, densité et taux de 
# défaillance sur un seul graphique. Loi de Weibull W(5,0.5).

par(mfrow=c(3,3))

beta<-0.5
eta<-10/gamma(1/beta+1)
curve(1-pweibull(x,shape=beta,scale=eta),0,40)
# ou curve(exp(-(x/eta)^beta),0,40)
curve(dweibull(x,shape=beta,scale=eta),0,40)
curve(dweibull(x,shape=beta,scale=eta)/(1-pweibull(x,shape=beta,scale=eta)),0,40)
# ou curve((beta/eta)*(x/eta)^(beta-1),0,40)

# Tracé des fonction de fiabilité, densité et taux de 
# défaillance sur un seul graphique. Loi de Weibull 
# W(11.077,1.5).

beta<-1.5
eta<-10/gamma(1/beta+1)
curve(1-pweibull(x,shape=beta,scale=eta),0,40)
curve(dweibull(x,shape=beta,scale=eta),0,40)
curve(dweibull(x,shape=beta,scale=eta)/(1-pweibull(x,shape=beta,scale=eta)),0,40)


# Tracé des fonction de fiabilité, densité et taux de 
# défaillance sur un seul graphique. Loi de Weibull 
# W(11.198,3).

beta<-3
eta<-10/gamma(1/beta+1)
curve(1-pweibull(x,shape=beta,scale=eta),0,30)
curve(dweibull(x,shape=beta,scale=eta),0,30)
curve(dweibull(x,shape=beta,scale=eta)/(1-pweibull(x,shape=beta,scale=eta)),0,30)



# Simulation d'un échantillon de taille 10000 de la loi 
# W(11,3).

eta<-11
beta<-3
n<-10000
x<-rweibull(n,shape=beta,scale=eta)

# Superposition de l'histogramme de l'échantillon simulé et de 
# la densité théorique de la loi W(11,3).

par(mfrow=c(1,1))

hist(x,prob=T)
curve(dweibull(x,shape=beta,scale=eta),add=T,col="red")


# MTTF théorique

eta*gamma(1/beta+1)

# MTTF empirique

mean(x)

# Variances théorique et empirique

eta^2*(gamma(2/beta+1)-gamma(1/beta+1)^2)
var(x)

# Loi de X^beta

y<-x^beta

hist(y,prob=T)

curve(dexp(x,eta^(-beta)),add=T,col="red")

mean(y)
eta^beta





# Exercice 4
############


# Simulation d'un échantillon de taille 10000 de la loi 
# exp(0.5).

n<-10000
lambda<-0.5
x<-rexp(n,lambda)

# Calcul du pourcentage d'observations supérieures à 5 parmi
# celles supérieures à 2.

y<-x[x>2]
length(y[y>5])/length(y)
mean(y>5)

# Calcul du pourcentage d'observations supérieures à 3.

length(x[x>3])/n
mean(x>3)

exp(-3*lambda)


# Simulation d'un échantillon de taille 10000 de la loi 
# W(11,3).

eta<-11
beta<-3
n<-10000
x<-rweibull(n,shape=beta,scale=eta)


# Calcul du pourcentage d'observations comprises entre 1 et 2 
# parmi celles supérieures à 1.

y<-x[x>1]
mean(y<2)

# Calcul du pourcentage d'observations comprises entre 3 et 4 
# parmi celles supérieures à 3.

y<-x[x>3]
mean(y<4)

# Calcul du pourcentage d'observations comprises entre 5 et 6 
# parmi celles supérieures à 5.

y<-x[x>5]
mean(y<6)

# Calcul du pourcentage d'observations comprises entre 10 et
# 11 parmi celles supérieures à 10.

y<-x[x>10]
mean(y<11)

# Calcul du pourcentage d'observations comprises entre 15 et
# 16 parmi celles supérieures à 15.

y<-x[x>15]
mean(y<16)

# Vraies valeurs

1-exp((1/eta)^beta-((2/eta)^beta))
1-exp((3/eta)^beta-((4/eta)^beta))
1-exp((5/eta)^beta-((6/eta)^beta))
1-exp((10/eta)^beta-((11/eta)^beta))
1-exp((15/eta)^beta-((16/eta)^beta))


# Fonction qui trace un taux de défaillance empirique d'un 
# échantillon


tauxdefemp <- function(x)
{
	n <- length(x) 
	pas <- (max(x)-min(x))/99
	abs <- seq(min(x),max(x),pas)
	ord<-NULL
	for (i in 1:99)
	{
		y<-x[x>abs[i]]
		ord[i]<-mean(y<abs[i]+1)
	}
	plot(abs[1:99],ord)
}


tauxdefemp(x)
curve(1-exp((x/eta)^beta-(((x+1)/eta)^beta)),add=T,col="blue")
curve(dweibull(x,shape=beta,scale=eta)/(1-pweibull(x,shape=beta,scale=eta)),add=T,col="red")




