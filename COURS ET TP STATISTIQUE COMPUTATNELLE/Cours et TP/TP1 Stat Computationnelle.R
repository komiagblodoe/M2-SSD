
#TP no 1 - Méthodes de Monte Carlo

#1 Simulation de variables aléatoires

#Exercice 1 (utilisation des fonctions de R)


?Distributions

#1. Générer des n-échantillons de taille croissante selon les lois normale, exponentielle, et beta

n<-1000
X<-rnorm(n)
Y<-rexp(n)
Z<-rbeta(n,2,5,0)

#2. Comparer les distributions empiriques obtenues et la distribution théorique

hist(X)
hist(Y)
hist(Z,5,10)

a<-density(X)
b<-density(Y)
c<-density(Z)

plot(a)
plot(b)
plot(c)


#Exercice 2 (simulation par la méthode d'inversion)


#Simuler une variable suivant une loi exponentielle de paramètre 0.5 par la méthode d'inversion et
#comparer à la méthode proposée par R

m<-100
lambda<-0.5

U<-runif(m)


V<- -log(1-U)/lambda
V
W<-rexp(m,lambda)
W


#Exercice 3 (simulation par la méthode du rejet)


#Soit la densité f(x) = 6x(1 ??? x) définie pour x ??? [0, 1].

#1. Vérifier que f est bien une densité et trouver le plus petit M tel que f(x) ??? M
#M=3/2

#2. Simuler 1000 variables aléatoires distribuées selon f en utilisant la fonction g(x) = M comme
#fonction majorante

n<-1000
k<-1


X<-runif(n)
U<-runif(n,0,1.5)
Y<-6*X*(1-X)

#val<-NULL

#for (i in 1:1000){
#if ( U[i]<=Y[i] )
#  val<- c(val,X[i])
#}

hist(val, breaks = 100)

hist(X[U<=Y], breaks = 100)     
 
#Mesurer le taux de rejet et vérifier que la distribution empirique correspond
#bien à la distribution théorique   

l<-length(X[U<=Y]) 
taux_dacceptation<-l/n
taux_rejet<-1-taux_dacceptation

#3. Tracer l'évolution du taux de rejet en fonction de M, quand M varie de sa valeur minimale à 3
#fois sa valeur minimal

taux<-NULL
for(M in seq(1.5,4.5,))

