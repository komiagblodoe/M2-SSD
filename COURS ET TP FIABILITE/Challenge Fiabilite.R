
#Data challenge

#Partie 1

#3
#Simulation

n<-1000
lambda<-0.01

X1<-rexp(n,lambda)
X2<-rexp(n,lambda)
X3<-rexp(n,lambda)
X4<-rexp(n,lambda)
X5<-rexp(n,lambda)

#Approximations MTTF du système et du MTTF d'un composant

fiabi_X1<- exp(-lambda*X1)
fiabi_X2<- exp(-lambda*X2)
fiabi_X3<- exp(-lambda*X3)
fiabi_X4<- exp(-lambda*X4)
fiabi_X5<- exp(-lambda*X5)

duree_vie_comp<-c(mean(fiabi_X1),mean(fiabi_X2),mean(fiabi_X3),mean(fiabi_X4),mean(fiabi_X5))

fiab_Syst<-(1-(1-fiabi_X1)*(1-fiabi_X2))*fiabi_X3*(1-(1-fiabi_X4)*(1-fiabi_X5))

duree_vie_syst<-mean(fiab_Syst)

#On obtient bien le résultat théorique.

#4

##Fiabilité et taux de défaillance du système


