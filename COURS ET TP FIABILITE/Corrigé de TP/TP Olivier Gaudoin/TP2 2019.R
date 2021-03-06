# Fiabilit� des Syst�mes
# TP 2 - Calculs de fiabilit� par structure
###########################################

# Exercice 1 : Syst�me s�rie
############################

# Trac� des fiabilit� et taux de d�faillance

par(mfrow=c(1,2))

# Avec deux taux de d�faillance croissants

eta1<-11
beta1<-3
eta2<-20
beta2<-2

curve((1-pweibull(x,shape=beta1,scale=eta1))*(1-pweibull(x,shape=beta2,scale=eta2)),0,20)
curve((beta1/eta1)*(x/eta1)^(beta1-1)+(beta2/eta2)*(x/eta2)^(beta2-1),0,20)

# Avec un taux croissant et un d�croissant

eta1<-11
beta1<-3
eta2<-5
beta2<-0.5

curve((1-pweibull(x,shape=beta1,scale=eta1))*(1-pweibull(x,shape=beta2,scale=eta2)),0,15)
curve((beta1/eta1)*(x/eta1)^(beta1-1)+(beta2/eta2)*(x/eta2)^(beta2-1),0,15)

# Avec deux taux constants

eta1<-11
beta1<-1
eta2<-5
beta2<-1

curve((1-pweibull(x,shape=beta1,scale=eta1))*(1-pweibull(x,shape=beta2,scale=eta2)),0,20)
curve((beta1/eta1)*(x/eta1)^(beta1-1)+(beta2/eta2)*(x/eta2)^(beta2-1),0,20)


# Simulation des dur�es de vie de chaque composant, 
# cas exponentiel

eta1<-11
beta1<-1
eta2<-5
beta2<-1

x1<-rweibull(10000,shape=beta1,scale=eta1)
x2<-rweibull(10000,shape=beta2,scale=eta2)

mean(x1)
eta1*gamma(1+1/beta1)
mean(x2)
eta2*gamma(1+1/beta2)

# Dur�es de vie du syst�me s�rie

# x<-min(x1,x2) ne marche pas : calcule le minimum de 
# l'ensemble des vecteurs x1 et x2

x<-NULL
for (i in 1:10000){x[i]<-min(x1[i],x2[i])}

# Estimation du MTTF du syst�me, cas exponentiel

mean(x)
1/(1/eta1+1/eta2)

par(mfrow=c(1,1))
hist(x,prob=T)
curve(dexp(x,1/3.4375),add=T,col="red")


# M�me chose dans un cas non exponentiel

eta1<-11
beta1<-3
eta2<-20
beta2<-2

x1<-rweibull(10000,shape=beta1,scale=eta1)
x2<-rweibull(10000,shape=beta2,scale=eta2)

mean(x1)
eta1*gamma(1+1/beta1)
mean(x2)
eta2*gamma(1+1/beta2)

# Dur�es de vie du syst�me s�rie

for (i in 1:10000){x[i]<-min(x1[i],x2[i])}

# Estimation du MTTF du syst�me

mean(x)

hist(x,prob=T)

# Fiabilit� empirique et th�orique

plot(ecdf(x))
curve(1-(1-pweibull(x,shape=beta1,scale=eta1))*(1-pweibull(x,shape=beta2,scale=eta2)),add=T,col="red")


# M�me chose avec un �chantillon de taille plus petite.

eta1<-11
beta1<-3
eta2<-20
beta2<-2

x1<-rweibull(50,shape=beta1,scale=eta1)
x2<-rweibull(50,shape=beta2,scale=eta2)
for (i in 1:10000){x[i]<-min(x1[i],x2[i])}
plot(ecdf(x))
curve(1-(1-pweibull(x,shape=beta1,scale=eta1))*(1-pweibull(x,shape=beta2,scale=eta2)),add=T,col="red")




# Exercice 2 : Syst�me parall�le
################################

# Trac� des fiabilit� et taux de d�faillance

par(mfrow=c(1,2))

# Avec deux taux constants

eta1<-11
beta1<-1
eta2<-5
beta2<-1

curve(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2)),0,50,ylab="")
curve((pweibull(x,shape=beta1,scale=eta1)*dweibull(x,shape=beta2,scale=eta2)+pweibull(x,shape=beta2,scale=eta2)*dweibull(x,shape=beta1,scale=eta1))/(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2))),0,50,ylab="")

min(1/eta1,1/eta2)

# Simulation des dur�es de vie du syst�me parall�le

x1<-rweibull(10000,shape=beta1,scale=eta1)
x2<-rweibull(10000,shape=beta2,scale=eta2)
for (i in 1:10000){x[i]<-max(x1[i],x2[i])}

# Estimation du MTTF du syst�me

mean(x)

# V�rification dans le cas exponentiel

eta1+eta2-1/(1/eta1+1/eta2)

par(mfrow=c(1,1))
hist(x,prob=T)


# Avec deux taux de d�faillance croissants

eta1<-11
beta1<-3
eta2<-20
beta2<-2

par(mfrow=c(1,2))

curve(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2)),0,40,ylab="")
curve((pweibull(x,shape=beta1,scale=eta1)*dweibull(x,shape=beta2,scale=eta2)+pweibull(x,shape=beta2,scale=eta2)*dweibull(x,shape=beta1,scale=eta1))/(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2))),0,40,ylab="")

x1<-rweibull(10000,shape=beta1,scale=eta1)
x2<-rweibull(10000,shape=beta2,scale=eta2)
for (i in 1:10000){x[i]<-max(x1[i],x2[i])}

mean(x)
par(mfrow=c(1,1))
hist(x,prob=T)

plot(ecdf(x))
curve((pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2)),add=T,col="red")


# Avec un taux croissant et un d�croissant

eta1<-11
beta1<-3
eta2<-5
beta2<-0.5

par(mfrow=c(1,2))
curve(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2)),0,40,ylab="")
curve((pweibull(x,shape=beta1,scale=eta1)*dweibull(x,shape=beta2,scale=eta2)+pweibull(x,shape=beta2,scale=eta2)*dweibull(x,shape=beta1,scale=eta1))/(1-(pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2))),0,40,ylab="")

x1<-rweibull(10000,shape=beta1,scale=eta1)
x2<-rweibull(10000,shape=beta2,scale=eta2)
for (i in 1:10000){x[i]<-max(x1[i],x2[i])}

mean(x)
par(mfrow=c(1,1))
hist(x,prob=T)
hist(x,prob=T,xlim=c(0,100),nclass=50)

plot(ecdf(x))
curve((pweibull(x,shape=beta1,scale=eta1))*(pweibull(x,shape=beta2,scale=eta2)),add=T,col="red")



# Exercice 3 : Gain de fiabilit� par les redondances
####################################################


lambda<-1
x1<-rexp(10000,lambda)
mean(x1)
1/lambda

x2<-rexp(10000,lambda)
for (i in 1:10000){x[i]<-max(x1[i],x2[i])}
mean(x)
1/lambda*(1+1/2)

x3<-rexp(10000,lambda)
for (i in 1:10000){x[i]<-max(x[i],x3[i])}
mean(x)
1/lambda*(1+1/2+1/3)

x<-rexp(10000,lambda)
for (j in 2:10)
{
y<-rexp(10000,lambda)
for (i in 1:10000){x[i]<-max(x[i],y[i])}
}
i<-seq(1:10)
sum(1/i)
mean(x)

# Nombre de composants pour doubler le MTTF

sum<-1
i<-1
continuer=T
while(continuer)
{i<-i+1
sum<-sum+1/i
continuer<-(sum<2)
}
sum
i

# Nombre de composants pour d�cupler le MTTF

sum<-1
i<-1
continuer=T
while(continuer)
{i<-i+1
sum<-sum+1/i
continuer<-(sum<10)
}
sum
i









