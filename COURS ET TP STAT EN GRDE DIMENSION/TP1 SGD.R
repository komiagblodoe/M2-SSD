
#TP1 Stat en Grde Dimension

#1 

set.seed(1)
n<-20
p<-19

#y<-rnorm(n,0,1)

#simul<-replicate(p,rnorm(n))
#X<-matrix(simul, nrow = n,ncol = p)

x<-matrix(rnorm(n*p),n,p)

#2

bigvar<-numeric(p)

for (i in 1:p){
 var<-solve(crossprod(X[,1:i]))#var(hat beta)=sigma^2 Var
 bigvar[i]<-max(diag(var))
}

plot(1:p,bigvar)


#ou soit on peut utiliser le log pour mieux voir l'augmentation des variances

plot(1:p,log(20*bigvar,2),ylab="La plus grande variance",xlab = "Nombre de colonnes")
logAxis(2, Base=2 )

bigvar[15]/bigvar[1]

#3

#Quand p s'approche de n, la variance augmente largement de valeur. On est donc en présence d'une non convergence

#Reprenons l'exo avec un p plus grand que n
set.seed(1)
n<-20
p<-25

#y<-rnorm(n,0,1)

#simul<-replicate(p,rnorm(n))
#X<-matrix(simul, nrow = n,ncol = p)

x<-matrix(rnorm(n*p),n,p)

#2

bigvar<-numeric(p)

for (i in 1:p){
  var<-solve(crossprod(X[,1:i]))#var(hat beta)=sigma^2 Var
  bigvar[i]<-max(diag(var))
}

plot(1:p,bigvar)

#Pour p supérieur à n, celà ne marche pas car la matrice n'est plus inversible.
#Il y a une colinéarité entre certaines colonnes.


##########################
#1.2 Problème de sélection de variable
################################

set.seed(1) n <- 25
p <- 100 
xnam <- paste0("V", 1 :p) 
form <- as.formula(paste("y ??? ", paste(xnam, collapse= "+")))

N <- 100
res <- cover <- pred <- NULL 
pb <- txtProgressBar(1, N, style=3) 

for (i in 1 :N) {
  X <- (matrix(runif(n*p), n, p))
  Data <- as.data.frame(X) 
  y <- rnorm(n) 
  j <- which.max(abs(crossprod(X, y - mean(y)))) 
  XX <- (matrix(runif(n*p), n, p)) 
  pData <- as.data.frame(XX) 
  yy <- rnorm(n) 
  fit0 <- lm(y~1, data=Data)
  fit <- step(fit0, scope=form, direction="forward", trace=0, k=log(n), steps=5) 
  res <- rbind(res, summary(fit)$coef[-1,]) 
  pred <- c(pred, yy - predict(fit, pData)) 
  cover <- c(cover, apply(confint(fit)[-1,,drop=FALSE], 1, prod) <= 0) 
  setTxtProgressBar(pb, i) 
} 


#2 Tracé l'histogramme du slide 17

hist(res[,1],breaks = 50, probability = TRUE, xlab = 'beta hat')
#Les valeurs estimées sont loin de réfléter la vraie valeur qui est égale à 0


#3  Calculer les indicateurs du Slide 19

summary(res[,1])#le quartile est de -1.51 et le troisième quartile est de 1.47. Ils sont tous différents de 0 qui est la vraie valeur


mse<-mean(pred^2)
mse

#Le MSE moyenne est de 2.57


crossprod(pred)/length(pred)

###La suite des indicateurs du slide 19 à faire.



############

#1.3 DEVOIR A FAIRE

############




################################

#Chap2 TP2

#############################


######## 2 - Sélection de modèle


########2-1 Les données


#Télecharger les données

library(lasso2)

data("Prostate")

#Vérifier bien que les données correspondent au tableau de description suivant 


#Faites quelques statistiques descriptives : dim, names, summary, cor, hist 

summary(Prostate)

##Lesvariables{\bfsvi}et{\bfgleason} 
#sont qualitatives
Prostate$svi=as.factor(Prostate$svi) 
Prostate$gleason=as.factor(Prostate$gleason)


dim(Prostate) 
names(Prostate)
summary(Prostate)
cor(Prostate[,-c(5,7)])
hist(Prostate$lpsa)

hist(Prostate$lcavol)
hist(Prostate$lweight)
hist(Prostate$age)
hist(Prostate$lbph)
hist(Prostate$svi)
hist(Prostate$lcp)
hist(Prostate$gleason)
hist(Prostate$pgg45)

#Commentaires à ajouter


#Données train et test


#Les valeurs de lpsa sont rangées par ordre croissant
#On conserve 1/4 des données pour l'échantillon test

ind.test=4*c(1:22)

Prostate.app=Prostate[-ind.test,]

Prostate.test=Prostate[c(ind.test),]

# Donner quelques statistiques descriptives des données : Prostate.app et Prostate.test

dim(Prostate.test)
dim(Prostate.app)

ntest=length(Prostate.test$lpsa)
napp=length(Prostate.app$lpsa)

summary(Prostate.app)
summary(Prostate.test)
#Commentaires à faire


####2.2 Modèle linéaire complet

#Une fonction utile de graphe des résidus : 

plot.res=function(x,y,titre=""){
  plot(x,y,col="blue",ylab="Résidus", xlab="Valeurs predites",main=titre) 
  abline(h=0,col="green")
}

#2.2.1 Estimation du modèle et graphes des résidus 


modlin=lm(lpsa~., data=Prostate.app) 
summary(modlin)# noter les p-valeurs 

#Residus

res=residuals(modlin) 

#Regroupement des graphiques sur la meme page 

par(mfrow=c(1,2)) 
hist(residuals(modlin))
qqnorm(res)
qqline(res, col = 2)

# retour au graphique standard 

par(mfrow=c(1,1)) 
plot.res(predict(modlin),res)
#Selon la répartition des points(uniformément répartis), 
#on peut dire que le résidu est sans biais



######2.2.2 Erreur d'apprentissage 


#Calculer l'erreur d'apprentissage 

mean(res**2)


#######2.2.3 Erreur sur l'échantillon test 

pred.test=predict(modlin, newdata=Prostate.test)
res.test=pred.test-Prostate.test$lpsa
mean(res.test**2)
#Commentaires à ajouter pour les erreurs


######2.2.4 Nouvelle paramétrisation 


#Aﬁn de faciliter l’interprétation des résultats concernant les variables qualitatives, on introduit une nouvelle 
#paramétrisation à l’aide de contrastes. Par défaut, la référence est prise pour la valeur 0 de svi et 6 de gleason,
#qui sont les plus petites valeurs. Les paramètres indiqués pour les variables svi1
#gleason 7, 8 et 9 indiquent l’écart estimé par rapport à cette référence. Il est plus intéressant en pratique de se 
#référer à la moyenne des observations sur toutes les modalités des variables qualitatives, et d’interpréter 
#les coefﬁcients comme des écarts à cette moyenne

contrasts(Prostate.app$svi)= 
  contr.sum(levels(Prostate.app$svi))
contrasts(Prostate.app$gleason)= 
  contr.sum(levels(Prostate.app$gleason))
modlin2=lm(lpsa~., Prostate.app)
summary(modlin2) 
# Attention au nom des variables : gleason1 =6, 
#gleason2 =7, gleason3 =8, gleason4 =9 (pas affiché), 
#la somme des coefficients associés à ces variables 
#est nulle
#svi1=0, svi2=1 (pas affiché) 
#la somme des deux coefficients est nulle



#####3 Sélection de modèle par sélection de variables


###3.1 Sélection par AIC et backward 


library(MASS)
modselect_b=stepAIC(modlin2,~.,trace=TRUE, direction=c("backward"))
summary(modselect_b)
# noter que des paramètres restent non significatifs


###3.2 Sélection par AIC et forward 

mod0=lm(lpsa~1,data=Prostate.app)
modselect_f=stepAIC(mod0,lpsa~lcavol+lweight
                    +age+lbph+svi+lcp+gleason+pgg45,data= 
                      Prostate.app,trace=TRUE,direction=c("forward"))
summary(modselect_f)


###3.3 Sélection par AIC et stepwise 

modselect=stepAIC(modlin2,~.,trace=TRUE, direction=c("both")) 
#both est l'option par défaut

summary(modselect) #On retrouve ici le meme modèle qu'avec l'agorithme backward


###3.4 Sélection par BIC et stepwise 


# k=log(napp) pour BIC au lieu de AIC.

modselect_BIC=stepAIC(modlin2,~.,trace=TRUE, direction=c("both"),k=log(napp)) 

summary(modselect_BIC) #Le modèle sélectionné est plus parcimonieux


#######3.5 Erreur sur l'échantillon d'apprentissage 


#Modèle stepwise AIC 
mean((predict(modselect)-Prostate.app[,"lpsa"])**2) 
#valeur trouvée 0.49


#Modèle stepwise BIC
mean((predict(modselect_BIC)-Prostate.app[,"lpsa"])**2)
# valeur trouvée 0.53



###3.6 Calcul de l'erreur sur l'échantillon test 


#Modèle stepwise AIC 
mean((predict(modselect,newdata=Prostate.test)-Prostate.test[,"lpsa"])**2) 
#valeur trouvée 0.46 

###Commentaires à ajouter


#Modèle stepwise BIC
mean((predict(modselect_BIC,newdata=Prostate.test)-Prostate.test[,"lpsa"])**2) 
# valeur trouvée 0.40


####### 2.4 Nouvelle paramétrisation

contrasts(Prostate.app$svi)= contr.sum(levels(Prostate.app$svi))
contrasts(Prostate.app$gleason)= contr.sum(levels(Prostate.app$gleason)) 
modlin2=lm(lpsa~.,Prostate.app) 
summary(modlin2) 


##### 3-Sélection de modèle par sélection de variables

##3.1 Sélection par AIC et backward


library(MASS) 
modselect_b=stepAIC(modlin2,~.,trace=TRUE, direction=c("backward"))
summary(modselect_b) 
#noter que des paramètres restent non significatifs

##3.2 Sélection par AIC et forward


mod0=lm(lpsa~1,data=Prostate.app) 
modselect_f=stepAIC(mod0,lpsa~lcavol+lweight 
                    +age+lbph+svi+lcp+gleason+pgg45,data= Prostate.app,trace=TRUE,direction=c("forward"))
summary(modselect_f)


##3.3 Sélection par AIC et stepwise

modselect=stepAIC(modlin2,~.,trace=TRUE, direction=c("both")) 
#both est l’option par défaut

summary(modselect) 
#On retrouve ici le même modèle qu’avec
#l’agorithme backward

## 3.4 Sélection par BIC et stepwise

#k=log(napp) pour BIC au lieu de AIC.

modselect_BIC=stepAIC(modlin2,~.,trace=TRUE, direction=c("both"),k=log(napp))
summary(modselect_BIC)
#Le modèle sélectionné est plus parcimonieux


## 3.5 Erreur sur l’échantillon d’apprentissage

#ModèlestepwiseAIC 
mean((predict(modselect)-Prostate.app[,"lpsa"])**2)
#valeurtrouvée0.49

#Modèle stepwise BIC 
mean((predict(modselect_BIC)-Prostate.app[,"lpsa"])**2) 
#valeurtrouvée0.53


## 3.6 Calcul de l’erreur sur l’échantillont est

#Modèle stepwise AIC
mean((predict(modselect,newdata=Prostate.test) -Prostate.test[,"lpsa"])**2)
#valeurtrouvée0.46 

#Modèle stepwise BIC 
mean((predict(modselect_BIC,newdata=Prostate.test) -Prostate.test[,"lpsa"])**2)
#valeurtrouvée0.40


#Les modèles sélectionnés ont une erreur plus grande que le modèle linéaire comprenant
#toutes les variables sur l’échantillon d’apprentissage (c’est normal!). Sur l’échantillon test, 
#le modèle qui minimise le critère BIC a de meilleures performances que le modèle initial. 
#Les deux modèles sélectionnés sont beaucoup plus parcimonieux que le modèle initial. 



##############################################################
#####TP3 SGD 4-Sélection de modèle par pénalisation Ridge
#################################################################

###NB Faire des commentaires sur les sorties Voir le fichier wikistat


#4.1 Comportement des coefﬁcients

#Calcul des coefﬁcients pour différentes valeurs du paramètre lambda

library(MASS) 
library(lasso2)

mod.ridge=lm.ridge(lpsa~.,data=Prostate.app, lambda=seq(0,20,0.1))

par(mfrow=c(1,1))
plot(mod.ridge) 

#évolution des coefficients 

matplot(t(mod.ridge$coef),lty=1:3,type="l",col=1:10) 
legend("top",legend=rownames(mod.ridge$coef), col=1:10,lty=1:3)

#4.2 Pénalisation optimale par validation croisée

select(mod.ridge) #noter la valeur puis estimer 
mod.ridgeopt=lm.ridge(lpsa~.,data=Prostate.app, lambda=10.4)

#4.3 Prévision et erreur d’apprentissage

#Coefficients du modèle sélectionné: 
coeff=coef(mod.ridgeopt)

#On crée des vecteurs pour :

#les variables qualitatives

svi0.app=1*c(Prostate.app$svi==0)
svi1.app=1-svi0.app
gl6.app=1*c(Prostate.app$gleason==6)
gl7.app=1*c(Prostate.app$gleason==7)
gl8.app=1*c(Prostate.app$gleason==8) 
gl9.app=1*c(Prostate.app$gleason==9) 

#variables quantitatives

lcavol.app=Prostate.app$lcavol
lweight.app=Prostate.app$lweight 
age.app=Prostate.app$age 
lbph.app=Prostate.app$lbph 
lcp.app=Prostate.app$lcp 
pgg45.app=Prostate.app$pgg45

#Calcul des valeurs prédites

fit.rid=rep(coeff[1],napp)+coeff[2]*lcavol.app+ 
  coeff[3]*lweight.app+coeff[4]*age.app+ 
  coeff[5]*lbph.app+coeff[6]*svi0.app-
  coeff[6]*svi1.app+coeff[7]*lcp.app+ 
  coeff[8]*gl6.app+coeff[9]*gl7.app+ 
  coeff[10]*gl8.app-(coeff[8]+coeff[9]+ 
  coeff[10])*gl9.app+coeff[11]*pgg45.app


#Tracé des valeurs prédites en fonctions 
#des valeurs observées 

plot(Prostate.app$lpsa,fit.rid) 
abline(0,1) 

#Calcul et tracé des résidus 
res.rid=fit.rid-Prostate.app[,"lpsa"]
plot.res(fit.rid,res.rid,titre="") 

#Erreurd’apprentissage
mean(res.rid**2) #0.478


##4.4 Prévision sur l’échantillon test

#Variables qualitatives

svi0.t=1*c(Prostate.test$svi==0)
svi1.t=1-svi0.t 
gl6.t=1*c(Prostate.test$gleason==6) 
gl7.t=1*c(Prostate.test$gleason==7) 
gl8.t=1*c(Prostate.test$gleason==8) 
gl9.t=1*c(Prostate.test$gleason==9) 

#Variables quantitatives

lcavol.t=Prostate.test$lcavol 
lweight.t=Prostate.test$lweight 
age.t=Prostate.test$age 
lbph.t=Prostate.test$lbph 
lcp.t=Prostate.test$lcp 
pgg45.t=Prostate.test$pgg45


prediction=rep(coeff[1],ntest)+coeff[2]* 
  lcavol.t+coeff[3]*lweight.t+coeff[4]*age.t+
  coeff[5]*lbph.t+coeff[6]*svi0.t-coeff[6]*svi1.t+
  coeff[7]*lcp.t+coeff[8]*gl6.t+coeff[9]*gl7.t+ 
  coeff[10]*gl8.t-(coeff[8]+coeff[9]+coeff[10])* 
  gl9.t+coeff[11]*pgg45.t

#Erreur sur l’échantillon test

mean((Prostate.test[,"lpsa"]-prediction)^2)
#0.424
#L’erreur d’apprentissage est légèrement plus élévée que pour le modèle linéaire sans pénalisation
#(c’estnormal!)l’erreur de test est plus faible.Les performances sont comparables sur l’échantillont
#est au modèle sélectionné par le critère BIC.En terme d’interprétation,les modèles sélectionnés par AIC et BIC sont préférables


## 5 Sélection de modèle parpénalisation Lasso

#5.1 Librairie Lasso2

library(lasso2)

#5.2 Construction du modèle

l1c.P<-l1ce(lpsa~.,Prostate.app, 
            bound=(1:100)/100,absolute.t=FALSE)


#La borne est ici relative,elle correspond à une certaine proportion de la norme L1 du vecteur des coefﬁcients des moindres carrés.
#Une borne égale à 1 correspond donc à l’absence de pénalité,on retrouve l’estimateur des moindres carrés.


#5.3 Visualisation des coefficients

coefficients=coef(l1c.P)
plot(l1c.P,col=1:11,lty=1:3,type="l")
legend("topleft",legend=colnames(coefficients),
       col=1:11,lty=1:3)

#On supprime le terme constant
penalite_relative=c(1:100)/100
matplot(penalite_relative,coefficients[,-1],
        lty=1:3,type="l",col=1:10)
legend("topleft",legend=
         colnames(coefficients[,-1]),col=1:10,lty=1:3)

#5.4 Sélection de la pénalité par validation croisée

vc=gcv(l1c.P)
crit.vc=vc[,"gcv"]
bound_opt=vc[which.min(crit.vc),"rel.bound"]
#0.95
l1c.opt <- l1ce(lpsa ~ ., Prostate.app,
                bound=bound_opt, absolute.t=FALSE)
coef=coef(l1c.opt)

#5.5 Erreur d’apprentissage

#erreur apprentissage

fit=fitted(l1c.opt)
mean((fit-Prostate.app[,"lpsa"])^2)
#0.46

#5.6 Erreur sur l’échantillon test

prediction=predict(l1c.opt,newdata=Prostate.test)
mean((prediction-Prostate.test[,"lpsa"])^2)
#0.44

#5.7 Librairie glmnet

#L’utilisation de la librairie glmnet fournit des résultats plus rapides, ce qui
#peut s’avérer important pour des données de grande dimension. Par contre, on
#ne peut pas traiter à priori des variables qualitatives. Nous allons donc devoir
#créer des vecteurs avec des variables indicatrices des diverses modalités pour
#les variables qualitatives. Nous ne prendrons pas en compte les contrastes.

#5.8 Mise en forme des variables

#on construit une matrice xx.app d’apprentissage et
#xx.test de test

data(Prostate)
Prostate.app=Prostate[-ind.test,]
Prostate.test=Prostate[c(ind.test),]

x.app=Prostate.app[,-9]
y.app=Prostate.app[,9]
x.app=as.matrix(x.app)

# construction des indicatrices

svi1.app=1*c(Prostate.app$svi==1)
gl7.app=1*c(Prostate.app$gleason==7)
gl8.app=1*c(Prostate.app$gleason==8)
gl9.app=1*c(Prostate.app$gleason==9)

#matrice avec les vecteurs indicatrices

xx.app=matrix(0,ncol=10,nrow=nrow(x.app))
xx.app[,1:6]=x.app[,1:6]
xx.app[,7:9]=cbind(gl7.app,gl8.app,gl9.app)
xx.app[,10]=x.app[,8]

#on nomme les colonnes avec le noms des variables

colnames(xx.app)=c("lcavol","lweight", "age",
                   "lbph","svi1","lcp","gl7","gl8","gl9","pgg45")

#on fait de même pour l’echantillon test

x.test=Prostate.test[,-9]
y.test=Prostate.test[,9]
x.test=as.matrix(x.test)
svi1.test=1*c(Prostate.test$svi==1)
gl7.test=1*c(Prostate.test$gleason==7)
gl8.test=1*c(Prostate.test$gleason==8)
gl9.test=1*c(Prostate.test$gleason==9)

#on construit une matrice avec les vecteurs
#indicatrices

xx.test=matrix(0,ncol=10,nrow=nrow(x.test))
xx.test[,1:6]=x.test[,1:6]
xx.test[,7:9]=cbind(gl7.test,gl8.test,gl9.test)
xx.test[,10]=x.test[,8]
colnames(xx.test)=colnames(xx.app)


##5.9 Construction du modèle

library(glmnet)
out.lasso = glmnet(xx.app,y.app)
l=length(out.lasso$lambda)
b=coef(out.lasso)[-1,1:l]


##5.10 Visualisation des coefficients

# chemin de régularisation du lasso

matplot(t(as.matrix(out.lasso$beta)),type="l",
        col=1:10,lty=1:3)
legend("topleft",legend=colnames(xx.app),
       col=1:10,lty=1:3)
title("Lasso")


##5.11 Sélection de la pénalité par validation croisée

y.app=as.matrix(y.app)
a=cv.glmnet(xx.app,y.app)

#le resultat est dans
lambda.opt=a$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)


##5.12 Erreur d’apprentissage

appr=predict(app,newx=xx.app)
mean((appr-Prostate.app[,9])^2)
#0.48


##5.13 Erreur sur l’échantillon test

pred=predict(app,newx=xx.test)
mean((pred-Prostate.test[,9])^2)
#0.39


##5.14 Elastic Net

#on peut jouer avec le paramètre alpha, de glmnet

out.elnet <- glmnet(xx.app,y.app,alpha=0.5)
a.elnet=cv.glmnet(xx.app,y.app,alpha=0.5)
lambda.opt=a.elnet$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)

#erreur apprentissage

app.elnet=predict(a.elnet,newx=xx.app)
mean((app.elnet-Prostate.app[,9])^2)
#0.60

#erreur de prédiction

predi.elnet=predict(a.elnet,newx=xx.test)
mean((predi.elnet-Prostate.test[,9])^2)
#0.37

#################################################################################################
# faire Chp3 Pratique de la régression Ridge et Elasticnet sur le fichier 
#poly-TP-M2MIASHS(page 13). NB: Problème sur les données. A rattraper selon le prof. Les données sont envoyées par mail le 18/12/19
####################################################################################################################



######################################################################
###TP4 Exemples sur le lien du mail.

#Pour less commentaires, il faut dire pourquoi telle ou telle méthode ne marche pas
#Ridge Lasso Elastic Net (les trois exemples)
##une fois c'est le lasso qui marche, des fois ridge ou elastic net.
#tout dépend de la complexité du modèle
######################################################################



#################### Exemple 1

###Generate data

library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models

# Generate data
set.seed(19875)  # Set seed for reproducibility
n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


###Fit Model

# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}


##Plot solution path and cross-validated MSE as function of λ

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")



#MSE on test set

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)
#LASSO is the winner! LASSO is good at picking up a small signal through lots of noise


######################## Exemple 2

##Generate Data

library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models

# Generate data
set.seed(19874)
n <- 1000    # Number of observations
p <- 5000     # Number of predictors included in model
real_p <- 1500  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

##Fit models


# Fit models:
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}


#Plot solution path and cross-validated MSE as function of λ.


# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


#MSE on test set

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)
#Ridge is the winner! Ridge in general is good at prediction, but is not very interpretable.


################ Exemple 3



# Generate data
set.seed(19873)
n <- 100    # Number of observations
p <- 50     # Number of predictors included in model
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n, rep(0,p), CovMatrix)
y <- 10 * apply(x[, 1:2], 1, sum) + 
  5 * apply(x[, 3:4], 1, sum) +
  apply(x[, 5:14], 1, sum) +
  rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


# Fit models:
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

#Plot solution path and cross-validated MSE as function of λ

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


#MSE on test set

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)
#Elastic Net is the winner! It’s interesting to note the best solution is “close” to Ridge, but Ridge (α=0) in fact performs the worst.




#################################################################################

#TP5 Analyse de données fonctionnelles

################################################################################

## 1.2 PARTIE 1

install.packages("fda")
library(fda)

#1.2.1 Examen des données 

#On charge les données et on retire les individus ayant des données manquantes
#de la manière suivante 

dat0 = as.matrix(read.table("Sistema.txt", head=TRUE)) 
na_count = rowSums(is.na(dat0)) 
dat = dat0[na_count == 0,]
#La variable heures contient le temps dans la journee en heures : toutes les 6 minutes (240 points).

heures = c(0 :239)/10



#1.(a) Vériﬁer la répartition des valeurs pour les débits. 

colnames(dat)<-heures
hist(dat,breaks = 1000)


#(b) Expliquer pourquoi on peut appliquer la méthode de stabilisation de 
#la variance pour un processus de Poisson. 


#(c) Est-ce que cela améliore la queue de distribution? 


#2. Tracer des courbes pour quelques journées (au moins 5) 

matplot(heures,dat[5,], type="l")

for (i in 1:4 ){
lines(heures,dat[i,], type="l")
}


#3. Tracer la courbe du débit moyen au cours de la journée. 

plot(colMeans(dat), type="l")
#Commenter la forme générale de la courbe: forme bimodale

#Voir la fin du cours4 chap 15 pour la suite
