
#TP 3 - LASSO

#EXEMPLE

library(CVXR)

n = 5
m = 3
sigma = 0.5

X = replicate(n, rnorm(m))
theta_true = c(1,0,0,0,5)
xi = rnorm(m)
y = X%*%theta_true + sigma*xi

X
theta_true
y

#2 Trouver l'estimateur par résolution du problème d'optimisation 

lambda<-0.1
theta<-Variable(n)

F<-0.5*p_norm(X%*%theta-y,2)^2+lambda*p_norm(theta,1)
obj<-Minimize(F)
problem<-Problem(obj)
result<-solve(problem)
theta_ds<-result$getValue(theta)
theta_ds

#Examiner la valeur de la solution pour lambda=0 puis 1 puis 10

lambda<-0
theta<-Variable(n)
theta_ds<-c()


F<-0.5*p_norm(X%*%theta-y,2)^2+lambda*p_norm(theta,1)
obj<-Minimize(F)
problem<-Problem(obj)
result<-solve(problem)
theta_ds<-result$getValue(theta)
theta_ds

lambda<-1
theta<-Variable(n)
theta_ds<-c()


F<-0.5*p_norm(X%*%theta-y,2)^2+lambda*p_norm(theta,1)
obj<-Minimize(F)
problem<-Problem(obj)
result<-solve(problem)
theta_ds<-result$getValue(theta)
theta_ds

lambda<-10
theta<-Variable(n)
theta_ds<-c()


F<-0.5*p_norm(X%*%theta-y,2)^2+lambda*p_norm(theta,1)
obj<-Minimize(F)
problem<-Problem(obj)
result<-solve(problem)
theta_ds<-result$getValue(theta)
theta_ds

#3. Fonction "Lasso"

#Écrivez une fonction MyLasso qui fait appel a CVXR pour calculer l'estimation ??L

 MyLasso<-function(X,y,lambda=0.1){
   
   n<-ncol(X)
   theta<-Variable(n)
   
   F<-0.5*p_norm(X%*%theta-y,2)^2+lambda*p_norm(theta,1)
   obj<-Minimize(F)
   problem<-Problem(obj)
   result<-solve(problem)
   
   theta_ds<-result$getValue(theta)
   resid<-y-X%*%result$getValue(theta)
   status<-result$status
   return(list(status=status,resid=resid,coef=theta_ds))
   
 }
 

 #a. Test quand on connait le vrai theta 
 
 #Testez votre fonction sur l' exemple ci-dessous
 
 m = 30
 n = 20
 X = replicate(n, rnorm(m))
 theta_true = c( rep(0,floor(n/5)) , rep(0.5,floor(n/5)) , rep(1.0,floor(n/5)), rep(4,floor(n/5)), rep(20,floor(n/5)) )
 sigma = 0.1
 y = X%*%theta_true + sigma*rnorm(m)

 f = MyLasso(X, y)
 str(f)

 comp = cbind(as.matrix(f$coef), as.matrix(theta_true))
 comp
 
 
 #b. Variation du paramètre ??
 
 #Examinez les solutions du problème précédent pour les suivantes
 #différentes valeurs de lambda
 
 
 
 m = 30
 n = 20
 X = replicate(n, rnorm(m))
 theta_true = c( rep(0,floor(n/5)) , rep(0.5,floor(n/5)) , rep(1.0,floor(n/5)), rep(4,floor(n/5)), rep(20,floor(n/5)) )
 sigma = 0.1
 y = X%*%theta_true + sigma*rnorm(m)
 
 
 
 comp<-as.matrix(theta_true)
 lambda<-c(10^-5,10^-4,10^-3,10^-2,10^-1,0,10,10^2)
 
  for(i in 1:length(lambda)){
    
   f = MyLasso(X, y,lambda[i])
   comp<-cbind(as.matrix(f$coef),comp)
  
  }
 
 comp
 
 
#4. Le Lasso en apprentissage
 
 crime_data = read.csv('C:/Users/im2ag/Desktop/M2 SSD/COURS ET TP RECHERCHE OPERATIONNELL/data/communities.data',sep=',', header=FALSE)
 m = dim(crime_data)[1]

#Lisez le dataset, découpez le en base d'apprentissage et de test.
 
 crime_dat<-crime_data[,-c(1:5,128)]
 crime_dat[crime_dat=='?'] <- 0
 crime_dat<-sapply(crime_dat,function(x) as.numeric(as.character(x)))
 target<-crime_data[,128]
 ind<-sample(1:m, round(0.8*m), replace= FALSE)
 X_train<-crime_dat[ind,]
 X_test<-crime_dat[-ind,]
 y_train<-target[ind]
 y_test<-target[-ind]
 
 require (Metrics)
 errors<-c()
 lambdas<-seq(0,10,1)
 
 for(i in 1:length(lambdas)){
   coefs<-MyLasso(X_train,y_train,lambdas[i])$coef
   pred<-X_test%*%coefs
   errors<-c(errors,mae(y_test,pred))
   
 }
 
 errors
 
 require(latex2exp)
 plot(lambdas,errors,type='l',main = 'prediction')
( best_lambda<-lambdas[which.min(errors)])
 
 
