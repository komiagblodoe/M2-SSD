
#1 MéthodesMCpourl'estimation 

#Exercice1 (Comparaisond'estimateurs) 

#1. Implémenter une procédure 
#pour simuler des observations selon ce modèle de mélange


n = 20; m = 1000; k = 5

 e = matrix(0, m, 4)
 p=list(1,0.95,0.9,0.8,0.7) ; sigma1=1;sigma2=10
 
 

for(i in 1:m) {
  
  for (j in 1:n) {
    p_i= runif(1,0,1)
    
    for(k in 1:length(p)){
      
    if (p_i<p[[i]]){
  x = sort(rnorm(n,0,sigma1))}
          else
           {x=sort(rnorm(n,0,sigma2))}
    }
  
  e[i,1] = mean(x)
  e[i,2] = mean(x[2:(n-1)])
  e[i,3] = mean(x[(k+1):(n-k)])
  e[i,4] = median(x)
  }
}
 
 mse = apply(e, 2, function(x){mean(x^2)})
 se = apply(e, 2, function(x){sqrt(sum((x - mean(x))^2)/m)})
 
 
#2
 
 n = 20; m = 1000; k = 5
 
 e = matrix(0, m, 4)
 p=list(1,0.95,0.9,0.8,0.7) ; sigma1=1;sigma2=10
 
 for(i in 1:m) {
   
   for (j in 1:n) {
     p_i= runif(1,0,1)
     
     for (k in 1:length(p))
     
     if (p_i<p[[k]]){
       x = sort(rnorm(n,0,sigma1))
     }
     
     else
      { x=sort(rnorm(n,0,sigma2))
      }
     e[i,1] = mean(x)
     e[i,2] = mean(x[2:(n-1)])
     e[i,3] = mean(x[(k+1):(n-k)])
     e[i,4] = median(x)
   }
 }
 mse = apply(e, 2, function(x){mean(x^2)})
 se = apply(e, 2, function(x){sqrt(sum((x - mean(x))^2)/m)})
 
 plot(mse)
 
# Comment interpréter ces résultats? 
 
 # La moyenne empirique semble être le meilleur estimateur de la moyenne.
 
 
 
 #Exercice2 (Estimation d'un niveau de con???ance) 
 
 
  m = 1000; n = 20; alpha = 0.05
  p=c(1,0.95,0.9,0.8,0.7) ; sigma1=1;sigma2=2
 
 I1 = numeric(m); I2 = numeric(m)
 
 res<-list()
   
     
     for(k in 1: length(p) ){
       
       for (i in 1:m){
         x=c()
         for (j in 1:n){
           
           p_j= runif(1,0,1)
           
           
             
             if (p_j<p[k]){
               x = c(x, rnorm(1,0,sigma1))
             }
             
             else
             { x=c(x,rnorm(1,0,sigma2))
             } 
         }
         
  
   I1[i] = (n-1)*var(x)/qchisq(1-alpha/2, df = n-1)
   I2[i] = (n-1)*var(x)/qchisq(alpha/2, df = n-1)
       }
 
 res[[k]]<-data.frame(I1,I2)
     }
   
  lapply( res, function (I) mean(sigma1^2 > I[1] & sigma1^2 < I[2]) )

  
  conf<-as.numeric(unlist(lapply( res, function (I) mean(sigma1^2 > I[1] & sigma1^2 < I[2]) )))
conf 



#Exercice3 (risque de première espèce empirique) 

mu0=500; alpha=0.05

n=20; m=10000;sigma=100

seuil<-qt(1-alpha,n-1)

prop<-0

for (i in 1:m){ 
  x<-rnorm(n,mu0,sigma/sqrt(n))
  
  if (x>mu0+seuil*sigma/sqrt(n)){
    prop<-prop+1
  }
  
}

risque_empirique<-prop/m


  