
install.packages("CVXR")
library(CVXR)

#1. UN PREMIER EXEMPLE

n <- 20
p <- 10

beta_star <- -4:5   # beta is just -4 through 5.

X <- matrix(rnorm(n * p), nrow=n)

Y <- X %*% beta_star + 0.1*rnorm(n)


#Resolution avec CVXR


#Étape 1. Définir la variable selon laquelle minimiser

beta <- Variable(p) # p est la taille de la variable, définie ci-dessus

#Étape 2. Définir la fonction objectif

L <- sum(( X %*% beta  -  Y)^2) # L est la fonction ci-dessus 

objective <- Minimize(L)

#Ici, la variable L est bien une fonction pour CVXR comme son expression fait
#intervenir un objet beta qui est une variable CVXR.

#L'objectif est ainsi bien de minimiser L (sous entendu par rapport à la variable beta ).


#Étape 3. Créer le problème à résoudre

problem <- Problem(objective)


#Étape 4. Le résoudre!

  result <- solve(problem)
  
#Étape 5. Analyser le résultat
  
  #La variable de sortie du solver possède (entre autres) les attributs suivants:
    
   # $status : est-ce que la solution trouvée est optimale
  
 # $value : valeur optimale de L
  
 # $solve_time : le temps mis à trouver une solution
  
  #$getValue : la fonction permettant d'avoir la valeur optimale (de la variable précisée)
  
  
result$status

result$solve_time

betaHat2 <- result$getValue(beta)
betaHat2
sum(betaHat2)



#2. UN PROBLEME AVEC CONTRAINTES


#Étape 1. Définir la variable selon laquelle minimiser

beta <- Variable(p) # p est la taille de la variable, définie ci-dessus


#Étape 2. Définir la fonction objectif et les contraintes
L <- sum(( X %*% beta  -  Y)^2) # L est la fonction ci-dessus 

objective <- Minimize(L)
contrainte1 <- beta>=0

un <- rep(1,p)
contrainte2 <- un%*%beta == 10


#Étape 3. Créer le problème à résoudre avec les contraintes

problem <- Problem(objective , constraints = list(contrainte1, contrainte2))


#Étape 4. Le résoudre!

  result <- solve(problem)
  

#Étape 5. Analyser le résultat

  result$status

  
betaHat2 <- result$getValue(beta)
betaHat2
sum(betaHat2)


#Le vecteur obtenu vérifie bien les contraintes ajoutées.

#(il est également possible de récupérer les variables duales associées aux contraintes)

result$getDualValue(contrainte1)

result$getDualValue(contrainte2)
