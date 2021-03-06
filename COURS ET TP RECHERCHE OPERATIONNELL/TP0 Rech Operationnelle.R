
install.packages("CVXR")
library(CVXR)

#1. UN PREMIER EXEMPLE

n <- 20
p <- 10

beta_star <- -4:5   # beta is just -4 through 5.

X <- matrix(rnorm(n * p), nrow=n)

Y <- X %*% beta_star + 0.1*rnorm(n)


#Resolution avec CVXR


#�tape 1. D�finir la variable selon laquelle minimiser

beta <- Variable(p) # p est la taille de la variable, d�finie ci-dessus

#�tape 2. D�finir la fonction objectif

L <- sum(( X %*% beta  -  Y)^2) # L est la fonction ci-dessus 

objective <- Minimize(L)

#Ici, la variable L est bien une fonction pour CVXR comme son expression fait
#intervenir un objet beta qui est une variable CVXR.

#L'objectif est ainsi bien de minimiser L (sous entendu par rapport � la variable beta ).


#�tape 3. Cr�er le probl�me � r�soudre

problem <- Problem(objective)


#�tape 4. Le r�soudre!

  result <- solve(problem)
  
#�tape 5. Analyser le r�sultat
  
  #La variable de sortie du solver poss�de (entre autres) les attributs suivants:
    
   # $status : est-ce que la solution trouv�e est optimale
  
 # $value : valeur optimale de L
  
 # $solve_time : le temps mis � trouver une solution
  
  #$getValue : la fonction permettant d'avoir la valeur optimale (de la variable pr�cis�e)
  
  
result$status

result$solve_time

betaHat2 <- result$getValue(beta)
betaHat2
sum(betaHat2)



#2. UN PROBLEME AVEC CONTRAINTES


#�tape 1. D�finir la variable selon laquelle minimiser

beta <- Variable(p) # p est la taille de la variable, d�finie ci-dessus


#�tape 2. D�finir la fonction objectif et les contraintes
L <- sum(( X %*% beta  -  Y)^2) # L est la fonction ci-dessus 

objective <- Minimize(L)
contrainte1 <- beta>=0

un <- rep(1,p)
contrainte2 <- un%*%beta == 10


#�tape 3. Cr�er le probl�me � r�soudre avec les contraintes

problem <- Problem(objective , constraints = list(contrainte1, contrainte2))


#�tape 4. Le r�soudre!

  result <- solve(problem)
  

#�tape 5. Analyser le r�sultat

  result$status

  
betaHat2 <- result$getValue(beta)
betaHat2
sum(betaHat2)


#Le vecteur obtenu v�rifie bien les contraintes ajout�es.

#(il est �galement possible de r�cup�rer les variables duales associ�es aux contraintes)

result$getDualValue(contrainte1)

result$getDualValue(contrainte2)
