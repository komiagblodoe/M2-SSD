
#TP no 4 - Tests par permutation


#Exercice 1 - tests de permutation et diff´erence de moyennes

#1

attach(chickwts)
View(chickwts)

# De combien d'observations dispose 
#t'on pour chaque type de nourriture?

(x<-table(chickwts))
colSums(x)


(model.chickwts<-lm(weight~feed, data = chickwts))

#Validation du modèle

#Test de la normalité des résidus : On effectue
#un test de Shapiro sur les résidus du modèle :


shapiro.test(residuals(model.chickwts))
#On ne rejette donc pas l'hypothèse de normalité.

boxplot(chickwts$weight~chickwts$feed, 
        col="red", main="poids des poulets en fonction des nourritures")

#2

#Test de l'homoscédasticité : Puisque l'hypothèse de normalité n'est pas rejetée,
#on peut effectuer un test de Bartlett pour tester l'homogénéité des variances :
 
bartlett.test(weight~feed, data=chickwts)
#L'hypothèse nulle n'est pas rejetée, on ne rejette donc pas l'hypothèse d'homoscédasticité


#Interprétation

anova(model.chickwts)

#On voit ainsi qu'on a un effet très marqué du type de nourriture. Plus précisément, cela implique qu'au moins 1 des
#types de nourriture est significativement différent des autres

#Pour affiner les conclusions, faisons le test de TUkey
#Cette fonction ne fonctionne pas avec des objets lm, mais avec des objets aov, que l'on
#obtient de la manière suivante


aov.chickwts<-aov(weight~feed, data=chickwts)
summary(aov.chickwts)

hsd.chickwts<-TukeyHSD(aov.chickwts)
hsd.chickwts
plot(hsd.chickwts)

#. Tous les intervalles de confiance qui ne recoupent pas 0 révèlent des différences
#significatives d'effets.
#On peut récupérer les effets (grâce à la fonction model.tables) de chaque type de nourriture et les ordonner :

effects.chickwts<-model.tables(aov.chickwts)
sort(effects.chickwts[["tables"]][["feed"]])

#4. Implémenter la procédure de permutation "randomisée"
#décrite en cours pour un nombre B = 1000 de tirages

#install.packages("pgirmess")
library(pgirmess)

#set.seed(100000)

#PermTest(model.chickwts, B=10000)#B correspond au nombre de permutations

#Procedure du cours

x0=as.vector(chickwts[23:36,])
x0
x=x0$weight
x

y0=as.vector(chickwts[11:22,])
y=y0$weight
y
y0
n=length(x)

B = 1000 # nombre de permutations 
 z = c(x,y) 
 t0 = t.test(x,y)$statistic 
 t.perm = numeric(B) 
 for(b in 1:B){ ind = sample(length(z),n) # n = length(x) 
 x1 = z[ind] 
 y1 = z[-ind]
 t.perm[b] = t.test(x1,y1)$statistic } 
 
  t.perm = c(t.perm, t0) 
  
#Quelle p-valeur obtenez-vous?
  
 pval = mean(t.perm >= t0)
 
# Representer la distribution de la statistique 
 #de test obtenue lors des permutations 
 # et positionner la valeur observee sur l'echantillon. 
 
 hist(t.perm,col = "red", main = "Distribution de la statistique de test des permutations")
abline(v=t0,col="yellow")
 
#5 

#Représenter l'évolution de la p-valeur estimée quand on passe 
#de B = 100 a B = 2000 tirages par pas de 100. La procedure converge t'elle rapidement? 
par(mfrow=c(1,1))


pval=numeric()

for (B in seq(100,2000,100)){
t.perm = numeric(B) 
for(b in 1:B){ ind = sample(length(z),n) # n = length(x) 
x1 = z[ind] 
y1 = z[-ind]
t.perm[b] = t.test(x1,y1)$statistic }

t.perm = c(t.perm, t0) 

pval.B = mean(t.perm >= t0)
pval=c(pval,pval.B)

}
hist(pval)

#6  Faire la meme analyse pour comparer l'alimentation 
#par linseed et sunflower en considérant B = 1000

#on fait la meme démarche que précédemment en changeant les vecteurs x et y



##----------Exercice 2 - tests de permutation et dé pipé------------------##

#1. Quel test paramétrique peut-on utiliser et avec quelle hypothese nulle? 
#Donner une d´e???nition intuitive de la statistique de test. 

?kable
library(kableExtra)


