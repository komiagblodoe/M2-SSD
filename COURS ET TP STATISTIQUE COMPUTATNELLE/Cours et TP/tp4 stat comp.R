
#TP no 4 - Tests par permutation


#Exercice 1 - tests de permutation et diff�erence de moyennes

#1

attach(chickwts)
View(chickwts)

# De combien d'observations dispose 
#t'on pour chaque type de nourriture?

(x<-table(chickwts))
colSums(x)


(model.chickwts<-lm(weight~feed, data = chickwts))

#Validation du mod�le

#Test de la normalit� des r�sidus : On effectue
#un test de Shapiro sur les r�sidus du mod�le :


shapiro.test(residuals(model.chickwts))
#On ne rejette donc pas l'hypoth�se de normalit�.

boxplot(chickwts$weight~chickwts$feed, 
        col="red", main="poids des poulets en fonction des nourritures")

#2

#Test de l'homosc�dasticit� : Puisque l'hypoth�se de normalit� n'est pas rejet�e,
#on peut effectuer un test de Bartlett pour tester l'homog�n�it� des variances :
 
bartlett.test(weight~feed, data=chickwts)
#L'hypoth�se nulle n'est pas rejet�e, on ne rejette donc pas l'hypoth�se d'homosc�dasticit�


#Interpr�tation

anova(model.chickwts)

#On voit ainsi qu'on a un effet tr�s marqu� du type de nourriture. Plus pr�cis�ment, cela implique qu'au moins 1 des
#types de nourriture est significativement diff�rent des autres

#Pour affiner les conclusions, faisons le test de TUkey
#Cette fonction ne fonctionne pas avec des objets lm, mais avec des objets aov, que l'on
#obtient de la mani�re suivante


aov.chickwts<-aov(weight~feed, data=chickwts)
summary(aov.chickwts)

hsd.chickwts<-TukeyHSD(aov.chickwts)
hsd.chickwts
plot(hsd.chickwts)

#. Tous les intervalles de confiance qui ne recoupent pas 0 r�v�lent des diff�rences
#significatives d'effets.
#On peut r�cup�rer les effets (gr�ce � la fonction model.tables) de chaque type de nourriture et les ordonner :

effects.chickwts<-model.tables(aov.chickwts)
sort(effects.chickwts[["tables"]][["feed"]])

#4. Impl�menter la proc�dure de permutation "randomis�e"
#d�crite en cours pour un nombre B = 1000 de tirages

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

#Repr�senter l'�volution de la p-valeur estim�e quand on passe 
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
#par linseed et sunflower en consid�rant B = 1000

#on fait la meme d�marche que pr�c�demment en changeant les vecteurs x et y



##----------Exercice 2 - tests de permutation et d� pip�------------------##

#1. Quel test param�trique peut-on utiliser et avec quelle hypothese nulle? 
#Donner une d�e???nition intuitive de la statistique de test. 

?kable
library(kableExtra)


