
#TP STAT NON PARAMETRIQUE

#EX6

#Test de signe

u<-c(12000,13000,21500,17000,15000,22000,11000,21000)
v<-c(11000,20000,31000,28000,26000,30000,16000,29000)

x<-u-v



binom.test(7,8,0.5, alternative = "greater")

# p-value = 0.03516. Au seuil alpha=0.05 on rejette H0

binom.test(7,8,0.5, alternative = "less")
binom.test(7,8,0.5, alternative = "two.sided")



#Test de wilcoson

u<-c(12000,13000,21500,17000,15000,22000,11000,21000)
v<-c(11000,20000,31000,28000,26000,30000,16000,29000)

x<-v-u

#H0 :p(x>0)=1/2. x>0 ie u plus petit que v

wilcox.test(u,v,alternative="less", paired=TRUE)
#ou 
wilcox.test(v,u,alternative="greater", paired=TRUE)

# p-value = 0.01028. Au seuil alpha=0.05 on rejette H0 car alpha > p-value


#Rang de valeur de x

y<-abs(x)
rank(y)

sn<-sum(x>0)
w8plus<- sum(rank(x))-1



#EXO7

Pansement <-c(659,984,397,574,447,479,676,761,647,577)

Points_de_suture <-c(452,587,460,787,351,277,234,516,577,513)

x<-Points_de_suture-Pansement

#H0 :p(x>0)=1/2. 

wilcox.test(Pansement,Points_de_suture, paired = TRUE, alternative = "less")

# p-value = 0.9814. Au seuil alpha=0.05 on conserve H0 car alpha < p-value



#Test de Mann whitney

#Exercice 5

#H: âge d'apparition de la maladie  chez l'homme

#F: âge d'apparition de la maladie  chez la femme

#H0: H et F ont la même loi

Hommes<-c(19, 26, 30, 23, 17, 20)
Femmes<-c(21, 25, 29, 31, 33)

#WH=6 (voir calcul fait sur le papier)

(rang<-rank(c(Hommes,Femmes)))

#H1: la fdr de H > la fdr de F ie H<F

wilcox.test(Hommes,Femmes,paired = FALSE, alternative = "less")

#On peut calculer aussi la pvaleur avec la fonction: 

n<-6
p<-5
WH<-6
p<-pwilcox(WH,n,p)

#Exercice 9

souris_non_traitees<-c(51, 55, 62, 63, 65, 68, 71, 75, 79) 
souris_traitees<-c(47, 49, 53, 57, 60, 61, 67)

#X: le nombre de verres chez les souris non traitées de fdr F
#Y: le nombre de verres chez les souris  traitées de fdr G

#H0: la fonction de répartition de X est la même que celle de Y 

#H1: le nbre de vers des souris traitées 
#est inférieur à celui des souris non traitées ie G>F (traitement efficace ie Y<X)

wilcox.test(souris_non_traitees,souris_traitees, paired = FALSE,alternative = "greater")
#ou 
wilcox.test(souris_traitees,souris_non_traitees,paired = FALSE, alternative = "less")


