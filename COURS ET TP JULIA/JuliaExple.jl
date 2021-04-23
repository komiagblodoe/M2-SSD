
#Des exemples d'utilisation

v=[1,3,2]
sum(v)
# ajout d'éléments
push!(v, 4)
push!(v, 3)
v2=[5;4;2] # équivalent à [5,4,2]
append!(v,v2)
v


# extraction
v[1]
v[end]
v[2:4]
v[2:end]
v[2:end-2]


# opération vectorielle
v^2# Format non correct
3^2
v.^2
sin(v)# format non correct
sin.(v)
# vecteur défini par extension
[sin(x) for x in v]
# composition de fonction
uppercase ∘ first
noms = ["eric","pierre","valérie"]
map(uppercase ∘ first, noms)
map(uppercasefirst,noms)
# Les String sont immu(t)ables
noms2 = similar(noms)
noms2 .= uppercasefirst.(noms)
@. noms2 = uppercasefirst(noms)
# en moins joli mais toutefois très riche
noms3=similar(noms);
for (i,v) = enumerate(noms) # = ou in
	noms3[i]=uppercasefirst(noms[i])
end
noms3

#Mini-Classification
using RDatasets, StatsKit, Plots
iris = dataset("datasets", "iris"); # load the data

features = collect(Matrix(iris[:, 1:4])'); # features to use for clustering
result = kmeans(features, 3); # run K-means for the 3 clusters

# plot with the point color mapped to the assigned cluster index
scatter(iris.PetalLength, iris.PetalWidth, marker_z=result.assignments,
        color=:lightrainbow, legend=false)

#Pour installer un package
#import Pkg;Pkg.add("Plots")

#Creation d'une fonction en Julia

f(x,y)=x+y

#ou bien
function somme(x,y)
	x + y
end

#ou bien
somme2(x,y) = x+y
#ou bien
somme3 = (x,y) -> x+y

somme(2,3), somme2(2,3), somme3(2,3)

#Ou bien
function Σ(x,y)
	x + y
end
Σ2(x,y) = x+y
Σ3 = (x,y) -> x+y
Σ(2,3),Σ2(2,3),Σ3(2,3)


#Fonction pour explorer les heritages de types

function treetypes(typ::Type,depth::Int64=100000;level::Int64=0)
  subtyps=subtypes(typ)
  println("	"^level,isconcretetype(typ) ? "(C)" : "(A)"," ",typ)
  if level <= depth
    for subtyp in subtyps
      if !(subtyp in [Any,Function])
        treetypes(subtyp,depth,level=level+1)
      end
    end
  end
end

#Appel de la fonction sur les types Réel
treetypes(Integer)

#Caractere(simple cote)
'a'
'∀'
#Chaine de caracteres(double cote)
b = "tot"
c = "tit"

#Concatenation de chaines de caracteres(*)
b*c
"chaîne1" * "chaîne2" * "chaîne3"

#Concatenation d'une même chaine de caracteres(^)
"chaine"^3

#Symboles
:toto
typeof(:toto)
Symbol("toto") == :toto

#Matrices
[[1 2 3];[3 4 5]]
mat=[1 2 3; 4 5 6]
mat2=[[1;2] [3;4] [5;6]]
mat2'
transpose(mat2)
[[1 2] [3 4] [5 6]] # Attention
mat3=reshape(1:15,3,5)
mat[2]
mat[2,3]
mat[1:2,2:3]
mat[1:2,[1,end]]
mat[:,[2,2,end]]
mat[[1:3;4;3:4]]
mat3[1:2,[1:2;2:-1:1;end]]
mat3[1:2,union(1:2,2:end)]
mat * mat2
mat * mat2'
mat4=reshape(1:6,2,3)
mat4 * mat2'


#Deuxieme seance

p1=Point(0.0,0.0)
translate(p1,3,4)


#TP NOTé CLASSIFICATION

#packages MLDatasets et DecisionTree installés
using ImageView, Images
using MLDatasets
using DecisionTree
using Plots
#partatge des données
train_x,train_y=MNIST.traindata()
test_x,test_y=MNIST.testdata()
##
plot(train_x[:,:,1])
train_x[:,:,1]
plot(train_x[1,:,:])

#pour la difference matricielle(1.-test pour obtenir les images en vouleurs noires)
test = 1 .- test_x

test1=transpose(test_x[:,:,2])
##package Images ImageMetadata ImageView TestImages installées

#Affichage de quelques images
imshow(test1)
imshow(test[:,:,1],axes=(2,1))
imshow(test[:,:,25],axes=(2,1))
imshow(test[:,:,1:20],axes=(2,1))


#Affichage des 10 premieres images
#hcat a installer

#preview_img=zeros(size(train_x, 1), 0)
 #for i=1:10
	# preview_img=hcat(preview_img, train_x[:,:,i])
#end

#imshow(Gray.(preview_img))

#You can place multiple images in the same window using canvasgrid:
#Ici on affiche certaines valeurs de test sur un même graphique
using ImageView, TestImages, Gtk.ShortNames
grid, frames, canvases = canvasgrid((4,4))  # 4 row, 4 columns
imshow(canvases[1,1], transpose(test_x[:,:,1]))
imshow(canvases[1,2], transpose(test_x[:,:,2]))
imshow(canvases[1,3], transpose(test_x[:,:,3]))
imshow(canvases[1,4], transpose(test_x[:,:,4]))
imshow(canvases[2,1], transpose(test_x[:,:,5]))
imshow(canvases[2,2], transpose(test_x[:,:,6]))
imshow(canvases[2,3], transpose(test_x[:,:,7]))
imshow(canvases[2,4], transpose(test_x[:,:,8]))
imshow(canvases[3,1], transpose(test_x[:,:,9]))
imshow(canvases[3,2], transpose(test_x[:,:,10]))
imshow(canvases[3,3], transpose(test_x[:,:,11]))
imshow(canvases[3,4], transpose(test_x[:,:,12]))
imshow(canvases[4,1], transpose(test_x[:,:,13]))
imshow(canvases[4,2], transpose(test_x[:,:,14]))
imshow(canvases[4,3], transpose(test_x[:,:,15]))
imshow(canvases[4,4], transpose(test_x[:,:,16]))

win = Window(grid)
Gtk.showall(win)

# Modèles et prédictions

using DecisionTree

test_x,test_y=MNIST.testdata()
test_y=string.(test_y)
test_x=reshape(test_x[:,:,1:size(test_x)[3]],:,size(test_x)[3])
test_x=transpose(test_x)

label=train_y
features=train_x

features=float.(features)
label=string.(label)

modèle = build_tree(label, features)
pred=apply_forest(model,test_x)
