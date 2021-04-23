#Chargement des packages
using MLDatasets
using DecisionTree
using Plots
using TestImages
using ImageView
using ImageMagick
using Gtk.ShortNames
using DecisionTree

#chargement des données
#train
train_x, train_y = MNIST.traindata()
#test
test_x, test_y = MNIST.testdata()

#affichage des premières images
#chiffre en noir sur fond blonc pour une meilleur visualisation
#############
grid, frames, canvases = canvasgrid((4,4))  # 4 row, 4 columns
imshow(canvases[1,1], transpose(1 .- test_x[:,:,1]))
imshow(canvases[1,2], transpose(1 .- test_x[:,:,2]))
imshow(canvases[1,3], transpose(1 .- test_x[:,:,3]))
imshow(canvases[1,4], transpose(1 .- test_x[:,:,4]))
imshow(canvases[2,1], transpose(1 .- test_x[:,:,5]))
imshow(canvases[2,2], transpose(1 .- test_x[:,:,6]))
imshow(canvases[2,3], transpose(1 .- test_x[:,:,7]))
imshow(canvases[2,4], transpose(1 .- test_x[:,:,8]))
imshow(canvases[3,1], transpose(1 .- test_x[:,:,9]))
imshow(canvases[3,2], transpose(1 .- test_x[:,:,10]))
imshow(canvases[3,3], transpose(1 .- test_x[:,:,11]))
imshow(canvases[3,4], transpose(1 .- test_x[:,:,12]))
imshow(canvases[4,1], transpose(1 .- test_x[:,:,13]))
imshow(canvases[4,2], transpose(1 .- test_x[:,:,14]))
imshow(canvases[4,3], transpose(1 .- test_x[:,:,45]))
imshow(canvases[4,4], transpose(1 .- test_x[:,:,16]))
win = Window(grid)
Gtk.showall(win)

## modification  test
test_y=string.(test_y)
test_x=reshape(test_x[:,:,1:size(test_x)[3]],:,size(test_x)[3])
test_x=transpose(test_x)
test_x=float.(test_x)
## modification train
train_y=string.(train_y)
train_x=reshape(train_x[:,:,1:size(train_x)[3]],:,size(train_x)[3])
train_x=transpose(train_x)
train_x=float.(train_x)

#Reformation du jeu de données complet
X=vcat(train_x,test_x)
Y=vcat(train_y,test_y)

##arbre de decision
features = train_x
labels = train_y
# the data loaded are of type Array{Any}
# cast them to concrete types for better performance
features = float.(features)
labels   = string.(labels)


#Modèle arbre de décision avec une profondeur de 2
model1 = DecisionTreeClassifier(max_depth=2)
fit!(model1, train_x, train_y)
#Visualisation de l'arbre
print_tree(model1, 5)
#Prédiction des y
predict(model1,test_x)
predict_proba(model1,test_x)


# train depth-truncated classifier
model2 = build_tree(labels,features)
# prune tree: merge leaves having >= 90% combined purity (default: 100%)
model2 = prune_tree(model2, 0.9)
# pretty print of the tree, to a depth of 5 nodes (optional)
print_tree(model2, 5)

#Prédiction des y
predictions=apply_tree(model2, test_x)
#Calcul de l'accuracy par validation croisée
n_folds=3
accuracy = nfoldCV_tree(test_y, test_x, n_folds)



# train random forest classifier
# using 2 random features, 10 trees, 0.5 portion of samples per tree, and a maximum tree depth of 6
models = build_forest(labels, features, 2, 10, 0.5, 6)
# apply learned model
apply_forest(models, test_x)
# run 3-fold cross validation of pruned tree
#classifcation parfaite accuracy de 1 // etonnant
n_folds=3
labels = train_y
features = train_x #test_x
accuracy = nfoldCV_tree(test_y,test_x, n_folds)


# set of classification parameters and respective default values
# n_subfeatures: number of features to consider at random per split (default: -1, sqrt(# features))
# n_trees: number of trees to train (default: 10)
# partial_sampling: fraction of samples to train each tree on (default: 0.7)
# max_depth: maximum depth of the decision trees (default: no maximum)
# min_samples_leaf: the minimum number of samples each leaf needs to have (default: 5)
# min_samples_split: the minimum number of samples in needed for a split (default: 2)
# min_purity_increase: minimum purity needed for a split (default: 0.0)
labels = test_y  #train_y
features = test_x #train_x
n_subfeatures=-1; n_trees=10; partial_sampling=0.7; max_depth=-1
min_samples_leaf=5; min_samples_split=2; min_purity_increase=0.0
# A quoi sert ce premier modèle ?
model1   =   build_forest(labels, features,
                          n_subfeatures,
                          n_trees,
                          partial_sampling,
                          max_depth,
                          min_samples_leaf,
                          min_samples_split,
                          min_purity_increase)
#calcul de l'accuracy
accuracy = nfoldCV_forest(labels, features,
                          n_folds,
                          n_subfeatures,
                          n_trees,
                          partial_sampling,
                          max_depth,
                          min_samples_leaf,
                          min_samples_split,
                          min_purity_increase)

#Choix des paramètres pour les 2 modèles suivants
n_subfeatures=0
max_depth=-1
min_samples_leaf=1
min_samples_split=2
min_purity_increase=0.0
pruning_purity = 0.9

#Modèle d'arbre de décision avec un jeu d'apprentissage et un jeu de test
model=build_tree(train_y, train_x,
                        n_subfeatures,
                        max_depth,
                        min_samples_leaf,
                        min_samples_split,
                        min_purity_increase)

#Calcul de l'accuracy par validation croisée sur les données complètes
accuracy = nfoldCV_tree(Y, X,
                        n_folds,
                        pruning_purity,
                        max_depth,
                        min_samples_leaf,
                        min_samples_split,
                        min_purity_increase)


#test tout le jeu de données ??
