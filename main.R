#-------------------------#
# PREPARATION DES DONNEES #
#-------------------------#

# Chargement des donnees
produit <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
str(produit)

# Creation des ensembles d'apprentissage et de test
produit_EA <- produit[1:720,]
produit_ET <- produit[721:1200,]

# Suppression de la variable ID de l'ensemble d'apprentissage
produit_EA <- subset(produit_EA, select=-customer)
# Installation/m-a-j des librairies si nnecessaire
install.packages("rpart")
install.packages("C50")
install.packages("tree")

# Activation des librairies
library(rpart)
library(C50)
library(tree)

#---------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS rpart() AVEC DIFFERENTS PARAMETRAGES #
#---------------------------------------------------------------------#

# Affichage de l'aide 
? rpart()

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 10
tree_rpart_Gini10 <- rpart(default~., produit_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 10))
plot(tree_rpart_Gini10)
text(tree_rpart_Gini10, pretty = 0)

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 5
tree_rpart_Gini5 <- rpart(default~., produit_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 5))
plot(tree_rpart_Gini5)
text(tree_rpart_Gini5, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 10
tree_rpart_Gain10 <- rpart(default~., produit_EA, parms = list(split = "information"), control = rpart.control(minbucket = 10))
plot(tree_rpart_Gain10)
text(tree_rpart_Gain10, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 5
tree_rpart_Gain5 <- rpart(default~., produit_EA, parms = list(split = "information"), control = rpart.control(minbucket = 5))
plot(tree_rpart_Gain5)
text(tree_rpart_Gain5, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS rpart() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Application de tree_rpart_Gini10 a l'ensemble de test
test_rpart_Gini10 <- predict(tree_rpart_Gini10, produit_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rpart_Gini10 <- nrow(produit_ET[produit_ET$default==test_rpart_Gini10,])/nrow(produit_ET))

# Application de tree_rpart_Gain10 (identique a tree_rpart_Gain5, tree_rpart_Gini5) a l'ensemble de test
test_rpart_Gain10 <- predict(tree_rpart_Gain10, produit_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rpart_Gain10 <- nrow(produit_ET[produit_ET$default==test_rpart_Gain10,])/nrow(produit_ET))

#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS C5.0() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Affichage de l'aide 
? C5.0()

# Apprentissage 1er paramétrage pour C5.0
tree_C50_10T <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 10, noGlobalPruning = T))
plot(tree_C50_10T, type="simple")

# Apprentissage 2nd paramétrage pour C5.0
tree_C50_10F <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 10, noGlobalPruning = F))
plot(tree_C50_10F, type="simple")

# Apprentissage 3eme paramétrage pour C5.0
tree_C50_5T <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 5, noGlobalPruning = T))
plot(tree_C50_5T, type="simple")

# Apprentissage 4eme paramétrage pour C5.0
tree_C50_5F <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 5, noGlobalPruning = F))
plot(tree_C50_5F, type="simple")

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS C5.0() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour C5.0()
test_C50_10T <- predict(tree_C50_10T, produit_ET, type="class")
print(taux_C50_10T <- nrow(produit_ET[produit_ET$default==test_C50_10T,])/nrow(produit_ET))

# Test et taux de succes pour le 2nd paramétrage pour C5.0()
test_C50_10F <- predict(tree_C50_10F, produit_ET, type="class")
print(taux_C50_10F <- nrow(produit_ET[produit_ET$default==test_C50_10F,])/nrow(produit_ET))

# Test et taux de succes pour le 3eme paramétrage pour C5.0()
test_C50_5T <- predict(tree_C50_5T, produit_ET, type="class")
print(taux_C50_5T <- nrow(produit_ET[produit_ET$default==test_C50_5T,])/nrow(produit_ET))

# Test et taux de succes pour le 4eme paramétrage pour C5.0()
test_C50_5F <- predict(tree_C50_5F, produit_ET, type="class")
print(taux_C50_5F <- nrow(produit_ET[produit_ET$default==test_C50_5F,])/nrow(produit_ET))


#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS tree() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Affichage de l'aide 
? tree()

# Apprentissage 1er paramétrage pour tree()
tree_tr_dev10 <- tree(default~., produit_EA, split = "deviance", control = tree.control(nrow(produit_EA), mincut = 10))
plot(tree_tr_dev10)
text(tree_tr_dev10, pretty = 0)

# Apprentissage 2nd paramétrage pour tree()
tree_tr_dev5 <- tree(default~., produit_EA, split = "deviance", control = tree.control(nrow(produit_EA), mincut = 5))
plot(tree_tr_dev5)
text(tree_tr_dev5, pretty = 0)

# Apprentissage 3eme paramétrage pour tree()
tree_tr_gini10 <- tree(default~., produit_EA, split = "gini", control = tree.control(nrow(produit_EA), mincut = 10))
plot(tree_tr_gini10)
text(tree_tr_gini10, pretty = 0)

# Apprentissage 4eme paramétrage pour tree()
tree_tr_gini5 <- tree(default~., produit_EA, split = "gini", control = tree.control(nrow(produit_EA), mincut = 5))
plot(tree_tr_gini5)
text(tree_tr_gini5, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS tree() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour tree()
test_tr_dev10 <- predict(tree_tr_dev10, produit_ET, type="class")
print(taux_tr_dev10 <- nrow(produit_ET[produit_ET$default==test_tr_dev10,])/nrow(produit_ET))

# Test et taux de succes pour le 2nd paramétrage pour tree()
test_tr_dev5 <- predict(tree_tr_dev5, produit_ET, type="class")
print(taux_tr_dev5 <- nrow(produit_ET[produit_ET$default==test_tr_dev5,])/nrow(produit_ET))

# Test et taux de succes pour le 3eme paramétrage pour tree()
test_tr_gini10 <- predict(tree_tr_gini10, produit_ET, type="class")
print(taux_tr_gini10 <- nrow(produit_ET[produit_ET$default==test_tr_gini10,])/nrow(produit_ET))

# Test et taux de succes pour le 4eme paramétrage pour tree()
test_tr_gini5 <- predict(tree_tr_gini5, produit_ET, type="class")
print(taux_tr_gini5 <- nrow(produit_ET[produit_ET$default==test_tr_gini5,])/nrow(produit_ET))

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################


