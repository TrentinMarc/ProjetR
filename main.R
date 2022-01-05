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
tree_rp1 <- rpart(default~., produit_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 10))
plot(tree_rp1)
text(tree_rp1, pretty = 0)

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 5
tree_rp2 <- rpart(default~., produit_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 5))
plot(tree_rp2)
text(tree_rp2, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 10
tree_rp3 <- rpart(default~., produit_EA, parms = list(split = "information"), control = rpart.control(minbucket = 10))
plot(tree_rp3)
text(tree_rp3, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 5
tree_rp4 <- rpart(default~., produit_EA, parms = list(split = "information"), control = rpart.control(minbucket = 5))
plot(tree_rp4)
text(tree_rp4, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS rpart() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Application de tree_rp1 (identique a tree_rp3) a l'ensemble de test
test_rp1 <- predict(tree_rp1, produit_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rp1 <- nrow(produit_ET[produit_ET$default==test_rp1,])/nrow(produit_ET))

# Application de tree2 (identique a tree_rp4) a l'ensemble de test
test_rp2 <- predict(tree_rp2, produit_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rp2 <- nrow(produit_ET[produit_ET$default==test_rp2,])/nrow(produit_ET))

#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS C5.0() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Affichage de l'aide 
? C5.0()

# Apprentissage 1er paramétrage pour C5.0
tree_C51 <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 10, noGlobalPruning = T))
plot(tree_C51, type="simple")

# Apprentissage 2nd paramétrage pour C5.0
tree_C52 <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 10, noGlobalPruning = F))
plot(tree_C52, type="simple")

# Apprentissage 3eme paramétrage pour C5.0
tree_C53 <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 5, noGlobalPruning = T))
plot(tree_C53, type="simple")

# Apprentissage 4eme paramétrage pour C5.0
tree_C54 <- C5.0(default~., produit_EA, control = C5.0Control(minCases = 5, noGlobalPruning = F))
plot(tree_C54, type="simple")

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS C5.0() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour C5.0()
test_C51 <- predict(tree_C51, produit_ET, type="class")
print(taux_C51 <- nrow(produit_ET[produit_ET$default==test_C51,])/nrow(produit_ET))

# Test et taux de succes pour le 2nd paramétrage pour C5.0()
test_C52 <- predict(tree_C52, produit_ET, type="class")
print(taux_C52 <- nrow(produit_ET[produit_ET$default==test_C52,])/nrow(produit_ET))

# Test et taux de succes pour le 3eme paramétrage pour C5.0()
test_C53 <- predict(tree_C53, produit_ET, type="class")
print(taux_C53 <- nrow(produit_ET[produit_ET$default==test_C53,])/nrow(produit_ET))

# Test et taux de succes pour le 4eme paramétrage pour C5.0()
test_C54 <- predict(tree_C54, produit_ET, type="class")
print(taux_C54 <- nrow(produit_ET[produit_ET$default==test_C54,])/nrow(produit_ET))


#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS tree() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Affichage de l'aide 
? tree()

# Apprentissage 1er paramétrage pour tree()
tree_tr1 <- tree(default~., produit_EA, split = "deviance", control = tree.control(nrow(produit_EA), mincut = 10))
plot(tree_tr1)
text(tree_tr1, pretty = 0)

# Apprentissage 2nd paramétrage pour tree()
tree_tr2 <- tree(default~., produit_EA, split = "deviance", control = tree.control(nrow(produit_EA), mincut = 5))
plot(tree_tr2)
text(tree_tr2, pretty = 0)

# Apprentissage 3eme paramétrage pour tree()
tree_tr3 <- tree(default~., produit_EA, split = "gini", control = tree.control(nrow(produit_EA), mincut = 10))
plot(tree_tr3)
text(tree_tr3, pretty = 0)

# Apprentissage 4eme paramétrage pour tree()
tree_tr4 <- tree(default~., produit_EA, split = "gini", control = tree.control(nrow(produit_EA), mincut = 5))
plot(tree_tr4)
text(tree_tr4, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS tree() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour tree()
test_tr1 <- predict(tree_tr1, produit_ET, type="class")
print(taux_tr1 <- nrow(produit_ET[produit_ET$default==test_tr1,])/nrow(produit_ET))

# Test et taux de succes pour le 2nd paramétrage pour tree()
test_tr2 <- predict(tree_tr2, produit_ET, type="class")
print(taux_tr2 <- nrow(produit_ET[produit_ET$default==test_tr2,])/nrow(produit_ET))

# Test et taux de succes pour le 3eme paramétrage pour tree()
test_tr3 <- predict(tree_tr3, produit_ET, type="class")
print(taux_tr3 <- nrow(produit_ET[produit_ET$default==test_tr3,])/nrow(produit_ET))

# Test et taux de succes pour le 4eme paramétrage pour tree()
test_tr4 <- predict(tree_tr4, produit_ET, type="class")
print(taux_tr4 <- nrow(produit_ET[produit_ET$default==test_tr4,])/nrow(produit_ET))

