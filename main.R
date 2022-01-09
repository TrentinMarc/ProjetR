#########################################################################
#############          Analyse Exploratoire Supervisée      ####################
#########################################################################
#########################################################################

# Chargement des donnees
data <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

##On retie ces deux champs qui ne nous servent pas dans l'étude
data <- subset(data, select=-customer)
data <- subset(data, select=-ncust)

#histogramme des defauts en fonction du niveau d'étude
qplot(ed, data=data, fill=default, bins=5, xlab='Niveau d\'éducation relativement au baccalauréat', ylab='Default') + theme(text = element_text(size=16))
#histogramme des defauts en fonction du nombre d'année à l' adresse actuelle
qplot(address, data=data, fill=default, bins=7,xlab='Nombre \' année à l\' adresse actuelle' ,ylab='Default') + theme(text = element_text(size=16))
#histogramme des defauts en fonction du Nb année avec employeur actuel
qplot(employ, data=data, fill=default, bins=7,xlab='Nb année avec employeur actuel',ylab='Default') + theme(text = element_text(size=16))
#histogramme des defauts en fonction de l'age
qplot(age, data=data, fill=default, bins=5, xlab='Age en nombre d\'année', ylab='Default') + theme(text = element_text(size=16))
#histogramme des defauts en fonction du revenu
qplot(income, data=data, fill=default, bins=20, xlab='Revenus du foyer en milliers de $',ylab='Default') + theme(text = element_text(size=16))


#boite à moustache de l'age
boxplot(age~default, data=data, col= c("red","blue"), main="Age par rappart à default", ylab="age", xlab="defaut")
#boite à moustache de l'address
boxplot(address~default, data=data, col= c("magenta","cyan"), main="adresse par rapport à default", ylab="adresse", xlab="defaut")
#boite à moustache de l'emploiy
boxplot(employ~default, data=data, col= c("magenta","cyan"), main="emploi par rapport à default", ylab="emploi", xlab="defaut")


#-------------------------#
# PREPARATION DES DONNEES #
#-------------------------#


# Creation des ensembles d'apprentissage et de test (60% / 40%)
data_EA <- data[1:800,]
data_ET <- data[801:1200,]

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

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 10
tree_rpart_Gini10 <- rpart(default~., data_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 10))
plot(tree_rpart_Gini10)
text(tree_rpart_Gini10, pretty = 0)

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 5
tree_rpart_Gini5 <- rpart(default~., data_EA, parms = list(split = "gini"), control = rpart.control(minbucket = 5))
plot(tree_rpart_Gini5)
text(tree_rpart_Gini5, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 10
tree_rpart_Gain10 <- rpart(default~., data_EA, parms = list(split = "information"), control = rpart.control(minbucket = 10))
plot(tree_rpart_Gain10)
text(tree_rpart_Gain10, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 5
tree_rpart_Gain5 <- rpart(default~., data_EA, parms = list(split = "information"), control = rpart.control(minbucket = 5))
plot(tree_rpart_Gain5)
text(tree_rpart_Gain5, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS rpart() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Application de tree_rpart_Gini10 a l'ensemble de test
test_rpart_Gini10 <- predict(tree_rpart_Gini10, data_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rpart_Gini10 <- nrow(data_ET[data_ET$default==test_rpart_Gini10,])/nrow(data_ET))

# Application de tree_rpart_Gain10 (identique a tree_rpart_Gain5, tree_rpart_Gini5) a l'ensemble de test
test_rpart_Gain10 <- predict(tree_rpart_Gain10, data_ET, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rpart_Gain10 <- nrow(data_ET[data_ET$default==test_rpart_Gain10,])/nrow(data_ET))

#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS C5.0() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Apprentissage 1er paramétrage pour C5.0
tree_C50_10T <- C5.0(default~., data_EA, control = C5.0Control(minCases = 10, noGlobalPruning = T))
plot(tree_C50_10T, type="simple")

# Apprentissage 2nd paramétrage pour C5.0
tree_C50_10F <- C5.0(default~., data_EA, control = C5.0Control(minCases = 10, noGlobalPruning = F))
plot(tree_C50_10F, type="simple")

# Apprentissage 3eme paramétrage pour C5.0
tree_C50_5T <- C5.0(default~., data_EA, control = C5.0Control(minCases = 5, noGlobalPruning = T))
plot(tree_C50_5T, type="simple")

# Apprentissage 4eme paramétrage pour C5.0
tree_C50_5F <- C5.0(default~., data_EA, control = C5.0Control(minCases = 5, noGlobalPruning = F))
plot(tree_C50_5F, type="simple")

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS C5.0() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour C5.0()
test_C50_10T <- predict(tree_C50_10T, data_ET, type="class")
print(taux_C50_10T <- nrow(data_ET[data_ET$default==test_C50_10T,])/nrow(data_ET))

# Test et taux de succes pour le 2nd paramétrage pour C5.0()
test_C50_10F <- predict(tree_C50_10F, data_ET, type="class")
print(taux_C50_10F <- nrow(data_ET[data_ET$default==test_C50_10F,])/nrow(data_ET))

# Test et taux de succes pour le 3eme paramétrage pour C5.0()
test_C50_5T <- predict(tree_C50_5T, data_ET, type="class")
print(taux_C50_5T <- nrow(data_ET[data_ET$default==test_C50_5T,])/nrow(data_ET))

# Test et taux de succes pour le 4eme paramétrage pour C5.0()
test_C50_5F <- predict(tree_C50_5F, data_ET, type="class")
print(taux_C50_5F <- nrow(data_ET[data_ET$default==test_C50_5F,])/nrow(data_ET))


#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS tree() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#


# Apprentissage 1er paramétrage pour tree()
tree_tr_dev10 <- tree(default~., data_EA, split = "deviance", control = tree.control(nrow(data_EA), mincut = 10))
plot(tree_tr_dev10)
text(tree_tr_dev10, pretty = 0)

# Apprentissage 2nd paramétrage pour tree()
tree_tr_dev5 <- tree(default~., data_EA, split = "deviance", control = tree.control(nrow(data_EA), mincut = 5))
plot(tree_tr_dev5)
text(tree_tr_dev5, pretty = 0)

# Apprentissage 3eme paramétrage pour tree()
tree_tr_gini10 <- tree(default~., data_EA, split = "gini", control = tree.control(nrow(data_EA), mincut = 10))
plot(tree_tr_gini10)
text(tree_tr_gini10, pretty = 0)

# Apprentissage 4eme paramétrage pour tree()
tree_tr_gini5 <- tree(default~., data_EA, split = "gini", control = tree.control(nrow(data_EA), mincut = 5))
plot(tree_tr_gini5)
text(tree_tr_gini5, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS tree() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour tree()
test_tr_dev10 <- predict(tree_tr_dev10, data_ET, type="class")
print(taux_tr_dev10 <- nrow(data_ET[data_ET$default==test_tr_dev10,])/nrow(data_ET))

# Test et taux de succes pour le 2nd paramétrage pour tree()
test_tr_dev5 <- predict(tree_tr_dev5, data_ET, type="class")
print(taux_tr_dev5 <- nrow(data_ET[data_ET$default==test_tr_dev5,])/nrow(data_ET))

# Test et taux de succes pour le 3eme paramétrage pour tree()
test_tr_gini10 <- predict(tree_tr_gini10, data_ET, type="class")
print(taux_tr_gini10 <- nrow(data_ET[data_ET$default==test_tr_gini10,])/nrow(data_ET))

# Test et taux de succes pour le 4eme paramétrage pour tree()
test_tr_gini5 <- predict(tree_tr_gini5, data_ET, type="class")
print(taux_tr_gini5 <- nrow(data_ET[data_ET$default==test_tr_gini5,])/nrow(data_ET))

#########################################################################
#############          Matrices de confusions        ####################
#########################################################################
#########################################################################
# Matrice de confusion pour rpart Gain 10
mc_rpart_gain10 <- table(data_ET$default, test_rpart_Gain10)
print(mc_rpart_gain10)
# Rappel
mc_rpart_gain10[2,2]/(mc_rpart_gain10[2,2]+mc_rpart_gain10[2,1])
# Sp�cificit�
mc_rpart_gain10[1,1]/(mc_rpart_gain10[1,1]+mc_rpart_gain10[1,2])
# Pr�cision 
mc_rpart_gain10[2,2]/(mc_rpart_gain10[2,2]+mc_rpart_gain10[1,2])
# Taux de Vrais N�gatifs 
mc_rpart_gain10[1,1]/(mc_rpart_gain10[1,1]+mc_rpart_gain10[2,1])

# Matrice de confusion pour c50 10F
mc_c50_10F <- table(data_ET$default, test_C50_10F)
print(mc_c50_10F)
# Rappel
mc_c50_10F[2,2]/(mc_c50_10F[2,2]+mc_c50_10F[2,1])
# Sp�cificit�
mc_c50_10F[1,1]/(mc_c50_10F[1,1]+mc_c50_10F[1,2])
# Pr�cision 
mc_c50_10F[2,2]/(mc_c50_10F[2,2]+mc_c50_10F[1,2])
# Taux de Vrais N�gatifs 
mc_c50_10F[1,1]/(mc_c50_10F[1,1]+mc_c50_10F[2,1])

# Matrice de confusion pour Tree dev 5
mc_tree_dev5 <- table(data_ET$default, test_tr_dev5)
print(mc_tree_dev5)
# Rappel
mc_tree_dev5[2,2]/(mc_tree_dev5[2,2]+mc_tree_dev5[2,1])
# Sp�cificit�
mc_tree_dev5[1,1]/(mc_tree_dev5[1,1]+mc_tree_dev5[1,2])
# Pr�cision 
mc_tree_dev5[2,2]/(mc_tree_dev5[2,2]+mc_tree_dev5[1,2])
# Taux de Vrais N�gatifs 
mc_tree_dev5[1,1]/(mc_tree_dev5[1,1]+mc_tree_dev5[2,1])


#########################################################################
#############          Courbes ROC        ####################
#########################################################################
#########################################################################

install.packages("rpart.plot")
library(rpart.plot)
install.packages("ROCR")
library(ROCR)
#------------------------------#
# COURBE ROC DE L'ARBRE 'rpart' #
#------------------------------#

# Génération des probabilites de prediction sur l'ensemble de test
prob_rpart <- predict(tree_rpart_Gain10, data_ET, type="prob")
print(prob_rpart)

# Génération des donnees necessaires pour la courbe ROC
roc_pred_rpart <- prediction(prob_rpart[,2], data_ET$default)
print(roc_pred_rpart)
roc_pred_rpart <- prediction(prob_rpart[,2], data_ET$default, label.ordering = c("Non", "Oui"))

# Calcul des taux de vrais positifs (tpr) et taux de faux positifs (fpr)
roc_perf_rpart <- performance(roc_pred_rpart,"tpr","fpr")
print(roc_perf_rpart)

# Tracage de la  courbe ROC
plot(roc_perf_rpart, col = "green")


#-------------------------------#
#  COURBE ROC DE L'ARBRE 'C5.0' #
#-------------------------------#

# Génération des probabilites de prediction sur l'ensemble de test
prob_c50 <- predict(tree_C50_10F, data_ET, type="prob")

# Génération des donnees necessaires pour la courbe ROC
roc_pred_c50 <- prediction(prob_c50[,2], data_ET$default)

# Calcul des taux de vrais positifs (tpr) et taux de faux positifs (fpr)
roc_perf_c50 <- performance(roc_pred_c50,"tpr","fpr")

# Tracage de la  courbe ROC
plot(roc_perf_c50, add = TRUE, col = "blue")

#------------------------------#
# COURBE ROC DE L'ARBRE 'tree' #
#------------------------------#

# Génération des probabilites de prediction sur l'ensemble de test
prob_tree <- predict(tree_tr_dev5, data_ET, type="vector")

# Génération des donnees necessaires pour la courbe ROC
roc_pred_tree <- prediction(prob_tree[,2], data_ET$default)

# Calcul des taux de vrais positifs (tpr) et taux de faux positifs (fpr)
roc_perf_tree <- performance(roc_pred_tree,"tpr","fpr")

# Ajout de la courbe ROC au precedent graphique
plot(roc_perf_tree, add = TRUE, col = "red")

#----------------------------------------#
# CALCUL DES INDICES AUC DES COURBES ROC #
#----------------------------------------#

# Calcul de l'AUC à partir des données générées : arbre 'rpart()'
auc_rpart <- performance(roc_pred_rpart, "auc")
# Affichage de la structure de l'objet 'auc_tree1' généré
str(auc_rpart)
# Affichage de la valeur de l'AUC stockee dans l'attribut 'y.values' de 'auc_tree1'
attr(auc_rpart, "y.values")

# Calcul de l'AUC de l'arbre 'C5.0()'
auc_c50 <- performance(roc_pred_c50, "auc")
# Affichage de la valeur de l'AUC
attr(auc_c50, "y.values")

# Calcul de l'AUC de l'arbre 'tree()'
auc_tree <- performance(roc_pred_tree, "auc")
# Affichage de la valeur de l'AUC
attr(auc_tree, "y.values")

#########################################################################
#############          Knn, forest etc...       ####################
#########################################################################
#########################################################################


#--------------------------------------------#
# INSTALLATION/MAJ DES LIRAIRIES NECESSAIRES #
#--------------------------------------------#

install.packages("rpart")
install.packages("randomForest")
install.packages("kknn")
install.packages("ROCR")
install.packages("e1071")
install.packages("naivebayes")
install.packages("ROCR")

#--------------------------------------#
# ACTIVATION DES LIRAIRIES NECESSAIRES #
#--------------------------------------#

library(randomForest)
library(kknn)
library(ROCR)

library(e1071)
library(naivebayes)
library(ROCR)


#----------------#
# RANDOM FORESTS #
#----------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_rf <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur
  rf <- randomForest(default~., data_EA, ntree = arg1, mtry = arg2)
  
  # Test du classifeur : classe predite
  rf_class <- predict(rf,data_ET, type="response")
  
  # Matrice de confusion
  print(table(data_ET$default, rf_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  rf_prob <- predict(rf, data_ET, type="prob")
  
  # Courbe ROC
  rf_pred <- prediction(rf_prob[,2], data_ET$default)
  rf_perf <- performance(rf_pred,"tpr","fpr")
  plot(rf_perf, main = "Random Forests randomForest()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  rf_auc <- performance(rf_pred, "auc")
  cat("AUC = ", as.character(attr(rf_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_knn <- function(arg1, arg2, arg3, arg4){
  # Apprentissage et test simultanes du classifeur de type k-nearest neighbors
  knn <- kknn(default~., data_EA, data_ET, k = arg1, distance = arg2)
  
  # Matrice de confusion
  print(table(data_ET$default, knn$fitted.values))
  
  # Courbe ROC
  knn_pred <- prediction(knn$prob[,2], data_ET$default)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  plot(knn_perf, main = "Classifeurs K-plus-proches-voisins kknn()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}
#-------------------------#
# SUPPORT VECTOR MACHINES #
#-------------------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_svm <- function(arg1, arg2, arg3){
  # Apprentissage du classifeur
  svm <- svm(default~., data_EA, probability=TRUE, kernel = arg1)
  
  # Test du classifeur : classe predite
  svm_class <- predict(svm, data_ET, type="response")
  print(table(data_ET$default, svm_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  svm_prob <- predict(svm, data_ET, probability=TRUE)
  svm_prob <- attr(svm_prob, "probabilities")
  
  # Courbe ROC
  svm_pred <- prediction(svm_prob[,2], data_ET$default)
  svm_perf <- performance(svm_pred,"tpr","fpr")
  
  plot(svm_perf, main = "Support vector machines svm()", add = arg2, col = arg3) 
  
  svm_auc <- performance(svm_pred, "auc")
  
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#-------------#
# NAIVE BAYES #
#-------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_nb <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur 
  nb <- naive_bayes(default~., data_EA, laplace = arg1, usekernel = arg2)
  
  # Test du classifeur : classe predite
  nb_class <- predict(nb, data_ET, type="class")
  
  # Matrice de confusion
  print(table(data_ET$default, nb_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  nb_prob <- predict(nb, data_ET, type="prob")
  
  # Courbe ROC
  nb_pred <- prediction(nb_prob[,2], data_ET$default)
  nb_perf <- performance(nb_pred,"tpr","fpr")
  plot(nb_perf, main = "Classifieurs bayésiens naïfs naiveBayes()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}
#-------------------------------------------------#
# APPRENTISSAGE DES CONFIGURATIONS ALGORITHMIQUES #
#-------------------------------------------------#

# Forets d'arbres decisionnels aleatoires
test_rf(300, 3, FALSE, "red")
test_rf(300, 5, TRUE, "blue")
test_rf(500, 3, TRUE, "green")
test_rf(500, 5, TRUE, "orange")

# K plus proches voisins
test_knn(50, 1, FALSE, "red")
test_knn(50, 4, TRUE, "blue")
test_knn(100, 1, TRUE, "green")
test_knn(100, 4, TRUE, "orange")

# Support vector machines
test_svm("linear", FALSE, "red")
test_svm("polynomial", TRUE, "blue")
test_svm("radial", TRUE, "green")
test_svm("sigmoid", TRUE, "orange")

# Naive Bayes
test_nb(0, FALSE, FALSE, "red")
test_nb(20, FALSE, TRUE, "blue")
test_nb(0, TRUE, TRUE, "green")
test_nb(20, TRUE, TRUE, "orange")


##########Application sur le second jeu de données + sauvegarde csv############
#### Organisation des données ######
data_new <- read.csv("Data Projet New.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
data_new <- subset(data_new, select=-customer)
data_new <- subset(data_new, select=-ncust)


###APPLICATION DE SVM ####
svm <- svm(default~., data, probability=TRUE, kernel = "linear")

# Test du classifeur : classe predite
svm_class <- predict(svm, data_new, type="response")
print(table(data_new$default, svm_class))

# Test du classifeur : probabilites pour chaque prediction
svm_prob <- predict(svm, data_new, probability=TRUE)
svm_prob <- attr(svm_prob, "probabilities")

data_new$Predictions <- svm_class

resultat <- read.csv("Data Projet New.csv", header = TRUE, sep = ",", dec = ".")
resultat$PredictionsDefaut <- svm_class
resultat$PROBA_OUI <- svm_prob[,2]
resultat$PROBA_NON <- svm_prob[,1]

## On retire tous les champsinutiles
resultat <- subset(resultat, select=-age)
resultat <- subset(resultat, select=-ed)
resultat <- subset(resultat, select=-employ)
resultat <- subset(resultat, select=-address)
resultat <- subset(resultat, select=-income)
resultat <- subset(resultat, select=-creddebt)
resultat <- subset(resultat, select=-othdebt)
resultat <- subset(resultat, select=-branch)
resultat <- subset(resultat, select=-ncust)
resultat <- subset(resultat, select=-debtinc)

dataframe_result <- data.frame(resultat$'PredictionsDefaut', svm_class, svm_prob[,2], svm_prob[,1])
colnames(dataframe_result) = list("Classes", "Prediction", "PROBA_OUI", "PROBA_NON")

summary(dataframe_result)


#Export du résultat en CSV

write.table(resultat, file='RésultatsProjet.csv', sep="\t", dec=".", row.names = F)
