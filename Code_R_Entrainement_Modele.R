# Chargement packages
install.packages("rstudioapi")
library(dplyr)
library(readxl)
library(sf)
library(foreign)
library(imputeTS)
library(tidyr)


## Import données
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Tableau_Entrainement <- read_excel("DATA/Tableau_Entrainement.xlsx")
View(Tableau_Entrainement)
Tableau_Test <- read_excel("DATA/Tableau_Test.xlsx")
View(Tableau_Test)

## Modèle stat
library(caret)

table(Tableau_Entrainement$Classe)
names(Tableau_Entrainement)

table(Tableau_Test$Classe)
names(Tableau_Test)

# premier test : 
plsFit <- train(
  Classe ~ Surface_attract + Surface_Peufreq + Surface_struct + Surface_bati + Surface_non_bati + Surface_Routes + Surface_Culture + Surface_Surf_perm, 
  data = Tableau_Entrainement,
  method = "pls",
  ## Center and scale the predictors for the training
  ## set and all future samples.
  preProc = c("center", "scale")
)

plsClasses <- predict(plsFit, newdata = Tableau_Test)
Tableau_Test$Classe <- as.factor(Tableau_Test$Classe)
confusionMatrix(data = plsClasses, Tableau_Test$Classe)
    #Résultat à 63.88% de précision

# Deuxième test : 
rfFit <- train(
  Classe ~ Surface_attract + Surface_Peufreq + Surface_struct + Surface_bati + Surface_non_bati + Surface_Routes + Surface_Surf_Eau + Surface_Culture + Surface_Surf_perm, 
  data = Tableau_Entrainement,
  method = "rf",
  ## Center and scale the predictors for the training
  ## set and all future samples.
  preProc = c("center", "scale")
)

rfClasses <- predict(rfFit, newdata = Tableau_Test)
Tableau_Test$Classe <- as.factor(Tableau_Test$Classe)
confusionMatrix(data = rfClasses, Tableau_Test$Classe)
    #Résultat à 64.55% de précision

# Troisième Test : 
library(rpart)
treeFit <- rpart( Classe ~ Surface_attract + Surface_Peufreq + Surface_struct + Surface_bati + Surface_non_bati + Surface_Routes + Surface_Culture + Surface_Surf_perm, data = Tableau_Entrainement)
treeClasses <- predict(treeFit, newdata = Tableau_Test, type = "class")
confusionMatrix(data = treeClasses, reference = Tableau_Test$Classe)
    #Résultat à 64.21% de précision


