# Modele statistique predictif de barriere
Modele statistique prédictif de barrière pour améliorer la modélisation de réseau écologique 

Ce travail nécessite de lire le mémoire intitulé "Écologie urbaine du hérisson : Méthodologie de l'étude des barrières dans la modélisation du réseau écologique pour limiter la fragmentation des habitats du hérisson à Caen" pour comprendre davantage la méthodologie mise en place et les objectifs de ce modèle. 

Le code intitulé "Code_R_Entrainement_Modele.R" est le code permettant d'entraîner le modèle des forêts aléatoires à la classification des types de barrières. 
Pour l'utilisation de ce code, les tableaux d'entraînement et de test du fichier DATA sont nécessaires. 

Le code intitulé "Code_R_Cadastre.R" est le code permettant de créer une zone tampon autour de chaque ligne du plan cadastral afin d'en calculer la surface pour appliquer ensuite le modèle des forêts aléatoires entraîné. 
Toutes les occupations du sol nécessaires pour faire tourner ce code se trouvent dans le fichier SIG.7z qui doit être dézippé
