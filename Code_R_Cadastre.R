# Charger les packages nécessaires
install.packages("rstudioapi")
library(sf)
library(dplyr)
library(mapview)

# Charger les shapefiles
Cadastre <- st_read("SIG/Cadastre_Caen_Shp.shp")
Attractif <- st_read("SIG/Final_attractif.shp")
Grande_culture <- st_read("SIG/Final_Grande_culture.shp")
Non_Bati <- st_read("SIG/Final_Non_Bati.shp")
Peu_Frequente <- st_read("SIG/Final_Peufrequente.shp")
Repulsif <- st_read("SIG/Final_repulsif.shp")
Routes <- st_read("SIG/Final_Routes.shp")
Structurante <- st_read("SIG/Final_structurante.shp")
Surface_Permeable <- st_read("SIG/Final_Surf_permeable.shp")
Bati <- st_read("SIG/Bati_Caen.shp")
Surface_En_Eau <- st_read("SIG/Surface_en_eau_Caen.shp")

# Valider les géométries de l'occupation du sol
valid_Cadastre <- Cadastre %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Attractif <- Attractif %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Grande_culture <- Grande_culture %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Non_Bati <- Non_Bati %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Peu_Frequente <- Peu_Frequente %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Repulsif <- Repulsif %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Routes <- Routes %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Structurante <- Structurante %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Surface_Permeable <- Surface_Permeable %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Bati <- Bati %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()
valid_Surface_En_Eau <- Surface_En_Eau %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()

# Créer les centroïdes des barrières
centroides <- st_centroid(Cadastre)

# Valider les géométries des centroïdes
valid_centroides <- centroides %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()

# Créer des tampons de 10 mètres autour des centroïdes valides
tampons <- st_buffer(valid_centroides, dist = 10)

# Valider les géométries des tampons
valid_tampons <- tampons %>%
  filter(st_is_valid(geometry)) %>%
  st_make_valid()

# Fonction pour calculer les surfaces d'intersection et les grouper par `fid`
calculate_surface <- function(tampons, layer, column_name) {
  intersection <- st_intersection(tampons, layer)
  intersection <- intersection %>%
    mutate(Area = st_area(.)) %>%
    group_by(fid) %>%
    summarize(!!column_name := sum(Area, na.rm = TRUE)) %>%
    st_drop_geometry()  # Supprimer les géométries pour la jointure
  return(intersection)
}

# Calculer les surfaces d'intersection pour chaque type de surface
Surface_attract <- calculate_surface(valid_tampons, valid_Attractif, "Surface_attract")
Surface_Culture <- calculate_surface(valid_tampons, valid_Grande_culture, "Surface_Culture")
Surface_non_bati <- calculate_surface(valid_tampons, valid_Non_Bati, "Surface_non_bati")
Surface_Peufreqm <- calculate_surface(valid_tampons, valid_Peu_Frequente, "Surface_Peufreqm")
surface_Repulsif <- calculate_surface(valid_tampons, valid_Repulsif, "Surface_Repulsif")
Surface_Routes <- calculate_surface(valid_tampons, valid_Routes, "Surface_Routes")
Surface_struct <- calculate_surface(valid_tampons, valid_Structurante, "Surface_struct")
Surface_Surf_perm <- calculate_surface(valid_tampons, valid_Surface_Permeable, "Surface_Surf_perm")
Surface_bati <- calculate_surface(valid_tampons, valid_Bati, "Surface_bati")
Surface_Surf_Eau <- calculate_surface(valid_tampons, valid_Surface_En_Eau, "Surface_Surf_Eau")

# Ajouter les colonnes de surface au tableau Cadastre
Cadastre <- Cadastre %>%
  left_join(Surface_attract, by = "fid") %>%
  left_join(Surface_Culture, by = "fid") %>%
  left_join(Surface_non_bati, by = "fid") %>%
  left_join(Surface_Peufreqm, by = "fid") %>%
  left_join(surface_Repulsif, by = "fid") %>%
  left_join(Surface_Routes, by = "fid") %>%
  left_join(Surface_struct, by = "fid") %>%
  left_join(Surface_Surf_perm, by = "fid") %>%
  left_join(Surface_bati, by = "fid") %>%
  left_join(Surface_Surf_Eau, by = "fid")

# Remplacer les NA par 0
Cadastre[is.na(Cadastre)] <- 0

# Créer une nouvelle colonne combinée "Surface_Peufreq" car Surface_Repulsif n'est pas présente dans les catégories du Modèle 
Cadastre <- Cadastre %>%
  mutate(Surface_Peufreq = Surface_Peufreqm + Surface_Repulsif) %>%
  select(-Surface_Peufreqm, -Surface_Repulsif)

# Calculer le total des surfaces   
Cadastre <- Cadastre %>%
  mutate(Total = Surface_attract + Surface_Culture + Surface_non_bati + 
           Surface_Peufreq + Surface_Routes + 
           Surface_struct + Surface_Surf_perm + 
           Surface_bati + Surface_Surf_Eau)

#renommer les Surf_Per en Surf_Permeable pour correspondre au modèle
Cadastre <- Cadastre %>%
  mutate(Type = recode(Type, "Surf_Per" = "Surf_Permeable"))

# Afficher le tableau final
print(Cadastre)

#view cadastre
View(Cadastre)

# Enregistrer l'environnement de travail
save(list = ls(), file = "I:/Mémoire/Modèle_Prédictif_Multinomial/Traitement_Cadastre_entier.RData")

#Application du modèle sur l'ensemble du cadastre : 

# Charger les packages nécessaires
install.packages("rstudioapi")
library(dplyr)
library(readxl)
library(sf)
library(foreign)
library(imputeTS)
library(tidyr)
library(caret)
library(rpart)

# Charger les données
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Tableau_Entrainement <- read_excel("DATA/Tableau_Entrainement.xlsx")
View(Tableau_Entrainement)

# Vérifier les niveaux de Classe dans Tableau_Entrainement
print(levels(Tableau_Entrainement$Classe))

# Préparer les données du tableau Cadastre
# Vérifier les colonnes nécessaires dans Cadastre
required_columns <- c("Surface_attract", "Surface_Peufreq", "Surface_struct", "Surface_bati", "Surface_non_bati", "Surface_Routes", "Surface_Culture", "Surface_Surf_perm", "Surface_Surf_Eau", "Classe")

if (!all(required_columns %in% names(Cadastre))) {
  stop("Certaines colonnes nécessaires pour la prédiction manquent dans le tableau Cadastre.")
}

# Vérifier les valeurs manquantes dans les colonnes nécessaires de Cadastre
summary(Cadastre[required_columns])

# Modèle Random Forest
rfFit <- train(
  Classe ~ Surface_attract + Surface_Peufreq + Surface_struct + Surface_bati + Surface_non_bati + Surface_Routes + Surface_Surf_Eau + Surface_Culture + Surface_Surf_perm, 
  data = Tableau_Entrainement,
  method = "rf",
  preProc = c("center", "scale")
)

# Faire des prédictions sur le tableau Cadastre
rfClasses <- predict(rfFit, newdata = Cadastre)

# Ajouter la colonne des prédictions au tableau Cadastre
Cadastre <- Cadastre %>%
  mutate(Classe = rfClasses)

# Afficher le tableau final
print(Cadastre)

# Optionnel : Enregistrer le tableau mis à jour
write_sf(Cadastre, "I:/Mémoire/Rstudio/SIG/Cadastre_Predit.shp")
