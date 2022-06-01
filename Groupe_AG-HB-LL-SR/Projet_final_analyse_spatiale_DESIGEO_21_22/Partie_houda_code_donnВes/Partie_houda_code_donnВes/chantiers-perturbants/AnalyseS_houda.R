#installation package 
if(!require(units))
{install.packages("units")
  library(units)}
if(!require(sf))
{install.packages("sf")
  library(sf)}
if(!require(osmdata))
{install.packages("osmdata")
  library(osmdata)}
if(!require(cartography))
{install.packages("cartography")
  library(cartography)}
if(!require(SpatialPosition))
{install.packages("SpatialPosition")
  library(SpatialPosition)}
if(!require(ggplot2))
{install.packages("ggplot2")
  library(ggplot2)}
if(!require(tidyr))
{install.packages("tidyr")
  library(tidyr)}
if(!require(readr))
{install.packages("readr")
  library(readr)}
if(!require(rgdal))
{install.packages("rgdal")
  library(rgdal)}
if(!require(spatstat))
{install.packages("spatstat")
  library(spatstat)}
if(!require(maptools))
{install.packages("maptools")
  library(maptools)}
if(!require(raster))
{install.packages("raster")
  library(raster)}

lieu <- opq(bbox = c(2.2247,48.8188,2.4611,48.9019)) 

#Extraction de la couche PARIS
req_limites_paris <- add_osm_feature(opq=lieu, key='name',value = "Paris") 
res_limites_paris<-osmdata_sf(req_limites_paris)
paris <- st_geometry(res_limites_paris$osm_multipolygons[1,])

#Extraction de la couche des restaurants de paris
req_restau<-add_osm_feature(opq =lieu, key = 'amenity', value = "restaurant")
res_restau<-osmdata_sf(req_restau)
restau_paris<-res_restau$osm_points
restau_paris <- restau_paris[restau_paris$amenity%in%"restaurant",]
restau_paris <- restau_paris[!is.na(restau_paris$name),]
restau_paris <- restau_paris[,"cuisine"]
restau_paris <- st_intersection(x = restau_paris, y = paris)
restau_paris$cuisine <- as.character(restau_paris$cuisine)

#Etration des restanrants de type cuisine indiane 
indian_restau <- restau_paris[restau_paris$cuisine=="indian", ]
#Suppression des valeurs nulles
indian_restau <- indian_restau  %>% drop_na()

#Lecture de la couche des chantiers perturbants
chantiers_perturbants = st_read(dsn='C:\\Users\\huda2\\OneDrive\\Bureau\\ProjetAnalyse\\chantiers-perturbants',layer='chantiers-perturbants')
chantiers_perturbants_j = read_sf(dsn='C:\\Users\\huda2\\OneDrive\\Bureau\\ProjetAnalyse\\chantiers-perturbants',layer='chantiers-perturbants')
chantiers_perturbants_csv = read_delim('C:\\Users\\huda2\\OneDrive\\Bureau\\ProjetAnalyse\\chantiers-perturbants.csv',delim = ";",col_names = T)

#Calcul des chantiers les plus proches aus restaurants
dis=c()
idChantier=c()
disMin=c()
for (i in 1:nrow(indian_restau)){
  disMin[i]=st_distance(indian_restau[1,],chantiers_perturbants[1,])
  idChantier[i]=chantiers_perturbants[1,]$identifiant
  for (j in 1:nrow(chantiers_perturbants)){
  dis[i]=st_distance(indian_restau[i,],chantiers_perturbants[j,])
  
  if(!is.na(dis[i]) && dis[i]<disMin[i]){
    disMin[i]=dis[i]
    idChantier[i]<- c(chantiers_perturbants[j,]$identifiant)
  }
  }
}
indian_restau$identifiant=idChantier

#Affectation des chantiers aux restaurants les plus proches 
library(dplyr)
idian_restau_per = inner_join(indian_restau,chantiers_perturbants_csv,c(),by=c("identifiant"="Identifiant"))
idian_restau_per$distance=disMin

#Changement des codes de valeurs de l'attribut "niveau de perturbation" par des valeurs plus parlantes
colnames(idian_restau_per)
idian_restau_per$"Niveau de perturbation"[idian_restau_per$"Niveau de perturbation"== 2] <- "Perturbant"
idian_restau_per$"Niveau de perturbation"[idian_restau_per$"Niveau de perturbation"== 1] <- "Très Perturbant"


#La cartograohie des distances des chantiers aux restaurants indiannes
plot(paris)
x=choroLayer(
  x = idian_restau_per, 
  var = "distance", 
  #breaks = c(4, 398, 796, 1194, 1592),
  #lwd = 1,
  #col =c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33"),
  #method = "quantille", 
  nclass = 4, 
  col = carto.pal(pal1 = "taupe.pal", n1 = 4),
  legend.title.txt = "distance en m²",
  legend.pos = "topleft",
  add=T
)
title(main = "La cartograohie des distances des chantiers perturbants les plus proches aux restaurants indiannes")


#Les restaurants indiennes ayant un accees perturbé
plot(paris)
#layoutLayer(#title = "Les restaurants indiennes ayant un accees perturbé", scale = 0,2,
#            author = "BOUAZZAOUI Houda, Paris Data et OpenStreetMap contributors.", 
#            sources = "cartography 2.0.2")

x=typoLayer(
  x = idian_restau_per, 
  var="Niveau de perturbation",
  col = c("aquamarine4", "yellow3"), 
  lwd = .5,
  legend.values.order = c("Perturbant",
                          "Très Perturbant"
  ),
  legend.pos = "topleft",
  legend.title.txt = ""
  ,add=T
  
)
title(main = "Les restaurants indiennes ayant un accees perturbé")





