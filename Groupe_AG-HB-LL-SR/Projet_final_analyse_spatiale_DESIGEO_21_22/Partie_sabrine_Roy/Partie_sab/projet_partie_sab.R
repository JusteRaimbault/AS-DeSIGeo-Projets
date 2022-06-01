rm(list=ls())

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

#definition de l'espace qu'on souhaite étudier (Paris)
lieu <- opq(bbox = c(2.2247,48.8188,2.4611,48.9019))

## EXTRACTION DES COUCHES

#extraction de la couche PARIS
req_limites_paris <- add_osm_feature(opq=lieu, key='name',value = "Paris") 
res_limites_paris<-osmdata_sf(req_limites_paris)
paris <- st_geometry(res_limites_paris$osm_multipolygons[1,])

paris <- st_transform(paris, 2154)

plot(st_geometry(paris),  # demande d'affichage de la geometrie de l'objet
     col = "grey",  # couleur de remplissage des polygones
     border = "white", # couleur de la ligne de bordure
     main = "paris") # titre

#importation couche seine 
##source:  OSM

req_seine <- add_osm_feature(opq = lieu, key = 'name', value = "La Seine")
res_seine <- osmdata_sf(req_seine)
seine_1 <- st_geometry(res_seine$osm_multilines)
req_seine_2 <- add_osm_feature(opq = lieu, key = 'name', 
                       value = "La Seine - Bras de la Monnaie") #seine séparée en 2 au niveau de l'ile st louis
res_seine_2 <- osmdata_sf(req_seine_2)
seine_2 <- st_geometry(res_seine_2$osm_lines)

seine_1 <- st_transform(seine_1, 2154)
seine_2 <- st_transform(seine_2, 2154)

plot(st_geometry(seine_1),  # demande d'affichage de la geometrie de l'objet
     col = "blue",  # couleur de remplissage des polygones
     border = "white", # couleur de la ligne de bordure
     main = "seine") # titre



#découpage seine dans la limite de Paris + mise dans la même couche de la seine
seine <- st_intersection(x = seine_1, y = paris)
seine <- c(st_cast(seine[1])[2:5], seine[2])
seine <-c(seine_1, seine_2)
seine <- st_transform(seine, 2154)

plot(st_geometry(seine),  # demande d'affichage de la geometrie de l'objet
     col = "blue",  # couleur de remplissage des polygones
     border = "white", # couleur de la ligne de bordure
     main = "seine") # titre


#importation couche quartier (plus détaillé que couche arrondissement pour faire des analyses)
#sources : datagouv
quartiers_paris <- read.csv("C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/quartier_paris.csv", header=TRUE, sep=";", dec=",")
quartiers_paris <- st_read(dsn = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data",layer = "quartier_paris", crs = 2154)
quartiers_paris<- st_transform(quartiers_paris, 2154)


plot(st_geometry(quartiers_paris), col = NA,lty = 5, lwd = 0.6, add=T)


#extraction parcs 
req_parc <- add_osm_feature(opq = lieu, key = 'leisure', value = "park") #extraction parc sur le périmètre def par "lieu"
res_parc<- osmdata_sf(req_parc )
res_parc1_paris <- st_geometry(res_parc$osm_polygons)
res_parc2_paris <- st_geometry(res_parc$osm_multipolygons)

parcs_paris_1 <- st_transform(res_parc1_paris, 2154)
parcs_paris_2 <- st_transform(res_parc2_paris, 2154)


#rassembler les couches des parcs
parcs_paris <- do.call(c, list(parcs_paris_1, parcs_paris_2))
parcs_paris <- st_union(x = st_buffer(parcs_paris,0), by_feature = F)
parcs_paris <- st_cast(parcs_paris , "POLYGON")
parcs_paris <- parcs_paris [st_area(parcs_paris )>=set_units(10000, "m^2")] #garde les objets de plus de 1ha sinon carte est illisible
parcs_paris <- st_intersection(x = parcs_paris, y = paris)


plot(st_geometry(parcs_paris),  # demande d'affichage de la geometrie de l'objet
     col = "#67F581",  # couleur de remplissage des polygones
     border = "#67F581", # couleur de la ligne de bordure
     main = "parcs_paris") # titre


#extraction restaurants
req_restau<-add_osm_feature(opq =lieu, key = 'amenity', value = "restaurant")
res_restau<-osmdata_sf(req_restau)
restau_paris<-res_restau$osm_points

restau_paris <- st_transform(restau_paris, 2154)

View(restau_paris)


##extraction bars
req_bar<-add_osm_feature(opq =lieu, key = 'amenity', value = "bar")
res_bar<-osmdata_sf(req_bar)
bar_paris<-res_bar$osm_points
bar_paris <- st_transform(bar_paris, 2154)
View(bar_paris)



#extraction station de métro
metro_rer_paris <- read.csv("C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/metro_rer_idf.csv", header=TRUE, sep=";", dec=",")
metro_rer_paris <- st_read(dsn = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data",layer = "metro_rer_paris", crs = 2154)
metro_rer_paris <- st_transform(metro_rer_paris, 2154)


plot(st_geometry(metro_rer_paris),  # demande d'affichage de la geometrie de l'objet
     col = "grey",  # couleur de remplissage des polygones
     border = "white", # couleur de la ligne de bordure
     main = "metro_rer_paris") # titre


#ne prendre que les métros pour ne pas surcharger la carte
View(metro_rer_paris)
metro_paris <- metro_rer_paris[metro_rer_paris$mode == "METRO",]
plot(st_geometry(metro_paris), col ="black")

#Arrangement des couches
#bar et pub à l'intérieur de Paris

bar_paris <- bar_paris[!is.na(bar_paris$name),]
bar_paris <- st_intersection(x = bar_paris, y = paris)

#restaurants à l'intérieur de Paris
restau_paris <- restau_paris[restau_paris$amenity%in%"restaurant",]
restau_paris <- restau_paris[!is.na(restau_paris$name),]
restau_paris <- restau_paris[,"cuisine"]
restau_paris <- st_intersection(x = restau_paris, y = paris)
restau_paris$cuisine <- as.character(restau_paris$cuisine)


#export et sauvegarde des données
save(list= c("paris", "quartiers_paris", "seine", "parcs_paris", "bars_paris", "metro_paris", "restau_paris"), 
     file = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/data/paris_donnees.RData", compress = "xz")


#Visualiser les données
load("C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/data/paris_donnees.RData")
library(sf)
library(units)
library(cartography)
par(mar = c(0,0,1.2,0))

#cartographie des données
plot(st_geometry(quartiers_paris), col = NA,lty = 5, lwd = 0.6, add=T)##FONCTIONNE
plot(paris, col = "#A8A6A2", border = NA, bg = "#FFFFFF")
plot(st_geometry(parcs_paris), col = "#8FDE67", border = NA, add=T)
plot(st_geometry(seine),  # demande d'affichage de la geometrie de l'objet
     col = "blue",  # couleur de remplissage des polygones
     border = "white", # couleur de la ligne de bordure
     main = "seine",# titre
     add=T) 
#plot(quartiers_paris$geometry, add=T,border = "#121613", lty= 2,lwd = 0.2)##### NE FONCTIONNE PAS A RESOUDRE
plot(st_geometry(metro_paris), col ="black", add=T)
plot(bars_paris, add=T, col = "#7D0BEB", pch = 20, cex = 0.2)
plot(st_geometry(restau_paris), add=T, col = "#EBA60B", pch = 20, cex = 0.2)
plot(paris, add=T, lwd = 0.7)
legend(x = "right", legend = c("métro et rer", "restaurants", "bars"), 
       col = c("#005050", "#ff0000", "#0000ff"), 
       pch = 20, pt.cex = c(1,0.2,0.2), cex = 0.7, bty = 'n')
layoutLayer(title = "Données quartiers de Paris", scale = 1,
            tabtitle = TRUE, frame = F, 
            author = "Sabrine Roy, Map data et OpenStreetMap contributors.", 
            sources = "cartography 2.0.2")



#carte de la densité des restaurants 
densityMap <- function(x = restau_paris, cuisine = NULL, title, sigma ){
  opar <- par(mar = c(0,0,0,0))
  if(!is.null(cuisine)){
    x <- x[x$cuisine %in% cuisine, ]
  }
  bb <- as(x, "Spatial")
  bbowin <- as.owin(as(paris, "Spatial"))
  pts <- coordinates(bb)
  p <- ppp(pts[,1], pts[,2], window=bbowin)
  ds <- density.ppp(p, sigma = sigma, eps = c(20,20))
  rasdens <- raster(ds) * 1000 * 1000
  rasdens <- rasdens+1
  bks <- getBreaks(values(rasdens), nclass = 12, method = "equal")
  cols <- colorRampPalette(c("black", "#940000", "white"))(length(bks)-1)
  plot(paris, col = NA, border = NA, main="")
  image(rasdens, breaks= bks, col=cols, add = T,legend=F)
  legendChoro(pos = "topright",cex = 0.7,title.cex = .8,
              title.txt = paste0(title, "\nsigma=500m,\nn=",nrow(pts)),
              breaks = bks-1, nodata = FALSE,values.rnd = 0,
              col = cols)
  plot(seine, col = "#AAD3DF", add=T, lwd = 4)
  plot(arrdt$geometry, add=T, border = "white",lty = 2, lwd = 0.1)
  plot(st_geometry(x), add=T, col = "blue", pch = 20, cex = 0.1)
  plot(paris, col = NA, add=T)
  barscale(size = 1)
  mtext(text = "Sabrine Roy, Projet Analyse Spatiale, MS DeSIGeo, cartography 2.0.2\nMap data © OpenStreetMap contributors, under CC BY SA.", 
        side = 1, line = -1, adj = c(0.01,0.01), cex = 0.6, font = 3)
  north(pos = c(661171.8,6858051))
  par(opar)
}

densityMap(x = restau_paris, title = "Les restaurants dans Paris", sigma = 500)


#STATISTIQUE SPATIALE


#calcul de l'indice du plus proche voisin dans le cas des restaurants indiens
#selection des restaurants indiens dans paris
indian_restau <- restau_paris[restau_paris$cuisine=="indian", ]
indian_restau <- st_transform(indian_restau , 2154)

indian_restau <- indian_restau  %>% drop_na()

distance_indian_restau = st_distance(indian_restau)
nearestneighdists = apply(distance_indian_restau,1,function(r){min(r[r>0])})
nndindex = 2*sqrt(nrow(indian_restau)/sum(st_area(paris)))*mean(nearestneighdists)
nndindex
st_write(obj = indian_restau, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/indian_restau.shp")


#resultat 0.728 : tendance à l'aggregation

#cartographie de la densité des points 



library(ggplot2)

g=ggplot(paris)
g+geom_sf()+geom_density2d_filled(data=data.frame(st_coordinates(indian_restau)),mapping=aes(x=X,y=Y),alpha=0.5)



# Chargement des données de la population en 2017

library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)

#chargement données de la population


poparrdt = read_delim('C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/pop_arrdt.csv', delim=",")
arrdt = read_sf(dsn='C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data',layer='arrondissements',crs = 2154)
st_crs(arrdt)
arrdt <- st_transform(arrdt, 2154) #reprojection en lambert 93 

arrdt = left_join(arrdt,poparrdt[,c("Code_postal","POPTOT")],by=c("CODE_POSTA"="Code_postal"))



st_write(obj = arrdt, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/arrdt_pop.shp")


# Calcul de la correlation des effectifs à la population 

##regropement du nombre de restaurant indien par arrondissement

restau_paris = st_join(indian_restau, arrdt) %>% group_by(CODE_POSTA) %>%
  summarise(num_indian_restau = n(), population = POPTOT[1])
##ne fonctionne pas => ne donne pas le count du  nombre de restaurant par arrondissement mais à tout compilé dans Paris malgré le group by par code postal


View(restau_paris)

#utilisation de qgis pour réaliser l'opération de jointure et de comptage de restaurants indiens dans les arrondissements de paris

arrdt_restau_indien = read_sf(dsn='C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data',layer='arrdt_restau_indien_2',crs = 2154)
st_crs(arrdt)
arrdt <- st_transform(arrdt, 2154) #reprojection en lambert 93 


cor.test(arrdt_restau_indien$Join_Count,arrdt_restau_indien$POPTOT)
cor.test(arrdt_restau_indien$Join_Count,arrdt_restau_indien$POPTOT,method = "spearman")


ggplot(data.frame(logrest=log(arrdt_restau_indien$Join_Count),logpop=log(arrdt_restau_indien$POPTOT)),aes(x=logpop,y=logrest))+
  geom_point()+geom_smooth()


summary(lm(data=data.frame(logrest=log(arrdt_restau_indien$Join_Count),logpop=log(arrdt_restau_indien$POPTOT)),
           formula = logrest~logpop
))


summary(lm(data=data.frame(rest=arrdt_restau_indien$Join_Count,pop=arrdt_restau_indien$POPTOT),
           formula = rest~pop
))


#Calcul autocorrélation spatiale
weightMatrix<-function(decay,layer){
  d = st_distance(st_centroid(layer))
  w = exp(-units::drop_units(d)/decay)
  diag(w)<-0
  return(w)
}

spAutocorr<-function(x,w,m){
  n=length(x)
  cx = x - mean(x)
  cxvec=matrix(cx,nrow=n,ncol=1)
  normalization = n/(sum(w)*sum(cxvec*cxvec))
  #ci = cxvec%*%matrix(data=rep(1,n),nrow=1)
  #cj = t(ci)
  #sum(w*ci*t(ci))*normalization
  return(n*mean(((matrix(data = rep(cx,n),ncol = n,nrow = n,byrow = FALSE)*w)%*%cxvec)/normalization))
}

n = nrow(arrdt_restau_indien)
m = matrix(rep(1,n*n),nrow=n,ncol=n);diag(m)<-0
decay = 100000
w=weightMatrix(decay,arrdt_restau_indien)

spAutocorr(arrdt_restau_indien$Join_Count,w,m)

#resultat -21856889


library(spdep)
library(mapsf)
depsnb = poly2nb(arrdt_restau_indien)
w = nb2listw(depsnb)

moran.test(arrdt_restau_indien$Join_Count,w,na.action = na.omit)

geary.test(arrdt_restau_indien$Join_Count,w)

locrestau_indien = localmoran(arrdt_restau_indien$Join_Count,w)

mf_map(x = arrdt_restau_indien, var = "locrestau_indien", type = "choro")
mf_map(x = arrdt_restau_indien, var = "Join_Count", type = "choro")
View(arrdt_restau_indien)




## Calcul des indices de concentration



#Construction des comptages pour l'ensemble des restaurants asiatiques par arrondissement 



#sélection des restaurants asiatiques
japanese_restau_paris<-restau_paris[restau_paris$cuisine=="japanese", ]
japanese_restau_paris<- japanese_restau_paris %>% drop_na()
japanese_restau_paris<- st_transform(japanese_restau_paris, 2154)
st_write(obj = japanese_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/japanese_restau_paris.gpkg")



vietnamese_restau_paris<-restau_paris[restau_paris$cuisine=="vietnamese",]
vietnamese_restau_paris <- vietnamese_restau_paris %>% drop_na()
vietnamese_restau_paris <- st_transform(vietnamese_restau_paris, 2154)
st_write(obj = vietnamese_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/vietnamese_restau_paris.gpkg")



thai_restau_paris<-restau_paris[restau_paris$cuisine=="thai",]
thai_restau_paris <- thai_restau_paris %>% drop_na()
thai_restau_paris <- st_transform(thai_restau_paris, 2154)
st_write(obj = thai_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/thai_restau_paris.gpkg")





korean_restau_paris<-restau_paris[restau_paris$cuisine=="korean",]
korean_restau_paris <- korean_restau_paris %>% drop_na()
korean_restau_paris <- st_transform(korean_restau_paris, 2154)
st_write(obj = korean_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/korean_restau_paris.gpkg")



sushi_restau_paris<-restau_paris[restau_paris$cuisine=="sushi",]
sushi_restau_paris  <- sushi_restau_paris  %>% drop_na()
sushi_restau_paris <- st_transform(sushi_restau_paris, 2154)
st_write(obj = sushi_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/sushi_restau_paris.gpkg")





chinese_restau_paris<-restau_paris[restau_paris$cuisine=="chinese",]
chinese_restau_paris<- chinese_restau_paris  %>% drop_na()
chinese_restau_paris <- st_transform(chinese_restau_paris, 2154)
st_write(obj = chinese_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/chinese_restau_paris.gpkg")





asian_restau_paris<-restau_paris[restau_paris$cuisine=="asian",]
asian_restau_paris <- asian_restau_paris %>% drop_na()
asian_restau_paris <- st_transform(asian_restau_paris, 2154)
st_write(obj = asian_restau_paris, #intitule de l'objet a exporter
         dsn  = "C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data/asian_restau_paris.gpkg")
View(restau_asiat)

restau_asiat =c(japanese_restau_paris="japanese_restau_paris.gpkg",
                vietnamese_restau_paris="vietnamese_restau_paris.gpkg",
                thai_restau_paris="thai_restau_paris.gpkg",
                korean_restau_paris="korean_restau_paris.gpkg",
                sushi_restau_paris="sushi_restau_paris.gpkg",
                chinese_restau_paris="chinese_restau_paris.gpkg",
                asian_restau_paris="asian_restau_paris.gpkg")


for(restau_asiat_paris in names(restau_asiat)){
  show(restau_asiat_paris)
  restau_asiat=st_transform(st_read(paste0("C:/Users/Sabrine/Documents/Mastère__DESIGEO/DeSIGeo_COURS/UE 5 Analyse Spatiale/Analyse spatiale/projet_analyse_spatiale/data",restau_asiat [[restau_asiat_paris]])),"EPSG:2154")
  aggrrestau=st_join(restau_asiat, arrdt) %>% group_by(CODE_POSTA) %>% summarise(num = n())
  aggrrestau[[restau_asiat_paris]]= aggrrestau$num
  arrdt=left_join(arrdt,as_tibble(aggrrestau)[,c("CODE_POSTA",restau_asiat_paris)],by=c("CODE_POSTA"="CODE_POSTA"))
}
specialite <- function(arrdt,type_restau,type_restau_specialite){
  counts = as_tibble(arrdt)[,type_restau]
  counts[is.na(counts)]=0
  localshare = counts[,type_restau_specialite] / rowSums(counts)
  globalShare = sum(counts[,type_restau_specialite])/sum(counts)
  return(localshare/globalShare)
}

#specialisation restaurants thailandais

arrdt$restau_asiat = specialite(arrdt,c("chinese_restau_paris","asian_restau_paris","sushi_restau_paris","korean_restau_paris","thai_restau_paris","vietnamese_restau_paris","japanese_restau_paris"),"thai_restau_paris")[[1]]

###################### N'A PAS FONCTIONNE

