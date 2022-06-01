set.seed(1)
setwd("C:/Users/patri/Downloads/data")

# libraries
#install.packages("sf")
library(sf)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)

# import de data.shp
#data <- as.data.frame(st_read("data/data.shp"))
data_2017_ech <- as.data.frame(st_read("data/echantillon.shp"))
iso <- as.data.frame(st_read("data/isochrones_driving_30min.shp"))

# interpolation
ech <- data_2017_ech[,c("id_mutatio","valeur_fon","longitude","latitude")]
point_plot <- ggplot(
  data = ech,
  mapping = aes(x = longitude, y = latitude, color = valeur_fon)) +
  geom_point(size = 3) +
  scale_color_gradientn(colors = c("blue", "yellow", "red"))

point_plot 

# import des isochrones

data$date_sec <- as.numeric(gsub("-","",data$date_mutat))
data$year <- substr(data$date_mutat, 0, 4)


# carte des isochrones
map_iso <- ggplot(data=iso$geometry) + geom_sf(fill=iso$AA_MINS)


# calcul de la valeur fonciere moyenne au metre caree par isochrone
data$val_moy_m2 <- data$valeur_fon/data$surface_re
isochrone_group_mean <- aggregate(val_moy_m2 ~ AA_MINS + year, data = data, FUN = mean)
# jointure avec les isochrones
geom_iso_grp_mean <- inner_join(isochrone_group_mean,iso[,c("AA_MINS","geometry")],by=c("AA_MINS"))
# graphe d'analyse

graph_mean <- ggplot(geom_iso_grp_mean, aes(x = year, y = val_moy_m2, color = as.factor(AA_MINS), group = as.factor(AA_MINS))) + geom_point() + geom_line() + labs(x = "Annee", y = "Prix / m2", color = "Isochrone",
                                    title = "Valeur moyene du m2 par isochrone a Montpellier")



