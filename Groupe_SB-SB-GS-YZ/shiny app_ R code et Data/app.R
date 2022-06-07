library(dplyr)
library(shiny)
library(readr)
#library(tidyverse)
library(sf)
library(stringr)
library(ggplot2)
library(tidyr)
library(tmap)


#######################################
#        Données d'entrée             #
#######################################

#repertoire de travail

#équipements
GLyonEquipement = st_read("data_base/equipement_L93.shp")
plot(GLyonEquipement$geometry)
#Communes
GLyonCommunes = st_read("data_base/ComArr2016GLyon_L93.shp")
#Iris
GLyonIris = st_read("data_base/RP16IrisGLyonW_93.shp")
GLyonIris$centroid <- st_centroid(GLyonIris$geometry)

#enregistrement dans une variable des intérêts equipement :
int_equip = distinct(st_drop_geometry(GLyonEquipement),type_equip)

########################################
#       Méthode R pour la matrice      #
########################################

#Distance entre les couches avec st_distance :
distmat <- st_distance(GLyonIris$centroid,GLyonEquipement$geometry,by_element = FALSE)
# On enlève l'unité en m pour avoir les id_equip
units(distmat)<-NULL
#on transforme en tableau en ajoutant les IRIS),
#en basculant les id_equipements avec les distances (gather)
#on arrange par IRIS et par distance puis on filtre les distances inf à 3750 le max en vélo en 1/4 d'h
colnames(distmat)<-GLyonEquipement$id_equip
distmatE <-as.data.frame(distmat) %>% 
    mutate(IRIS=GLyonIris$IRIS) %>% 
    gather("id_equip","dist",1:357)  %>% 
    group_by(IRIS) %>% 
    arrange (IRIS,dist)%>%
    filter(dist<=3750)

#Jointure avec la table equipement et IRIS pour récupérer les autres champs
distmatE$id_equip= as.double(distmatE$id_equip)
distmatE_joinEquip = left_join(distmatE,GLyonEquipement,by="id_equip")
distmatE_joinEquip_joinIRIS = left_join(distmatE_joinEquip,select(GLyonIris,"IRIS","NOM_IRI"),by="IRIS")

#############################################
#               Fonctions                   #
#############################################

#fonctions de sélection selon l'Iris de départ, du type équipement, transport et du temps maximum

#Pour la méthode avec la matrice de distance calculée avec R
f_dist_equip = function(depart,transp,temps,equip=NULL){
    if(length(equip)==0){
        return("Veuillez sélectionner au moins un intérêt") 
    }
    else {
        dist_equip = distmatE_joinEquip_joinIRIS %>% 
            filter(
                NOM_IRI==depart,
                dist<= as.numeric(transp) * (temps/60),
                type_equip==equip[1]|
                type_equip==equip[2])
        return(dist_equip)}
    }


##########################################
#  Matrice de distance selon un réseaux  #
##########################################
## Partie qui n'est pas utilisée dans l'application Shiny.
## Le package "riverdist" permet le calcul de distances le long de réseaux de rivières.
## Il est construit à partir de fonctionnalités "sf" et "rgdal" pour la lecture de shp.
## Workflow typique : import d'un linéaire en shp (line2netwotk()) ;
## nettoyer le réseau (cleanup()) ; accrochage des points aux rivières (ptshp2segvert()) ;
## et enfin, lancer analyses.

library(riverdist)
library(sf)
library(rgdal)

#NB : vert = vertex (sommet) et seg = segment

chemin ="data_base/Echantillon_riverdist_R" 

## Import du r?seau = création d'un objet "réseau"
voirie <- line2network(path=chemin, layer="voirie_echantillon_dissolve")
#import d'un shp de PETITE taille et auparavant regroupé (dissolve) en SIG car le shp doit être le plus simple possible

plot(voirie)
topologydots(voirie) #vérifier la topologie du réseau (segments connectés = verts, les non-connect?s = rouges)

## Import des couches de points
equipement <- pointshp2segvert(path = chemin, layer="equipement_echantillon", rivers = voirie)
iris <- pointshp2segvert(path = chemin, layer="centroid_iris_echantillon", rivers = voirie)
#les ?quipements/centro?des sont directement connectés au réseau existant via l'argument "rivers", cf la table : var "seg" correspond au segment, et "vert" au sommet
#pour visualiser les points sur le r?seau : 
riverpoints(seg=equipement$seg, vert=equipement$vert, rivers = voirie, pch=15, col="blue") #on indique les champs seg et vert de la couche résultante, le réseau (voirie), les paramètres esthétiques (pch et col)
riverpoints(seg=iris$seg, vert=iris$vert, rivers = voirie, pch=15, col="red")
## Calcul des matrices de distances entre deux jeux de données:
mdist <- riverdistancetofrom(seg1 = equipement$seg, vert1 = equipement$vert, seg2 = iris$seg, vert2 = iris$vert, ID1=equipement$id_equip, ID2 = iris$IRIS, rivers = voirie) %>% as.data.frame()

## Fonction calcul distance-réseau. Résultat = points compris dans un rayon de distance-temps
villequartheure <- function(temps, transport, pt_depart) {
    #les couches sont d?finies plus haut donc il n'y a pas besoin de les appeler ? ce moment-l?.
    #voirie <- line2network(path=chemin, layer="voirie_echantillon_dissolve")
    #equipement <- pointshp2segvert(path = chemin, layer="equipement_echantillon", rivers = voirie)
    #iris <- pointshp2segvert(path = chemin, layer="centroid_iris_echantillon", rivers = voirie)
    id_centroid <- filter(iris, iris$NOM_IRI %in% pt_depart)
    mdist <- riverdistancetofrom(seg1 = equipement$seg, vert1 = equipement$vert, seg2 = id_centroid$seg, vert2 = id_centroid$vert, ID1 = equipement$id_equip, ID2 = id_centroid$NOM_IRI, rivers = voirie, stopiferror = FALSE) %>% as.data.frame()
    if(transport == "velo"){
        vitesse <- 250
    } else if (transport == "pied"){
        vitesse <- 78.3 
    }
    print(vitesse)
    seuil <- vitesse*temps
    print(seuil)
    #le filtre se fait sur la première et unique colonne de la matrice : (évite de devoir remplacer par le paramètre en entrée)
    pt_access <- filter(mdist, mdist[,1] <= seuil)
}

#test par exemple sur l'Iris République :
pt_acces = villequartheure(5,"velo","Republique")


###############################
#     Application Shiny       #
###############################

# Partie utilisateur
ui <- fluidPage(
    includeCSS("www/style.css"),
    # Application title
    fluidRow(
            column (width=4,img(src="logo.png")),
            column (width = 8,h1("Lyon en un quart d'heure, quelle accessibilité aux AMAP et composteurs ?"))
    ),
    
    
    # Barre de sélection du temps
    sidebarLayout(
        sidebarPanel(
            h3("Sélection du temps de trajet"),
            sliderInput("temps",
                        "Temps d'accessibilité en min",
                        min = 1,
                        max = 15,
                        value = 7.5,
                        step = 0.5),#sliderInput
            h3("Mode de transport"),
            radioButtons("transport",
                         "Choix de transport",
                         c("Vélo" = 15000,
                           "Piéton" = 5000)),#radioButtons
            h3("Point de départ"),
            selectInput("depart",
                        "Centroïde de l'Iris",
                        choices = GLyonIris$NOM_IRI),#selectInput
            h3("Centre d'intérêt"),
            checkboxGroupInput("interet",
                               "Choix de vos intérêts",
                               int_equip$type_equip,
                               selected = TRUE
                               )#checkboxGroupInput
        ),#sidebarPanel

        # onglet d'affichage :
        mainPanel(
            tabsetPanel(
               tabPanel("Carte",
                        tmapOutput("map_dist",width="900px",height = "550px")
                        ),#tabPanel
               tabPanel("Graphique",
                        plotOutput("plot_dist")
                        ),#tabPanel
               tabPanel("Tableau",
                        dataTableOutput("table_dist")
                        )#tabPanel
            )#tabsetPanel
           
        )#mainPanel
    )#sidebarLayout
)#ui

#Partie Serveur

server <- function(input, output) {

    output$table_dist <- renderDataTable({
        print(length(input$interet))
        print(input$interet)
        dist_equip = f_dist_equip(input$depart,
                                  input$transport,
                                  input$temps,
                                  input$interet)
        dist_equip = dist_equip %>% select(nom,type_equip,site_inter,adresse,)
    })#table_dist
    
    output$plot_dist <- renderPlot({
        dist_equip = f_dist_equip(input$depart,
                                  input$transport,
                                  input$temps,
                                  input$interet)
        
        ggplot(dist_equip, aes(x = dist, color = type_equip)) +
            geom_density(aes(fill = type_equip), alpha = 0.5)+
            scale_color_discrete(name="Type d'équipement")+
            scale_fill_discrete(name="Type d'équipement")+
            labs(x="Distance en mètres", y="Densité d'équipements")
    })#plot_dist
    
    output$map_dist <- renderTmap({
        tmap_mode("view")
        if(length(input$interet)==0){
            tm_shape(GLyonCommunes)+tm_borders(col="black")
        }
        else{
            dist_equip = st_sf(f_dist_equip(input$depart,
                                      input$transport,
                                      input$temps,
                                      input$interet)
                        )
            
            depart = GLyonIris %>% filter(NOM_IRI == input$depart)
            dist_dep = as.numeric(input$transport)*(input$temps/60)
            buf_dep = st_buffer(depart,dist_dep)
        
            if(length(dist_equip$dist)==0){
                tm_shape(GLyonCommunes)+tm_borders(col="black")
            }
            else{
                tm_shape(buf_dep)+tm_polygons(col="#c6d3b1",alpha=0.65,legend.show = FALSE)+
                tm_shape(dist_equip)+tm_dots(title="Type d'équipement", col="type_equip", id="nom")+
                tm_shape(depart)+tm_dots(legend.show = FALSE)
            }
        }
        
    })#map_dist
}#server
    


# Envoi de l'application
shinyApp(ui = ui, server = server)
