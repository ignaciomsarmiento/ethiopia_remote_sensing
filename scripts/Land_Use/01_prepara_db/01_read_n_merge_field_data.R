##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","sf","leaflet","stringr","readxl")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Research/Ethiopia/REMOTE_SENSING/")

#Convert kml to shapefiles: https://www.stevencanplan.com/2009/02/converting-shapefiles-and-kml-files/
#land use polygons
lu_shp<-read_sf("data/Pilot/SiFi_LU/SiFi_LU.shp")
lu_shp<- lu_shp %>% filter(!c(Name%in%c("o066l","t069r"))) #don't know about this polygons, same name different location
lu_shp<- lu_shp %>% filter(Name!="c010") #this polygon was damaged, fix on the next line
c010<-read_sf("data/Pilot/c010/c010.shp")
lu_data<-read_excel("data/Pilot/sifi_land_use_data_v1.xlsx")
colnames(lu_data)[1]<-"Name"
lu_data<- lu_data %>% dplyr::select(Name,woreda,kebele,watershed,owner,land_use,species) %>% distinct(.keep_all = TRUE)

#improved forages polygons
if_shp<-read_sf("data/Pilot/SiFi_IF/SiFi_IF.shp")
if_data<-read_excel("data/Pilot/sifi_if_data_v1.xlsx")
colnames(if_data)[1]<-"Name"
colnames(if_data)[6]<-"land_use"
if_data<- if_data %>% dplyr::select(Name,woreda,kebele,watershed,owner,land_use,species) %>% distinct(.keep_all = TRUE)

#merge
lu_shp<- lu_shp %>% 
           left_join(.,lu_data) %>% 
            dplyr::select("Name",
                          "woreda",
                          "kebele",
                          "watershed",
                          "owner",
                          "land_use",
                          "species",
                          "geometry")
  

if_shp<- if_shp %>% 
  left_join(.,if_data)  %>% 
  dplyr::select("Name",
                "woreda",
                "kebele",
                "watershed",
                "owner",
                "land_use",
                "species",
                "geometry")



# stack
data_shp<-bind_rows(lu_shp,if_shp)
rm(if_data,if_shp,lu_data,lu_shp)

data_shp <- st_zm(data_shp) #XYZ polygon to XY polygon


#%>% filter(kebele=="Wufeta Dati")
leaflet(data_shp ) %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons( color = "red",weight = 4, smoothFactor = 1,
               opacity = 1, fillOpacity = 0, group = "Exclousures")

saveRDS(data_shp,"data/pilot_poligons_LU.rds")  
  
