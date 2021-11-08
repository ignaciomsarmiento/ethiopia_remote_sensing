##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","here","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

#https://cran.r-project.org/web/packages/sen2r/vignettes/docker.html
#https://github.com/16eagle/getSpatialData/
#devtools::install_github("ignaciomsarmiento/getSpatialData")
# install.packages("mapview")
require("getSpatialData")


spdf<-readRDS(here("data/pilot_poligons_LU.rds"))
spdf<- spdf %>% filter(Name!="c010")
spdf<-spdf[-3,]
#x<-spdf[3,] has an error

# 
# leaflet(x) %>%
#    addProviderTiles("Esri.WorldImagery") %>%
#   addPolygons(data=spdf, color = "red",weight = 4, smoothFactor = 1,
#               opacity = 1, fillOpacity = 0) %>% 
#    addPolygons( color = "yellow",weight = 4, smoothFactor = 1,
#                opacity = 1, fillOpacity = 0) 
#   
#   
# 
# spdf$geometry <- spdf$geometry %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc()


#Convert to Planar
lonlat2UTM = function(lonlat) {
  #From https://geocompr.robinlovelace.net/reproj-geo-data.html
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}



# leaflet(spdf) %>%
#    addProviderTiles("Esri.WorldImagery") %>%
#    addPolygons(data=spdf, color = "yellow",weight = 4, smoothFactor = 1,
#                opacity = 1, fillOpacity = 0)

#sf::sf_use_s2(FALSE) #https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

new_crs <- lonlat2UTM(st_coordinates(spdf))
spdf<-st_transform(spdf,new_crs)

spatial_part <- as(st_geometry(spdf), "Spatial")
new_crs <- sp::CRS(paste0("+init=epsg:",new_crs))
spatial_part <- sp::spTransform(spatial_part,new_crs)
#sp::plot(spatial_part)

set_aoi(spatial_part)
get_aoi()


#Login to copernicus
login_CopHub(username = "i.sarmiento")
data("aoi_data")

records <- get_records(time_range = c("2019-01-01", "2021-10-09"),
                       products = c("Sentinel-2"))


#View(records)

records2down0<- records %>% filter(grepl("2A",record_id),
                                   platform_serial=="Sentinel-2A",
                                   level=="Level-2A",
                                   grepl("PBN",tile_id))


#saveRDS(records2down0,here("data/records_2A_2019_2021.rds"))


#September-October 2021 #done
#June- August 2021 #done
#June 2020- May 2021 running
#January- June 2020- + left overs to run
#October 2019 Would be 2 years

already_down<-list.files("data/Sentinel-2/")
already_down<-already_down[grepl("S2A",already_down)]
already_down<-str_remove_all(already_down,".zip")
#already_down
records2down1<- records2down0 %>% filter(!(record_id %in% already_down))
records2down1<- records2down1 %>% mutate(date_acquisition=ymd(date_acquisition)) 
records2down_ready<- records2down1 %>% filter(date_acquisition>=ymd("2019-9-01")) #this have to change

records2down_ready[,1:4]
records2down_ready <- check_availability(records2down_ready, verbose = TRUE)
records2down_ready <- order_data(records2down_ready, wait_to_complete = TRUE)
records2down_ready <- get_data(records2down_ready,dir_out = here("data/Sentinel-2/"))



#Step1


