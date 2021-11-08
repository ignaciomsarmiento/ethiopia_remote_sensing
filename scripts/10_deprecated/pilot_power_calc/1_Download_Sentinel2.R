##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","here")
lapply(pkg, require, character.only=T)
rm(pkg)

#https://cran.r-project.org/web/packages/sen2r/vignettes/docker.html
#https://github.com/16eagle/getSpatialData/
devtools::install_github("ignaciomsarmiento/getSpatialData")
# install.packages("mapview")
require("getSpatialData")

login_CopHub(username = "i.sarmiento")
 
data("aoi_data")



pilot<-read_sf(here("data/Pilot/2_SIFI_Jun21.kml"))

plots<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=FALSE)])
exclosures<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=TRUE)])

spdf <- st_zm(exclosures) #XYZ polygon to XY polygon


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
#   addProviderTiles("Esri.WorldImagery") %>%
#   addMarkers(data=plots) %>% 
#   addPolygons(data=spdf, color = "yellow",weight = 4, smoothFactor = 1,
#               opacity = 1, fillOpacity = 0, group = "AddisAmba")
# 
# 

new_crs <- lonlat2UTM(st_coordinates(spdf))
new_crs <- sp::CRS(paste0("+init=epsg:",new_crs))
spdf<-st_transform(spdf,new_crs)
spatial_part <- as(st_geometry(spdf), "Spatial")

spatial_part <- sp::spTransform(spatial_part,new_crs)
#sp::plot(spatial_part)

set_aoi(spatial_part)
get_aoi()

#pre-June 2017 ‘before’, and make post-July 2017 ‘after’
records <- get_records(time_range = c("2017-01-01", "2017-12-31"),
                       products = c("Sentinel-2"))


View(records)

records<- records %>% mutate(month=lubridate::month(date_acquisition))
table(records$month)
records2down<- records %>% filter(grepl("2A",record_id),platform_serial=="Sentinel-2A", level=="Level-1C",month%in%c(9,10,11,12))

records2down <- check_availability(records2down)
records2down <- order_data(records2down,wait_to_complete=TRUE)
records2down <- get_data(records2down,dir_out = here("data/new"))
