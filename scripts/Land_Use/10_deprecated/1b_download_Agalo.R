##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr")
lapply(pkg, require, character.only=T)
rm(pkg)

#https://cran.r-project.org/web/packages/sen2r/vignettes/docker.html
#https://github.com/16eagle/getSpatialData/
#devtools::install_github("16EAGLE/getSpatialData")
# install.packages("mapview")
library(getSpatialData)

login_CopHub(username = "i.sarmiento")
data("aoi_data")

setwd("~/Dropbox/Research/Ethiopia")


exclosures<-read_sf("data/CBINReMP_May18.kml")
excl_points<-exclosures %>% filter(Name%in% exclosures$Name[str_detect(exclosures$Name,"P",negate=TRUE)])
exclosures1<-excl_points %>% filter(Name=="Agalo")
spdf <- st_zm(exclosures1) #XYZ polygon to XY polygon


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



new_crs <- lonlat2UTM(st_coordinates(spdf))
new_crs <- sp::CRS(paste0("+init=epsg:",new_crs))
spdf<-st_transform(spdf,new_crs)
spatial_part <- as(st_geometry(spdf), "Spatial")
st_crs(spdf)
st_coordinates(spdf)
spatial_part <- sp::spTransform(spatial_part,new_crs)

#define the area of interest
set_aoi(spatial_part)
get_aoi()

records <- get_records(time_range = c("2020-03-01", "2021-03-01"),
                       products = c("Sentinel-2"))


View(records)

records2down<- records %>% filter(cloudcov<.1, grepl("2A",record_id),platform_serial=="Sentinel-2A", level=="Level-2A")

records2down <- check_availability(records2down)

records2down <- get_data(records2down,dir_out = "data")
