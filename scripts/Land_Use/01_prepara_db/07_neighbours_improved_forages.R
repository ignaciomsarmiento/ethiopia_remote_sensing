##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf","mapview")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)

here()
dta<-readRDS(here("data/pilot_poligons_LU.rds"))








# Reproject to Planar -------------------------------------------------------

lonlat2UTM = function(lonlat) {
  #From https://geocompr.robinlovelace.net/reproj-geo-data.html
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

# # -----------------------------------------------------------------------

new_crs = lonlat2UTM(st_coordinates(dta[1,]))
dta
dta<-st_transform(dta,new_crs)
dta
imp_forage<- dta %>% filter(land_use=="Improved forage")
other<- dta %>% filter(land_use!="Improved forage")


z<-st_join(imp_forage, other, st_is_within_distance, dist = units::set_units(20, m))
#z1<-z %>% filter(Name.x=="if302")
names<- c(z$Name.x,z$Name.y)
z1<-dta %>% filter(Name%in%names)
names
mapview(list(z1,dta),col.regions=c("red","blue"))


drop<-c("g199l","t225r")

add<-c("t176",'tc329',"if326_335")

names<-names[!c(names%in%drop)]
names<-unique(c(names,add))


z1<-dta %>% filter(Name%in%names)
mapview(list(z1,dta),col.regions=c("red","blue"))


saveRDS(names,here("data/names_neighbours.Rds"))
