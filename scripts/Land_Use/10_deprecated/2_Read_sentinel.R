##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","raster","sp")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Research/Ethiopia")


exclosures<-read_sf("data/CBINReMP_May18.kml")
excl_points<-exclosures %>% filter(Name%in% exclosures$Name[str_detect(exclosures$Name,"P",negate=TRUE)])
excl_points<-excl_points[-17,]
exclosures1<-excl_points[1,] 
spdf <- st_zm(exclosures1) #XYZ polygon to XY polygon
spatial_part <- as(st_geometry(spdf), "Spatial")


spdfs<-st_zm(excl_points)

#read sentinel data
#https://www.youtube.com/watch?v=1dAhrc-kw8o
S2<-"data/Sentinel-2/"
S2<-list.files(S2,recursive = TRUE, full.names = TRUE, pattern = "B0[2348]_10m.jp2$")
S2<-lapply(1:length(S2),function(x) raster(S2[[x]]))
S2[3]

S2_stack<-stack(S2)
spDF<-as_Spatial(spdf)
require(rgdal)
proj4string(spDF)
spDF<-spTransform(spDF,crs(S2_stack))
S2_stack_crop<-crop(S2_stack,spDF)
#S2_stack_crop<-S2_stack


plotRGB(S2_stack_crop, r=3,g=2,b=1, scale=maxValue(S2[[2]]), stretch='hist') # stretch="lin


plotRGB(S2_stack_crop, r=7,g=6,b=5, scale=maxValue(S2[[2]]), stretch='hist') #
#shift the near infrared
plotRGB(S2_stack_crop, r=4,g=3,b=2, scale=maxValue(S2[[2]]), stretch='hist')

#Calculate NDVI
NDVI<-list()
i<-1
NDVI[[i]]<-overlay(S2_stack_crop[[(i-1)*4+3]],S2_stack_crop[[(i-1)*4+4]],fun=function(x,y) (y-x)/(y+x))
  #names(NDVI[[i]])<-paste0("NDVI_",strsplit(strsplit(names(S2_stack_crop[[(i-1)*4+4]])[[1]][2]),"T")[[1]][[1]])

i<-2
NDVI[[i]]<-overlay(S2_stack_crop[[(i-1)*4+3]],S2_stack_crop[[(i-1)*4+4]],fun=function(x,y) (y-x)/(y+x))


NDVI2<-NDVI[[1]]
NDVI3<-NDVI[[2]]

cols<-c("#d73027",
        "#fc8d59",
        "#fee08b",
        "#ffffbf",
        "#d9ef8b",
        "#91cf60",
        "#1a9850")

pal <- colorNumeric(cols, values(NDVI2),
                    na.color = "transparent")



spdfs<-st_transform(spdfs,st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
spdf<-st_transform(spdf,st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#crs(NDVI2) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
NDVI2<-projectRaster(NDVI2,crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
NDVI3<-projectRaster(NDVI3,crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


#AddisAmba
leaflet(spdfs) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addRasterImage(NDVI2, colors = pal, opacity = 1, group = "2020-04-16") %>%
  addRasterImage(NDVI3, colors = pal, opacity = 1, group = "2021-01-21") %>% 
  addPolygons( color = "red",weight = 4, smoothFactor = 1,
                opacity = 1, fillOpacity = 0, group = "Exclousures", popup = as.character(spdfs$Name)) %>%
  addPolygons(data=spdf, color = "yellow",weight = 4, smoothFactor = 1,
               opacity = 1, fillOpacity = 0, group = "AddisAmba") %>%
  addLegend(pal = pal, values = values(NDVI2),
            title = "NDVI") %>%
   addLayersControl(
     overlayGroups = c("2020-04-16","2021-01-21","Exclousures","AddisAmba"),
     options = layersControlOptions(collapsed = FALSE)
   )
  #

