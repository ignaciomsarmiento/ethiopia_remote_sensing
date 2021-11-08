##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","raster","sp","here","stars","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------
pilot<-read_sf(here("data/Pilot/2_SIFI_Jun21.kml"))

plots<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=FALSE)])
exclosures_shp<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=TRUE)])

spdf <- st_zm(exclosures_shp) #XYZ polygon to XY polygon


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


#read sentinel data
#https://www.youtube.com/watch?v=1dAhrc-kw8o
SPBN<-"data/Sentinel-2/2020/S2A_MSIL2A_20200317T074641_N0214_R135_T37PBN_20200317T103629.SAFE"
SPCN<-"data/Sentinel-2/2020/S2A_MSIL2A_20200317T074641_N0214_R135_T37PCN_20200317T103629.SAFE"


read_files<-function(sp){
  sp<-list.files(sp,recursive = TRUE, full.names = TRUE, pattern = "B0[2348]_10m.jp2$")
  sp<-lapply(1:length(sp),function(x) raster(sp[[x]]))
  return(sp)
}

SPBN<-read_files(SPBN)
SPCN<-read_files(SPCN)

SPBN<-stack(SPBN)
SPCN<-stack(SPCN)

spDF<-as_Spatial(spdf)
spDF<-spTransform(spDF,crs(SPBN))

SPCN_crop<-crop(SPCN,spDF[spDF@data$Name=="Zerehila",])

names<-as.list(spDF@data$Name[spDF@data$Name!="Zerehila"])

SPBN_crop<-lapply(names, function(x)crop(SPBN,spDF[spDF@data$Name==x,]))


# plotRGB(SPCN_crop, r=3,g=2,b=1, scale=maxValue(SPCN_crop[[2]]), stretch='hist') # stretch="lin
# plot(spDF,add=TRUE)

# plotRGB(SPBN_crop[[3]], r=3,g=2,b=1, scale=maxValue(SPCN_crop[[2]]), stretch='hist') # stretch="lin
# plot(spDF,add=TRUE)
# 

exclosures<-SPBN_crop
exclosures[[11]]<-SPCN_crop


#Calculate NDVI
calculate_NDVI<-function(sp){
  NDVI<-overlay(sp[[3]],sp[[4]],fun=function(x,y) (y-x)/(y+x))
  return(NDVI)
}

NDVI_exclosures<-lapply(exclosures,calculate_NDVI)

#converts to an sf object, first it goes to stars then to an sf
NDVI_exclosures<-lapply(NDVI_exclosures,st_as_stars)
NDVI_exclosures<-lapply(NDVI_exclosures,st_as_sf, as_points = FALSE, merge = TRUE)

for(i in 1:10){
  NDVI_exclosures[[i]]$Name_exclosure<-names[i]
}
NDVI_exclosures[[11]]$Name_exclosure<-"Zerehila"



NDVI_exclosures<-lapply(NDVI_exclosures,st_transform,st_crs(plots))

grids_exclosures<-do.call(rbind,NDVI_exclosures)

plot1<-st_transform(plots,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
exclosures_shp<-st_transform(spdf,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# leaflet() %>%
#   addProviderTiles("Esri.WorldImagery") %>%
#   addMarkers(data=plot1,popup=~as.character(plot1$Name ))  %>%
#   addPolygons(data=grids_exclosures, color = "yellow",weight = 4, smoothFactor = 1,
#               opacity = 1, fillOpacity = 0) %>% 
#   addPolygons(data=exclosures_shp)



neigb<-function(exclosure){
  exclosure<-exclosure %>% mutate(grid_number=rownames(.))
  p1<- plots %>% filter(grepl("P1",Name))
  p2<- plots %>% filter(grepl("P2",Name))
  
  z<-st_join(exclosure, p1, join = st_intersects)
  grid<-z %>% filter(!is.na(Name)) 
  x<-st_touches(z,grid,sparse=FALSE)
  x<-as_tibble(x)
  x<-x %>% mutate(grid_number=rownames(x))
  x<-x %>% filter(V1==TRUE)
  z<- z %>%  filter(grid_number%in%x$grid_number)
  z<-rbind(z,grid) 
  z<- z %>% mutate(NamePlot=grid$Name)
  
  if(exclosure$Name_exclosure%in%c("Guber","Zalant")){
    z1<-st_join(exclosure, p2, join = st_intersects)
    grid1<-z1 %>% filter(!is.na(Name)) 
    x1<-st_touches(z1,grid1,sparse=FALSE)
    x1<-as_tibble(x1)
    x1<-x1 %>% mutate(grid_number=rownames(x1))
    x1<-x1 %>% filter(V1==TRUE)
    z1<- z1 %>%  filter(grid_number%in%x1$grid_number)
    z1<-rbind(z1,grid1) 
    z1<- z1 %>% mutate(NamePlot=grid1$Name)
    z<-rbind(z,z1) 
  }
  
  return(z)
}

NDVI_neigbs<-lapply(NDVI_exclosures,neigb)
NDVI_neigbs<-do.call(rbind,NDVI_neigbs)



NDVI_neigbs<-st_transform(NDVI_neigbs,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


cols<-c("#d73027",
        "#fc8d59",
        "#fee08b",
        "#ffffbf",
        "#d9ef8b",
        "#91cf60",
        "#1a9850")
pal <- colorNumeric(cols, grids_exclosures$layer,
                    na.color = "transparent")


leaflet(plot1) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(popup=~as.character(plot1$Name ))  %>%
  #addPolygons(data=grids_exclosures, color = "#444444", fillColor = ~pal(layer),weight = 1, smoothFactor = 1,
              #opacity = 1, fillOpacity = 0) %>%
  # addPolygons(data=NDVI_neigbs, col = "red",weight = 1, smoothFactor = 1,
  #             opacity = 1, fillOpacity = 1) %>% 
  addLegend(pal = pal, values = grids_exclosures$layer,
            title = "NDVI") %>%
  addPolygons(data=spdf, color = "blue",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 0) %>% 
  addPolygons(data=grids_exclosures,color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", layer)(layer),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),group="Exclousures") %>% 
  addLayersControl(
    overlayGroups = c("Exclousures"),
    options = layersControlOptions(collapsed = FALSE)
  )  



require(lfe)
db<- NDVI_neigbs %>% mutate(T=ifelse(is.na(Name),0,1))

summary(felm(layer~T|NamePlot,data=db))

x<-db %>% group_by(T,NamePlot) %>% summarise(mean=mean(layer))


#get XX random points
guber<-spdf %>% filter(Name=="Guber")
random<-grids_exclosures %>% filter(Name_exclosure=="Guber")

random<-st_join(random, guber, join = st_intersects)
random<- random %>% filter(!is.na(Name))
random<- random %>%  mutate(grid_number=rownames(.))
random<- random %>% filter(!(grid_number%in%NDVI_neigbs$grid_number))

random<-random %>% sample_n(20)


#Plot Gubber
x1<-NDVI_neigbs %>% filter(Name_exclosure=="Guber")
points<- plots %>% filter(grepl("Guber",Name))


require("RColorBrewer")
ggplot() +
  geom_sf(data=guber,fill="white") +
  geom_sf(data=x1,aes(fill=layer)) +
  geom_sf(data=random,aes(fill=layer)) +
  scale_fill_continuous(type ="viridis") +
  geom_sf(data=points) +
  theme_bw() +
  labs(fill='NDVI \n (2020-03-17)') 
ggsave("views/guber.pdf",height=9,width = 6)
