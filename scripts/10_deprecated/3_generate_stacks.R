##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","raster","sp","here","stars","ggplot2","tidyr","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------
pilot<-read_sf(here("data/Pilot/2_SIFI_Jun21.kml"))

plots<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=FALSE)])
exclosures_shp<-pilot %>% filter(Name%in% pilot$Name[str_detect(pilot$Name,"P",negate=TRUE)])

exclosures_shp <- st_zm(exclosures_shp) #XYZ polygon to XY polygon
exclosures_shp<- exclosures_shp %>% mutate(Name=ifelse(Name=="Silbet-1","Silbet",Name))

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

read_files<-function(sp){
  sp<-list.files(sp,recursive = TRUE, full.names = TRUE, pattern = "B0[2348]_10m.jp2$")
  sp<-lapply(1:length(sp),function(x) raster(sp[[x]]))
  return(sp)
}


sentinel<-list.files(here("data/Sentinel-2"))
sentinel<-sentinel[grepl(".SAFE",sentinel)]
sentinel<-as.list(sentinel)


sentinel<-paste0(here("data/Sentinel-2/"),sentinel)
sentinel<-lapply(sentinel,read_files)
sentinel<-lapply(sentinel,stack)


spDF<-as_Spatial(exclosures_shp)
spDF<-spTransform(spDF,crs(sentinel[[1]]))

# rster<-sentinel[[2]]
# excl<-spDF
# rm(rster,excl,sat_crop)
crop_sen2<-function(rster,excl){
  
  if(grepl("PBN",names(rster)[1])==FALSE){
   sat_crop<-crop(rster,excl[excl@data$Name=="Zerehila",])  
   names(sat_crop)<-paste(names(sat_crop),"Zerehila",sep="_")
  }else{
    names<-as.list(excl@data$Name[excl@data$Name!="Zerehila"])
  
    sat_crop<-lapply(names, function(x) {
                                  sat_crop<-crop(rster,excl[excl@data$Name==x,])
                                  names(sat_crop)<-paste(names(sat_crop),x,sep="_")
                                  return(sat_crop)
                                })
  }
  return(sat_crop)
}



SPBN_crop<-list()
for(i in 1:7){
  SPBN_crop[[i]]<-crop_sen2(sentinel[[i]],spDF)
}

cropped <- unlist(SPBN_crop,recursive=TRUE)


       #Calculate NDVI
calculate_NDVI<-function(sp){
  NDVI<-overlay(sp[[3]],sp[[4]],fun=function(x,y) (y-x)/(y+x))
  names(NDVI)<-names(sp[[3]])
  return(NDVI)
}

NDVI_exclosures<-lapply(cropped,calculate_NDVI)

#converts to an sf object, first it goes to stars then to an sf
NDVI_exclosures<-lapply(NDVI_exclosures,st_as_stars)
NDVI_exclosures<-lapply(NDVI_exclosures,st_as_sf, as_points = FALSE, merge = TRUE)

NDVI_exclosures<-lapply(NDVI_exclosures,st_transform,st_crs(plots))

NDVI_exclosures<-lapply(NDVI_exclosures, function(x) {
                                              x$date_exclosure<-colnames(x)[1]
                                              colnames(x)[1]<-"NDVI"
                                              return(x)
                                              })

grids_exclosures<-do.call(rbind,NDVI_exclosures)

grids_exclosures<- grids_exclosures %>% separate(date_exclosure,into=c("tile","date","band","resolution","exclosure"),sep="_")  
grids_exclosures<- grids_exclosures %>% separate(date,into=c("date","time"),sep="T")  
grids_exclosures<- grids_exclosures %>% mutate(date=ymd(date))

  


neigb<-function(exclosure){
  exclosure<- exclosure %>% separate(date_exclosure,into=c("tile","date","band","resolution","exclosure"),sep="_")  
  exclosure<- exclosure %>% separate(date,into=c("date","time"),sep="T")  
  
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
  
  if(exclosure$exclosure[1]%in%c("Guber","Zalant")){
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

NDVI_neigbs_l<-lapply(NDVI_exclosures,neigb)
NDVI_neigbs<-do.call(rbind,NDVI_neigbs_l)


NDVI_neigbs<-st_transform(NDVI_neigbs,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

NDVI_neigbs<- NDVI_neigbs %>% mutate(date=ymd(date),
                                     mes=month(date))  
table(NDVI_neigbs$mes)

NDVI_neigbs<- NDVI_neigbs %>% mutate(post=ifelse(mes==8,1,0),
                                     treatment=ifelse(!is.na(Name),1,0),
                                     treatment_post=treatment*post)

require(lfe)  

x<-felm(NDVI~post+treatment+treatment_post|NamePlot,data=NDVI_neigbs)
stargazer::stargazer(x,type='text')

table(NDVI_neigbs$date,NDVI_neigbs$exclosure)

SPBN_crop[[1]]
s1<-NDVI_neigbs %>% filter(date=="2017-05-02",exclosure=='Amdata')
s2<-NDVI_neigbs %>% filter(date=="2017-05-12",exclosure=='Amdata')
s3<-NDVI_neigbs %>% filter(date=="2017-05-22",exclosure=='Amdata')
s4<-NDVI_neigbs %>% filter(date=="2017-08-30",exclosure=='Amdata')


cols<-c("#d73027",
        "#fc8d59",
        "#fee08b",
        "#ffffbf",
        "#d9ef8b",
        "#91cf60",
        "#1a9850")
pal <- colorNumeric(cols, grids_exclosures$layer,
                    na.color = "transparent")

plot1<-st_transform(plots,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
exclosures_shp<-st_transform(exclosures_shp,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

leaflet(plot1) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(popup=~as.character(plot1$Name ))  %>%
  addPolygons(data=s1, col = "red",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 1, group = "2017-05-02") %>% 
  addPolygons(data=s2, col = "blue",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 1, group = "2017-05-12") %>%
  addPolygons(data=s3, col = "orange",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 1, group = "2017-05-22") %>% 
  addPolygons(data=s4, col = "green",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 1, group = "2017-08-30") %>% 
  addLegend(pal = pal, values = grids_exclosures$NDVI,
            title = "NDVI") %>%
  addPolygons(data=exclosures_shp, color = "blue",weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 0,group="Exclousures") %>% 
  addLayersControl(
    overlayGroups = c("2017-05-02","2017-05-22","2017-05-30","2017-08-30","Exclousures"),
    options = layersControlOptions(collapsed = FALSE)
  )


NDVI_neigbs<-NDVI_neigbs %>% group_by(grid_number) %>% mutate(obs=n())

table(NDVI_neigbs$obs)
View(NDVI_neigbs %>% filter(obs==5))


x<-NDVI_neigbs %>% filter(exclosure=='Amdata')

ggplot() +
  geom_sf(data=x,aes(fill=NDVI)) +
  theme_bw()+
  facet_grid(~date)

