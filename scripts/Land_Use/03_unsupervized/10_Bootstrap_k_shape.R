##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","groupdata2","TSclust","units","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)



set.seed(1010) #4 centroids

bootstrap<-300
# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))
min(dta$date)
max(dta$date)
x<-unique(dta$date)
land_use<-readRDS(here("data/land_use_labels.rds"))
land_use<- land_use %>% filter(area_land_use>=set_units(100,m^2)) 
land_use<- land_use %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))
land_use<- land_use %>% distinct(pixel_id,land_use)

land_use_dta<- dta %>% left_join(.,land_use)

ndvi<- land_use_dta %>% 
  arrange(pixel_id,date) %>% 
  select(kebele,pixel_id,land_use,date,ndvi_w_center_9) %>% 
  distinct(pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() %>% 
  rename(ndvi=ndvi_w_center_9)

ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 
ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,pixel_id)



ndvi_long<- ndvi_long %>% select(-pixel_id)

ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)

# Bootstrap samples of the pixels
ndvi_long_split_boot<-list()
for(i in 1:bootstrap){
  ndvi_long_split_boot[[i]]<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))
  for(j in 1:8) ndvi_long_split_boot[[i]][[j]]$kebele<-NULL
  ndvi_long_split_boot[[i]]<-do.call(rbind,ndvi_long_split_boot[[i]])
  ndvi_long_split_boot[[i]]<-ndvi_long_split_boot[[i]] %>% select(-land_use)
  ndvi_long_split_boot[[i]]<-tslist(ndvi_long_split_boot[[i]])
}


# i<-1
# semilla<-i
# x<-ndvi_long_split_boot[[i]]
theo_cetroids <- function(x,semilla){
  pc<-tsclust(x, k =4, distance = "sbd", centroid = "shape", seed = semilla, trace = TRUE)
  
  c1<-tibble(centroid=1,ts_centroid=pc@centroids[[1]],date=colnames(ndvi_long)[3:70])
  c2<-tibble(centroid=2,ts_centroid=pc@centroids[[2]],date=colnames(ndvi_long)[3:70])
  c3<-tibble(centroid=3,ts_centroid=pc@centroids[[3]],date=colnames(ndvi_long)[3:70])
  c4<-tibble(centroid=4,ts_centroid=pc@centroids[[4]],date=colnames(ndvi_long)[3:70])
  cent<-rbind(c1,c2,c3,c4)
  newname = paste0("ts_centroid",semilla)
  
  cent<- cent %>% mutate(date=lubridate::ymd(date)) %>% 
          rename(!!newname:=ts_centroid)
  return(cent)
} 

pc_clusters<-list()
for(i in 1:bootstrap){
  pc_clusters[[i]]<-theo_cetroids(ndvi_long_split_boot[[i]],semilla=i)
}
View(pc_clusters[[1]])
db_centroids<-left_join(pc_clusters[[1]],pc_clusters[[2]])

for(i in 3:bootstrap){
  db_centroids<-left_join(db_centroids,pc_clusters[[i]])
}


centroids<-db_centroids %>% pivot_longer(!c(centroid,date),names_to = "bootstrap_iteration", values_to = "ndvi")
bag_centroids<-centroids %>% 
                group_by(centroid,date) %>% 
                dplyr::summarize(ndvi=mean(ndvi),.groups='drop')
                

save.image(here("data/bag_centroids.RData"))

