##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","groupdata2","TSclust")
lapply(pkg, require, character.only=T)
rm(pkg)




# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))


ndvi<- dta %>% 
  arrange(pixel_id,date,land_use) %>% 
  select(kebele,land_use,pixel_id,date,ndvi) %>% 
  distinct(land_use,pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() 
ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 

ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,pixel_id)
ndvi_long<- ndvi_long %>% select(-pixel_id)


ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)


ts_ndvi_long_split<-lapply(ndvi_long_split,tslist)

clusterizer <- function(x) tsclust(x, k = 2L:16L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

pc_clusters<-lapply(ts_ndvi_long_split,clusterizer)


save.image(here("data/results/kshape_ndvi_regions.RData"))


sil_cvi<- function(x) cvi(x,type="Sil")

Results<-lapply(pc_clusters,function(x) sapply(x,sil_cvi))
Results

Results_db<-do.call(rbind,Results)
Results_db
colnames(Results_db)<-paste0("k_",2L:16L)

Results_db



