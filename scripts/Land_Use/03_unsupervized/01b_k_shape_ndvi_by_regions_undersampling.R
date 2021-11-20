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

z<-ndvi_long_split[[1]]
ndvi_long_split<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))

for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL

lapply(ndvi_long_split,function(x) table(x$land_use))
for(i in 1:8) ndvi_long_split[[i]]$land_use<-NULL

ts_ndvi_long_split<-lapply(ndvi_long_split,tslist)

clusterizer <- function(x) tsclust(x, k = 2L:16L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

pc_clusters<-lapply(ts_ndvi_long_split,clusterizer)


save.image(here("data/results/kshape_regions_undersampling.RData"))


