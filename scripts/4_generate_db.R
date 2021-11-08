##########################################################
# author: Ignacio Sarmiento-Barbieri
# Notes: had some issues with layers PBN and PCN overlapping, opted to use one layer PBN
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------
files<-list.files(here("data/temp_bases/"))
files<- files[files!="old"]
dbs<-lapply(files,function(x) readRDS(here("data/temp_bases/",x)))


# create_id<-function(x){
#   x<-x %>% 
#     dplyr::mutate(centroid=st_centroid(geometry)) %>% 
#     dplyr::mutate(panel_id=as.character(centroid)) %>% 
#     dplyr::select(-centroid,-geometry) 
#   x
# }
# 
# dbs<-lapply(dbs,create_id)


# db1<-dbs[[2]]
# with(db1,table(woreda,layer))
# with(db0,table(woreda,layer))
# db0<-dbs[[1]]
# with(db0,table(woreda,layer))
# mapview::mapview(list(db0), zcol = "woreda")

dta<-do.call(rbind,dbs)

dta<- dta %>% rename(record_id=layer_id)
layer_data<-readRDS(here("data/records_2A_2019_2021.rds"))
layer_data<- layer_data %>% st_drop_geometry()
dta_no_geog<-dta %>%  left_join(layer_data)

dta3 <- dta_no_geog %>% group_by(pixel_id) %>% mutate(n=n())
table(dta3$n)
saveRDS(dta_no_geog,here("data/merge_data_polyogns.Rds"))
