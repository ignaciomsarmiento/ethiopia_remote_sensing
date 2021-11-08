##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf","stars")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------
files<-list.files()
db1<-lapply(files,function(x) readRDS(here("data/temp_bases/",x)))


db1<-readRDS(here("data/temp_bases/S2A_MSIL2A_20200317T074641_N0214_R135_T37PBN_20200317T103629.rds"))
db2<-readRDS(here("data/temp_bases/S2A_MSIL2A_20200317T074641_N0214_R135_T37PCN_20200317T103629.rds"))

mapview::mapview(list(db1,db2),col.regions=c("red","blue"),layer.name = c("PBN", "PCN"))
mapview::mapview(list(db2),col.regions=c("blue"),layer.name = c("PCN"))

db1<- filter(db1,Name=="t176")
db2<- filter(db2,Name=="t176")

file_layer<-"S2A_MSIL2A_20200317T074641_N0214_R135_T37PBN_20200317T103629"
unzip(here("data/Sentinel-2",file_layer),exdir=here("data/Sentinel-2"))
layer_id<-str_remove(file_layer,".zip")
file<-str_remove(here("data/Sentinel-2",file_layer),".zip")

sp<-list.files(paste0(file,".SAFE"),recursive = TRUE, full.names = TRUE, pattern = "*_10m.jp2$")
layer_star <- lapply(sp, read_stars) #read the layers



file_layer<-"S2A_MSIL2A_20200317T074641_N0214_R135_T37PCN_20200317T103629"
unzip(here("data/Sentinel-2",file_layer),exdir=here("data/Sentinel-2"))
layer_id<-str_remove(file_layer,".zip")
file<-str_remove(here("data/Sentinel-2",file_layer),".zip")

file<-"~/Dropbox/Research/Ethiopia/REMOTE_SENSING/data/Sentinel-2/S2A_MSIL2A_20200317T074641_N0214_R135_T37PCN_20200317T103629"
sp<-list.files(paste0(file,".SAFE"),recursive = TRUE, full.names = TRUE, pattern = "*_10m.jp2$")
layer_star2 <- lapply(sp, read_stars) #read the layers


poly_layer_sf1<-st_as_sf(layer_star[[1]])
poly_layer_sf2<-st_as_sf(layer_star2[[1]])
mapview::mapview(list(layer_star,layer_star2),col.regions=c("red","blue"),layer.name = c("PBN", "PCN"))
