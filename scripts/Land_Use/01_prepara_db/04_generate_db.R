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


dta<-do.call(rbind,dbs)

dta<- dta %>% rename(record_id=layer_id)
layer_data<-readRDS(here("data/records_2A_2019_2021.rds"))
layer_data<- layer_data %>% st_drop_geometry()
dta_no_geog<-dta %>%  left_join(layer_data)

#Pixel overlap up to 4 polygons
dta3 <- dta_no_geog %>% group_by(pixel_id,date_acquisition) %>% mutate(n=n())
table(dta3$n)
View(dta3 %>% filter(n>3))

#For example this pixel overlaps 4 polygons, with different land use
# x<-dta3 %>% filter(pixel_id=="c(261725, 1254525)")
# mapview::mapview(x)


#going to generate 2 dbs, one with land use, the other with the unique pixels

saveRDS(dta_no_geog,here("data/merge_data_polygons.Rds"))
