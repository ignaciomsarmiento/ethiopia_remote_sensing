##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","sf","leaflet","stringr","readxl","here")
lapply(pkg, require, character.only=T)
rm(pkg)
here()



soil_erosion<-read_excel(here("data/soil_erosion_location.xlsx"))
soil_erosion<-st_as_sf(soil_erosion, coords = c("Longitude", "Latitude"),crs=4326)



site1 <- read_excel(here("data/soil_erosion_location.xlsx"),sheet = "Site 1", skip=1)
site2 <- read_excel(here("data/soil_erosion_location.xlsx"),sheet = "Site 2", skip=1) 
site3 <- read_excel(here("data/soil_erosion_location.xlsx"),sheet = "Site 3", skip=1) 
site4 <- read_excel(here("data/soil_erosion_location.xlsx"),sheet = "Site 4", skip=1) 
site5 <- read_excel(here("data/soil_erosion_location.xlsx"),sheet = "Site 5", skip=1) 


colnames(site1)<-colnames(site2)<-colnames(site3)<-colnames(site4)<-colnames(site5)<-c("Date",
                                                                                       "Etio_Day",
                                                                                       "Etio_Month",
                                                                                       "Etio_Year",
                                                                                       "Rain_7",
                                                                                       "Rain_13",
                                                                                       "Rain_18",
                                                                                       "Rain_other",
                                                                                       "Rain_total",
                                                                                       "control_Runoff_volume ",
                                                                                       "control_Volume_Sample_ml	",
                                                                                       "control_wt_filter_paper ",
                                                                                       "control_wt_Sed_FP_g	",
                                                                                       "control_wt_Sed_g",
                                                                                       "control_sediment_gl",
                                                                                       "control_sediment_load_kg",
                                                                                       "treatment_Runoff_volume ",
                                                                                       "treatment_Volume_Sample_ml	",
                                                                                       "treatment_wt_filter_paper ",
                                                                                       "treatment_wt_Sed_FP_g	",
                                                                                       "treatment_wt_Sed_g",
                                                                                       "treatment_sediment_gl",
                                                                                       "treatment_sediment_load_kg")