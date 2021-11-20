
##########################################################
# author: Ignacio Sarmiento-Barbieri
# Adapted from https://github.com/bevingtona/planetR/
# and
# https://bevingtona.github.io/20210507_PlanetR_Timelapse.html
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage




#### STEP 1: LIBRARIES ####

# remotes::install_github("bevingtona/planetR", force = T)
# install.packages(c("here", "httr", "jsonlite", "raster"))


library(here)
library(httr)
library(jsonlite)
library(raster)
library(stringr)
library(tidyverse)
library(sf)

files.sources = list.files("R")
sapply(here("R",files.sources), source)
#### STEP 2: USER VARIABLES: Set variables for Get_Planet function ####

# Site name that will be used in the export folder name
site = "download"



# Set API (manually in the script or in a attached file)
 api_key = "6e12f1e3c63e4efca38ed56cde443b64" # OPTION 2

# Date range of interest
start_year = 2019
end_year   = 2021
start_doy  = 290
end_doy    = 300
date_start = as.Date(paste0(start_year,"-01-01"))+start_doy
date_end   = as.Date(paste0(end_year,"-11-01"))+end_doy

# Metadata filters
cloud_lim    = 0.3 # percent scaled from 0-1
item_name    = "PSScene4Band" #PSOrthoTile")#,"PSScene3Band") #c(#c("Sentinel2L1C") #"PSOrthoTile"
product      = "analytic_sr" #c("analytic_b1","analytic_b2")

# Set AOI (many ways to set this!) Ultimately just need an extent()
my_aoi       = readRDS("~/Dropbox/Research/Ethiopia/REMOTE_SENSING/data/pilot_poligons_LU.rds")
my_aoi<- my_aoi %>% dplyr::filter(kebele=="Wufeta Dati")
bbox         = extent(my_aoi)
#mapview::mapview(my_aoi)



# Set/Create Export Folder (optional)
exportfolder = paste(site, item_name, product, start_year, end_year, start_doy, end_doy, sep = "_")
dir.create(exportfolder, showWarnings = F)

#### STEP 3: PLANET_SEARCH: Search API ####
source(here("R/planet_search.R"))
response <- planet_search(bbox, date_end, date_start, cloud_lim, item_name)
print(paste("Images available:", nrow(response), item_name, product))


#### STEP 4: PLANET_ACTIVATE: Batch Activate ####
source(here("R/planet_activate.R"))
#for(i in 1:nrow(response)) {
for(i in 1:2) {
  planet_activate(i, item_name = item_name)
  print(paste("Activating", i, "of", nrow(response)))}

#### STEP 5: PLANET_DOWNLOAD: Batch Download ####
source(here("R/planet_download.R"))
#for(i in 1:nrow(response)) {
for(i in 1:2) {
  planet_download(i)
  print(paste("Downloading", i, "of", nrow(response)))}

layers<-read_stars(here("download_PSScene4Band_analytic_sr_2019_2021_290_300/20211024_073852_1008.tif"))
mapview::mapview(list(layers,my_aoi))
x<-layers %>% filter(band==1)
plot(x)

table(my_aoi$woreda)
