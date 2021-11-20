##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","sf","here","stars","raster","httr","jsonlite","stringr","sf","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# load extrafunctions -----------------------------------------------------


# Site name that will be used in the export folder name
site = "PlanetR"

# Set API (manually in the script or in a attached file)
api_key = "6e12f1e3c63e4efca38ed56cde443b64" # OPTION 2

# Date range of interest
start_year = 2021
end_year   = 2021
#start_doy  = 290 # OR FROM DATE as.numeric(format(as.Date('2000-07-15'),"%j"))
start_doy  =  as.numeric(format(as.Date('2021-03-01'),"%j"))
#end_doy    = 300 # OR FROM DATE as.numeric(format(as.Date('2000-08-15'),"%j"))
end_doy    =  as.numeric(format(as.Date('2021-08-31'),"%j"))
date_start = as.Date(paste0(start_year,"-03-01"))
date_end   = as.Date(paste0(end_year,"-08-31"))
date_start
# Metadata filters
cloud_lim    = 0.05 # percent scaled from 0-1
item_name    = "PSScene4Band" #PSOrthoTile")#,"PSScene3Band") #c(#c("Sentinel2L1C") #"PSOrthoTile"
product      = "analytic_sr" #c("analytic_b1","analytic_b2")

# Set AOI (many ways to set this!) Ultimately just need an extent()
my_aoi       = readRDS("~/Dropbox/Research/Ethiopia/REMOTE_SENSING/data/pilot_poligons_LU.rds")
#two areas of interes?
# my_aoi1<- my_aoi %>% dplyr::filter(kebele%in%c("Enashenefalen", "Bachema"))
# my_aoi2<- my_aoi %>% dplyr::filter(!(kebele%in%c("Enashenefalen", "Bachema")))
bbox         = extent(my_aoi)



# Set/Create Export Folder (optional)
exportfolder = paste(site, item_name, product, start_year, end_year, sep = "_")
dir.create(exportfolder, showWarnings = F)

#### STEP 3: PLANET_SEARCH: Search API ####
source(here("scripts/R/planet_search_own.R"))
response0 <- planet_search(bbox, date_end, date_start, cloud_lim, item_name)

response_filter<- response0 %>% mutate(date_time=ymd_hms(acquired),
                                      yr=year(date_time),
                                      mth=month(date_time),
                                      dy=day(date_time),
                                      date=paste0(yr,"-",mth,"-",dy))
response_filter_sum<- response_filter %>%
                    group_by(date) %>%
                    summarise(visible_percent=mean(visible_percent),
                              anomalous_pixels=mean(anomalous_pixels),
                              n=n(),.groups='drop') %>%
                    filter(visible_percent>99.7)



response_down<-response_filter %>% filter(date%in%c("2021-6-2","2021-7-2","2021-8-17"))
source(here("scripts/R/planet_search.R"))
response <- planet_search(bbox, date_end, date_start, cloud_lim, item_name)
response<-response %>% filter(`resDFid.response_doy...`%in%response_down$id)
print(paste("Images available:", nrow(response), item_name, product))

#### STEP 4: PLANET_ACTIVATE: Batch Activate ####
source(here("scripts/R/planet_activate.R"))
for(i in 1:nrow(response)) {
  planet_activate(i, item_name = item_name)
  print(paste("Activating", i, "of", nrow(response)))}



#### STEP 5: PLANET_DOWNLOAD: Batch Download ####
source(here("scripts/R/planet_download.R"))
for(i in 3:nrow(response)) {
  planet_download(i)
  print(paste("Downloading", i, "of", nrow(response)))}
