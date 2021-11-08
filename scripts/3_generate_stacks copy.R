##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","sf","here","stars","ggplot2","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)


# Read data ---------------------------------------------------------------
polygons<-readRDS(here("data/pilot_poligons_LU.rds"))
#polygons<- polygons %>% filter(Name!="c010")

#read sentinel data
sentinel<-list.files(here("data/Sentinel-2"))
sentinel<-sentinel[grepl(".zip",sentinel)]
sentinel<-sentinel[grepl("PBN",sentinel)]


# # Check if there are already preprocessed data ----------------------------
# preproc<-list.files(here("data/temp_bases"))
# preproc<-preproc[grepl(".Rds",preproc)]
# preproc<-str_remove(preproc,".Rds")
# 
# 
# preproc<-paste(preproc,collapse="|")
# sentinel<-sentinel[grepl(preproc,sentinel)==FALSE]


#file_layer<-sentinel[[41]]
create_db<-function(file_layer){
  #This function merges and rasters to polygons and adds sentinel bands to each pixel
  unzip(here("data/Sentinel-2",file_layer),exdir=here("data/Sentinel-2"))
  layer_id<-str_remove(file_layer,".zip")
  file<-str_remove(here("data/Sentinel-2",file_layer),".zip")
  
  sp<-list.files(paste0(file,".SAFE"),recursive = TRUE, full.names = TRUE, pattern = "*_10m.jp2$")
  layer_star <- lapply(sp, read_stars) #read the layers
  polygons<-st_transform(polygons,st_crs(layer_star[[1]])) #change CRS
  
  #Polygon by polygon
  list_polygons<-split(polygons,polygons$Name)
  
  intersecter<-function(nm){
    poly_layer<-lapply(layer_star,function(x) st_crop(x, st_bbox(nm)))
    poly_layer_sf<-lapply(poly_layer,st_as_sf)
    
    intersection<-lapply(poly_layer_sf,function(x) st_intersection(x,nm))
    db <- left_join(intersection[[1]], as.data.frame(intersection[[2]]))
    for(i in 3:7) db <- left_join(db, as.data.frame(intersection[[i]]))
    
    
    
    db<- db %>% gather(id,value,colnames(db)[grepl(".jp2",colnames(db))])
    
    db<- db %>% separate(col=id,into=c("layer","date","band","resolution"),sep="_",remove=TRUE) 
    db<- db %>% mutate(version=ifelse(grepl("V",resolution),resolution,NA),
                       version=str_remove(version,"10m.jp2."),
                       band=ifelse(!is.na(version),paste0(band,"_",version),band),
                       resolution=str_remove(resolution,".jp2"),
                       resolution=str_remove(resolution,".V[1-3]")
    ) %>% 
      dplyr::select(-version)
    
    
    db<- db %>% spread(band,value)
    
    db<- db %>% mutate(layer_id=layer_id) %>% relocate(layer_id)
    
    return(db)
  }
  
  
  
  
  #save db
  saveRDS(db,here(paste0("data/temp_bases/",layer_id,".Rds")))  
  #remove decrompresed file
  
  
  return(db)
}

#dta<-create_db(sentinel[[1]])

require(parallel)
detectCores(logical = FALSE)


sentinel<-sentinel[2:3]
dbs<-mclapply(sentinel,create_db, mc.cores = 4)
#dbs<-lapply(test,create_db) 
#mapview(dta)

system("rm -rf data/Sentinel-2/*.SAFE")

