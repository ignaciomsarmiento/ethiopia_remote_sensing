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
poly_data<-readRDS(here("data/pilot_poligons_LU.rds"))
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
  poly_data<-st_transform(poly_data,st_crs(layer_star[[1]])) #change CRS
  
  #Polygon by polygon
  list_polygons<-split(poly_data,poly_data$Name)
  
  #generate area land use for each pixel
  intersecter<-function(nm){
    
    poly_layer<- st_crop(layer_star[[1]], st_bbox(nm))
    poly_layer_sf<-st_as_sf(poly_layer)
    
    #add centroid identifyier
    poly_layer_sf<-poly_layer_sf %>% dplyr::mutate(pixel_id=as.character(st_centroid(geometry)),
                                                   area_pixel=st_area(geometry))
    
    intersection<-suppressWarnings(st_intersection(poly_layer_sf,nm))
    intersection<- intersection %>% mutate(area_land_use=st_area(geometry)) %>% dplyr::select(Name,pixel_id,land_use,area_pixel,area_land_use) %>% st_drop_geometry()
    return(intersection)
  }    
  #list_polygons<-list_polygons[1:2]
  polygons_ls<-lapply(list_polygons,intersecter)
  
  land_use_area<-do.call(rbind,polygons_ls)
  #--------------------
  
  # nm<-list_polygons[[1]]
  # x<-layer_star[[1]]
  
  croper_centroid<-function(x,pgn){
    cell<-st_crop(x,st_bbox(pgn))
    cell<-st_as_sf(cell)
    cell<- cell %>% dplyr::mutate(pixel_id=as.character(st_centroid(geometry)))
    cell
  }
  
  poly_layer0<-lapply(list_polygons,function(x) lapply(layer_star,croper_centroid,pgn=x))
  
  #x<-poly_layer0[[1]]
  #rm(x)
  band_pirate<-function(x){
    db <- suppressMessages(left_join(x[[1]], as.data.frame(x[[2]])))
    for(i in 3:7) db <- suppressMessages(left_join(db, as.data.frame(x[[i]])))
    
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
  
  poly_layer<-lapply(poly_layer0,band_pirate)
  cells<-do.call(rbind,poly_layer)
  cells <- cells %>% filter(pixel_id %in%land_use_area$pixel_id)
  # x<- poly_layer[[1]] %>% filter(pixel_id %in%land_use_area$pixel_id)
  # mapview::mapview(list(x,list_polygons[[1]]))
  #cell <- cell %>% filter(pixel_id%in%land_use_area$pixel_id)
  cells<-left_join(land_use_area,cells)
  db<- cells %>% distinct(.keep_all = TRUE)
  
  
  
  saveRDS(db,here(paste0("data/temp_bases/",layer_id,".Rds")))  
  #remove decrompresed file
  
  
  return(db)
}

#dta<-create_db(sentinel[[1]])

require(parallel)
detectCores(logical = FALSE)


#sentinel<-sentinel[2:3]
dbs<-mclapply(sentinel,create_db, mc.cores = 4)
# dta<-do.call(rbind,dbs)
# dta<- dta %>% group_by(pixel_id) %>% mutate(n=n())

#dbs<-lapply(test,create_db) 
#mapview(dta)

#system("rm -rf data/Sentinel-2/*.SAFE")

