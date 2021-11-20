##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","sf","leaflet","stringr","readxl","here","stars")
lapply(pkg, require, character.only=T)
rm(pkg)
here()



soil_erosion<-read_excel(here("data/soil_erosion_location.xlsx"))
poly_data<-st_as_sf(soil_erosion, coords = c("Longitude", "Latitude"),crs=4326)

#mapview::mapview(list(poly_data))


#read sentinel data
sentinel<-list.files(here("data/Sentinel-2"))
sentinel<-sentinel[grepl(".zip",sentinel)]
sentinel<-sentinel[grepl("PBN",sentinel)]


#file_layer<-sentinel[[41]]
create_db<-function(file_layer){
  #This function merges and rasters to polygons and adds sentinel bands to each pixel
  unzip(here("data/Sentinel-2",file_layer),exdir=here("data/Sentinel-2"))
  layer_id<-str_remove(file_layer,".zip")
  file<-str_remove(here("data/Sentinel-2",file_layer),".zip")
  
  sp<-list.files(paste0(file,".SAFE"),recursive = TRUE, full.names = TRUE, pattern = "*_20m.jp2$")
  layer_star <- lapply(sp, read_stars) #read the layers
  poly_data<-st_transform(poly_data,st_crs(layer_star[[1]])) #change CRS
  
  list_polygons<-split(poly_data,poly_data$Plots)
  
  #nm<-list_polygons[[1]]
 
  #mapview::mapview(list(nm))
  
  
  #generate area land use for each pixel
  intersecter<-function(nm){
    
    poly_layer<- st_crop(layer_star[[1]], st_bbox(nm))
    poly_layer_sf<-st_as_sf(poly_layer)
    
    #add centroid identifyier
    poly_layer_sf<-poly_layer_sf %>% dplyr::mutate(pixel_id=as.character(st_centroid(geometry)),
                                                   area_pixel=st_area(geometry))
    
    intersection<-suppressWarnings(st_intersection(poly_layer_sf,nm)) 
    intersection<- intersection %>% dplyr::select(Plots,pixel_id,area_pixel) %>% st_drop_geometry()
    
    return(intersection)
  }    
  #list_polygons<-list_polygons[1:2]
  polygons_ls<-lapply(list_polygons,intersecter)
  
  land_use_area<-do.call(rbind,polygons_ls)
  #--------------------
  #mapview::mapview(list(land_use_area))
  # nm<-list_polygons[[1]]
  # x<-layer_star[[1]]
  # generate pixel_id  
  #mapview::mapview(x)
  croper_centroid<-function(x,pgn){
    cell<-st_crop(x,st_bbox(pgn))
    cell<-st_as_sf(cell)
    cell<- cell %>% dplyr::mutate(pixel_id=as.character(st_centroid(geometry)))
    cell
  }
  
  poly_layer0<-lapply(list_polygons,function(x) lapply(layer_star,croper_centroid,pgn=x))
  
  #x<-poly_layer0[[1]]
  #mapview::mapview(x)
  #rm(x)
  # generate bands for each pixel
  band_pirate<-function(x){
    db <- suppressMessages(left_join(x[[2]], as.data.frame(x[[3]])))
    for(i in 3:10) db <- suppressMessages(left_join(db, as.data.frame(x[[i]])))
    
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
  st_geometry(cells)<-cells$geometry
  db<- cells %>% distinct(.keep_all = TRUE)
  
  
  mapview::mapview(db)
  
  saveRDS(db,here(paste0("data/temp_bases_20/",layer_id,".Rds")))  
  #remove decrompresed file
  
  
  return(db)
}

#dta<-create_db(sentinel[[1]])

require(parallel)
detectCores(logical = FALSE)


sentinel<-sentinel[2:3]
dbs<-mclapply(sentinel,create_db, mc.cores = 4)
# dta<-do.call(rbind,dbs)
# dta<- dta %>% group_by(pixel_id) %>% mutate(n=n())

#dbs<-lapply(test,create_db) 
#mapview(dta)

#system("rm -rf data/Sentinel-2/*.SAFE")

