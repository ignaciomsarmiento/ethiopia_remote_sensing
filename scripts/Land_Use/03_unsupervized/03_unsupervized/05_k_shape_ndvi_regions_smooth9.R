##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","TSclust")
lapply(pkg, require, character.only=T)
rm(pkg)


set.seed(101010)

# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))


ndvi<- dta %>% 
  arrange(pixel_id,date) %>% 
  select(kebele,pixel_id,date,ndvi_w_center_9) %>% 
  distinct(pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() %>% 
  rename(ndvi=ndvi_w_center_9)
ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 

ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,pixel_id)
ndvi_long<- ndvi_long %>% select(-pixel_id)


ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)

for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL

ts_ndvi_long_split<-lapply(ndvi_long_split,tslist)



# create multi-process workers
workers <- makeCluster(detectCores())
# load dtwclust in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
  library(dtwclust)
  RcppParallel::setThreadOptions(1L)
}))
# register your workers, e.g. with doParallel
require(doParallel)
registerDoParallel(workers)


clusterizer <- function(x) tsclust(x, k = 2L:16L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

pc_clusters<-lapply(ts_ndvi_long_split,clusterizer)



sil_cvi<- function(x) cvi(x,type="Sil")

Results<-lapply(pc_clusters,function(x) sapply(x,sil_cvi))
Results

Results_db<-do.call(rbind,Results)
Results_db<-data.frame(Results_db)
colnames(Results_db)<-paste0("k_",2L:16L)


Results_db<- Results_db %>% mutate(regions=rownames(Results_db)) %>% relocate(regions)

Results_long <-Results_db %>%  pivot_longer(!regions,names_to="clusters",values_to = "sil" )
Results_long <- Results_long %>% group_by(regions) %>% mutate(max=max(sil))
Results_long <- Results_long %>% mutate(ind=ifelse(sil==max,1,0))
Results_long <- Results_long %>% filter(ind==1)
Results_long <- Results_long  %>% select(regions,clusters,sil)

Results_long

save.image(here("data/results/kshape_ndvi_regions_smooth9.RData"))


# Return to sequential computations. 
registerDoSEQ()
