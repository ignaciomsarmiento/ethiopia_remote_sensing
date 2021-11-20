##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","TSclust","parallel")
lapply(pkg, require, character.only=T)
rm(pkg)





# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))

ndvi<- dta %>% 
        arrange(panel_id,date,land_use) %>% 
        select(kebele,land_use,panel_id,date,ndvi) %>% 
        distinct(land_use,panel_id,date,.keep_all = TRUE)%>% 
        ungroup() 
ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 

ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,panel_id)
ndvi_long<- ndvi_long %>% select(-land_use,-panel_id)


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
warnings()
pc_clusters1<-pc_clusters[[1]]
stopCluster(workers)
# Return to sequential computations. 
registerDoSEQ()

save.image(here("data/results/kshape_regions.RData"))
