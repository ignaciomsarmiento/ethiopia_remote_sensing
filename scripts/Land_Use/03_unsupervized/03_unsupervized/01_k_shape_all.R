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


set.seed(101010)


# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))

ndvi<- dta %>% 
  arrange(pixel_id,date) %>% 
  select(pixel_id,date,ndvi) %>% 
  distinct(pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() 

ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 
labels_ndvi_long<- ndvi_long %>% select(pixel_id)
ndvi_long<- ndvi_long %>% select(-pixel_id)


ts_ndvi_long<-tslist(ndvi_long)



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


pc_clusters<-tsclust(ts_ndvi_long, k = 2L:16L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

warnings()



# Eval Results ------------------------------------------------------------


sil_cvi<- function(x) cvi(x,type="Sil")
Results_all<-sapply(pc_clusters,sil_cvi)
names(Results_all)<-paste0("k_",2L:16L)
Results_all
Results_all[which.max(Results_all)]

library(stargazer)

stargazer(data.frame(Results_all),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/eval_ndvi_full_sample.tex"))


# Return to sequential computations. 
registerDoSEQ()

save.image(here("data/results/kshape_all.RData"))

#done
