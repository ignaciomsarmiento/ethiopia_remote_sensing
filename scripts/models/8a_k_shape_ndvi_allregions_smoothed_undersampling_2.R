##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","groupdata2","TSclust")
lapply(pkg, require, character.only=T)
rm(pkg)


set.seed(10101)

# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))


ndvi<- dta %>% 
  arrange(panel_id,date,land_use) %>% 
  select(kebele,land_use,panel_id,date,ndvi_w_center_9) %>% 
  distinct(land_use,panel_id,date,.keep_all = TRUE)%>% 
  ungroup() 
ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi_w_center_9) 

ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,panel_id)
ndvi_long<- ndvi_long %>% select(-panel_id)


ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)

z<-ndvi_long_split[[1]]
ndvi_long_split<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))

for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL

ndvi_long_split<-do.call(rbind,ndvi_long_split)


ndvi_long_split_labels<-ndvi_long_split %>% select(land_use)
ndvi_long_split<-ndvi_long_split %>% select(-land_use)


ts_ndvi_long_split<-tslist(ndvi_long_split)

clusterizer <- function(x) tsclust(x, k = 2L:16L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

pc_clusters<-clusterizer(ts_ndvi_long_split)



sil_cvi<- function(x) cvi(x,type="Sil")

Results<-sapply(pc_clusters,sil_cvi)
Results
colnames(Results)<-paste0("k_",2L:16L)

Results_db

save.image(here("data/results/kshape_smoothed_undersampling.RData"))



#optimal is 4


clust_opt <- tsclust(ts_ndvi_long_split, k = 4L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

plot(clust_opt,type="centroids")
ggsave("views/centroids_smooth_all_regions_undersampling.pdf",height = 6,width=8)

plot(clust_opt,type="series")
ggsave("views/series_smooth_all_regions_undersampling.pdf",height = 6,width=8)


labels_ndvi_long<- labels_ndvi_long %>% mutate(cluster = clust_opt@cluster)

require(xtable)
table<-with(ndvi_long_split_labels,table(land_use,cluster))
xtable(table, type = "latex")
print(xtable(table, type = "latex"), file = "views/confusion_smooth_undersampling.tex")
?print.xtable
