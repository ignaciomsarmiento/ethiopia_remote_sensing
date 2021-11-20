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

#evaluation
load(here("data/results/kshape_regions_smoothed_undersampling.RData"))

pc_clusters_regions<-pc_clusters
sil_cvi<- function(x) cvi(x,type="Sil")

Results<-lapply(pc_clusters,function(x) sapply(x,sil_cvi))
Results

Results_db<-do.call(rbind,Results)
Results_db
colnames(Results_db)<-paste0("k_",2L:16L)

Results_db

load(here("data/results/kshape_smoothed_undersampling.RData"))

Results_all<-sapply(pc_clusters,sil_cvi)
names(Results_all)<-paste0("k_",2L:16L)


Results<-rbind(Results_db,Results_all)
Results<- data.frame(Results)
Results<- Results %>% mutate(regions=rownames(Results)) %>% relocate(regions)

Results_long <-Results %>%  pivot_longer(!regions,names_to="clusters",values_to = "sil" )
Results_long <- Results_long %>% group_by(regions) %>% mutate(max=max(sil))
Results_long <- Results_long %>% mutate(ind=ifelse(sil==max,1,0))
Results_long <- Results_long %>% filter(ind==1)
Results_long <- Results_long  %>% select(regions,clusters,sil)

library(stargazer)
?stargazer
stargazer(data.frame(Results),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/eval_smooth_undersampling.tex"))
?latex
stargazer(data.frame(Results_long),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/eval_smooth_undersampling_max_cluster.tex"))
