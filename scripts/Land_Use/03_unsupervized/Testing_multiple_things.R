##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","groupdata2","TSclust","units","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

# w 80m^2
#set.seed(1010101) #2 centorids
set.seed(1010) #4 centroids
#set.seed(2525) #2 centroids
#set.seed(25251) #2 centroids
# w 100m^2
#set.seed(25251) #2 centroids
# Load Data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))

land_use<-readRDS(here("data/land_use_labels.rds"))
land_use<- land_use %>% filter(area_land_use>=set_units(100,m^2)) 
land_use<- land_use %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))
land_use<- land_use %>% distinct(pixel_id,land_use)

land_use_dta<- dta %>% left_join(.,land_use)
ndvi<- land_use_dta %>% 
  arrange(pixel_id,date) %>% 
  select(kebele,pixel_id,land_use,date,ndvi_w_center_9) %>% 
  distinct(pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() %>% 
  rename(ndvi=ndvi_w_center_9)
rm(dta,land_use_dta,land_use)

ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 

ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,pixel_id)



ndvi_long<- ndvi_long %>% select(-pixel_id)

ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)

#It1 
#z<-ndvi_long_split[[1]]
ndvi_long_split<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))
for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL
ndvi_long_split<-do.call(rbind,ndvi_long_split)

#x_pixel_id1<-ndvi_long_split$pixel_id

# #It2 
# ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)
# ndvi_long_split<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))
# for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL
# ndvi_long_split<-do.call(rbind,ndvi_long_split)
# 
# x_pixel_id2<-ndvi_long_split$pixel_id
# 
# #It3 
# ndvi_long_split<-split(ndvi_long,ndvi_long$kebele)
# ndvi_long_split<- lapply(ndvi_long_split,function(x) balance(x,size="min",cat_col="land_use"))
# for(i in 1:8) ndvi_long_split[[i]]$kebele<-NULL
# ndvi_long_split<-do.call(rbind,ndvi_long_split)
# 
# x_pixel_id3<-ndvi_long_split$pixel_id
# 
# sum(x_pixel_id1%in%x_pixel_id2)/length(x_pixel_id1)
# sum(x_pixel_id1%in%x_pixel_id3)/length(x_pixel_id1)
# sum(x_pixel_id2%in%x_pixel_id3)/length(x_pixel_id1)
# 
ndvi_long_split_labels<-ndvi_long_split %>% select(land_use)

#try to get the truth, doesn't work wells
# truth<-ndvi_long_split_labels %>% mutate(truth=case_when(land_use=="Improved forage"~1,
#                                                    land_use=="Crop"~2,
#                                                    land_use=="Tree"~3,
#                                                    land_use=="Grazing"~4))
# truth<-truth %>% mutate(truth=factor(truth, levels=c(1,2,3,4), labels=c("Improved forage","Crop","Tree","Grazing")))
# 
# truth<-truth %>% select(truth)
# truth<-truth$truth


ndvi_long_split<-ndvi_long_split %>% select(-land_use)


ts_ndvi_long_split<-tslist(ndvi_long_split)

#did with 16 and 32 doesnt make much differnce
clusterizer <- function(x) tsclust(x, k = 2L:8L, distance = "sbd", centroid = "shape", seed = 8, trace = TRUE)

pc_clusters<-clusterizer(ts_ndvi_long_split)
?tsclust


# Eval_results ------------------------------------------------------------

sil_cvi<- function(x) cvi(x,type="Sil")

z<-pc_clusters[[3]]@cldist

Results_all<-sapply(pc_clusters,sil_cvi)
names(Results_all)<-paste0("k_",2L:8L)
Results_all<-data.frame(Results_all)
Results_all

Results_all <- Results_all %>% mutate(max=max(Results_all))
Results_all <- Results_all %>% mutate(ind=ifelse(Results_all==max,1,0))
Results_all %>% filter(ind==1)






# plot(c1)
# lines(c2,col="red")
# lines(c3,col="orange")
# lines(c4,col="green")

c1<-x@centroids[[1]]
c2<-x@centroids[[2]]
c3<-x@centroids[[3]]
c4<-x@centroids[[4]]

c1<-tibble(centroid=1,ts_centroid=c1,date=colnames(ndvi_long)[3:70])
c2<-tibble(centroid=2,ts_centroid=c2,date=colnames(ndvi_long)[3:70])
c3<-tibble(centroid=3,ts_centroid=c3,date=colnames(ndvi_long)[3:70])
c4<-tibble(centroid=4,ts_centroid=c4,date=colnames(ndvi_long)[3:70])
cent<-rbind(c1,c2,c3,c4)
cent<- cent %>% mutate(date=lubridate::ymd(date))

cent<-cent %>% pivot_wider(names_from=date,values_from=ts_centroid)
cent<-cent %>% select(-centroid)
cent<-tslist(cent)



g1<-ts_ndvi_long_split
sbD <- proxy::dist(g1, cent, method = "SBD", znorm = TRUE)

View(sbD)
z<-apply(sbD,1L,which.min)
z

# cent<-cent %>% mutate(centroid=factor(centroid))
# ggplot(cent) +
#   geom_line(aes(x=dates,y=ts_centroid,col=centroid,group=centroid)) +
#   theme_classic()
