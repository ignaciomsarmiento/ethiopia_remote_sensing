##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage



pkg<-list("tidyverse","here","dtwclust","groupdata2","TSclust","units","lubridate","stargazer")
lapply(pkg, require, character.only=T)
rm(pkg)



set.seed(1010) #4 centroids




# Load data ---------------------------------------------------------------
dta<-readRDS(here("data/base_panel.Rds"))
land_use<-readRDS(here("data/land_use_labels.rds"))
land_use<- land_use %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))

ndvi<- dta %>% 
  arrange(pixel_id,date) %>% 
  select(pixel_id,date,ndvi_w_center_9) %>% 
  distinct(pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() %>% 
  rename(ndvi=ndvi_w_center_9)


ndvi<- ndvi %>% na.omit()
ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 
ndvi_long<- ndvi_long %>% select(where(~!any(is.na(.))))
ndvi_long_pixel<- ndvi_long %>% select(pixel_id)
ndvi_long<- ndvi_long %>% select(-pixel_id)

ndvi_long_ts<- tslist(ndvi_long)



# load centroids ----------------------------------------------------------
centroids<-readRDS(here("data/bagging_centroids.Rds"))

cent<-centroids %>% pivot_wider(names_from=date,values_from=ndvi)
cent<-cent %>% select(-centroid)
cent<-tslist(cent)


# calculate distances -----------------------------------------------------

sbD <- proxy::dist(ndvi_long_ts, cent, method = "SBD", znorm = TRUE)

z<-apply(sbD,1L,which.min)
z
View(z)
ndvi_long_pixel<-cbind(ndvi_long_pixel,z)

ndvi_long_pixel<-  ndvi_long_pixel %>% rename(closest_centroid=z)


land_use_pixels<-land_use %>% distinct(pixel_id,Name,land_use,area_pixel,area_land_use)
# unique<-land_use %>% distinct(pixel_id,date_acquisition)
# unique<-unique  %>% distinct(pixel_id)

land_use_pixels<-left_join(land_use_pixels,ndvi_long_pixel)

with(land_use_pixels,table(land_use,closest_centroid))
with(land_use_pixels %>% filter(area_land_use>=set_units(100,m^2)),table(land_use,closest_centroid))


confusion_table_100<-land_use_pixels %>% 
                      filter(area_land_use>=set_units(100,m^2)) %>% 
                      group_by(land_use,closest_centroid) %>% 
                      tally() %>% 
                      pivot_wider(names_from=closest_centroid,values_from=n,values_fill=0)

confusion_table_100_perc<- confusion_table_100 %>% 
                            mutate(total_series=`1`+`2`+`3`+`4`,
                                   `1`=`1`*100/total_series,
                                   `2`=`2`*100/total_series,
                                   `3`=`3`*100/total_series,
                                   `4`=`4`*100/total_series)

stargazer(data.frame(confusion_table_100_perc),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/confusion_table100.tex"))


confusion_table<-land_use_pixels %>% 
  group_by(land_use,closest_centroid) %>% 
  tally() %>% 
  pivot_wider(names_from=closest_centroid,values_from=n,values_fill=0)

confusion_table_perc<- confusion_table %>% 
  mutate(total_series=`1`+`2`+`3`+`4`,
         `1`=`1`*100/total_series,
         `2`=`2`*100/total_series,
         `3`=`3`*100/total_series,
         `4`=`4`*100/total_series)
stargazer(data.frame(confusion_table_perc),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/confusion_table_all_series.tex"))


centroids<- centroids %>% mutate(land_use=case_when(centroid==1~"Grazing",
                                                    centroid==2~"Crop",
                                                    centroid==3~"Improved Forage",
                                                    centroid==4~"Tree"))
                          
ggplot(centroids) +
  geom_line(aes(x=date,y=ndvi,col=land_use,group=land_use)) +
  xlab("Date") +
  ylab("Normalized NDVI \n Centroids") +
  theme_classic()
ggsave(here(paste0("views/centroids_model.png")),height=4,width = 9)  
