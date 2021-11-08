##########################################################
# author: Ignacio Sarmiento-Barbieri
# Notes: had some issues with layers PBN and PCN overlapping, opted to use one layer PBN
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf","xts")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)





# Read data ---------------------------------------------------------------


dta<-readRDS(here("data/base_panel.Rds"))


# By total sample ---------------------------------------------------------
dta_mod_tot<- dta %>%  
          group_by(land_use,date) %>% 
          summarize(ndvi=mean(ndvi),
                    ndvi_smooth=mean(ndvi_w_center_9),
                    .groups='drop') 




ggplot(dta_mod_tot,aes(x=date,y=ndvi,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_raw_final_db.png",height=4,width = 9)

ggplot(dta_mod_tot %>% filter(land_use%in%c("Improved forage","Grazing")),aes(x=date,y=ndvi,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_imp_forage_grazing_raw_final_db.png",height=4,width = 9)


ggplot(dta_mod_tot,aes(x=date,y=ndvi_smooth,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_smooth_final_db.png",height=4,width = 9)


ggplot(dta_mod_tot %>% filter(land_use%in%c("Improved forage","Grazing")),aes(x=date,y=ndvi_smooth,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_imp_forage_grazing_smooth_final_db.png",height=4,width = 9)




# by kebele ---------------------------------------------------------------

dta_mod_tot_reg<- dta %>%  
  group_by(kebele,land_use,date) %>% 
  summarize(ndvi=mean(ndvi),
            ndvi_smooth=mean(ndvi_w_center_9),
            .groups='drop') 




ggplot(dta_mod_tot_reg,aes(x=date,y=ndvi,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  facet_wrap(.~kebele,ncol=2) +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_raw_final_db_regions.png",height=16,width = 9)


ggplot(dta_mod_tot_reg,aes(x=date,y=ndvi_smooth,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  facet_wrap(.~kebele,ncol=2) +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_smooth_final_db_regions.png",height=16,width = 9)
