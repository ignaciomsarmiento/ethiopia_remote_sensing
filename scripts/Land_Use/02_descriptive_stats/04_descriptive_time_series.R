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


dta_no_geog<-readRDS("data/merge_data.Rds")



# Generate NDVI ------------------------------------------
dta_mod<- dta_no_geog %>% st_drop_geometry() %>% mutate(date=ymd(date_acquisition))
dta_mod <-dta_mod %>% mutate(NDVI=(B08-B04)/(B08+B04)) 


# By total sample ---------------------------------------------------------
dta_mod_tot<- dta_mod %>%  
          group_by(land_use,date) %>% 
          summarize(NDVI=mean(NDVI),.groups='drop') 

dta_mod_tot<- na.omit(dta_mod_tot)



require("xts")
dta_mod_tot<-dta_mod_tot %>% pivot_wider(names_from = land_use,values_from=NDVI)
dta_xts<- xts(dta_mod_tot[,-1],order.by = dta_mod_tot$date)



plot(dta_xts,
     lty = c("dotted", "solid","dashed","longdash","twodash","dotdash"),
     legend.loc = "left")



ma_ibm <- rollmean(dta_xts, 6, align = "right")
z<-fortify(ma_ibm)
z<- z %>% pivot_longer(!Index,names_to = "land_use",values_to = "NDVI")

ggplot(z,aes(x=Index,y=NDVI,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average.png",height=4,width = 9)

ggplot(z %>% filter(land_use%in%c("Improved forage","Grazing")),aes(x=Index,y=NDVI,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_imp_forage_grazing_smoothed.png",height=4,width = 9)



dta_xts_fort<-fortify(dta_xts)
dta_xts_fort<- dta_xts_fort %>% pivot_longer(!Index,names_to = "land_use",values_to = "NDVI")

ggplot(dta_xts_fort %>% filter(land_use%in%c("Improved forage","Grazing")),aes(x=Index,y=NDVI,group=land_use))+
  geom_line(aes(lty=land_use,col=land_use)) +
  scale_x_date(breaks=scales::breaks_pretty(12)) +
  xlab("Months") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("views/NDVI_average_imp_forage_grazing_raw.png",height=4,width = 9)




# by kebele ---------------------------------------------------------------

dta_mod_tot<- dta_mod %>%  
  group_by(kebele,land_use,date) %>% 
  summarize(NDVI=mean(NDVI),.groups='drop') 

dta_mod_tot<- na.omit(dta_mod_tot)


x<-dta_mod_tot %>%
  split(.$kebele) %>% # from base R
  purrr::map( .f = ~tidyr::pivot_wider(.x, names_from = land_use,values_from=NDVI)) 
  

dta_xts<- xts(dta_mod_tot[,-1],order.by = dta_mod_tot$date)

xtser<-function(df){
   db<- df[,-c(1,2)]
   db<-xts(db,order.by = df$date)
   db
}

x_dta_xts<-lapply(x,xtser)



ma_x <- lapply(x_dta_xts, rollmean, 6, align = "right")


ploter<-function(db,region,plot_type){
  z<-fortify(db)
  z<- z %>% pivot_longer(!Index,names_to = "land_use",values_to = "NDVI")


  ggplot(z,aes(x=Index,y=NDVI,group=land_use))+
    geom_line(aes(lty=land_use,col=land_use)) +
    scale_x_date(breaks=scales::breaks_pretty(12)) +
    xlab("Months") +
    theme_bw() +
    theme(legend.position = "bottom")
    ggsave(paste0("views/NDVI_average_imp_forage_grazing",plot_type,region,".png"),height=4,width = 9)
}


for(i in 1:8){
  ploter(ma_x[[i]],names(ma_x)[i],plot_type="_smoothed_average_")
}

for(i in 1:8){
  ploter(x_dta_xts[[i]],names(ma_x)[i],plot_type="_raw_")
}