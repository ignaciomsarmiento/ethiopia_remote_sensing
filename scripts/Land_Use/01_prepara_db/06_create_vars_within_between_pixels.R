##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)




#measures within pixel

db_fin<-readRDS(here("data/base_panel.Rds"))

#entire period
db_fin <-db_fin %>% 
  group_by(panel_id) %>% 
  mutate(across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~mean(.x, na.rm = TRUE),.names = "mean.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~sd(.x, na.rm = TRUE),.names = "sd.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~max(.x, na.rm = TRUE),.names = "max.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~min(.x, na.rm = TRUE),.names = "min.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~IQR(.x, na.rm = TRUE),.names = "iqr.{.col}")
  )

colnames(db_fin)         
#measures within polygon
db_fin <-db_fin %>% 
  group_by(Name) %>% 
  mutate(across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~mean(.x, na.rm = TRUE),.names = "mean_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~sd(.x, na.rm = TRUE),.names = "sd_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~max(.x, na.rm = TRUE),.names = "max_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~min(.x, na.rm = TRUE),.names = "min_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~IQR(.x, na.rm = TRUE),.names = "iqr_polygon.{.col}")
  )


db_cross<- db_fin %>% 
  select( "panel_id",
          "Name",
          "woreda",
          "kebele",
          "land_use",
          starts_with("mean"),
          starts_with("sd"),
          starts_with("max"),
          starts_with("min"),
          starts_with("iqr")) %>% 
  distinct(.keep_all = TRUE)



# # -----------------------------------------------------------------------
# # Seasons ---------------------------------------------------------------
# # -----------------------------------------------------------------------


db_fin_seasons <-db_fin %>% 
  mutate(seasons=case_when(month(date)%in%c(12,1,2)~"Summer",
                           month(date)%in%c(3,4,5)~"Fall",
                           month(date)%in%c(6,7,8)~"Winter",
                           month(date)%in%c(9,10,11)~"Spring",
  ))


db_fin_seasons <-db_fin_seasons %>% 
  group_by(seasons) %>% 
  mutate(across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~mean(.x, na.rm = TRUE),.names = "mean_season.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~sd(.x, na.rm = TRUE),.names = "sd_season.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~max(.x, na.rm = TRUE),.names = "max_season.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~min(.x, na.rm = TRUE),.names = "min_season.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~IQR(.x, na.rm = TRUE),.names = "iqr_season.{.col}")
  )

db_fin_seasons <-db_fin_seasons %>% 
  group_by(Name,seasons) %>% 
  mutate(across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~mean(.x, na.rm = TRUE),.names = "mean_season_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~sd(.x, na.rm = TRUE),.names = "sd_season_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~max(.x, na.rm = TRUE),.names = "max_season_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~min(.x, na.rm = TRUE),.names = "min_season_polygon.{.col}"),
         across(c(blue,green,red,nir,ndvi,blue_w_center_9,green_w_center_9,red_w_center_9,nir_w_center_9,ndvi_w_center_9),.fns=~IQR(.x, na.rm = TRUE),.names = "iqr_season_polygon.{.col}"), 
         .groups="drop"
  )

colnames(db_fin_seasons_cross)

db_fin_seasons_cross<- db_fin_seasons %>% 
  select( "panel_id",
          "Name",
          "seasons",
          "woreda",
          "kebele",
          "land_use",
          starts_with("mean"),
          starts_with("sd"),
          starts_with("max"),
          starts_with("min"),
          starts_with("iqr")) %>% 
  distinct(.keep_all = TRUE) %>% 
  ungroup()


summer<- db_fin_seasons_cross %>% filter(seasons=="Summer") %>% dplyr::select(-seasons)
colnames(summer)[-c(1:5)]<-paste0("summer_",colnames(summer)[-c(1:5)])

winter<- db_fin_seasons_cross %>% filter(seasons=="Winter") %>% select(-seasons)
colnames(winter)[-c(1:5)]<-paste0("winter_",colnames(winter)[-c(1:5)])

fall<- db_fin_seasons_cross %>% filter(seasons=="Fall") %>% select(-seasons)
colnames(fall)[-c(1:5)]<-paste0("fall_",colnames(fall)[-c(1:5)])

spring<- db_fin_seasons_cross %>% filter(seasons=="Spring") %>% select(-seasons)
colnames(spring)[-c(1:5)]<-paste0("spring_",colnames(spring)[-c(1:5)])


seasons<-summer %>% 
  left_join(.,winter) %>% 
  left_join(.,fall) %>% 
  left_join(.,spring)



db_cross_seasons <-  left_join(db_cross,seasons)
summary(db_cross_seasons)
db_cross_seasons$spring_mean.blue

saveRDS(db_cross,here("data/crossectional_data.Rds"))
