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



# Read data ---------------------------------------------------------------
dta<-readRDS(here("data/merge_data_polyogns.Rds"))
# 
# dta<- dta %>% group_by(pixel_id) %>% mutate(drop=case_when(land_use=="Bare"~"Bare",
#                                                            land_use=="Other"~"Other",
#                                                            land_use=="Tree/Crop"~"Tree/Crop"
#                                                            ))
# dta3<- dta %>%group_by(pixel_id,date) %>% mutate(n=n())
# z<- unique(dta3$pixel_id[!is.na(dta3$drop)])
# x<-dta3 %>% filter(pixel_id%in%z) %>% select(pixel_id,date_acquisition,land_use,drop,n)
# View(x)
# View(x %>% filter(n>1))
#rm(dta3,x)

#land use labels
land_use<-dta %>% dplyr::select(pixel_id,date_acquisition,Name,land_use,area_pixel,area_land_use)
saveRDS(land_use,here("data/land_use_labels.rds"))
rm(land_use)


#keep pixels that belongs to one of these 3 classes
dta<- dta %>% filter(land_use%in%c("Crop","Grazing","Improved forage","Tree"))
poly_data<-readRDS(here("data/pilot_poligons_LU.rds")) %>% dplyr::select("Name",
                                                                         "kebele",
                                                                         "woreda") %>% st_drop_geometry()

dta <-dta %>% left_join(.,poly_data)
#Create date, ndvi, rename bands
dta <-dta %>%  mutate(date=ymd(date_acquisition),
                        ndvi=(B08-B04)/(B08+B04)) %>% 
              rename(blue=B02,
                     green=B03,
                     red=B04,
                     nir=B08) %>% 
              dplyr::select("pixel_id",
                            "date",
                            "kebele",
                            "blue",
                            "green",
                            "red",
                            "nir",
                            "ndvi",
                            "cloudcov") %>% distinct(.keep_all = TRUE)

#some of the pixels are in more than 1 kebele, assign to first
dta<-dta %>%group_by(pixel_id,date) %>%  mutate(n=n(),
                                                one=1,
                                                cumsum1=cumsum(one))

#View(dta %>% filter(n>1))
dta<- dta %>% filter(cumsum1==1) %>%  dplyr::select(-n,-one,-cumsum1)
dta3<- dta %>%group_by(pixel_id,date) %>% mutate(n=n())
table(dta3$n)
rm(dta3)
rm(dta3,poly_data)
#dta_test<- dta %>% filter(panel_id %in% c("c(303985, 1265245)","c(301715, 1264405)"))
#create lags

db <-dta %>% 
          arrange(pixel_id,date) %>%   
          group_by(pixel_id) %>% 
          mutate(across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=1,order_by = date),.names = "lag1.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=2,order_by = date),.names = "lag2.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=3,order_by = date),.names = "lag3.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=4,order_by = date),.names = "lag4.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=5,order_by = date),.names = "lag5.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lag(.x,n=6,order_by = date),.names = "lag6.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lead(.x,n=1,order_by = date),.names = "lead1.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lead(.x,n=2,order_by = date),.names = "lead2.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lead(.x,n=3,order_by = date),.names = "lead3.{.col}"),
                 across(c(blue,green,red,nir,ndvi,cloudcov),.fns=~dplyr::lead(.x,n=4,order_by = date),.names = "lead4.{.col}")
                 )

#create weights
db <- db %>% mutate(w0=1/cloudcov,
                    w1=1/lag1.cloudcov,
                    w2=1/lag2.cloudcov,
                    w3=1/lag3.cloudcov,
                    w4=1/lag4.cloudcov,
                    w5=1/lag5.cloudcov,
                    wl1=1/lead1.cloudcov,
                    wl2=1/lead2.cloudcov,
                    wl3=1/lead3.cloudcov,
                    wl4=1/lead4.cloudcov
                    )

db <- db %>% mutate(bottom_4=w0+w1+w2+w3,
                    bottom_6=w0+w1+w2+w3+w4+w5,
                    botom_lead_lag_5=wl2+wl1+w0+w1+w2,
                    botom_lead_lag_7=wl3+wl2+wl1+w0+w1+w2+w3,
                    botom_lead_lag_9=wl4+wl3+wl2+wl1+w0+w1+w2+w3+w4
                    )

db <- db %>% mutate(#cloud weights lags 40 days
                    w0_4=w0/bottom_4,
                    w1_4=w1/bottom_4,
                    w2_4=w2/bottom_4,
                    w3_4=w3/bottom_4,
                    # lags 60 days
                    w0_6=w0/bottom_6,
                    w1_6=w1/bottom_6,
                    w2_6=w2/bottom_6,
                    w3_6=w3/bottom_6,
                    w4_6=w4/bottom_6,
                    w5_6=w5/bottom_6,
                    # lead and lags 40 days
                    wldlg0=w0/botom_lead_lag_5,
                    wld1=wl1/botom_lead_lag_5,
                    wld2=wl2/botom_lead_lag_5,
                    wlg1=w1/botom_lead_lag_5,
                    wlg2=w2/botom_lead_lag_5,
                    # lead and lags 60 days
                    wldlg0_7=w0/botom_lead_lag_7,
                    wld1_7=wl1/botom_lead_lag_7,
                    wld2_7=wl2/botom_lead_lag_7,
                    wld3_7=wl3/botom_lead_lag_7,
                    wlg1_7=w1/botom_lead_lag_7,
                    wlg2_7=w2/botom_lead_lag_7,
                    wlg3_7=w3/botom_lead_lag_7,
                    # lead and lags 80 days
                    wldlg0_9=w0/botom_lead_lag_9,
                    wld1_9=wl1/botom_lead_lag_9,
                    wld2_9=wl2/botom_lead_lag_9,
                    wld3_9=wl3/botom_lead_lag_9,
                    wld4_9=wl4/botom_lead_lag_9,
                    wlg1_9=w1/botom_lead_lag_9,
                    wlg2_9=w2/botom_lead_lag_9,
                    wlg3_9=w3/botom_lead_lag_9,
                    wlg4_9=w4/botom_lead_lag_9,
                    )

#create moving averages
db <- db %>% mutate(#simple monthly (4) moving average
                    blue_ma4 =(blue+lag1.blue+lag2.blue+lag3.blue)/4,
                    green_ma4=(green+lag1.green+lag2.green+lag3.green)/4,
                    red_ma4  =(red+lag1.red+lag2.red+lag3.red)/4,
                    nir_ma4  =(nir+lag1.nir+lag2.nir+lag3.nir)/4,
                    ndvi_ma4 =(ndvi+lag1.ndvi+lag2.ndvi+lag3.ndvi)/4,
                    #weighted monthly (40 days)y moving average
                    blue_w_ma4 =(blue*w0_4+lag1.blue*w1_4+lag2.blue*w2_4+lag3.blue*w3_4)/4,
                    green_w_ma4=(green*w0_4+lag1.green*w1_4+lag2.green*w2_4+lag3.green*w3_4)/4,
                    red_w_ma4  =(red*w0_4+lag1.red*w1_4+lag2.red*w2_4+lag3.red*w3_4)/4,
                    nir_w_ma4  =(nir*w0_4+lag1.nir*w1_4+lag2.nir*w2_4+lag3.nir*w3_4)/4,
                    ndvi_w_ma4 =(ndvi*w0_4+lag1.ndvi*w1_4+lag2.ndvi*w2_4+lag3.ndvi*w3_4)/4,
                    
                    #simple 60 day moving average
                    blue_ma6=(blue+lag1.blue+lag2.blue+lag3.blue+lag4.blue+lag5.blue)/6,
                    green_ma6=(green+lag1.green+lag2.green+lag3.green+lag4.green+lag5.green)/6,
                    red_ma6=(red+lag1.red+lag2.red+lag3.red+lag4.red+lag5.red)/6,
                    nir_ma6=(nir+lag1.nir+lag2.nir+lag3.nir+lag4.nir+lag5.nir)/6,
                    ndvi_ma6=(ndvi+lag1.ndvi+lag2.ndvi+lag3.ndvi+lag4.ndvi+lag5.ndvi)/6,
                    
                    #weighted 60 day moving average
                    blue_w_ma6=blue*w0_6+lag1.blue*w1_6+lag2.blue*w2_6+lag3.blue*w3_6+lag4.blue*w4_6+lag5.blue*w5_6,
                    green_w_ma6=green*w0_6+lag1.green*w1_6+lag2.green*w2_6+lag3.green*w3_6+lag4.green*w4_6+lag5.green*w5_6,
                    red_w_ma6=red*w0_6+lag1.red*w1_6+lag2.red*w2_6+lag3.red*w3_6+lag4.red*w4_6+lag5.red*w5_6,
                    nir_w_ma6=nir*w0_6+lag1.nir*w1_6+lag2.nir*w2_6+lag3.nir*w3_6+lag4.nir*w4_6+lag5.nir*w5_6,
                    ndvi_w_ma6=ndvi*w0_6+lag1.ndvi*w1_6+lag2.ndvi*w2_6+lag3.ndvi*w3_6+lag4.ndvi*w4_6+lag5.ndvi*w5_6,
                    # centered 50 day moving average
                    blue_center=(blue+lag1.blue+lag2.blue+lead1.blue+lead2.blue)/5,
                    green_center=(green+lag1.green+lag2.green+lead1.green+lead2.green)/5,
                    red_center=(red+lag1.red+lag2.red+lead1.red+lead2.red)/5,
                    nir_center=(nir+lag1.nir+lag2.nir+lead1.nir+lead2.nir)/5,
                    ndvi_center=(ndvi+lag1.ndvi+lag2.ndvi+lead1.ndvi+lead2.ndvi)/5,
                    #weighted centered 50 day moving average
                    blue_w_center=blue*wldlg0+lag1.blue*wlg1+lag2.blue*wlg2+lead1.blue*wld1+lead2.blue*wld2,
                    green_w_center=green*wldlg0+lag1.green*wlg1+lag2.green*wlg2+lead1.green*wld1+lead2.green*wld2,
                    red_w_center=red*wldlg0+lag1.red*wlg1+lag2.red*wlg2+lead1.red*wld1+lead2.red*wld2,
                    nir_w_center=nir*wldlg0+lag1.nir*wlg1+lag2.nir*wlg2+lead1.nir*wld1+lead2.nir*wld2,
                    ndvi_w_center=ndvi*wldlg0+lag1.ndvi*wlg1+lag2.ndvi*wlg2+lead1.ndvi*wld1+lead2.ndvi*wld2,
                    # centered 70 day moving average
                    blue_center_7=(blue +lag1.blue+lag2.blue+lag3.blue+lead1.blue+lead2.blue+lead3.blue)/7,
                    green_center_7=(green+lag1.green+lag2.green+lag3.green+lead1.green+lead2.green+lead3.green)/7,
                    red_center_7=(red+lag1.red+lag2.red+lag3.red+lead1.red+lead2.red+lead3.red)/7,
                    nir_center_7=(nir+lag1.nir+lag2.nir+lag3.nir+lead1.nir+lead2.nir+lead3.nir)/7,
                    ndvi_center_7=(ndvi+lag1.ndvi+lag2.ndvi+lag3.ndvi+lead1.ndvi+lead2.ndvi+lead3.ndvi)/7,
                    #weighted centered 70 day moving average
                    blue_w_center_7=blue*wldlg0_7 +lag1.blue*wlg1_7+lag2.blue*wlg2_7+lag3.blue*wlg3_7+lead1.blue*wld1_7+lead2.blue*wld2_7+lead3.blue*wld3_7,
                    green_w_center_7=green*wldlg0_7+lag1.green*wlg1_7+lag2.green*wlg2_7+lag3.green*wlg3_7+lead1.green*wld1_7+lead2.green*wld2_7+lead3.green*wld3_7,
                    red_w_center_7=red*wldlg0_7+lag1.red*wlg1_7+lag2.red*wlg2_7+lag3.red*wlg3_7+lead1.red*wld1_7+lead2.red*wld2_7+lead3.red*wld3_7,
                    nir_w_center_7=nir*wldlg0_7+lag1.nir*wlg1_7+lag2.nir*wlg2_7+lag3.nir*wlg3_7+lead1.nir*wld1_7+lead2.nir*wld2_7+lead3.nir*wld3_7,
                    ndvi_w_center_7=ndvi*wldlg0_7+lag1.ndvi*wlg1_7+lag2.ndvi*wlg2_7+lag3.ndvi*wlg3_7+lead1.ndvi*wld1_7+lead2.ndvi*wld2_7+lead3.ndvi*wld3_7,
                    # centered 90 day moving average
                    blue_center_9=(blue +lag1.blue+lag2.blue+lag3.blue+lag4.blue+lead1.blue+lead2.blue+lead3.blue+lead4.blue)/9,
                    green_center_9=(green +lag1.green+lag2.green+lag3.green+lag4.green+lead1.green+lead2.green+lead3.green+lead4.green)/9,
                    red_center_9=(red +lag1.red+lag2.red+lag3.red+lag4.red+lead1.red+lead2.red+lead3.red+lead4.red)/9,
                    nir_center_9=(nir +lag1.nir+lag2.nir+lag3.nir+lag4.nir+lead1.nir+lead2.nir+lead3.nir+lead4.nir)/9,
                    ndvi_center_9=(ndvi +lag1.ndvi+lag2.ndvi+lag3.ndvi+lag4.ndvi+lead1.ndvi+lead2.ndvi+lead3.ndvi+lead4.ndvi)/9,
                    #weighted centered 90 day moving average
                    blue_w_center_9=blue*wldlg0_9 +lag1.blue*wlg1_9+lag2.blue*wlg2_9+lag3.blue*wlg3_9+lag4.blue*wlg4_9+lead1.blue*wld1_9+lead2.blue*wld2_9+lead3.blue*wld3_9+lead4.blue*wld4_9,
                    green_w_center_9=green*wldlg0_9 +lag1.green*wlg1_9+lag2.green*wlg2_9+lag3.green*wlg3_9+lag4.green*wlg4_9+lead1.green*wld1_9+lead2.green*wld2_9+lead3.green*wld3_9+lead4.green*wld4_9,
                    red_w_center_9=red*wldlg0_9 +lag1.red*wlg1_9+lag2.red*wlg2_9+lag3.red*wlg3_9+lag4.red*wlg4_9+lead1.red*wld1_9+lead2.red*wld2_9+lead3.red*wld3_9+lead4.red*wld4_9,
                    nir_w_center_9=nir*wldlg0_9 +lag1.nir*wlg1_9+lag2.nir*wlg2_9+lag3.nir*wlg3_9+lag4.nir*wlg4_9+lead1.nir*wld1_9+lead2.nir*wld2_9+lead3.nir*wld3_9+lead4.nir*wld4_9,
                    ndvi_w_center_9=ndvi*wldlg0_9 +lag1.ndvi*wlg1_9+lag2.ndvi*wlg2_9+lag3.ndvi*wlg3_9+lag4.ndvi*wlg4_9+lead1.ndvi*wld1_9+lead2.ndvi*wld2_9+lead3.ndvi*wld3_9+lead4.ndvi*wld4_9
                     )


#From all the smoothers ther best seem to be a ndvi weighted average centered 50 day. The weighs are the inverse of cloud coverage
#
# ggplot(impfor , aes(x=date,y=ndvi,group=smoother,col=smoother)) +
#   geom_line() +
#   theme_bw()
# 
# 
# ggplot(impfor%>% filter(smoother%in%c("ndvi","ndvi_w_center_9")) , aes(x=date,y=ndvi,group=smoother,col=smoother)) +
#   geom_line() +
#   theme_bw()





db_fin <-db  %>% 
  dplyr::select("panel_id",
                "date",
                "Name",
                "woreda",
                "kebele",
                "land_use",
                "blue",
                "green",
                "red",
                "nir",
                "ndvi",
                "cloudcov",
                ends_with("_w_center_9")
                )




# # -----------------------------------------------------------------------
# Clean Land Use variable -------------------------------------------------
# # -----------------------------------------------------------------------

table(db_fin$land_use)

db_fin<- db_fin %>% filter(land_use%in%c("Crop","Grazing","Improved forage","Tree"))
db_fin<- db_fin %>% distinct(panel_id,date,.keep_all = TRUE)

haven::write_dta(db_fin,here("data/panel_stata.dta"))
db_fin <- db_fin  %>% dplyr::select(-cloudcov)
prop.table(table(db_fin$land_use))*100
saveRDS(db_fin,here("data/base_panel.Rds"))
#
