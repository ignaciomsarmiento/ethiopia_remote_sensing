##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf","ggthemes","viridis")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------

dta<-readRDS(here("data/base_panel.Rds"))

polygons<-readRDS(here("data/pilot_poligons_LU.rds"))

ploter<-function(x){
  db<- polygons %>% filter(kebele==x)
  ggplot() +
    geom_sf(data=db,aes(group=land_use,fill=land_use)) +
    theme_map()
  ggsave(here(paste0("views/",x,"_land_use.png")),height=4,width = 9)
}

kebeles<- as.list(unique(dta$kebele))
lapply(kebeles,ploter)

polygons<- polygons %>% filter(Name%in%unique(dta$Name))
polygons<- polygons %>% mutate(area=st_area(geometry))
polygons_nb<- polygons %>% 
                group_by(kebele) %>% 
                summarize(Polygons=n(),
                          mean_area=sum(area),.groups = "drop")%>% 
  st_drop_geometry()


polygons_nb_lu<- polygons %>% 
  group_by(kebele,land_use) %>% 
  summarize(n_polygons=n(),
            mean_area=sum(area),.groups = "drop") %>% 
  st_drop_geometry()
 
polygons_nb_lu<- polygons_nb_lu %>%  pivot_wider(!n_polygons,names_from=land_use,values_from = mean_area)

polygons_nb<- polygons_nb %>% left_join(polygons_nb_lu)

polygons_nb<- polygons_nb %>% mutate(Crop=Crop/mean_area*100,
                                     Grazing=Grazing/mean_area*100,
                                     `Improved forage`=`Improved forage`/mean_area*100,
                                     Tree=Tree/mean_area*100
                                     ) 

polygons_nb<-units::drop_units(polygons_nb)
stargazer(data.frame(polygons_nb),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/areas_polygons_sample.tex"))





# Load Data ---------------------------------------------------------------
ndvi<- dta %>% 
  arrange(panel_id,date,land_use) %>% 
  select(kebele,land_use,panel_id,date,ndvi) %>% 
  distinct(kebele,land_use,panel_id,date,.keep_all = TRUE)%>% 
  ungroup() 

ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,panel_id)
ndvi_long<- ndvi_long %>% select(-land_use,-panel_id)

land_use<- labels_ndvi_long %>% group_by(land_use) %>%  tally()
land_use <- land_use %>% pivot_wider(names_from=land_use,values_from = n)
land_use <- land_use %>% mutate(kebele="All")
kebele_land_use<- labels_ndvi_long %>% group_by(kebele,land_use) %>%  tally()
kebele_land_use<- kebele_land_use %>% pivot_wider(names_from=land_use,values_from = n)


luse<-rbind(land_use,kebele_land_use)
luse <- luse %>% relocate(kebele)
luse

stargazer(data.frame(luse),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/pixels_by_kebele.tex"))
