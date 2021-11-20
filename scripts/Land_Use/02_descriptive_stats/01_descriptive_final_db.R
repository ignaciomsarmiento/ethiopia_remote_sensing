##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","here","ggplot2","lubridate","sf","ggthemes","stargazer","units")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)



# Read data ---------------------------------------------------------------

dta<-readRDS(here("data/base_panel.Rds"))
land_use<-readRDS(here("data/land_use_labels.rds"))
#land_use<- land_use %>% filter(area_land_use>=set_units(100,m^2)) 
land_use<- land_use %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))
land_use<- land_use %>% distinct(pixel_id,land_use)
dta<- dta %>% left_join(.,land_use)


polygons<-readRDS(here("data/pilot_poligons_LU.rds"))



ploter<-function(x){
  db<- polygons %>% filter(kebele==x)
  p<-ggplot() +
    geom_sf(data=db,aes(group=land_use,fill=land_use)) +
    theme_map() +
    theme(legend.position = "bottom")
  ggsave(here(paste0("views/",x,"_land_use.png")),height=4,width = 9)
  p
}

kebeles<- as.list(unique(dta$kebele))
ploter(kebeles[[1]])
lapply(kebeles,ploter)


polygons<- polygons %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))
polygons<- polygons %>% mutate(area=st_area(geometry))
polygons_nb<- polygons %>% 
                group_by(kebele) %>% 
                summarize(Polygons=n(),
                          mean_area=sum(area),.groups = "drop")%>% 
  st_drop_geometry()


polygons_land_use<- polygons %>% 
  group_by(kebele,land_use) %>% 
  summarize(n_polygons=n(),.groups = "drop") %>% 
  st_drop_geometry() %>% 
   pivot_wider(names_from=land_use,values_from = n_polygons)

polygons_land_use_total<- polygons %>% 
  group_by(land_use) %>% 
  summarize(n_polygons=n(),.groups = "drop") %>% 
  st_drop_geometry()%>% 
  pivot_wider(names_from=land_use,values_from = n_polygons) %>% 
  mutate(kebele="Total")

polygons_land_use<-rbind(polygons_land_use,polygons_land_use_total)

stargazer(data.frame(polygons_land_use),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/land_use_polygons_sample.tex"))

polygons_nb_lu<- polygons %>% 
  group_by(kebele,land_use) %>% 
  summarize(n_polygons=n(),
            mean_area=sum(area),.groups = "drop") %>% 
  st_drop_geometry()

polygons_nb_lu_total<- polygons %>% 
  group_by(land_use) %>% 
  summarize(n_polygons=n(),
            mean_area=sum(area),.groups = "drop") %>% 
  st_drop_geometry()

polygons_nb_lu<- polygons_nb_lu %>%  pivot_wider(!n_polygons,names_from=land_use,values_from = mean_area)


stargazer(data.frame(polygons_nb),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/areas_polygons_sample.tex"))


polygons_nb<- polygons_nb %>% left_join(polygons_nb_lu)

polygons_nb<- polygons_nb %>% mutate(Crop=Crop/mean_area*100,
                                     Grazing=Grazing/mean_area*100,
                                     `Improved forage`=`Improved forage`/mean_area*100,
                                     Tree=Tree/mean_area*100
                                     ) 

polygons_nb<-units::drop_units(polygons_nb)
stargazer(data.frame(polygons_nb),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/areas_polygons_sample.tex"))



#For the final land use
ploter<-function(x){
  db<- polygons %>% filter(kebele==x)
  p<-ggplot() +
    geom_sf(data=db,aes(group=land_use,fill=land_use)) +
    theme_map() +
    theme(legend.position = "bottom")
  ggsave(here(paste0("views/",x,"_land_use_final.png")),height=4,width = 9)
  p
}

kebeles<- as.list(unique(dta$kebele))
lapply(kebeles,ploter)

# Pixel Data ---------------------------------------------------------------
# All Pixels 
rm(dta)
dta<-readRDS(here("data/base_panel.Rds"))
ndvi<- dta %>% 
  arrange(pixel_id,date,land_use) %>% 
  select(kebele,land_use,pixel_id,date,ndvi) %>% 
  distinct(kebele,land_use,pixel_id,date,.keep_all = TRUE)%>% 
  ungroup() 

ndvi_long<-  ndvi %>% pivot_wider(names_from=date,values_from=ndvi) 
labels_ndvi_long<- ndvi_long %>% select(kebele,land_use,pixel_id)
ndvi_long<- ndvi_long %>% select(-land_use,-pixel_id)

land_use<- labels_ndvi_long %>% group_by(land_use) %>%  tally()
land_use <- land_use %>% pivot_wider(names_from=land_use,values_from = n)
land_use <- land_use %>% mutate(kebele="All")
kebele_land_use<- labels_ndvi_long %>% group_by(kebele,land_use) %>%  tally()
kebele_land_use<- kebele_land_use %>% pivot_wider(names_from=land_use,values_from = n)


luse<-rbind(kebele_land_use,land_use)
luse <- luse %>% relocate(kebele)
luse

stargazer(data.frame(luse),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/pixels_by_kebele.tex"))


#Train pixels that have 100% coverage
rm(dta,land_use)
dta<-readRDS(here("data/base_panel.Rds"))
land_use<-readRDS(here("data/land_use_labels.rds"))
land_use<- land_use %>% filter(area_land_use>=set_units(100,m^2)) 
land_use<- land_use %>% filter(land_use%in%c("Improved forage","Grazing","Tree","Crop"))
land_use<- land_use %>% distinct(pixel_id,land_use)
dta<- dta %>% left_join(.,land_use)
land_use<- dta %>% distinct(pixel_id,kebele,land_use)
land_use<- na.omit(land_use)


kebele_land_use<- land_use %>% group_by(kebele,land_use) %>%  tally()
kebele_land_use<- kebele_land_use %>% pivot_wider(names_from=land_use,values_from = n)

land_use<- land_use %>% group_by(land_use) %>%  
                        tally() %>% 
                        pivot_wider(names_from=land_use,values_from = n) %>% 
                        mutate(kebele="All")


luse100<-rbind(kebele_land_use,land_use)
luse100 <- luse100 %>% relocate(kebele)
luse100
stargazer(data.frame(luse100),summary=FALSE, rownames=FALSE,type="latex",digits=2,out=here("views/pixels_by_kebele_sample100.tex"))


# (300000000*4+6*300000000*8)/10242
# N<-7000000
# V<-6
# W<-4
# ((N*V*W + 4*N)/10242)*0.001
