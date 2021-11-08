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

dta<-readRDS(here("data/merge_data.Rds"))

# dta1<-readRDS(here("data/temp_bases/",files[[1]]))
# require("mapview")
# Legaba<- dta1 %>% filter(kebele=="Legaba")
# Legaba<-Legaba %>% mutate(NDVI=(B08-B04)/(B08+B04))
# mapview::mapview(Legaba ,zcol="land_use")
# mapview::mapview(Legaba ,zcol="NDVI")
# require(ggthemes)
# require(viridis)

# plot_kb<-function(kebl){
#   ggplot(dta %>% filter(kebel=kebl)) +
#     geom_sf(aes(fill = land_use),lwd=0) +
#     scale_fill_viridis() + 
#     theme_map()
#   ggsave(here(paste0("views",kebl,".png")),height=5,width=8)
# }


polygons<-readRDS(here("data/pilot_poligons_LU.rds"))
polygons<- polygons %>% filter(Name!="c010")
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

polygons_nb<- polygons_nb %>% mutate(Crop=Crop/mean_area,
                                     Grazing=Grazing/mean_area,
                                     `Improved forage`=`Improved forage`/mean_area,
                                     Tree=Tree/mean_area,
                                     `Tree/Crop`=`Tree/Crop`/mean_area,
                                     Other=Other/mean_area,
                                     Bare=Bare/mean_area) 
class(polygons_nb)

write_csv(polygons_nb,here("views/areas_polygons.csv"))


dta <-dta %>% mutate(NDVI=(B08-B04)/(B08+B04)) 
dta3<- dta %>% dplyr::select(land_use, B02,B03, B04, B08,NDVI,cloudcov) %>% st_drop_geometry()
require(gtsummary)
table1 <- tbl_summary(dta3,    by = land_use, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                               all_categorical() ~ "{n} / {N} ({p}%)"))

table1%>%
  as_gt() %>% gt::as_latex() %>% as.character() %>%cat()



dta4<- dta_no_sp %>% dplyr::select(land_use,B02,B03, B04, B08,NDVI,cloudcov)
dta4<- dta4 %>% mutate(ImpFor=ifelse(land_use=="Improved forage",1,0),)
require(gtsummary)
table2 <- tbl_summary(dta4,    by = ImpFor, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                               all_categorical() ~ "{n} / {N} ({p}%)"))

table2%>%
  as_gt() %>% gt::as_latex() %>% as.character() %>%cat()


prop.table(table(dta_no_sp$land_use))


dta_lu <-dta %>% st_drop_geometry() %>% filter(land_use=="Improved forage")

write.csv(dta_lu,"data/improved_forages_pixels.csv")
