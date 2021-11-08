##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","sf","leaflet","stringr","tidyr","lubridate","here")
lapply(pkg, require, character.only=T)
rm(pkg)


x<-list.files(here("data/Sentinel-2"))
x<-x[grepl(".SAFE",x)]
x<-tibble(x)

x<-x %>% tidyr::separate(x,into=c("satellite","mision","date","number","relative","tile","extra"),sep ="_",remove=FALSE)

x<-x %>% tidyr::separate(date,into=c("date","time"),sep ="T")
x<- x %>% arrange(date,tile)
x<- x %>% mutate(date=ymd(date))

# d<-x %>% filter(grepl("1C",mision)) %>% select(x)
# setwd(here("data/Sentinel-2/"))
# for(i in 1:9){
#   system(paste("rm -rf ",d$x[i] ))
#   }

