##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("tidyverse","caret","here","ggplot2","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)


# Read data ---------------------------------------------------------------
dta<-readRDS(here("data/merge_data.Rds"))

dta<-dta %>% rename(panel_id=panel_ir)

dta<-dta %>% mutate(NDVI=(B08-B04)/(B08+B04))
table(dta$land_use)
dta<-dta %>% mutate(ImpFor=ifelse(land_use=="Improved forage",1,0),
                    bare=ifelse(land_use=="Bare",1,0),
                    crop=ifelse(land_use=="Crop",1,0),
                    grazing=ifelse(land_use=="Grazing",1,0),
                    other=ifelse(land_use=="Other",1,0),
                    tree=ifelse(land_use=="Tree",1,0),
                    tree_crop=ifelse(land_use=="Tree/Crop",1,0))

class(dta$date_acquisition)
dta<-dta %>% mutate(date_acquisition=ymd(date_acquisition))
dta<-dta %>% mutate(yr=year(date_acquisition),
                    mth=month(date_acquisition),
                    dy=day(date_acquisition))
table(dta$yr)



dta_mod<-dta %>% dplyr::select(Name, #name polygon
                               panel_id,
                               date_acquisition,
                               ImpFor,
                               bare,
                               crop,
                               grazing,
                               other,
                               tree,
                               tree_crop,
                               B02, #blue
                               B03, #green
                               B04, #red
                               B08, #NIR
                               cloudcov,
                               cloudcov_notvegetated,
                               tile_id)

prop.table(table(dta_mod$ImpFor))

dta_mod <- dta_mod %>% mutate(Name=factor(Name),
                              tile_id=factor(tile_id))


dta_mod<- dta_mod %>% mutate(positive=factor(ImpFor,levels=c(0,1),labels=c("negative","positive"))) %>% dplyr::select(-ImpFor)

dta_mod<- dta_mod %>% na.omit()
prop.table(table(dta_mod$positive))
dta_mod<- dta_mod %>% distinct(panel_id,date_acquisition,.keep_all = TRUE)
dta_mod<-dta_mod %>% mutate(NDVI=(B08-B04)/(B08+B04)) 
# Partition Train-TEst ----------------------------------------------------

panel<- dta_mod %>% distinct(panel_id,positive)
positive<- panel %>% filter(positive=="positive") %>% 
                    sample_frac(size=.1)
negative<- panel %>% filter(positive=="negative") %>% 
  sample_frac(size=.1)

index<-c(positive$panel_id,negative$panel_id)
Train<-dta_mod %>% filter(panel_id%in%index)
prop.table(table(Train$positive))
prop.table(table(dta_mod$positive))


# 
# index <- createDataPartition(dta_mod$panel_id, p = .9, 
#                              list = FALSE, 
#                              times = 1)
# 
# Train <- dta_mod[ index,]
# Test  <- dta_mod[-index,]

# require("pglm")
# Train<-pdata.frame(Train, index=c("panel_id","date_acquisition"))
# Test<-pdata.frame(Test, index=c("panel_id","date_acquisition"))



x<-glm(positive~
       bare+
       crop+
       grazing+
       other+
       tree+
       tree_crop+
       B02+
       B03+
       B04+
       B08+
      NDVI +   
       cloudcov+
       cloudcov_notvegetated,data=Train,family = "binomial")



require("ROSE")

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(5627)
over <- train(positive ~ .,
                   data = Train,
                    method = "treebag",
                    nbagg = 50,
                   metric = "ROC",
                   trControl = ctrl)



set.seed(9560)
up_train <- upSample(x = Train_Sample[, -ncol(Train_Sample)],
                     y = Train_Sample$positive)                         



set.seed(5627)
up_outside <- train(positive ~ ., data = up_train, 
                    method = "treebag",
                    nbagg = 50,
                    metric = "ROC",
                    trControl = ctrl)
