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
                               yr,
                               mth,
                               dy,
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




dta_mod<- dta_mod %>% mutate(positive=factor(ImpFor,levels=c(0,1),labels=c("negative","positive"))) %>% dplyr::select(-ImpFor)

dta_mod<- dta_mod %>% na.omit()

# Partition Train-TEst ----------------------------------------------------

index <- createDataPartition(dta_mod$positive, p = .7, 
                             list = FALSE, 
                             times = 1)

Train <- dta_mod[ index,]
Test  <- dta_mod[-index,]




ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


logit_orgi <- train(positive ~ .,
                   data = Train,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl
                  )


test_roc <- function(model, data) {
  roc(data$positive,
      predict(model, data, type = "prob")[, "positive"])
  
}


logit_orgi %>%
  test_roc(data = Test) %>%
  auc()



