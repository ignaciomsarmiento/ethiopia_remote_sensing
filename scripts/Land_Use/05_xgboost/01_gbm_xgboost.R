##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Sys.getenv("LD_LIBRARY_PATH")
#Load Packages
pkg<-list("dplyr","here","caret","gbm","xgboost")
lapply(pkg, require, character.only=T)
rm(pkg)

#set seed
set.seed(10101)


# Read data ---------------------------------------------------------------
dta<-readRDS(here("data/crossectional_data.Rds"))


# dta<-dta %>% mutate(ImpFor=ifelse(land_use=="Improved forage",1,0),
#                     crop=ifelse(land_use=="Crop",1,0),
#                     grazing=ifelse(land_use=="Grazing",1,0),
#                     tree=ifelse(land_use=="Tree",1,0))



# Clean and prepare data --------------------------------------------------

dta<- dta %>% mutate(land_use=ifelse(land_use=="Improved forage","Improv_forage",land_use))

dta<- dta %>% mutate(land_use=factor(land_use))
table(dta$land_use)
covariate_names<-colnames(dta)
covariate_names<- covariate_names[grepl('w_center',covariate_names)==FALSE]
covariate_names<- covariate_names[grepl('summer|winter|fall|spring',covariate_names)==FALSE]
covariate_names<- covariate_names[!(covariate_names%in%c("seasons","panel_id","Name","woreda","kebele"))]


dta1<- dta %>% ungroup() %>% dplyr::select(land_use,covariate_names) 
summary(dta1)




#  Gradient Boosting Machines -----------------------------------------

#GBM

set.seed(123)
seeds <- vector(mode = "list", length = 16)
for(i in 1:16) seeds[[i]] <- sample.int(1000, 22)
seeds[[16]] <- sample.int(1000, 1)
seeds
control <- trainControl(method="repeatedcv", 
                        number=5, 
                        repeats = 3,
                        seeds = seeds,
                        classProbs= TRUE, 
                        summaryFunction = multiClassSummary,
                        allowParallel = TRUE )


# can be "Accuracy",   "logLoss", "ROC",   "Kappa"
metric <- "logLoss"
# 
# gbmGrid <-  expand.grid(interaction.depth = c(1,3,5,7, 9), 
#                         n.trees = (1:50)*50, 
#                         shrinkage = c(0.001,0.01,0.1),
#                         n.minobsinnode = 10)
# 
# set.seed(825)
# gbm <- train(land_use~., 
#              data=dta1, 
#              method="gbm", 
#              metric=metric, 
#              tune_grid=gbmGrid,
#              trControl=control)
# print(gbm)





# XGBoost -----------------------------------------------------------------

set.seed(123)
seeds <- vector(mode = "list", length = 16)
for(i in 1:16) seeds[[i]] <- sample.int(1000, 36)
seeds[[16]] <- sample.int(1000, 1)
seeds
control <- trainControl(method="repeatedcv", 
                        number=5, 
                        repeats = 3,
                        seeds = seeds,
                        classProbs= TRUE, 
                        summaryFunction = multiClassSummary,
                        allowParallel = TRUE )


xgboost_grid <- expand.grid(
  nrounds = (1:50)*50,
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(1,3,5,7, 9),
  gamma = seq(0,10,by=0.5),
  colsample_bytree = .5,
  subsample = .7
)


set.seed(825)
xboost <- train(land_use~., 
                data=dta1, 
                method="xgbTree",
                tune_grid=xgboost_grid,
                metric=metric, 
                trControl=control)
print(xboost)


save.image(here("data/results/multiclass_gbm_xgboost.RData"))



