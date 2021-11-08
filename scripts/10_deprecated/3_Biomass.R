##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
gc() # free up memory and report memory usage

#Load Packages
pkg<-list("dplyr","readxl","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)



dta<-read_excel("data/Amh_EPI_PRIVATE.xlsx")

sum_dta<-dta %>% group_by(name_plot) %>% summarize(mean=mean(biomass_yield_tha),
                                                        sd=sd(biomass_yield_tha),
                                                        n=n())

sum_dta<- sum_dta %>% mutate(uci=mean+1.96*(sd/sqrt(n)),
                             lci=mean-1.96*(sd/sqrt(n)),
                             ) %>% na.omit()


ggplot(sum_dta) +
  geom_point(aes(x=name_plot,y=mean)) +
  geom_errorbar(aes(x=name_plot,ymax=uci,ymin=lci), width = 0.25) +
  xlab("plots") +
  ylab("Average Biomass") +
  theme_bw()
ggsave("views/biomass.pdf",height=9,width = 6)


dta<- dta %>% mutate(C=ifelse(treatment=="C",0,1),
                     R=ifelse(treatment=="R",0,1),
                     D=ifelse(treatment=="D",0,1),
                     W=ifelse(treatment=="W",0,1))

require(lfe)

summary(felm(biomass_yield_tha~D+R+W|name_plot,data=dta))
