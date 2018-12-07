#################################################
#### organize data set and prepare variables ####
#################################################
###started github repisotory
########## Clear Environment  ###################
rm(list=ls())

### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\nss_opioid")
# bernes




### use packages ###
library(Hmisc)
library(rms)
library(MASS)
library(survival)
library(plyr)
library(tidyverse)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

library(Hmisc)
library(rms)
library(MASS)
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(labelled)
library(foreign)
library(tree)
library(randomForest)
library(ppcor)
library(rms)
library(plotly)
require(gridExtra)
library(readxl)
library("xtable", lib.loc="~/R/win-library/3.4")

### read raw data sets ###


################## Question 2 ################################

prescriber=read.csv('data\\prescriber-info.csv', stringsAsFactors=FALSE)
prescriber_list=c(prescriber$NPI)

#######2013##############
opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2013.csv', stringsAsFactors=FALSE)
opioids=subset(opioids, NPI %in% prescriber_list)
opioids=subset(opioids,Opioid.Claim.Count>1)
bernes<-opioids %>%
  dplyr::summarize(Opioids_sum= sum(`Opioid.Claim.Count`,na.rm=TRUE),
                   Total_claims = sum(`Total.Claim.Count`,na.rm=TRUE)) %>% 
  mutate(limit = Opioids_sum*0.8)%>%ungroup

opioids$limit<-bernes$limit
opioids<-arrange(opioids, desc(Opioid.Claim.Count))




opioids <-opioids%>%mutate(cum_claim = cumsum(`Opioid.Claim.Count`))
opioids$passed_limit<-0
opioids$passed_limit[opioids["cum_claim"]>opioids["limit"]]<-1


first_index<-min(which(opioids$passed_limit == 1))
pareto_2013=first_index/nrow(opioids)*100

top_10_opioids_prescriber_2013 <- opioids  %>% 
  select("NPI","Specialty.Description","NPPES.Provider.State","Opioid.Claim.Count")%>% 
  top_n(n = 20, wt = Opioid.Claim.Count)

names_2013<-c(top_10_opioids_prescriber_2013$NPI)


#######2014##############
opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv', stringsAsFactors=FALSE)
opioids=subset(opioids, NPI %in% prescriber_list)
opioids=subset(opioids,Opioid.Claim.Count>1)

bernes<-opioids %>%
  dplyr::summarize(Opioids_sum= sum(`Opioid.Claim.Count`,na.rm=TRUE),
                   Total_claims = sum(`Total.Claim.Count`,na.rm=TRUE)) %>% 
  mutate(limit = Opioids_sum*0.8)%>%ungroup

opioids$limit<-bernes$limit
opioids<-arrange(opioids, desc(Opioid.Claim.Count))



opioids <-opioids%>%mutate(cum_claim = cumsum(`Opioid.Claim.Count`))
opioids$passed_limit<-0
opioids$passed_limit[opioids["cum_claim"]>opioids["limit"]]<-1


first_index<-min(which(opioids$passed_limit == 1))
pareto_2014=first_index/nrow(opioids)*100

top_10_opioids_prescriber_2014 <- opioids  %>% 
  select("NPI","Specialty.Description","Opioid.Prescribing.Rate","Opioid.Claim.Count")%>%
  top_n(n = 20, wt = Opioid.Claim.Count)

names_2014<-c(top_10_opioids_prescriber_2014$NPI)


#######2015##############

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2015.csv', stringsAsFactors=FALSE)
opioids=subset(opioids, NPI %in% prescriber_list)
opioids=subset(opioids,Opioid.Claim.Count>1)
bernes<-opioids %>%
  dplyr::summarize(Opioids_sum= sum(`Opioid.Claim.Count`,na.rm=TRUE),
                   Total_claims = sum(`Total.Claim.Count`,na.rm=TRUE)) %>% 
  mutate(limit = Opioids_sum*0.8)%>%ungroup

opioids$limit<-bernes$limit
opioids<-arrange(opioids, desc(Opioid.Claim.Count))



opioids <-opioids%>%mutate(cum_claim = cumsum(`Opioid.Claim.Count`))
opioids=subset(opioids, NPI %in% prescriber_list)
opioids$passed_limit<-0
opioids$passed_limit[opioids["cum_claim"]>opioids["limit"]]<-1


first_index<-min(which(opioids$passed_limit == 1))
pareto_2015=first_index/nrow(opioids)*100

top_10_opioids_prescriber_2015 <- opioids %>%
  select("NPI","Specialty.Description","Opioid.Prescribing.Rate","Opioid.Claim.Count" ) %>% 
  top_n(n = 20, wt = Opioid.Claim.Count)




names_2015<-c(top_10_opioids_prescriber_2015$NPI)

#######2016##############

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2016.csv', stringsAsFactors=FALSE)
opioids=subset(opioids, NPI %in% prescriber_list)
opioids=subset(opioids,Opioid.Claim.Count>1)
bernes<-opioids %>%
  dplyr::summarize(Opioids_sum= sum(`Opioid.Claim.Count`,na.rm=TRUE),
                   Total_claims = sum(`Total.Claim.Count`,na.rm=TRUE)) %>% 
  mutate(limit = Opioids_sum*0.8)%>%ungroup

opioids$limit<-bernes$limit
opioids<-arrange(opioids, desc(Opioid.Claim.Count))



opioids <-opioids%>%mutate(cum_claim = cumsum(`Opioid.Claim.Count`))
opioids$passed_limit<-0
opioids$passed_limit[opioids["cum_claim"]>opioids["limit"]]<-1


first_index<-min(which(opioids$passed_limit == 1))
pareto_2016=first_index/nrow(opioids)*100

top_10_opioids_prescriber_2016 <- opioids %>% 
  select("NPI","Specialty.Description","Opioid.Prescribing.Rate","Opioid.Claim.Count")%>%
  top_n(n = 20, wt = Opioid.Claim.Count)

names_2016<-c(top_10_opioids_prescriber_2016$NPI)


top_dr<-Reduce(intersect, list(names_2013, names_2014,names_2015,names_2016))



######################tOP PRESCRIBERS###########################

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2016.csv', stringsAsFactors=FALSE)

top_2016<-subset(opioids,NPI  %in% top_dr)
top_2016 <- top_2016 %>% 
  mutate(year = 2016) 

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2015.csv', stringsAsFactors=FALSE)

top_2015<-subset(opioids,NPI  %in% top_dr)
top_2015 <- top_2015 %>% 
  mutate(year = 2015) 

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv', stringsAsFactors=FALSE)

top_2014<-subset(opioids,NPI  %in% top_dr)
top_2014 <- top_2014 %>% 
  mutate(year = 2014) 

opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2013.csv', stringsAsFactors=FALSE)

top_2013<-subset(opioids,NPI  %in% top_dr)
top_2013 <- top_2013 %>% 
  mutate(year = 2013) 

cms_combined <- bind_rows(top_2013,top_2014,top_2015,top_2016)



############# Death Model #########################3

model=read.csv('data/davis.csv', stringsAsFactors=FALSE)
gdp=read.csv('data/qgdpstate_all_R.csv', stringsAsFactors=FALSE)
#us_states=read.csv('data/us_states.csv', stringsAsFactors=FALSE)

gdp_long <- melt(gdp, id.vars = c("State", "Region", "ComponentName"))
gdp_long$year<-as.numeric(str_sub(gdp_long$variable, start= 2))
#long_combined <- merge(x=long, y=us_states, by.x='State',by.y='States', all.y=TRUE)

model_data <- merge(x=gdp_long, y=model, by.x=c('State',"year"),by.y=c('State',"year"), all.y=TRUE)

model_data<-model_data %>%
  mutate(gdp_percap=value*1000000/Population)

model_data<-model_data %>%
  mutate(opioid_cost_percap=opioid_cost/Population)


model_data<-model_data %>%
  mutate(male_female=male/female)



model_data<-model_data %>%
  mutate(white_other=white/(black+asian+hispanic+native_indian+other_race))

cms_combined<- cms_combined %>%
  group_by(NPI) %>%  
  mutate(id=paste(NPPES.Provider.First.Name,NPPES.Provider.State, sep="\n"))%>% 
  ungroup()

Sweave("opioids_report.Rnw", output=paste0("DQ5", ".tex"))



for (q in 1:3) {
  if (system(paste0("pdflatex -halt-on-error ", "DQ5", ".tex")) != 0L)
    stop("Unable to compile latex document ", "DQ5", ".tex")
}
