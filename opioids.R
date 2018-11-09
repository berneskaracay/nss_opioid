#################################################
#### organize data set and prepare variables ####
#################################################
###started github repisotory
########## Clear Environment  ###################
rm(list=ls())

### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\data-question-5-opioids-navy-cats")





### use packages ###
library(Hmisc)
library(rms)
library(MASS)
library(survival)
library(plyr)
library(tidyverse)

### read raw data sets ###

opioids=read.csv('opioids.csv', stringsAsFactors=FALSE)
overdoses=read.csv('overdoses.csv', stringsAsFactors=FALSE)
prescriber=read.csv('prescriber-info.csv', stringsAsFactors=FALSE)
cms=read.csv('cms.csv', stringsAsFactors=FALSE)

### check identifier variable ###
table(overdoses$Abbrev,exclude=NULL)
table(prescriber$State,exclude=NULL)

overdoses$Deaths<-as.numeric(gsub(",", "", overdoses$Deaths))
overdoses$Population<-as.numeric(gsub(",", "", overdoses$Population))
overdoses$death_percent<-(overdoses$Deaths/overdoses$Population)*(100)
overdoses$state_code <- factor(overdoses$Abbrev, levels=unique(as.character(overdoses$Abbrev)))


data_1 <- merge(x=overdoses, y=prescriber, by.x='Abbrev',by.y='State', all.x=TRUE)
data_combined <- merge(x=cms, y=data_1, by.x='NPI',by.y='NPI', all.y=TRUE)

table(data_1$State,exclude=NULL)

table(overdoses$death_percent,exclude=NULL)

table(data_combined$Deaths,exclude=NULL)
table(data_combined$state_code,exclude=NULL)


table(overdoses$Deaths,exclude=NULL)
table(overdoses$state_code,exclude=NULL)

class(data_combined$Deaths)
class(data_combined$state_code)
class(data_combined$state_code)
data_combined$ones<-1




#z <- data_combined[!duplicated(data_combined$state_code),]

#liste=c("Abbrev","ones","State","Population","Deaths","death_percent","state_code","NPI","Gender","Credentials","Specialty","Opioid.Prescriber")
#count_data <- data_combined[liste]


count_dr_group<-data_combined %>%filter(Opioid.Prescriber==1)%>% 
  group_by(Specialty.Description) %>% 
  summarise(agg =sum(Opioid.Claim.Count))

count_state_group<-data_combined %>%filter(Opioid.Prescriber==1)%>% 
  group_by(State) %>% 
  summarise(agg =sum(Opioid.Claim.Count))

count_state_percent_group<-data_combined %>%filter(Opioid.Prescriber==1)%>% mutate(Opioid.Claim.Count = Opioid.Claim.Count/Population, na.rm = TRUE)%>%
  group_by(State) %>% 
  summarise(agg =sum(Opioid.Claim.Count))



#bernes <- merge(x=count_dr, y=data_combined, by.x='State',by.y='State', all=TRUE)

mysample <- data_combined[sample(1:nrow(data_combined), 50,
                          replace=FALSE),]

Sweave("opioids_report.Rnw", output=paste0("DQ5", ".tex"))



for (q in 1:3) {
  if (system(paste0("pdflatex -halt-on-error ", "DQ5", ".tex")) != 0L)
    stop("Unable to compile latex document ", "DQ5", ".tex")
}
