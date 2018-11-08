#################################################
#### organize data set and prepare variables ####
#################################################
###started github repisotory
########## Clear Environment  ###################
rm(list=ls())

### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\us-opiate-prescriptions")





### use packages ###
library(Hmisc)
library(rms)
library(MASS)
library(survival)
library(plyr)


### read raw data sets ###

opioids=read.csv('opioids.csv', stringsAsFactors=FALSE)
overdoses=read.csv('overdoses.csv', stringsAsFactors=FALSE)
prescriber=read.csv('prescriber-info.csv', stringsAsFactors=FALSE)

### check identifier variable ###
table(overdoses$Abbrev,exclude=NULL)
table(prescriber$State,exclude=NULL)

overdoses$Deaths<-as.numeric(gsub(",", "", overdoses$Deaths))
overdoses$Population<-as.numeric(gsub(",", "", overdoses$Population))
overdoses$death_percent<-(overdoses$Deaths/overdoses$Population)*(100)
overdoses$state_code <- factor(overdoses$Abbrev, levels=unique(as.character(overdoses$Abbrev)))



data_combined <- merge(x=overdoses, y=prescriber, by.x='Abbrev',by.y='State', all=TRUE)

table(overdoses$death_percent,exclude=NULL)

table(data_combined$Deaths,exclude=NULL)
table(data_combined$state_code,exclude=NULL)


table(overdoses$Deaths,exclude=NULL)
table(overdoses$state_code,exclude=NULL)

class(data_combined$Deaths)
class(data_combined$state_code)
class(data_combined$state_code)

Sweave("opioids_report.Rnw", output=paste0("DQ4", ".tex"))



for (q in 1:3) {
  if (system(paste0("pdflatex -halt-on-error ", "DQ4", ".tex")) != 0L)
    stop("Unable to compile latex document ", "DQ4", ".tex")
}
