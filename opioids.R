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
library("xtable", lib.loc="~/R/win-library/3.4")

### read raw data sets ###

opioids=read.csv('data/opioids.csv', stringsAsFactors=FALSE)
overdoses=read.csv('data/overdoses.csv', stringsAsFactors=FALSE)
prescriber=read.csv('data/prescriber-info.csv', stringsAsFactors=FALSE)
cms=read.csv('data/cms.csv', stringsAsFactors=FALSE)
pres_14=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv', stringsAsFactors=FALSE)

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


#############################################################################
#################### prescriber pareto analysis #############################
#############################################################################

colnames(pres_14)[colnames(pres_14)=="NPPES.Provider.State"]<-"State"
table(pres_14["State"],exclude = NULL)
not_state=c('AA','GU','AE','ZZ',"XX","AP","AS","GU","MP","PR")
pres_14=pres_14[pres_14$State %nin% not_state,]
table(pres_14["State"],exclude = NULL)
colnames(pres_14)[colnames(pres_14)=="NPPES.Provider.Last.Org.Name"]<-"last_name"
colnames(pres_14)[colnames(pres_14)=="NPPES.Provider.First.Name"]<-"first_name"
colnames(pres_14)[colnames(pres_14)=="Total.Claim.Count"]<-"total_claim"
colnames(pres_14)[colnames(pres_14)=="Opioid.Prescribing.Rate"]<-"opioid_rate"
colnames(pres_14)[colnames(pres_14)=="Opioid.Claim.Count"]<-"opioid_claim"
colnames(pres_14)[colnames(pres_14)=="NPPES.Provider.Zip.Code"]<-"zipcode"
colnames(pres_14)[colnames(pres_14)=="Specialty.Description"]<-"Specialty"


glimpse(pres_14)
filter(pres_14,NPI==1518048750)


################## Question 2 ################################



#######2013##############
opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2013.csv', stringsAsFactors=FALSE)

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
  select("NPI","Specialty.Description","Opioid.Prescribing.Rate","Opioid.Claim.Count")%>% 
  top_n(n = 20, wt = Opioid.Claim.Count)

names_2013<-c(top_10_opioids_prescriber_2013$NPI)


#######2014##############
opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv', stringsAsFactors=FALSE)


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
pareto_2015=first_index/nrow(opioids)*100

top_10_opioids_prescriber_2015 <- opioids %>%
  select("NPI","Specialty.Description","Opioid.Prescribing.Rate","Opioid.Claim.Count" ) %>% 
  top_n(n = 20, wt = Opioid.Claim.Count)




names_2015<-c(top_10_opioids_prescriber_2015$NPI)

#######2016##############
opioids=read.csv('data/Medicare_Part_D_Opioid_Prescriber_Summary_File_2016.csv', stringsAsFactors=FALSE)

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


print(top_dr)

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

ggplot(cms_combined, aes(x=year,
                         y=Opioid.Claim.Count)) + 
  facet_wrap(~ `NPI`) +
  geom_col() +
  xlab("Generic Drug Name") +
  ylab("Opioid Prescription Rate") +
  ggtitle("2013 Opioid Prescription Rate", subtitle = "For Top 5 States by Drug Overdose Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25))






Sweave("opioids_report.Rnw", output=paste0("DQ5", ".tex"))



for (q in 1:3) {
  if (system(paste0("pdflatex -halt-on-error ", "DQ5", ".tex")) != 0L)
    stop("Unable to compile latex document ", "DQ5", ".tex")
}
