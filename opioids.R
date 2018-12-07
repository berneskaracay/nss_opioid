library(tidyverse)

<<<<<<< HEAD
### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\nss_opioid")
# bernes
=======
opioids <- read_csv("data/opioids.csv")
prescribers <- read_csv("data/prescriber-info.csv")
overdoses <- read_csv("data/overdoses.csv")
cms14 <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2014.csv")
cms13 <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2013.csv")
cms15 <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2015.csv")
cms16 <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2016.csv")
overdoses_combined <- read_csv("data/NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv")

>>>>>>> 882db04efd03e7bac2fa5ad9d5c180a90c7b5a22

not_states <- list('AA','GU','AE','ZZ')
prescribers <- prescribers[!(prescribers$State %in% not_states), ]


overdoses <- overdoses %>% 
  mutate(Percentage = (Deaths/Population)*100)

<<<<<<< HEAD
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
=======

#filter for only opioid prescribers and group by state to get a count
state_counts <- prescribers %>%
  filter(Opioid.Prescriber == 1) %>%
  group_by(State) %>% 
  summarise(count_npi = n_distinct(NPI)) %>% 
  arrange(desc(count_npi))
>>>>>>> 882db04efd03e7bac2fa5ad9d5c180a90c7b5a22


<<<<<<< HEAD

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
=======
# create a list of NPIs from original dataset to subset each dataframe by
npi_list = c(prescribers$NPI)

cms13 <- cms13 %>% 
  mutate(year = 2013)

cms14 <- cms14 %>% 
  mutate(year = 2014)

cms15 <- cms15 %>% 
  mutate(year = 2015)

cms16 <- cms16 %>% 
  mutate(year = 2016)

# bind all dataframes together to one combined dataframe
cms_combined <- bind_rows(cms13,cms14,cms15,cms16)

# remove unnecessary state codes (AA, GU, AE, ZZ)
cms_combined <- cms_combined[!(cms_combined$nppes_provider_state %in% not_states), ]

# change orthopaedic to orthopedic
cms_combined$specialty_description <- gsub("Orthopaedic Surgery", "Orthopedic Surgery",
                                           cms_combined$specialty_description)

cms_combined$specialty_description <- gsub("Certified Registered Nurse Anesthetist (CRNA)",
                                           "CRNA", cms_combined$specialty_description)

cms_providers <- cms_combined %>% 
  filter(npi %in% npi_list)

cms_providers <- cms_providers %>% 
  filter(specialty_description != "Behavioral Analyst")

cms_opioid_providers <- cms_providers %>%
  filter(!is.na(opioid_claim_count))

cms_opioid_providers <- cms_providers %>% 
  filter(opioid_claim_count != 0)

# list of NPIs used in our analysis
providers_list <- c(cms_opioid_providers$npi)

# specialties dataframe
specialties_totals <- cms_opioid_providers %>% 
  group_by(specialty_description) %>% 
  summarize(opioid_claims = sum(opioid_claim_count, na.rm = TRUE),
            total_claims = sum(total_claim_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(specialty_description, opioid_claims, total_claims)

# create a new column for prescription rate
specialties_totals <- specialties_totals %>% 
  mutate(opioid_prescription_rate = (opioid_claims/total_claims)*100)

# top 5 specialties by opioid claims, regardless of prescription rate
top5_specs_counts_list <- list("Family Practice", "Internal Medicine", "Nurse Practitioner",
                          "Physician Assistant", "Orthopedic Surgery")

# top 5 specialties by rate, with den > 10,000,000 total claims
top5_specs_rates_list <- list("Pain Management", "Anesthesiology",
                         "Interventional Pain Management", "Physical Medicine and Rehabilitation", 
                         "Orthopedic Surgery")

# filter specialties_totals by top 5 by claims
top5_specs_counts <- specialties_totals %>% 
  filter(specialty_description %in% top5_specs_counts_list)

# create a new column to divide opioid claims by a million for plotting
top5_specs_counts_millions <- top5_specs_counts %>%
  mutate(opioid_claim_count = opioid_claims/1000000)

# filter specialties_totals by top 5 by rates
top5_specs_rates <- specialties_totals %>% 
  filter(specialty_description %in% top5_specs_rates_list)


# plot top 5 specialties by opioid claims
ggplot(top5_specs_counts_millions, aes(x=reorder(specialty_description, -opioid_claim_count),
                                       y=opioid_claim_count)) +
  geom_bar(stat = "identity") +
  labs(x="Specialty", y="Opioid Claims (Millions)",
       title="Top Five Specialties by Opioid Claims",
       subtitle = "United States, 2013-2016") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))

# plot top 5 specialties by opioid prescription rate
ggplot(top5_specs_rates, aes(x=reorder(specialty_description, -opioid_prescription_rate),
                             y=opioid_prescription_rate)) +
  geom_bar(stat = "identity") +
  labs(x="Specialty", y="Opioid Prescription Rate", 
       title="Opioid Prescription Rate by Specialty", subtitle="United States, 2013-2016") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))

# create a dataframe of specialties over time
specialties_yearly <- cms_opioid_providers %>% 
  group_by(specialty_description, year) %>% 
  summarize(opioid_claims = sum(opioid_claim_count, na.rm = TRUE),
            total_claims = sum(total_claim_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(specialty_description, year, opioid_claims, total_claims)

specialties_yearly <- specialties_yearly %>% 
  mutate(opioid_prescription_rate = (opioid_claims/total_claims)*100)

top5_specs_year <- specialties_yearly %>% 
  filter(specialty_description %in% top5_specs_rates_list)

top5_specs_box_data <- cms_opioid_providers %>% 
  filter(specialty_description %in% top5_specs_rates_list)

ggplot(top5_specs_box_data, aes(x=reorder(specialty_description, -opioid_prescriber_rate),
                            y=opioid_prescriber_rate)) +
  facet_wrap(ncol = 2, ~year) +
  geom_boxplot() +
  geom_point(aes(color = specialty_description),
             position=position_jitter(width = 0.2), alpha = 0.6) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_discrete(name = "Specialty", breaks = c("Interventional Pain Management",
                                                      "Pain Management",
                                                      "Anesthesiology", 
                                                      "Orthopedic Surgery", 
                                                      "Physical Medicine and Rehabilitation")) +
  labs(y="Opioid Prescription Rate", title="Opioid Prescription Rate by Specialty")

ggplot(top5_specs_year, aes(x=year, y=opioid_prescription_rate, color = specialty_description)) +
  geom_line() +
  scale_color_discrete(name = "Specialty", breaks = c("Interventional Pain Management",
                                                      "Pain Management",
                                                      "Anesthesiology", 
                                                      "Orthopedic Surgery", 
                                                      "Physical Medicine and Rehabilitation")) +
  labs(x="year", y="Opioid Prescription Rate", title="Opioid Prescription Rate by Specialty")


# counts by state, by year
cms_state_providers <- cms_providers %>% 
  group_by(nppes_provider_state, year) %>% 
  summarize(count_npi = n_distinct(npi),
            total_beneficiaries = sum(bene_count, na.rm = TRUE),
            total_claims = sum(total_claim_count, na.rm = TRUE),
            opioid_beneficiaries = sum(opioid_bene_count, na.rm = TRUE),
            opioid_claims = sum(opioid_claim_count, na.rm = TRUE),
            drug_supply = sum(total_day_supply, na.rm = TRUE),
            drug_cost = sum(total_drug_cost, na.rm = TRUE),
            opioid_supply = sum(opioid_day_supply, na.rm = TRUE),
            opioid_cost = sum(opioid_drug_cost, na.rm = TRUE),
            er_beneficiaries = sum(er_opioid_bene_count, na.rm = TRUE),
            er_claims = sum(er_opioid_claim_count, na.rm = TRUE),
            er_supply = sum(er_opioid_day_supply, na.rm = TRUE),
            er_cost = sum(er_opioid_drug_cost, na.rm = TRUE),
            total_30_day_fills = sum(total_30_day_fill_count, na.rm = TRUE),
            female = sum(beneficiary_female_count, na.rm = TRUE),
            male = sum(beneficiary_male_count, na.rm = TRUE),
            white = sum(beneficiary_race_white_count, na.rm = TRUE),
            black = sum(beneficiary_race_black_count, na.rm = TRUE),
            asian = sum(beneficiary_race_asian_pi_count, na.rm = TRUE),
            hispanic = sum(beneficiary_race_hispanic_count, na.rm = TRUE),
            native_indian = sum(beneficiary_race_nat_ind_count, na.rm = TRUE),
            other_race = sum(beneficiary_race_other_count, na.rm = TRUE),
            average_age = mean(average_age_of_beneficiaries, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(nppes_provider_state, year, count_npi, total_beneficiaries, total_claims,
         opioid_beneficiaries, opioid_claims, drug_supply, drug_cost, opioid_supply, opioid_cost,
         er_beneficiaries, er_claims, er_supply, er_cost, total_30_day_fills, female, male,
         white, black, asian, hispanic, native_indian, other_race, average_age)


cms_state_providers %>% 
  mutate(opioid_prescription_rate = (opioid_claims/total_claims)*100)


# need to review things after this line

specialty_state_counts <- cms_combined %>% 
  group_by(nppes_provider_state, year) %>% 
  summarize(hand_surgery = sum(specialty_description == "Hand Surgery"),
            int_pain_mgmt = sum(specialty_description %in% "Interventional Pain Management"),
            pain_mgmt = sum(specialty_description %in% "Pain Management"),
            ortho = sum(specialty_description %in% "Orthopedic Surgery"),
            oral_maxillofacial = sum(specialty_description %in% "Oral & Maxillofacial Surgery")) %>% 
  ungroup() %>% 
  select(nppes_provider_state, year, hand_surgery, int_pain_mgmt, pain_mgmt, ortho,
         oral_maxillofacial)


# plotting crude death rate per region over time
ggplot(providers_overdoses_combined, aes(x = year, y = `Crude Death Rate`)) +
  facet_wrap(~Region) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Crude Death Rate", title = "Crude Death Rate per Region")

# plotting opioid prescription rate per region over time
ggplot(providers_overdoses_combined, aes(x = year, y = opioid_prescription_rate)) +
  facet_wrap(~Region, ncol = 4) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Opioid Prescription Rate", title = "Opioid Prescription Rate per Region") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
>>>>>>> 882db04efd03e7bac2fa5ad9d5c180a90c7b5a22

model_data<-model_data %>%
  mutate(opioid_cost_percap=opioid_cost/Population)


<<<<<<< HEAD
model_data<-model_data %>%
  mutate(male_female=male/female)
=======
>>>>>>> 882db04efd03e7bac2fa5ad9d5c180a90c7b5a22



model_data<-model_data %>%
  mutate(white_other=white/(black+asian+hispanic+native_indian+other_race))

<<<<<<< HEAD
cms_combined<- cms_combined %>%
  group_by(NPI) %>%  
  mutate(id=paste(NPPES.Provider.First.Name,NPPES.Provider.State, sep="\n"))%>% 
  ungroup()

Sweave("opioids_report.Rnw", output=paste0("DQ5", ".tex"))



for (q in 1:3) {
  if (system(paste0("pdflatex -halt-on-error ", "DQ5", ".tex")) != 0L)
    stop("Unable to compile latex document ", "DQ5", ".tex")
}
=======
>>>>>>> 882db04efd03e7bac2fa5ad9d5c180a90c7b5a22
