library(plyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
library(mapproj)
library(gridExtra)
library(reshape2)
library(stargazer)
#library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(labelled)
library(readxl)
library(shinydashboard)

mod2 <- readRDS("data\\mymodel.rds")

states <- as.data.frame(mod2) %>% 
  select('NPPES.Provider.State') %>% 
  unique()

states <- sort(states$NPPES.Provider.State)