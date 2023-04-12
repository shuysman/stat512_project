#this is the code script with our final data cleaning and analysis
#Steve Huysman and Parker Levinson
#Stat 512
#April 14, 2023

#load required packages ####
library(raster)
#library(tidyverse)
library(lme4)
library(effects)
library(sf)
library(ggplot2)
library(stars)
library(dplyr)
library(lmerTest)
library(MuMIn)
library(GGally)

#read the dataframe ####
df_raw <- read.csv('./laufenberg-df.csv')

#clean it up ####
df<-df_raw%>%
  group_by(unit) %>%
  #we're not entirely sure why we have to do all these adjustements, probably because the growing season is 7 months long
  mutate(aet = grow_aetmean * 7, pet = grow_petmean * 7, p = annual_p * 12, log_growth_rt = log(growth_rt * 7, 10)) 


# Exploratory data figure ####

#look for correlations within variables of question
interested_var<-df%>%
  dplyr::select(log_growth_rt, aet, pet, comp_number, annual_tmean, micro, PICO, PIEN, ABLA)

pairs_plot<-ggpairs(data = interested_var)

# recreate raw data visualization from paper ####
fig4 <- df %>%
  group_by(unit) %>%
  ggplot(aes(x = unit, y = growth_rt)) +
  geom_boxplot()



