#this is the code script with our final data cleaning and analysis
#Steve Huysman and Parker Levinson
#Stat 512
#April 14, 2023

#load required packages ####
#library(raster)
library(tidyverse)
library(lme4)
library(effects)
#library(sf)
library(ggplot2)
#library(stars)
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
    select(log_growth_rt, age, annual_tmax, annual_p, spring_snow, spring_rain, grow_dmean, monthly_dmax, max_dsum, aet, pet, grow_gdd, comp_number, annual_tmean, micro, PICO, PIEN, ABLA, micro)
## I misunderstood something here, we need to pass all variables from table 5 into the corr matrix, then select the more "biologically meaningful" variable from pairs that exceed 0.6 collinearity threshold.
## This should just be the full model they used in the model evaluation then.  For example, we choose AET over D for IGR model because it is more biologically relevant - see discussion section for physiological basis.
## however, many of these other columns likely have the same data cleanup issues as the other monthly/annual values.
## DF had code to run a correlation matrix I added to this repo, but I'm not sure how to get it to run on our df
## Not sure which column from the df is micro_pop in table 5 - SH

pairs_plot<-ggpairs(data = interested_var)

# recreate raw data visualization from paper ####
fig4 <- df %>%
  group_by(unit) %>%
  ggplot(aes(x = unit, y = growth_rt)) +
  geom_boxplot()

full_cubic <- lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + poly(p, 3) + poly(p,2) + p + poly(annual_tmean,3) + poly(annual_tmean, 2) + annual_tmean + micro + comp_number +PICO  + PIEN +ABLA +  (1 | unit), df)
#aicc 1134.65
#laufenbery got 3262.92

par(mfrow=c(2,2))
plot(full_cubic)
# create table of model selection 
1134.65
