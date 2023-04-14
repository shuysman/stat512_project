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
library(kableExtra)
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
  mutate(aet = grow_aetmean * 7, pet = grow_petmean * 7, p = annual_p * 12, log_growth_rt = log(growth_rt * 7, 10)) %>%
  ungroup()


# Exploratory data figure ####

#look for correlations within variables of question
interested_var<-df %>%
        select(log_growth_rt, annual_tmax, annual_p, spring_snow, spring_rain, grow_dmean, monthly_dmax, max_dsum, aet, pet, grow_gdd, annual_tmean)
## I misunderstood something here, we need to pass all variables from table 5 into the corr matrix, then select the more "biologically meaningful" variable from pairs that exceed 0.6 collinearity threshold.
## This should just be the full model they used in the model evaluation then.  For example, we choose AET over D for IGR model because it is more biologically relevant - see discussion section for physiological basis.
## however, many of these other columns likely have the same data cleanup issues as the other monthly/annual values.
## DF had code to run a correlation matrix I added to this repo, but I'm not sure how to get it to run on our df
## Not sure which column from the df is micro_pop in table 5 - SH

pairs_plot<-ggpairs(data = interested_var, upper = "blank")

interested_var.cor <- interested_var %>% cor()
##write.csv(interested_var.cor, file = "corr_matrix.csv")

library(corrplot)
corrplot(interested_var.cor, method="circle", type = "upper", tl.col="black", tl.srt=45, addCoef.col = "black")


# recreate raw data visualization from paper ####
fig4 <- df %>%
  group_by(unit) %>%
  ggplot(aes(x = unit, y = growth_rt)) +
  geom_boxplot()

full_cubic <- lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + poly(annual_p, 3) + poly(annual_p) + annual_p + poly(annual_tmax,3) + poly(annual_tmax, 2) +  annual_tmax+ micro + comp_number +PICO  + PIEN +ABLA +  (1 | unit), df)
AICc(full_cubic)
#aicc 1126.504

drop1(full_cubic, test = "Chisq", k = 2, trace = TRUE)


final_model<-lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + comp_number +  (1 | unit), df)
AICc(final_model)
#1130.444

# create a pretty table of model selection ####
models <- data.frame(model = "log_growth_rt ~ aet^3 + aet^2 + aet + pet^3+ pet^2 + pet + comp_number^3 + comp_number^2 + comp_number + annual_p^3 +annual_p^2 + annual_p + annual_tmax^3 + annual_tmax^2 + annual_tmax +PICO  + PIEN +ABLA +  (1 | unit)", AICc = 1126.504 )

models<-models%>%add_row(model="log_growth_rt ~ aet^3 + aet^2 + aet + pet^3+ pet^2 + pet + comp_number^3 + comp_number^2 + comp_number +  (1 | unit)", AICc = AICc(final_model))

modelTable<-models%>%
  kbl(caption = "Model Suite Tested") %>%
  kable_classic(full_width = T, html_font = "Cambria")

#laufenbery got 3262.92

par(mfrow=c(2,2))
plot(full_cubic)
# create table of model selection 
1134.65
