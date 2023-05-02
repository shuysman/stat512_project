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
library(ggpubr)

#read the dataframe ####
df_raw <- read.csv('./laufenberg-df.csv')

#clean it up ####
df<-df_raw%>%
  group_by(unit) %>%
  #we're not entirely sure why we have to do all these adjustements, probably because the growing season is 7 months long
    mutate(aet = grow_aetmean * 7,
           cwd = grow_dmean * 7,
           pet = grow_petmean * 7,
           p = annual_p * 12) %>%
  ungroup()%>%
  mutate(PICO = as.factor(PICO), micro = as.factor(micro), ABLA = as.factor(ABLA), PIEN = as.factor(PIEN))


# Exploratory data figure ####

## recreate raw data visualization from paper ####
## why do we need to log growth_rt?
## variances look unequal, look better after logging
fig4 <- df %>%
    group_by(unit) %>%
    ggplot(aes(x = unit, y = growth_rt)) +
    geom_boxplot()

fig4_logged <- df %>%
    group_by(unit) %>%
    ggplot(aes(x = unit, y = log_growth_rt)) +
    geom_boxplot()

fig4_combined <- ggarrange(fig4, fig4_logged)

### From Katie's comments, the above graph reproducing fig4 is not the
### right raw data plot to include there is no reason to visualize by
### planting unit.  Since we are including site in the model as a
### fixed effect, I think we should include of a plot of aet x growth
### rate, visualized by planting site.  We can do this either by
### faceting on site or col = site in the aes(), both are visually
### noisy but the facet_wrap seems easier to read.  from this plot, I
### can't see any reason to log transform growth rate.
aet_x_growthrt <- df %>%
    ggplot(aes(x = aet, y = growth_rt)) +
    geom_point() +
    facet_wrap(~site)

#look for correlations within variables of question
interested_var<-df %>%
        select(log_growth_rt, annual_tmax, annual_p, spring_snow, spring_rain, grow_dmean, monthly_dmax, max_dsum, aet, pet, grow_gdd, annual_tmean)
## I misunderstood something here, we need to pass all variables from table 5 into the corr matrix, then select the more "biologically meaningful" variable from pairs that exceed 0.6 collinearity threshold.
## This should just be the full model they used in the model evaluation then.  For example, we choose AET over D for IGR model because it is more biologically relevant - see discussion section for physiological basis.
## however, many of these other columns likely have the same data cleanup issues as the other monthly/annual values.
## DF had code to run a correlation matrix I added to this repo, but I'm not sure how to get it to run on our df
## Not sure which column from the df is micro_pop in table 5 - SH

interested_var.cor <- interested_var %>% cor()
##write.csv(interested_var.cor, file = "corr_matrix.csv")

library(corrplot)
corrplot(interested_var.cor, method="circle", type = "upper", tl.col="black", tl.srt=45, addCoef.col = "black")

##$T_{max}$, PPT, PET, AET, comp_number, micro, PICO, PIEN, ABLA
selected_vars <- df %>% select(log_growth_rt, annual_tmax, annual_p, aet, pet, comp_number, micro, PICO, PIEN, ABLA)

pairs_plot<-ggpairs(data = selected_vars, upper = "blank")

full_cubic <- lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + poly(annual_p, 3) + poly(annual_p) + annual_p + poly(annual_tmax,3) + poly(annual_tmax, 2) +  annual_tmax+ micro + comp_number +PICO  + PIEN +ABLA +  (1 | unit), df)

AICc(full_cubic)
#aicc 1126.504

null_model <- lmer(log_growth_rt ~ 1|unit, data = df)
AICc(null_model)

#backward selection on full model
step(full_cubic, scope = formula(null_model), direction = "backward")

final_cubic<-lmer(log_growth_rt ~ poly(aet, 3) + poly(pet, 3) + poly(aet, 2) + poly(pet, 2) + poly(comp_number, 3) + poly(comp_number, 2) + aet + pet + poly(annual_p, 3) + poly(annual_p) + annual_p + poly(annual_tmax, 2) + annual_tmax + comp_number + PICO + ABLA + (1 | unit), df)
AICc(final_cubic)
#AIC is 1111.886


step(null_model, scope = formula(full_cubic), direction = "forward")


#trying without cubic
full_first <- lmer(log_growth_rt ~ aet + pet + annual_p  +  annual_tmax+ micro + comp_number +PICO  + PIEN +ABLA +  (1 | unit), df)
step(full_first, direction = "backward")
#get aet + comp_number + PICO + ABLA + (1 | unit) without cubic terms

AICc(lmer(log_growth_rt~aet + comp_number + PICO + ABLA + (1 | unit), df))
#AICc: 1160.613

#then, if we cube the terms, we get this.
full_first_1<-lmer(log_growth_rt ~ poly(aet,3) + poly(comp_number,3) + PICO + ABLA + (1 | unit), df)
step(full_first_1)
AICc(full_cubic)
#AICc is 1126.504 which is the same score as the full cubic, with fewer terms. 

final_model<-lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + comp_number +  (1 | unit), df)
AICc(final_model)
#1130.444

# create a pretty table of model selection ####
models <- data.frame(model = "log_growth_rt ~ aet^3 + aet^2 + aet + pet^3+ pet^2 + pet + comp_number^3 + comp_number^2 + comp_number + annual_p^3 +annual_p^2 + annual_p + annual_tmax^3 + annual_tmax^2 + annual_tmax +PICO  + PIEN +ABLA +  (1 | unit)", AICc = AICc(full_cubic) )

models<-models%>%add_row(model="log_growth_rt ~ aet^3 + aet^2 + aet + pet^3+ pet^2 + pet + comp_number^3 + comp_number^2 + comp_number +  (1 | unit)", AICc = AICc(final_model))

modelTable<-models%>%
  kbl(caption = "Model Suite Tested") %>%
  kable_classic(full_width = T, html_font = "Cambria")

#laufenbery got 3262.92

par(mfrow=c(2,2))
plot(full_cubic)
# create table of model selection 
1134.65





#### Assuming we are skipping model selection, re: Katie's comments,
#### and proceeding with a model based on our RQ Our RQ could be
#### something like: what is the relationship between growth rate and
#### cwd/aet after accounting for comp number, unit, and site-to-site
#### variability?  It makes sense to me to include both aet/cwd if we
#### are asking a directed question, as they are typically treated as
#### a pair.  Can cite Stephenson 1998 for reasoning behind this.
m1 <- lmer(growth_rt ~ poly(aet,3) + poly(aet,2) + aet + poly(cwd,3) + poly(cwd,2) + cwd + comp_number + unit + (1|site), data = df)
summary(m1)

std_epsilon <- sigma(m1)
std_site <- VarCorr(m1)[[1]][1]

icc <- std_site**2 / (std_site**2 + std_epsilon**2)
### icc seems really low... (=0.05)

## likelihood ratio test for site RE
m1_remove_site <- lm(growth_rt ~ poly(aet,3) + poly(aet,2) + aet + poly(cwd,3) + poly(cwd,2) + cwd + comp_number + unit, data = df)
anova(m1, m1_remove_site)
### p = 1.417e-06, we conclude there is site-to-site variability

## Test if unit should be included
m1_remove_unit <- lmer(growth_rt ~ poly(aet,3) + poly(aet,2) + aet + poly(cwd,3) + poly(cwd,2) + cwd + comp_number +(1|site), data = df)
anova(m1, m1_remove_unit) ### indicates we should keep unit - still not sure it makes sense as a FE over RE to mean


## Effects plot
plot(effect(c("comp_number"), m1))
### > Error in mod.matrix %*% scoef : non-conformable arguments
### related to warning "fixed-effect model matrix is rank deficient so
### dropping 6 columns / coefficients" when fitting model?
