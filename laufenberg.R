library(raster)
library(tidyverse)
library(lme4)
library(effects)
library(sf)
library(stars)
library(lmerTest)
library(MuMIn)

df <- read_csv('./laufenberg-df.csv')

## Figure 11. Mean growing season AET across all units.
## Laufenberg et al. 2020 states that AET is Mean actual evapotranspiration (April–October)
## The values given in df.csv provided by DL are much lower than values shown in paper and
## MS thesis.  When plotted, their pattern matches that of figure 11 in MS thesis, however
## the values are lower.  The growing season provided is 7 months long, and multipling the values
## in df.csv by 7 allows us to recreate the plot in figure 11 apparently exactly.  It will therefore
## be assumed that the aet values modeled in the paper are equal to 7x the grow_aetmean provided
## in df.csv
df %>%
    group_by(unit) %>%
    mutate(aet = grow_aetmean * 7) %>%
    ggplot(aes(x = unit, y = aet)) +
    geom_boxplot()

## Figure 10. Mean growing season PET across all units.
df %>%
    group_by(unit) %>%
    mutate(pet = grow_petmean * 7) %>%
    ggplot(aes(x = unit, y = pet)) +
    geom_boxplot()

## Figure 9. Mean annual precipitation for all units.
## Seems that annual_p is monthly, needs to be multiplied by 12 for total precipitation per year
df %>%
    group_by(unit) %>%
    mutate(p = annual_p * 12) %>%
    ggplot(aes(x = unit, y = p)) +
    geom_boxplot()

## Figure 8. Mean annual temperatures for all units (BT = Beartooth, EC = East Centennial, WC = West Centennial, WI = Wind River, WY = West Yellowstone).
## annual_tmean does not seem to need to be modified
df %>%
    group_by(unit) %>%
    ggplot(aes(x = unit, y = annual_tmean)) +
    geom_boxplot()

df <- df %>%
    mutate(aet = grow_aetmean * 7,
           log_growth_rt = log(growth_rt * 7, 10),
           pet = grow_petmean * 7,
           p = annual_p * 12) ### Growth rate is also monthly (?) - I can't find this anywhere in the paper, but it is the only way to make the plots match
                                        # Also, paper specifies they used natural log, however numbers given in log_growth_rt are from log base 10
                                        # Also, what is the purpose of the log_growth_rt column, it doesn't seem to have been used in this analysis
                                        # I was only able to get the plots to match (roughly) recreating log_growth_rt here
## also assuming other variables with grow_ are per growing season and need to be multiplied by 7 based on same reasoning


### Variable Selection

## From paper:
## Individual Growth Rate Models AICc K
## Null Model
## Log(growth_rate) ~ 1 + random (Unit)
## 3225.31 3
## Full Model
## Log(growth_rate) ~ AET + PET + PPT + T + Micro + Comp_number + PICO + PIEN + ABLA + random (Unit)
## 3262.92 12
## Best Model
## Log(growth_rate) ~ AET3 + Comp_number3 + random (Unit)
## 3186.57 9

null_model <- lmer(log_growth_rt ~ 1 + (1 | unit), df)
full_model <- lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + aet + pet + p + annual_tmean + micro + comp_number + PICO + PIEN + ABLA +  (1 | unit), df)
#make everything have cubic functions
full_cubic <- lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + poly(p, 3) + poly(p,2) + p + poly(annual_tmean,3) + poly(annual_tmean, 2) + annual_tmean + micro + comp_number +PICO  + PIEN +ABLA +  (1 | unit), df)

AICc(full_cubic) #[1] 1134.65

## Backwards selection.  We can't use step() for mixed models
## Does not seem to give the same result as the paper
## I don't know how to implement forward selection for mixed effects models

lmerTest::step(full_model)
lmerTest::step(full_cubic)

#maybe model, this is the model that was selected using step from the full cubic model. it suggests leaving the lower order polynomial terms in - PL
reduced_model<-lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + comp_number +PICO +ABLA +  (1 | unit), df)

AICc(reduced_model) #[1] 1125.392

lmerTest::step(reduced_model)

#play around with backwards selection without mixed effects
full_lm<-step(lm(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + aet + pet + p + annual_tmean + micro + comp_number + PICO + PIEN + ABLA, df), direction = "backward")

#log_growth_rt ~ poly(aet, 3) + poly(pet, 3) + p + annual_tmean + micro + comp_number + PICO + PIEN + ABLA


## All sub-sets
## Also does not give a similar result to the paper
options(na.action = "na.fail") # Must be run to use dredge
allSubsets_aqi <- dredge(full_model, rank = "AICc",
                         m.lim = c(3,10)) #Defaults into using
# visualize results
par(mar = c(3,5,6,4), mfrow = c(1,1))
plot(subset(allSubsets_aqi, delta < 4),
     labAsExpr = TRUE)


## This gives similar results to the paper, I think this is the way to
## go.  Methods from Zuur et al. "Mixed Effects Models and Extensi ns
## in Ecology with R" which is cited in the paper
drop1(full_model, test = "Chisq", k = 2, trace = TRUE)
#drop doesn't give cAIC values

drop1(full_cubic, test = "Chisq", k = 2, trace = TRUE)

drop_model<-lmer(log_growth_rt ~ poly(aet,3) + poly(pet,3) + poly(aet,2) + poly(pet,2) + poly(comp_number,3) + poly(comp_number,2) + aet + pet + comp_number +PICO +  (1 | unit), df)
AICc(drop_model) #1122.347

#try to do step wise selection with mixed effects
library(cAIC4)
mixed_step <- stepcAIC(full_cubic,direction="backward", trace=TRUE, data=df)

#this was the best model but the AIC score is so much smaller
# Best model:
#   ~ aet + pet + p + annual_tmean + micro + comp_number + PICO + PIEN + ABLA + (1 | unit) ,
# cAIC: 1105.043 


### Mixed Models

#lm1 <- lmer(log_growth_rt ~ poly(aet, 3) + poly(comp_number, 3) + ( 1 | unit ) + ( 1 | site ), df)
lm1 <- lmer(log_growth_rt ~ poly(aet, 3) + poly(comp_number, 3) + ( 1 | unit ), df)

# Step 1: Save the effect size estimates into a data.frame
# Use the effects package --> effect function. term= the fixed effect you want to get data on, mod= name of your model.
effects_aet <- effects::effect(term= "aet", mod= lm1)
summary(effects_aet)

effects_comp <- effects::effect(term = "comp_number", mod=lm1)
summary(effects_comp)

# Save the effects values as a df:
x_aet <- as.data.frame(effects_aet)
x_comp <- as.data.frame(effects_comp)

## Growth rate and AET
aet_plot <- ggplot() + 
    geom_point(data=subset(df), aes(x=aet, y=log_growth_rt)) + 
    geom_point(data=x_aet, aes(x=aet, y=fit), color="blue") +
    geom_line(data=x_aet, aes(x=aet, y=fit), color="blue") +
    geom_ribbon(data= x_aet, aes(x=aet, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
    labs(x="AET", y="log(Growth Rate)")

aet_plot


## Growth Rate and Competition

comp_plot <- ggplot() + 
    geom_point(data=subset(df), aes(x=comp_number, y=log_growth_rt)) + 
    geom_point(data=x_comp, aes(x=comp_number, y=fit), color="blue") +
    geom_line(data=x_comp, aes(x=comp_number, y=fit), color="blue") +
    geom_ribbon(data= x_comp, aes(x=comp_number, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
    labs(x="Comp Number", y="log(Growth Rate)")

comp_plot


### growth_rt ~ aet
lm2 <- lmer(log_growth_rt ~ poly(aet, 3) + ( 1 | unit ) + ( 1 | site ), df)

effects_aet2 <- effects::effect(term= "aet", mod= lm2)
summary(effects_aet2)

x_aet2 <- as.data.frame(effects_aet2)

## Growth rate and AET
aet_plot <- ggplot() + 
    geom_point(data=subset(df), aes(x=aet, y=log_growth_rt)) + 
    geom_point(data=x_aet2, aes(x=aet, y=fit), color="blue") +
    geom_line(data=x_aet2, aes(x=aet, y=fit), color="blue") +
    geom_ribbon(data= x_aet2, aes(x=aet, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
    labs(x="AET", y="log(Growth Rate)")

aet_plot

#' Transform raster as data.frame to be later used with ggplot
#' Modified from rasterVis::gplot
#'
#' @param x A Raster* object
#' @param maxpixels Maximum number of pixels to use
#'
#' @details rasterVis::gplot is nice to plot a raster in a ggplot but
#' if you want to plot different rasters on the same plot, you are stuck.
#' If you want to add other information or transform your raster as a
#' category raster, you can not do it. With `SDMSelect::gplot_data`, you retrieve your
#' raster as a tibble that can be modified as wanted using `dplyr` and
#' then plot in `ggplot` using `geom_tile`.
#' If Raster has levels, they will be joined to the final tibble.
#'
#' @export

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')

  dat <- dplyr::as.tbl(data.frame(coords, dat))

  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}


###
r_aet <- raster("/media/steve/storage/waterbalance/summary_layers/AET/rcp85/ensembles/ensemble_2040_2069_summer_rcp85_AET_units_mm.tif")

## Simulate competition layer, duplicate AET raster and set value to 1 everywhere
r_comp <- r_aet
values(r_comp) <- 1

rasters <- stack(r_aet, r_comp)
names(rasters) <- c("aet", "comp_number")

## https://stackoverflow.com/questions/35904671/generating-a-spatial-prediction-raster-from-a-glmm-with-a-random-intercept-and
## To make marginal/unconditional predictions, you need to make use of the re.form argument. 
pred <- predict(rasters, lm1, re.form = NA)

writeRaster(pred, filename="pred.tif", overwrite=TRUE)

gplot_pred <- gplot_data(pred)

gye_boundary <- st_read("/home/steve/OneDrive/whitebark/gyeboundary/GYE_boundary_dd.shp")

ggplot() +
    geom_tile(data = dplyr::filter(gplot_pred, !is.na(value)), 
              aes(x = x, y = y)) +
    geom_sf(data = gye_boundary) +
    coord_sf() 

plot(gye_boundary[1])
plot(pred, add = TRUE)

################################# Other Work from Paper
## Figure 4 Adjusted growth rates (cm year−1) across units
fig4 <- df %>%
    group_by(unit) %>%
    ggplot(aes(x = unit, y = growth_rt)) +
    geom_boxplot()
    
## Figure 6 Density change ratio among sites and across units
df %>%
    group_by(unit) %>%
    ggplot(aes(x = unit, y = den_change)) +
    geom_boxplot()


## Log(density_change_ratio) ~ Tmax3+ Comp_number3 + random (Unit) 72.42 9

lm2 <- lmer(den_change ~ poly(annual_tmax, 3) + poly(comp_number, 3) + ( 1 | unit) , df)


