library(raster)
library(tidyverse)
library(lme4)
library(effects)
library(sf)
library(stars)

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


df <- df %>%
    mutate(aet = grow_aetmean * 7,
           log_growth_rt = log(growth_rt * 7, 10)) ### Growth rate is also monthly (?) - I can't find this anywhere in the paper, but it is the only way to make the plots match
                                        # Also, paper specifies they used natural log, however numbers given in log_growth_rt are from log base 10
                                        # Also, what is the purpose of the log_growth_rt column, it doesn't seem to have been used in this analysis
# I was only able to get the plots to match (roughly) recreating log_growth_rt here

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
df %>%
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


