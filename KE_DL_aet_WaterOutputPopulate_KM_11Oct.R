#######################
# Script for Dave L
# Created Dec. 2018
#Updated Oct. 2019 KM
#######################
library(tidyverse)

# Open master data file
path <- "C:\\Users\\admitjyxj\\Desktop\\DLauf\\WBP\\oct2019run\\monthly\\"
field<-read.csv(paste(path,"p.csv",sep=""),header=T)

nrow<-nrow(field)

# Loop through all individuals
for (i in 1:1) { #change to 1:nrow after tested
  
# Read individual ID (outfile)
ID <- field[i,"outfile"]

# Read planting year
PY <- field[i,"plant_yr"]

#######################
# Open water balance file for individual
wb.full <- read.csv(paste(path,ID,".csv",sep=""),header=T)

#Create max of month and max of year variables
wb.full$Max.of.month<- as.numeric(substring(wb.full$Row.Labels, regexpr("_", wb.full$Row.Labels)+1))
wb.full$Max.of.year<- as.numeric(substring(wb.full$Row.Labels, 1, 4))

wb.short<- wb.full[-c(457:468),]
wb<- wb.short %>% mutate_each(list(~as.numeric(as.character(.))), Average.of.T:Sum.of.GDD)


#######################
# T calculation

#startrow <- which(wb$Max.of.year==(PY-1) & wb$Max.of.month==1)
#endrow <- 456 # year 2017

#wb.d <- wb[startrow:endrow,"Sum.of.D.num"]

wb.d<- wb %>% filter(Max.of.year>=PY-1) %>% select(Sum.of.D)

max.d<-max(wb.d)
# Check
# print(max.d) #comment out when code tested

# Add to field data dataframe
field [i,"d_max_month"] <- max.d

spring<- wb %>% filter(Max.of.month<=6 & Max.of.month>=4) %>% select(Max.of.year, Sum.of.P,
                                                                     Average.of.T, Sum.of.PET, Sum.of.AET, Sum.of.D, Sum.of.GDD) %>% group_by(Max.of.year) %>% 
  summarise_all(list(mean))

#startrow <- which(spring$Max.of.year==(PY))
#endrow <- which(spring$Max.of.year==2017)

spring.year <- as.matrix(spring %>% filter(Max.of.year>=PY & Max.of.year<=2017))

spring.means<- apply(spring.year, 2, mean)
# Check
# print(max.d) #comment out when code tested

# Add to field data dataframe
field [i,"spring_p"] <- spring.means["Sum.of.P"]
field[i, "spring_t"] <- spring.means["Average.of.T"]
field[i, "spring_pet"] <- spring.means["Sum.of.PET"]
field[i, "spring_aet"] <- spring.means["Sum.of.AET"]
field[i, "spring_d"] <- spring.means["Sum.of.D"]
field[i, "spring_gdd"] <- spring.means["Sum.of.GDD"]




# Check
# print(field[i,"T_mean"])

#######################
# other variables ...

} # end individual loop

#######################
# Write file

write.csv(field, paste(path,"d_max_month.csv"),col.names = T)

