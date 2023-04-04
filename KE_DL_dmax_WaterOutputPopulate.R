#######################
# Script for Dave L
# Created Dec. 2018

#######################
# Open master data file
path <- "C:\\Users\\t88b649\\Documents\\DLauf\\WBP\\Water Balance Model\\feb 2019 run\\feb5\\annual\\"
field<-read.csv(paste(path,"field_et.csv",sep=""),header=T)

nrow<-nrow(field)

# Loop through all individuals
for (i in 1:nrow) { #change to 1:nrow after tested
  
# Read individual ID (outfile)
ID <- field[i,"outfile"]

# Read planting year
PY <- field[i,"plant_yr"]

#######################
# Open water balance file for individual
wb <- read.csv(paste(path,ID,".csv",sep=""),header=T)

#######################
# rain_mean calculation

startrow <- (PY - 1980 + 1)
endrow <- 38 # year 2017

wb.rain_mean <- wb[startrow:endrow,"Sum.of.RAIN"]

rain_mean<-mean(wb.rain_mean)
# Check
## print(rain_mean) #comment out when code tested

# Add to field data dataframe
field[i,"rain_mean"] <- rain_mean

# Check
## print(field[i,"rain_mean"])

#######################
# other variables ...

} # end individual loop

#######################
# Write file

write.csv(field, paste(path,"rain_mean.csv"),col.names = T)
