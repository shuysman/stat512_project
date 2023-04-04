
#summarize by vegetation type using an input data file where all site time series are stacked into one super long file

setwd("C:\\David\\Water balance\\GRSA\\Paper folder")
master<-read.csv("append_monthly_all_with_veg.csv",header=TRUE, stringsAsFactors=FALSE,na.strings=c(""," ","na","no trees"))#remove all the garbage that can't be plotted
head(master)
master_sub<-subset(master, year>1999 & year<2016)#veg analysis was 2000-2016
min(master_sub$year)
max(master_sub$year)


library(plyr)
#get to annual summary by rolling up monthy values.
#step 0 average across all veg polygons both water and temperature variables
#step 1 sum monthly water variables by year, average montly t by year
#step 2 average all water sums across years, average monthly t by year

#average polygon monthly values by the vegetation types to obtain monthly values for each veg type
master_sub_annual_summary<-ddply(master_sub,.(veg,year,Month),plyr::summarize,meanp=mean(P),meant=mean(T),meand=mean(D),meanaet=mean(ET))
head(master_sub_annual_summary)

#sum monthly values to obtain annual values
master_annual_summary<-ddply(master_sub_annual_summary,.(veg,year),plyr::summarize,meanp=sum(meanp),meant=mean(meant),meand=sum(meand),meanaet=sum(meanaet))
head(master_annual_summary)

#average annual value to obtain long-term annual averages
master_ann_avg<-ddply(master_annual_summary,.(veg),plyr::summarize,meanp=mean(meanp),meant=mean(meant),meand=mean(meand),meanaet=mean(meanaet))
head(master_ann_avg)

#if need a summary of long-term monthly weather
#average monthly values across years to obtain long-term monthy values
master_monthly_summary<-ddply(master_sub_annual_summary,.(veg,Month),plyr::summarize,meanp=mean(meanp),meant=mean(meant),meand=mean(meand),meanaet=mean(meanaet))
head(master_monthly_summary)

write.csv(master_ann_avg, "wx_ann_avg.csv")
write.csv(master_monthly_summary, "wx_monthly_avg.csv")