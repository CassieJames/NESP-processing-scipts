########################################################################################
#	Script to calculate RMS error of current meter readings processed for 1 second data
#	C James script
########################################################################################
library(dplyr)
library(lubridate)
library(zoo)

setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Dunk/MarotteHS_2020-08-03_145303/")
home<-getwd()

data.dir="C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/RMS calculations/Dunk/"
files=list.files(home,pattern=".csv")




for(f in files){

print(f)

mydat=read.csv(paste(home,"/",f, sep=""))
mydat2=mydat

mydat2$datetime <- as.POSIXct(strptime(mydat2$datetime,"%Y-%m-%d %H:%M:%S",tz=""))

mydat2$Ten_mins <-round_date(mydat2$datetime, "10 mins")


mydat3 <- mydat2 %>% 
		group_by(Ten_mins) %>%
        summarise(currentAvg = mean(speed..m.s., na.rm=T),currentSd = sd(speed..m.s., na.rm=T),datapoint_count = n())
  
mydat4=merge(mydat2,mydat3, by="Ten_mins")
mydat4$Diff=(mydat4$speed..m.s. - mydat4$currentAvg)^2


mydat5 <- mydat4 %>% 
		group_by(Ten_mins) %>%
        summarise(currentAvg = mean(speed..m.s., na.rm=T),currentSd = mean(currentSd, na.rm=T),currentRMS=(sum(Diff)/max(datapoint_count))^1/2)
		
write.csv(mydat5,paste(data.dir,"RMS_ten_min_",f, sep=""))

}

#############################################################################################

########################################################################################
#	Script to calculate RMS error of current meter readings processed for 1 second data
#	C James script
########################################################################################
library(svMisc)

setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Dunk/MarotteHS_2020-08-03_145303/")
home<-getwd()

data.dir="C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/RMS calculations/Dunk/"
files=list.files(home,pattern=".csv")
files=files[1:7]

for(f in files){

print(f)

mydat=read.csv(paste(home,"/",f, sep=""))
mydat2=mydat

mydat2$datetime <- as.POSIXct(strptime(mydat2$datetime,"%Y-%m-%d %H:%M:%S",tz=""))
mydat2$Ten_mins <-ceiling_date(mydat2$datetime, "10 mins")

########################################################################################
# calculate RMS current with lags
# create 10 minute dataset
mydat3 <- mydat2 %>% 
		group_by(Ten_mins) %>%
        summarise(currentAvg = mean(speed..m.s., na.rm=T))
		
# create empty data columns for results
mydat3$currRMS10 <-NA
mydat3$currRMS10=as.numeric(mydat3$currRMS10)
mydat3$currRMS10D90<-NA
mydat3$currRMS10D95<-NA

mydat3$currRMS30 <-NA
mydat3$currRMS30=as.numeric(mydat3$currRMS30)
mydat3$currRMS30D90<-NA
mydat3$currRMS30D95<-NA

mydat3$currRMSHr <-NA
mydat3$currRMSHr=as.numeric(mydat3$currRMSHr)
mydat3$currRMSHrD90<-NA
mydat3$currRMSHrD95<-NA

mydat3$currRMS3Hr <-NA
mydat3$currRMS3Hr=as.numeric(mydat3$currRMS3Hr)
mydat3$currRMS3HrD90<-NA
mydat3$currRMS3HrD95<-NA

mydat3$currRMS12Hr <-NA
mydat3$currRMS12Hr=as.numeric(mydat3$currRMS12Hr)
mydat3$currRMS12HrD90<-NA
mydat3$currRMS12HrD95<-NA

for (i in 1:nrow(mydat3)){
print(i)
roi=mydat3[i,]
doi=roi[,1]
doi=as.data.frame(doi)
doi=doi[[1]]
d10min = doi-(599)
record=mydat2[mydat2$datetime %in%(doi:d10min),] # extracts ten minute data
record$average <-mean(record$speed..m.s,na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr10<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr10D90<-(quantile(record$diff,na.rm=TRUE, c(0.9)))^1/2
rmsCurr10D95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS10")]<-rmsCurr10
mydat3[i,c("currRMS10D90")]<-rmsCurr10D90
mydat3[i,c("currRMS10D95")]<-rmsCurr10D95

d30min = doi-((600*3)-1)
record=mydat2[mydat2$datetime %in%(doi:d30min),] # extracts 30 minute data
record$average <-mean(record$speed..m.s,na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr30<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr30D90<-(quantile(record$diff,na.rm=TRUE, c(0.9)))^1/2
rmsCurr30D95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS30")]<-rmsCurr30
mydat3[i,c("currRMS30D90")]<-rmsCurr30D90
mydat3[i,c("currRMS30D95")]<-rmsCurr30D95

d1hr = doi-((600*6)-1)
record=mydat2[mydat2$datetime %in%(doi:d1hr),] # extracts 1 hour
record$average <-mean(record$speed..m.s, na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr1hr<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr1hrD90<-(quantile(record$diff,na.rm=TRUE, c(0.9)))^1/2
rmsCurr1hrD95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMSHr")]<-rmsCurr1hr
mydat3[i,c("currRMSHr0D90")]<-rmsCurr1hrD90
mydat3[i,c("currRMSHrD95")]<-rmsCurr1hrD95

d3hr = doi-((600*18)-1)
record=mydat2[mydat2$datetime %in%(doi:d3hr),] # extracts 3 hours
record$average <-mean(record$speed..m.s, na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr3hr<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr3hrD90<-(quantile(record$diff,na.rm=TRUE, c(0.9)))^1/2
rmsCurr3hrD95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS3Hr")]<-rmsCurr3hr
mydat3[i,c("currRMS3Hr0D90")]<-rmsCurr3hrD90
mydat3[i,c("currRMS3HrD95")]<-rmsCurr3hrD95

d12hr = doi-((600*72)-1)
record=mydat2[mydat2$datetime %in%(doi:d12hr),] # extracts 12 hours
record$average <-mean(record$speed..m.s,na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr12hr<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr12hrD90<-(quantile(record$diff,na.rm=TRUE, c(0.9)))^1/2
rmsCurr12hrD95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS12Hr")]<-rmsCurr12hr
mydat3[i,c("currRMS12Hr0D90")]<-rmsCurr12hrD90
mydat3[i,c("currRMS12HrD95")]<-rmsCurr12hrD95
}

write.csv(mydat3,paste(data.dir,"RMS_lags_",f, sep="")) 
 }
 


