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
library(outliers)

setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Dunk/MarotteHS_2020-08-03_145303/")
home<-getwd()

data.dir="C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/RMS calculations/Dunk/"
files=list.files(home,pattern=".csv")
files=files[1:8]
files=files[8]

for(f in files){

print(f)

mydat=read.csv(paste(home,"/",f, sep=""))
mydat2=mydat

mydat2$datetime <- as.POSIXct(strptime(mydat2$datetime,"%Y-%m-%d %H:%M:%S",tz=""))
mydat2$Ten_mins <-ceiling_date(mydat2$datetime, "10 mins")
mydat2=mydat2[complete.cases(mydat2), ]
outs=scores(mydat2$speed..m.s., type="t",prob=0.9999)
mydat2=mydat2[!outs,]


########################################################################################
# calculate RMS current with lags
# create 10 minute dataset
mydat3 <- mydat2 %>% 
		group_by(Ten_mins) %>%
        summarise(currentAvg = mean(speed..m.s., na.rm=T))
		
# create empty data columns for results
mydat3$currRMS10 <-NA
mydat3$currRMS10=as.numeric(mydat3$currRMS10)
mydat3$currRMS10D95<-NA
mydat3$currRMS10D95=as.numeric(mydat3$currRMS10D95)

mydat3$currRMSHr <-NA
mydat3$currRMSHr=as.numeric(mydat3$currRMSHr)
mydat3$currRMSHrD95<-NA
mydat3$currRMSHrD95=as.numeric(mydat3$currRMSHrD95)

mydat3$currRMS3Hr <-NA
mydat3$currRMS3Hr=as.numeric(mydat3$currRMS3Hr)
mydat3$currRMS3HrD95<-NA
mydat3$currRMS3HrD95=as.numeric(mydat3$currRMS3HrD95)


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
rmsCurr10D95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS10")]<-rmsCurr10
mydat3[i,c("currRMS10D95")]<-rmsCurr10D95

d1hr = doi-((600*6)-1)
record=mydat2[mydat2$datetime %in%(doi:d1hr),] # extracts 1 hour
record$average <-mean(record$speed..m.s, na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr1hr<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr1hrD95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMSHr")]<-rmsCurr1hr
mydat3[i,c("currRMSHrD95")]<-rmsCurr1hrD95

d3hr = doi-((600*18)-1)
record=mydat2[mydat2$datetime %in%(doi:d3hr),] # extracts 3 hours
record$average <-mean(record$speed..m.s, na.rm=TRUE)
record$diff =(record$speed..m.s.-record$average)^2
rmsCurr3hr<-(mean(record$diff,na.rm=TRUE))^1/2
rmsCurr3hrD95<-(quantile(record$diff,na.rm=TRUE, c(0.95)))^1/2
mydat3[i,c("currRMS3Hr")]<-rmsCurr3hr
mydat3[i,c("currRMS3HrD95")]<-rmsCurr3hrD95

}

write.csv(mydat3,paste(data.dir,"RMS_lags_",f,"_Dec2020", sep="")) 
 }
 


