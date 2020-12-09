

########################################################################################
#	Script to calculate statistics of current meter readings processed for 1 second data
#	C James script
########################################################################################
library(dplyr)
library(lubridate)
library(zoo)
library(svMisc)
library(outliers)
#############################################################################################

setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Dunk/MarotteHS_2020-08-03_145303/")
setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Geoffrey/MarotteHS_2020-08-03_214732/")
setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Cleveland/ALL current/")
setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Havannah/MarotteHS_2020-08-07_091649/")
# set analysis to deployments 1,2,3,4,5,10 and 12 as others are rubbish
files=files[c(2,4,6,8,11,12)]

setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Middle/MarotteHS_2020-08-10_095634/")
setwd("E:/Field Instrument Data/1_Resuspension Sites/1_Current meter/3-Processed_CM_CJ/Orchard/MarotteHS_2020-08-05_104817/")


home<-getwd()

data.dir="C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/RMS calculations/Orchard/"
files=list.files(home,pattern=".csv")

for(f in files){

print(f)

mydat=read.csv(paste(home,"/",f, sep=""))
mydat2=mydat

mydat2$datetime <- as.POSIXct(strptime(mydat2$datetime,"%Y-%m-%d %H:%M:%S",tz=""))
mydat2$Ten_mins <-ceiling_date(mydat2$datetime, "10 mins")
mydat2=mydat2[complete.cases(mydat2), ]
outs=scores(mydat2$speed..m.s., type="t",prob=0.9999)
mydat2=mydat2[!outs,]

if (f==files[1]){finaldat=mydat2}
else {finaldat=rbind(finaldat, mydat2)}

}

mymean <-mean(finaldat$speed..m.s,na.rm=TRUE)
D95<-quantile(finaldat$speed..m.s,na.rm=TRUE, c(0.95))
D90<-quantile(finaldat$speed..m.s,na.rm=TRUE, c(0.90))


mymean
D90
D95