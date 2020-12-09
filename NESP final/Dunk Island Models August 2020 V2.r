#### DUNK model trials

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(data.table)
library(openair)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(car)
library(relaimpo)
library(PerformanceAnalytics)
library(mgcv)
library(itsadug)

image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"


setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")
setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

########################################################################################
#### Dunk

coltypes=c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric")
mydat.x1<- read_xlsx("Dunk Island timeseries.xlsx", sheet=1, col_types = coltypes)
mydat.x2<- read_xlsx("Dunk Island timeseries.xlsx", sheet=2, col_types = coltypes)
mydat.x3<- read_xlsx("Dunk Island timeseries.xlsx", sheet=3, col_types = coltypes)
mydat.x4<- read_xlsx("Dunk Island timeseries.xlsx", sheet=4,col_types = coltypes)

mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)

tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4)


flowdata=data.frame(read.csv("All discharge data.csv"))
flowdata$Date <- as.Date(flowdata$Date, format="%d/%m/%Y") 

plot(tdata$timestamp,tdata$'speed (m/s)')

deployments<- read_xlsx("Deployment_dates.xlsx", sheet=1)
deploys=as.data.frame(deployments)
deploys=deploys[which(deploys$Site=="Dunk"),]
tdata$deploys<-NA
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[1], deploys$Date_in[2])]<-1
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[2], deploys$Date_in[3])]<-2
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[3], deploys$Date_in[4])]<-3
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[4], deploys$Date_in[5])]<-4
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[5], deploys$Date_in[6])]<-5
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[6], deploys$Date_in[7])]<-6
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[7], deploys$Date_in[8])]<-7
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[8], deploys$Date_in[9])]<-8

tdata2=tdata

colnames(tdata2)[9] <- "Speed"
colnames(tdata2)[11] <- "SpeedUpper"

tdata2=tdata2[with(tdata2, order(deploys, tdata2$Speed)),]
tdata2$timestamp=as.factor(tdata2$timestamp)

tdata3<- tdata2 %>%
  mutate(Speed) %>%
  group_by(deploys,Speed) %>%
  # 1. Remove grouping
  ungroup() %>%
  mutate(order = row_number())

##### Merge with wave rider buoy data 

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/NESP work with wave rider data etc/Waverider data downloads/"; setwd(data.dir)
TV_2016<-fread("townsville_2016.csv",sep=",")
TV_2016[,'DateTime'] <- as.POSIXct(TV_2016$"Date/Time", format = "%d/%m/%Y %H:%M")
TV_2016[,'Date/Time'] <- NULL # so we don't imply Date/Time!

TV_2017<-fread("townsville_2017.csv",sep=",")
TV_2017[,'DateTime'] <- as.POSIXct(TV_2017$"Date/Time", format = "%m/%d/%Y %H:%M")
TV_2017[,'Date/Time'] <- NULL # so we don't imply Date/Time!

TV_2018<-fread("townsville_2018.csv",sep=",")
TV_2018[,'DateTime'] <- as.POSIXct(TV_2018$"Date/Time", format = "%m/%d/%Y %H:%M")
TV_2018[,'Date/Time'] <- NULL # so we don't imply Date/Time!

TV_2019<-fread("townsville_2019.csv",sep=",")
TV_2019[,'DateTime'] <- as.POSIXct(TV_2019$"Date/Time", format = "%d/%m/%Y %H:%M")
TV_2019[,'Date/Time'] <- NULL # so we don't imply Date/Time!

TV=rbind(TV_2016, TV_2017, TV_2018, TV_2019)

TV$Hs[ TV$Hs<0 ] <- ""
TV$Hmax[ TV$Hmax<0 ] <- ""
TV$Tz[ TV$Tz<0 ] <- ""
TV$Tz[ TV$Tz<0 ] <- ""
TV$Tp[ TV$Tp<0 ] <- ""

TV.2<-setkey(TV, DateTime )

tdata5=setDT(tdata3)
tdata5$timestamp<-as.POSIXct(tdata5$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
tdata5$timestamp<-round_date(tdata5$timestamp, "10 mins") # issue with time stamp - its reading varing seconds around 10 min intervals

Cleve.2<-setkey(tdata5, timestamp)

MG <- TV.2[ Cleve.2, roll = "nearest",allow.cartesian=TRUE]

##### Merge with tide height data (10 minute data)

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/NESP work with wave rider data etc/Townsville tidal data/"; setwd(data.dir)
TV_tide_ht<-fread("Townsville_tide_Ht_10Mins.csv",sep=",")
TV_tide_ht$DateTime<-as.POSIXct(TV_tide_ht$DateTime, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

TV_tide_ht<-setkey(TV_tide_ht, DateTime)
MG<-setkey(MG, DateTime)
MG2 <-TV_tide_ht[ MG, roll = "nearest",allow.cartesian=TRUE]

##### Merge with RMS current data

#data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
#RMScurrent<-fread("Dunk_RMS_ALL.csv",sep=",")
#RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

#MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)#

#write.csv(MG2,paste('Dunk_deploys1_2_data2model.csv',sep=''),row.names=F) # save as CSV file


##### Merge with new RMS current data with lags

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
RMScurrent<-fread("Dunk_RMS_ALL_LAGS.csv",sep=",")
RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)

##### Merge with RMS current data with no lag

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
RMScurrent<-fread("Dunk_RMS_ALL.csv",sep=",")
RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)

##### Merge with river flows

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
flowdata=data.frame(read.csv("All discharge data.csv"))
flowdata$Date <- as.Date(flowdata$Date, format="%d/%m/%Y") 

MG2$Date <- format(as.POSIXct(strptime(MG2$DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
MG2$Date <- as.Date(MG2$Date, format="%Y-%m-%d") 
MG2=merge(MG2, flowdata,by="Date",all.x=TRUE, all.y=FALSE)

MG2$Hs=as.numeric(MG2$Hs)
MG2$Tz=as.numeric(MG2$Tz)
MG2$Tp=as.numeric(MG2$Tp)
MG2$Hmax=as.numeric(MG2$Hmax)

#############################################################################################
##### Deployments 1 and 2 with RMS pressure
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 

MG2_d1=MG2_d1[complete.cases(MG2_d1),]
#MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers
#MG2_d1=MG2_d1[MG2_d1$Speed<0.2,]
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.01,]
#MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
#MG2_d1=MG2_d1[MG2_d1$currRMS12Hr<0.004,]
#MG2_d1=MG2_d1[MG2_d1$currRMSHr<0.01,]
#MG2_d1=MG2_d1[MG2_d1$SSC>0,]

#### models using mgvc gams
# compare single predictors

# RMS ~19.6% (14.5% with deployments 1,2)
m1 <- bam(log(SSC) ~ s((RMS)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1 <- bam(log(SSC) ~ s((RMS)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMS_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Pressure RMS Fit from Dunk Island deployments 1 and 2 no AR1")


datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMS_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Pressure RMS Fit from Dunk Island deployments 1 and 2 AR1")  

############################################################################################
##### Deployments 1 - 6 with RMS pressure and NTUe
MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6)),c("DateTime", "NTUe", "RMS")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


#### models using mgvc gams
# compare single predictors

# RMS ~19.6% (14.5% with deployments 1,2)
m1 <- bam(log(NTUe) ~ s((RMS)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((RMS)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMS_deploys1to6_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Pressure RMS Fit to NTUe from Dunk Island deployments 1 to 6 no AR1")


datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMS_deploys1to6_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Pressure RMS Fit to NTUe from Dunk Island deployments 1 to 6 AR1")  

############################################################################################
##### Significant wave height
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime", "NTUe", "Hs")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# Hs 33.5% (31.9% with deployments 1 and 2)
m1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
Hs_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Hs Fit from Dunk Island deployments 1 and 2 AR1") 
  
  datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
Hs_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Hs Fit from Dunk Island deployments 1 and 2 no AR1")  
  

MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6)),c("DateTime", "NTUe", "Hs")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


m1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
Hs_deploys1to6_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Hs Fit from Dunk Island deployments 1 to 6 AR1")   
  
 datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
Hs_deploys1to6_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Hs Fit from Dunk Island deployments 1 to 6 no AR1")   
  
#############################################################################################
##### Deployments 1 and 2 with RMS current
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","NTUe","currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "deploys")] 

MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.01,]
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$currRMS12Hr<0.004,]
MG2_d1=MG2_d1[MG2_d1$currRMSHr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]  
  

# compare lags in currentRMS

# 36% (29.2% with deployments 1,2)
m1 <- bam(log(NTUe) ~ s((currRMS10)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMS10)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS10_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 10 minute lags Fit from Dunk Island deployments 1 and 2 no AR1")   
  
datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS10_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 10 minute lags Fit from Dunk Island deployments 1 and 2 AR1")   


# 36% (29.2% with deployments 1,2)
m1 <- bam(log(NTUe) ~ s((currRMS30)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMS30)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS30_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 30 minute lags Fit from Dunk Island deployments 1 and 2 no AR1")   
  
datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS30_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 30 minute lags Fit from Dunk Island deployments 1 and 2 AR1")   
 

# compare lags in currentRMS
 
m1 <- bam(log(NTUe) ~ s((currRMSHr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMSHr)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMSHr_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 1 hr lags Fit from Dunk Island deployments 1 and 2 no AR1")   
  
datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMSHr_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 1 hr lags Fit from Dunk Island deployments 1 and 2 AR1")   
   
  
# compare lags in currentRMS
 
m1 <- bam(log(NTUe) ~ s((currRMS3Hr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMS3Hr)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS3Hr_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 3 hr lags Fit from Dunk Island deployments 1 and 2 no AR1")   
  
datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS3Hr_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 3 hr lags Fit from Dunk Island deployments 1 and 2 AR1")   
   
# compare lags in currentRMS
 
m1 <- bam(log(NTUe) ~ s((currRMS12Hr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMS12Hr)),data=MG2_d1,rho=r1)
summary(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS12Hr_deploys12_no_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 12 hr lags Fit from Dunk Island deployments 1 and 2 no AR1")   
  
datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1.AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS12Hr_deploys12_ar1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Current RMS 12 hr lags Fit from Dunk Island deployments 1 and 2 AR1")   

##############################################################################################################################  
#### Print out plots comparing different single predictors
image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"

png(paste(image.dir,"Dunk Island RMS pressure deploys 1 and 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(RMS_deploys12_no_ar1,RMS_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS pressure deploys 1 to 6.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(RMS_deploys1to6_no_ar1,RMS_deploys1to6_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island Hs deploys 1 and 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(Hs_deploys12_no_ar1,Hs_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island Hs deploys 1 to 6.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(Hs_deploys1to6_no_ar1,Hs_deploys1to6_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS current 10 mins deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS10_deploys12_no_ar1,CurrRMS10_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS current 30 mins deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS30_deploys12_no_ar1,CurrRMS30_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS current 1 Hour deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMSHr_deploys12_no_ar1,CurrRMSHr_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS3Hr_deploys12_no_ar1,CurrRMS3Hr_deploys12_ar1, align = "v", nrow = 2)
dev.off()

png(paste(image.dir,"Dunk Island RMS current 12 Hours deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS12Hr_deploys12_no_ar1,CurrRMS12Hr_deploys12_ar1, align = "v", nrow = 2)
dev.off()


##############################################################################################################################
#### Look at model fits for 3 hour model
library(gratia)

m1 <- bam(log(NTUe) ~ s((currRMS3Hr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((currRMS3Hr)),data=MG2_d1,rho=r1)
summary(m1.AR1)

P1<-appraise(m1.AR1)
P2<-draw(m1.AR1)

png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys 1 to 2 gam checks.png",sep=''),width=40, height=40, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P1
dev.off()


png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys 1 to 2 visualise fit.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P2
dev.off()

  
##############################################################################################################################
# Compare models with two predictors using 3 hour
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime", "NTUe","currRMS3Hr", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1$Frequency=1/MG2_d1$Tp
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# 56.9, 60.5 with interaction (53.6 with deployments 1, 2 and 3)

m1 <- bam(log(NTUe) ~ s(currRMS3Hr)+s(Hs)+s(Height),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

m1AR1 <- bam(log(NTUe) ~ s(currRMS3Hr)+s(Hs),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1)



par(mfrow=c(1,2), cex=1.1)
acf_resid(m1)
acf_resid(m1.AR1)

datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1AR1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS3HrHs_interaction_AR1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS 3 hour and Hs from Dunk Island deployments 1 and 2 AR1")
  
  datas <- rbindlist(list(MG2_d1[, .(NTUe, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
CurrRMS3HrHs_interaction_no_AR1<-ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS 3 hour and Hs Fit from Dunk Island deployments 1 and 2 no AR1")
  
  
  png(paste(image.dir,"Dunk Island RMS current 3 Hours with Hs deploys 1 to 2.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS3HrHs_interaction_AR1,CurrRMS3HrHs_interaction_no_AR1, align = "v", nrow = 2)
dev.off()
  

################################################################################################## 
#### Compare RMS current with estimated orbital velocity max

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.01,]

MG2_d1$WL=(3.13*sqrt(MG2_d1$Depth))/(1/MG2_d1$Tp) # approximates wavelength as speed/frequency
MG2_d1$OrbitalV=(MG2_d1$Hs*pi)/(MG2_d1$Tp*sinh((2*pi/MG2_d1$WL)*MG2_d1$Depth))
summary(MG2_d1$Depth/MG2_d1$WL)

CurrRMS10vsOrbitalV<-ggplot(data = MG2_d1, aes(OrbitalV,log(currRMS10),colour = OrbitalV)) +
  geom_point(size = 0.8) +
  theme_bw() +
  labs(title = "Current RMS 10 mins versus maximum orbital velocity")
  
 CurrRMS3hrvsOrbitalV<-ggplot(data = MG2_d1, aes(OrbitalV,log(currRMS3Hr),colour = OrbitalV)) +
  geom_point(size = 0.8) +
  theme_bw() +
  labs(title = "Current RMS 3 hr versus maximum orbital velocity") 
  
  

png(paste(image.dir,"Dunk Island RMS current versus maximum orbital velocity.png",sep=''),width=40, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(CurrRMS10vsOrbitalV,CurrRMS3hrvsOrbitalV, align = "v", nrow = 2)
dev.off()
  
  
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
#MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers
#MG2_d1=MG2_d1[MG2_d1$Speed<0.2,]
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.01,]
#MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
#MG2_d1=MG2_d1[MG2_d1$currRMS12Hr<0.004,]
#MG2_d1=MG2_d1[MG2_d1$currRMSHr<0.01,]
#MG2_d1=MG2_d1[MG2_d1$SSC>0,]

 p <- ggplot(data = MG2_d1, aes(DateTime,OrbitalV, colour="OrbitalV"))+
  geom_point(size = 0.8)  
 p <- p+geom_line(aes(y=SSC/25,colour="SSC"),size = 0.8)
 p <- p+geom_line(aes(y=Hs,colour="Hs"),size = 0.8)
 p <- p+geom_line(aes(y=Height,colour="Height"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*25, name = "SSC"))
 p <- p + scale_color_manual(values = wes_palette("FantasticFox1", n = 4))
 p <- p + labs(y = "Maximum Orbital Velocity",
                x = "Date",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.1, 0.9))
  
  
###################################################################################################################
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime", "NTUe","currRMS3Hr", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1$Frequency=1/MG2_d1$Tp
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


m1 <- bam(log(NTUe) ~ s(currRMS3Hr)+s(Hs),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

m1AR1 <- bam(log(NTUe) ~ s(currRMS3Hr)+s(Hs),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1)


myres=residuals(m1AR1, type = "response") # data minus fitted values

MG2_d1$Residuals=exp(myres) # back transform residuals


 p <- ggplot(data = MG2_d1, aes(DateTime,Residuals, colour="Residuals"))+
  geom_point(size = 0.8)  
 p <- p+geom_line(aes(y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + scale_colour_manual(values = c("red", "blue"))
 p <- p + labs(y = "Fitted Residuals",
                x = "Date",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.1, 0.9))

  
png(paste(image.dir,"Dunk Island RMS current 3 Hours with Hs residuals versus Tully flow deploys 1 to 2.png",sep=''),width=40, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
p
dev.off()
  
 
  

###################################################################################################################
##### Analysis

MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6)),c("DateTime","NTUe", "RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Height", "deploys", "Tully")] 
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


# RMS
m1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","NTUe", "RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Height", "deploys", "Tully")] 
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# currRMS10
m1 <- bam(log(NTUe) ~ s(log(currRMS10)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(currRMS10)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)# currRMS10


m1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s((Hs)),data=MG2_d1,rho=r1)
summary(m1.AR1)

appraise(m1.AR1)
draw(m1.AR1)

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","NTUe", "RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs","Tz", "Height", "deploys", "Tully")] 
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1=MG2_d1[MG2_d1$currRMSHr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# currRMSHr
m1 <- bam(log(NTUe) ~ s(log(currRMSHr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(currRMSHr)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)# currRMSHr


MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","NTUe", "RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Height", "deploys", "Tully")] 
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# currRMS3Hr
m1 <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)# 


MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6)),c("DateTime","NTUe", "RMS", "Speed", "currRMS10","currRMS30", "currRMSHr", "currRMS3Hr", "currRMS12Hr", "Hs", "Tz","Height", "deploys", "Tully")] 
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1$Tz=as.numeric(MG2_d1$Tz)
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]
MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers


# currRMS3Hr
m1 <- bam(log(NTUe) ~ s(log(RMS))+s(Hs)+s(Tz),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(RMS))+s(Hs)+s(Tz),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)




datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS Fit from Dunk Island deployments 1 and 2")
