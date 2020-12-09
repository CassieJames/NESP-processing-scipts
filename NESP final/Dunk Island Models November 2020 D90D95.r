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
library(gratia)
library(wesanderson)



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

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
RMScurrent<-fread("Dunk_RMS_ALL.csv",sep=",")
RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)#


##### Merge with new RMS current data with lags

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
RMScurrent<-fread("Dunk_RMS_ALL_LAGS_D90D95.csv",sep=",")
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

MGFINAL=MG2
##### Wind data from Cardwell


NESP.dir="C:/Users/jc246980/Documents/Current projects/NESP/Wind/"

wind=data.frame(read.csv(paste(NESP.dir,"WindDaily_wind_data_CARDWELL.csv",sep="")))
wind$datestamp <- as.Date(wind$datestamp, format="%d/%m/%Y") 

MG2=merge(MG2, wind, by.x="Date",by.y="datestamp", all.x=TRUE, all.y=FALSE) 

wind_weekly=data.frame(read.csv(paste(NESP.dir,"WindWeekly_wind_data_CARDWELL.csv",sep="")))
wind_weekly$Week <- as.Date(wind_weekly$Week,format="%Y-%m-%d") 


##### Merge with AIMS NTUe data
setwd("C:/Users/jc246980/Documents/Current projects/NESP/")
AIMs_NTUe<-fread("AIMs_data_NTU_2020.csv",sep=",")
Dunk_aims=AIMs_NTUe[AIMs_NTUe$STATION_ID=="Dunk",]
Dunk_aims$date=as.POSIXct(Dunk_aims$SAMPLE_TIME, format="%d/%m/%Y %H:%M")


min10 <-  timeAverage(Dunk_aims, avg.time = "10 min", fill = FALSE)

MG3=merge(MG2, min10, by.x="DateTime", by.y="date",all.x=TRUE, all.y=FALSE)

datas <- rbindlist(list(MG3[, .(NTUe,Date)],
                        data.table(value = (MG3$NTU_QA),
                                   Date = MG3[, Date])))
datas[, Source := c(rep("JCU", nrow(MG3)), rep("AIMS", nrow(MG3)))] 

pp=ggplot(data = datas, aes(Date,(NTUe),colour = Source)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ Source, nrow=2)+
  labs(x = "Time", y = "NTU",
  title = "JCU NTU data and AIMS NTU data from Dunk Island June 2016 - March 2019")+scale_x_date(date_breaks = "3 month") 

pp=pp+scale_color_manual(values=wes_palette(n=2, name="GrandBudapest1"))+coord_cartesian(ylim = c(0, 150)) 


png(paste(image.dir,"Dunk Island NTU data JCU versus AIMS.png",sep=''),width=30, height=15, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
pp
dev.off()
  
#############################################################################################
##### Deployments 1 and 2 
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 

#chart.Correlation(MG2_cor, histogram=TRUE, pch=19)


#### models using mgvc gams
# compare single predictors for current

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1=MG2_d1[MG2_d1$currRMS3HrD95<0.025,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

#  with currRMS3HrD95 (51.1%)
m1 <- bam(log(NTUe) ~ s(log(currRMS3HrD95),k=-1),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.3HrD95 <- bam(log(NTUe) ~ s(log(currRMS3HrD95), k=-1), data=MG2_d1,rho=r1)
summary(m1.AR1.3HrD95)
m1.AR1.3HrD95_draw=draw(m1.AR1.3HrD95)


#  with currRMS3HrD90 (50.9%)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")]
MG2_d1=MG2_d1[MG2_d1$currRMS3HrD90<0.025,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS3HrD90), k=-1),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.3HrD90 <- bam(log(NTUe) ~ s(log(currRMS3HrD90), k=-1),data=MG2_d1,rho=r1)
summary(m1.AR1.3HrD90)
m1.AR1.3HrD90_draw=draw(m1.AR1.3HrD90)

#  with currRMS3Hr (49.4%)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")]
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.3Hr <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1,rho=r1)
summary(m1.AR1.3Hr)
m1.AR1.3Hr_draw=draw(m1.AR1.3Hr)

#  with currRMS10D95 (30.9%)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1=MG2_d1[MG2_d1$currRMS10D95<0.02,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS10D95)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.10D95 <- bam(log(NTUe) ~ s(log(currRMS10D95)),data=MG2_d1,rho=r1)
summary(m1.AR1.10D95)
m1.AR1.10D95_draw=draw(m1.AR1.10D95)

#  with currRMS10D90 (29.9%)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1=MG2_d1[MG2_d1$currRMS10D90<0.01,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS10D90)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.10D90 <- bam(log(NTUe) ~ s(log(currRMS10D90)),data=MG2_d1,rho=r1)
summary(m1.AR1.10D90)
m1.AR1.10D90_draw=draw(m1.AR1.10D90)

#  with currRMS10D90 (32.2%)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")] 
MG2_d1=MG2_d1[MG2_d1$currRMS10<0.005,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS10)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.10 <- bam(log(NTUe) ~ s(log(currRMS10)),data=MG2_d1,rho=r1)
summary(m1.AR1.10)
m1.AR1.10_draw=draw(m1.AR1.10)

png(paste(image.dir,"Dunk Island D90D95 deploys 1 and 2.png",sep=''),width=30, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(m1.AR1.10_draw,m1.AR1.10D90_draw,m1.AR1.10D95_draw,m1.AR1.3Hr_draw,m1.AR1.3HrD90_draw,m1.AR1.3HrD95_draw, align = "v", nrow = 2)
dev.off()


##############################################################################################################################
#### Look at model fits for 3 hour model
library(gratia)

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "NTUe","RMS", "Speed", "currRMS10","currRMS10D90", "currRMS10D95", "currRMS3Hr", "currRMS3HrD90","currRMS3HrD95", "Hs", "Tp","Tz","Height","Depth", "deploys", "Tully")]
MG2_d1=MG2_d1[MG2_d1$currRMS3HrD90<0.025,]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS3HrD95), k=-1),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=TRUE)$acf[2]
m1.AR1.3HrD90 <- bam(log(NTUe) ~ s(log(currRMS3HrD90), k=-1),data=MG2_d1,rho=r1)
summary(m1.AR1.3HrD95)
m1.AR1.3HrD95_draw=draw(m1.AR1.3HrD90)


P1<-appraise(m1.AR1.3HrD95)
P2<-draw(m1.AR1.3HrD95)

png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys D95 1 to 2 gam checks.png",sep=''),width=40, height=40, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P1
dev.off()


png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys D95 1 to 2 visualise fit.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P2
dev.off()


  
##############################################################################################################################
# RMS pressure model no ARI and all deployments
MG2$Date=as.Date(MG2$DateTime)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","SSC","NTUe","RMS","deploys", "Tully", "Hs")]
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","Tully")]
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

lower_bound <- quantile(MG2_d1$RMS, 0.005)
lower_bound

upper_bound <- quantile(MG2_d1$RMS, 0.995)
upper_bound

MG2_d1=MG2_d1[MG2_d1$RMS>lower_bound,]
MG2_d1=MG2_d1[MG2_d1$RMS<upper_bound,]

# deviance explained =37.8
m1.RMS.Hs <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,method = "fREML")


myres=residuals(m1.RMS.Hs, type = "response") # data minus fitted values

MG2_d1$Residuals=exp(myres) # back transform residuals

pal=wes_palette(n=5,name="Zissou1")

 p <- ggplot(data = MG2_d1, aes(Date,Residuals, colour="Residuals"))+
  geom_point(size = 0.8)+theme_bw()
 p <- p+geom_line(data=MG2_Tully,aes(x=Date,y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",
                x = "Date",
                colour = "RMS pressure")
  pressure.model <- p + theme(legend.position = c(0.1, 0.8))+coord_cartesian(ylim = c(0, 60)) 
 pressure.model= pressure.model+scale_color_manual(values=c(pal[5], pal[1]))+ scale_x_date(date_breaks = "2 month") 
 
 
 png(paste(image.dir,"Dunk Island NTUe RMS pressure model all deployments.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
pressure.model
dev.off()

#### fitted versus modelled

xpos=c(as.POSIXct("2016-06-01 12:00", format = "%Y-%m-%d %H:%M") ,as.POSIXct("2019-04-07 23:50", format = "%Y-%m-%d %H:%M"))
xpos=as.Date(xpos)

datas <- rbindlist(list(MG2_d1[, .(NTUe, Date)],
                        data.table(value = exp(m1.RMS.Hs$fitted.values),
                                   DateTime = MG2_d1[, Date])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMSpressure.Hs<-ggplot(data = datas, aes(Date,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "RMS pressure from Dunk Island all deployments")+ scale_x_date(date_breaks = "3 month", limits=c(xpos),date_labels = "%d/%m/%Y",expand=c(0,0)) 

##############################################################################################################################
# RMS current model no ARI and all deployments
MG2$Date=as.Date(MG2$DateTime)
MG2_d1=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","NTUe","Height","currRMS3Hr","deploys", "Tully", "Herbert")]
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","Tully")]
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

lower_bound <- quantile(MG2_d1$currRMS3Hr, 0.001)
lower_bound

upper_bound <- quantile(MG2_d1$currRMS3Hr, 0.995)
upper_bound

MG2_d1=MG2_d1[MG2_d1$currRMS3Hr>lower_bound,]
MG2_d1=MG2_d1[MG2_d1$currRMS3Hr<upper_bound,]

# deviance explained = 49.4
m1.RMScurr.all <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1,method = "fREML")

myres=residuals(m1.RMScurr.all, type = "response") # data minus fitted values

MG2_d1$Residuals=exp(myres) # back transform residuals

pal=wes_palette(n=5,name="Zissou1")

 p <- ggplot(data = MG2_d1, aes(Date,Residuals, colour="Residuals"))+
  geom_point(size = 0.8)+theme_bw() 
 p <- p+geom_line(data=MG2_Tully,aes(x=Date,y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",
                x = "Date",
                colour = "RMS current 3 hour")
  RMScurr.model <- p + theme(legend.position = c(0.1, 0.8))+coord_cartesian(ylim = c(0, 60)) 
 RMScurr.model= RMScurr.model+scale_color_manual(values=c(pal[5], pal[1]))+ scale_x_date(date_breaks = "2 month") 
 
 
 png(paste(image.dir,"Dunk Island NTUe RMS current 3 hr lag model all deployments.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
 RMScurr.model
dev.off()


#### fitted versus modelled

xpos=c(as.POSIXct("2016-06-01 12:00", format = "%Y-%m-%d %H:%M") ,as.POSIXct("2019-04-07 23:50", format = "%Y-%m-%d %H:%M"))
xpos=as.Date(xpos)
datas <- rbindlist(list(MG2_d1[, .(NTUe, Date)],
                        data.table(value = exp(m1.RMScurr.all$fitted.values),
                                   DateTime = MG2_d1[, Date])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
RMScurr3hr<-ggplot(data = datas, aes(Date,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",title = "RMS (current) with 3 hr lag from Dunk Island all deployments")+
  scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0), limits=c(xpos))


##############################################################################################################################

png(paste(image.dir,"Dunk Island RMS current and RMS pressure model all deployments no AR1.png",sep=''),width=40, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(pressure.model,RMScurr.model, align = "v", nrow = 2)
dev.off()
  
png(paste(image.dir,"Dunk Island RMS current and RMS pressure model all deployments fitted versus modelled.png",sep=''),width=40, height=30, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(RMSpressure.Hs,RMScurr3hr, align = "v", nrow = 2)
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
MG2$Date=as.Date(MG2$DateTime)

lower_bound <- quantile(MG2$currRMS3Hr, 0.001, na.rm=TRUE)
lower_bound
upper_bound <- quantile(MG2$currRMS3Hr, 0.995,na.rm=TRUE)
upper_bound

MG2=MG2[MG2$currRMS3Hr>lower_bound,]
MG2=MG2[MG2$currRMS3Hr<upper_bound,]

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","Date", "NTUe","currRMS3Hr","Height", "deploys", "Tully")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

m1 <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

# 49.1%
m1AR1 <- bam(log(NTUe) ~ s(log(currRMS3Hr)),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1)

MG2_d1$myres=exp(residuals(m1AR1, type = "response")) # data minus fitted values

###################################################################################################################
### predict BAM to new data
MG2$Date=as.Date(MG2$DateTime)
MG2_test=MG2[which(MG2$deploys %in% c(3,4,5,6,7,8)),c("DateTime", "Date","NTUe","currRMS3Hr","deploys", "Tully", "Herbert")] 
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Tully")] 
MG2_Herbert=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Herbert")] 
MG2_test=MG2_test[complete.cases(MG2_test),]
MG2_test=MG2_test[MG2_test$NTUe>0,]

MG2_test$pred <- predict.bam(m1AR1, newdata=MG2_test, type="response")


datas <- rbindlist(list(MG2_test[, .(NTUe, DateTime)],
                        data.table(value = exp(MG2_test$pred),
                                   DateTime = MG2_test[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_test)), rep("Predicted", nrow(MG2_test)))] 
pred.model=ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Model predicted NTUe to deploy 3 to 8")
  

  
predres=exp((log(MG2_test$NTUe))-(MG2_test$pred))

MG2_test$Residuals=predres # back transform residuals


 p <- ggplot(data = MG2_d1, aes(Date,myres, colour="myres"))+geom_point(size = 0.8)
  p<-p+geom_point(data=MG2_test,aes(Date,Residuals, colour="Residuals"), size = 0.8)
 p <- p+geom_line(data=MG2_Tully, aes(x=Date, y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",x = "Date", colour="")+theme_bw()
  p <- p + theme(legend.position = c(0.1,0.8),legend.title = element_blank())+scale_color_manual(values=c(pal[5], pal[4], pal[1], pal[3]),
  labels=c("Model residuals", "Predicted residuals", "Tully discharge"))+ scale_x_date(date_breaks = "3 month")+ylim(0, 50)

  
png(paste(image.dir,"Dunk Island RMS current 3 Hours residuals versus Tully flow model and predicted using deployments 1 and 2 as model.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
p
dev.off()

###################################################################################################################
MG2=MGFINAL
MG2$Date=as.Date(MG2$DateTime)

lower_bound <- quantile(MG2$RMS, 0.01, na.rm=TRUE)
lower_bound
upper_bound <- quantile(MG2$RMS, 0.995, na.rm=TRUE)
upper_bound

MG2=MG2[MG2$RMS>lower_bound,]
MG2=MG2[MG2$RMS<upper_bound,]

MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","Date", "NTUe","RMS","Height", "deploys", "Tully")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]

# RMS
m1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)


m1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

m1AR1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1) # 15.4

MG2_d1$myres=exp(residuals(m1AR1, type = "response")) # data minus fitted values

###################################################################################################################
### predict BAM to new data
MG2$Date=as.Date(MG2$DateTime)
MG2_test=MG2[which(MG2$deploys %in% c(3,4,5,6,7,8)),c("DateTime", "Date","NTUe","RMS","deploys", "Tully", "Herbert")] 
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","NTUe", "deploys", "Tully")] 
MG2_Herbert=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Herbert")] 
MG2_test=MG2_test[complete.cases(MG2_test),]
MG2_test=MG2_test[MG2_test$NTUe>0,]

MG2_test$pred <- predict.bam(m1AR1, newdata=MG2_test, type="response")

datas <- rbindlist(list(MG2_test[, .(NTUe, DateTime)],
                        data.table(value = exp(MG2_test$pred),
                                   DateTime = MG2_test[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_test)), rep("Predicted", nrow(MG2_test)))] 
pred.model=ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Model predicted NTUe to deploy 3 to 8")
  

  
predres=exp((log(MG2_test$NTUe))-(MG2_test$pred))

MG2_test$Residuals=predres # back transform residuals


 p <- ggplot(data = MG2_d1, aes(Date,myres, colour="myres"))+geom_point(size = 0.8)
  p<-p+geom_point(data=MG2_test,aes(Date,Residuals, colour="Residuals"), size = 0.8)
 p <- p+geom_line(data=MG2_Tully, aes(x=Date, y=Tully/1000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",x = "Date", colour="")+theme_bw()
  p <- p + theme(legend.position = c(0.1,0.8),legend.title = element_blank())+scale_color_manual(values=c(pal[5], pal[4], pal[1], pal[3]),
  labels=c("Model residuals", "Predicted residuals", "Tully discharge"))+ scale_x_date(date_breaks = "3 month")+
  ylim(0, 150)

  
png(paste(image.dir,"Dunk Island RMS pressure residuals versus Tully flow model and predicted using deployments 1 and 2 to model.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
p
dev.off()

###################################################################################################################
# prediction model using deployments 5 and 6

MG2=MGFINAL
MG2$Date=as.Date(MG2$DateTime)

lower_bound <- quantile(MG2$currRMS3Hr, 0.01, na.rm=TRUE)
lower_bound
upper_bound <- quantile(MG2$currRMS3Hr, 0.995, na.rm=TRUE)
upper_bound

MG2=MG2[MG2$currRMS3Hr>lower_bound,]
MG2=MG2[MG2$currRMS3Hr<upper_bound,]

MG2_d1=MG2[which(MG2$deploys %in% c(5,6)),c("DateTime","Date", "NTUe","currRMS3Hr","Height", "deploys", "Tully")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


m1 <- bam(log(NTUe) ~ s(log(currRMS3Hr), k=-1),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

# 49.4%
m1AR1 <- bam(log(NTUe) ~ s(log(currRMS3Hr),  k=-1),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1)

MG2_d1$myres=exp(residuals(m1AR1, type = "response")) # data minus fitted values

P1<-appraise(m1AR1)
P2<-draw(m1AR1)

png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys 5 to 6 gam checks.png",sep=''),width=40, height=40, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P1
dev.off()


png(paste(image.dir,"Dunk Island RMS current 3 Hours deploys 5 to 6 visualise fit.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
P2
dev.off()

  
###################################################################################################################
### predict BAM to new data
MG2$Date=as.Date(MG2$DateTime)
MG2_test=MG2[which(MG2$deploys %in% c(1,2,3,4,7,8)),c("DateTime", "Date","NTUe","currRMS3Hr","deploys", "Tully", "Herbert")] 
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Tully")] 
MG2_Herbert=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Herbert")] 
MG2_test=MG2_test[complete.cases(MG2_test),]
MG2_test=MG2_test[MG2_test$NTUe>0,]

MG2_test$pred <- predict.bam(m1AR1, newdata=MG2_test, type="response")


datas <- rbindlist(list(MG2_test[, .(NTUe, DateTime)],
                        data.table(value = exp(MG2_test$pred),
                                   DateTime = MG2_test[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_test)), rep("Predicted", nrow(MG2_test)))] 
pred.model=ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Model predicted NTUe to deploy 1,2,3,4,7 and 8")
  

predres=exp((log(MG2_test$NTUe))-(MG2_test$pred))

MG2_test$Residuals=predres # back transform residuals


 p <- ggplot(data = MG2_d1, aes(Date,myres, colour="myres"))+geom_point(size = 0.8)
 p <- p+geom_point(data=MG2_test,aes(Date,Residuals, colour="Residuals"), size = 0.8)
 p <- p+geom_line(data=MG2_Tully, aes(x=Date, y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",x = "Date", colour="")+theme_bw()
 p <- p + theme(legend.position = c(0.1,0.8),legend.title = element_blank())+scale_color_manual(values=c(pal[5], pal[4], pal[1], pal[3]),
  labels=c("Model residuals", "Predicted residuals", "Tully discharge"))+ scale_x_date(date_breaks = "3 month")+ylim(0, 50)

  
png(paste(image.dir,"Dunk Island RMS current 3 Hours residuals versus Tully flow model and predicted using deploys 5 and 6 as model.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
p
dev.off()

###################################################################################################################
# prediction model with RMS pressure using deployments 5 and 6

MG2=MGFINAL
MG2$Date=as.Date(MG2$DateTime)

lower_bound <- quantile(MG2$RMS, 0.01, na.rm=TRUE)
lower_bound
upper_bound <- quantile(MG2$RMS, 0.995, na.rm=TRUE)
upper_bound

MG2=MG2[MG2$RMS>lower_bound,]
MG2=MG2[MG2$RMS<upper_bound,]
MG2$Date=as.Date(MG2$DateTime)
MG2_d1=MG2[which(MG2$deploys %in% c(5,6)),c("DateTime","Date", "NTUe","RMS","Height", "deploys", "Tully")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1=MG2_d1[MG2_d1$NTUe>0,]


# RMS
m1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1.AR1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,rho=r1)
summary(m1.AR1)
appraise(m1.AR1)
draw(m1.AR1)


m1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]

m1AR1 <- bam(log(NTUe) ~ s(log(RMS)),data=MG2_d1,rho=r1,method = "fREML", ARI_start=NULL)
summary(m1AR1)

MG2_d1$myres=exp(residuals(m1AR1, type = "response")) # data minus fitted values


  
###################################################################################################################
### predict BAM to new data
MG2$Date=as.Date(MG2$DateTime)
MG2_test=MG2[which(MG2$deploys %in% c(1,2,3,4,7,8)),c("DateTime", "Date","NTUe","RMS","deploys", "Tully", "Herbert")] 
MG2_Tully=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date","NTUe", "deploys", "Tully")] 
MG2_Herbert=MG2[which(MG2$deploys %in% c(1,2,3,4,5,6,7,8)),c("DateTime","Date", "deploys", "Herbert")] 
MG2_test=MG2_test[complete.cases(MG2_test),]
MG2_test=MG2_test[MG2_test$NTUe>0,]
MG2_test=MG2_test[MG2_test$RMS>0.000000001,]# remove very small values=outliers


MG2_test$pred <- predict.bam(m1AR1, newdata=MG2_test, type="response")


datas <- rbindlist(list(MG2_test[, .(NTUe, DateTime)],
                        data.table(value = exp(MG2_test$pred),
                                   DateTime = MG2_test[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_test)), rep("Predicted", nrow(MG2_test)))] 
pred.model=ggplot(data = datas, aes(DateTime,(NTUe),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "NTUe",
  title = "Model predicted NTUe to deploy 1,2,3,4,7 and 8")
  

  
predres=exp((log(MG2_test$NTUe))-(MG2_test$pred))

MG2_test$Residuals=predres # back transform residuals


 p <- ggplot(data = MG2_d1, aes(Date,myres, colour="myres"))+geom_point(size = 0.8)
  p<-p+geom_point(data=MG2_test,aes(Date,Residuals, colour="Residuals"), size = 0.8)
 p <- p+geom_line(data=MG2_Tully, aes(x=Date, y=Tully/1000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",x = "Date", colour="")+theme_bw()
  p <- p + theme(legend.position = c(0.1,0.8),legend.title = element_blank())+scale_color_manual(values=c(pal[5], pal[4], pal[1], pal[3]),
  labels=c("Model residuals", "Predicted residuals", "Tully discharge"))+ scale_x_date(date_breaks = "3 month")+
  ylim(0, 50)

  
png(paste(image.dir,"Dunk Island RMS pressure residuals versus Tully flow model and predicted using deployments 5 and 6 as model.png",sep=''),width=30, height=10, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
p
dev.off()