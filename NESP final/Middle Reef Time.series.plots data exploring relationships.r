#################################################################################
#### Steve Lewis Time Series plots

library(ggplot2)
library(binhf)
library(RColorBrewer)
library(cowplot)
library(data.table)
library(openair)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)



image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"

####Middle Reef
setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

mydat.x1<- read_xlsx("Middle Reef timeseries.xlsx", sheet=1, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x2<- read_xlsx("Middle Reef timeseries.xlsx", sheet=2,col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x3<- read_xlsx("Middle Reef timeseries.xlsx", sheet=3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x4<- read_xlsx("Middle Reef timeseries.xlsx", sheet=4, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x5<- read_xlsx("Middle Reef timeseries.xlsx", sheet=5, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))

mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)
mydat.x5=as.data.frame(mydat.x5)


tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4, mydat.x5)


flowdata=data.frame(read.csv("All discharge data.csv"))
flowdata$Date <- as.Date(flowdata$Date, format="%d/%m/%Y") 

deployments<- read_xlsx("Deployment_dates.xlsx", sheet=1)
deploys=as.data.frame(deployments)
deploys=deploys[which(deploys$Site=="Middle"),]
tdata$deploys<-NA
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[1], deploys$Date_in[2])]<-1
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[2], deploys$Date_in[3])]<-2
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[3], deploys$Date_in[4])]<-3
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[4], deploys$Date_in[5])]<-4
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[5], deploys$Date_in[6])]<-5
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[6], deploys$Date_in[7])]<-6
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[7], deploys$Date_in[8])]<-7
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[8], deploys$Date_in[9])]<-8
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[9], deploys$Date_in[10])]<-9
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[10], deploys$Date_in[11])]<-10

tdata2=tdata

colnames(tdata2)[9] <- "Speed"

tdata2=tdata2[with(tdata2, order(deploys, tdata2$Speed)),]
tdata2$timestamp=as.factor(tdata2$timestamp)

tdata3<- tdata2 %>%
  mutate(Speed) %>%
  group_by(deploys,Speed) %>%
  # 1. Remove grouping
  ungroup() %>%
  mutate(order = row_number())

tdata2$deploys=as.factor(tdata2$deploys)

tdata3_summary<- tdata2 %>%
  select(deploys,Speed) %>%
  group_by(tdata2$deploys) %>%
  summarise(mean_deployment=mean(Speed),quant95=quantile(Speed,probs=0.95))

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

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Middle/"; setwd(data.dir)
RMScurrent<-fread("Middle_RMS_ALL.csv",sep=",")
RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)

##### plots


ypos = min(MG2$RMS,na.rm = TRUE) + 0.95*diff(range(MG2$RMS,na.rm = TRUE))

p.RMS <- ggplot(MG2, aes(DateTime, RMS)) + geom_point(stat="identity",colour="#4DAF4A", size=0.3) + theme_minimal() + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab("RMS (pressure)")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6) 

ypos = min(MG2$SSC,na.rm = TRUE) + 0.95*diff(range(MG2$SSC,na.rm = TRUE))
ypos=150		
p.SSC <- ggplot(MG2,aes(DateTime, SSC)) + geom_point(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=125, label= "a.", size=6)

ypos = min(MG2$NTUe,na.rm = TRUE) + 0.95*diff(range(MG2$NTUe,na.rm = TRUE))

p.NTUe <- ggplot(MG2,aes(DateTime, NTUe)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('NTUe'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 


ypos = min(as.numeric(MG2$Hs),na.rm = TRUE) + 0.95*diff(range(as.numeric(MG2$Hs),na.rm = TRUE))		
p.Hs <- ggplot(MG2,aes(DateTime, as.numeric(Hs))) + geom_point(stat="identity", colour="steelblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Sig wave Ht (m)'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 
		
ypos = min(as.numeric(MG2$Hmax),na.rm = TRUE) + 0.95*diff(range(as.numeric(MG2$Hmax),na.rm = TRUE))		
p.Hmax <- ggplot(MG2,aes(DateTime, as.numeric(Hmax))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Max wave Ht (m)'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 
		
ypos = min(MG2$Speed,na.rm = TRUE) + 0.95*diff(range(MG2$Speed,na.rm = TRUE))		

p.Speed <- ggplot(MG2,aes(DateTime, as.numeric(Speed))) + geom_point(stat="identity", colour="darkslategray3", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Current (m/s)'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=0.45, label= "c.", size=6)+ylim(0,0.3)
		
ypos = min(MG2$Height,na.rm = TRUE) + 0.95*diff(range(MG2$Height,na.rm = TRUE))		

p.Height <- ggplot(MG2,aes(DateTime, as.numeric(Height))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Tide height (m)'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "f.", size=6)#+ylim(0,0.8)
		

ypos = min(MG2$currentRMS,na.rm = TRUE) + 0.95*diff(range(MG2$currentRMS,na.rm = TRUE))		
		
p.RMScurrent <-ggplot(MG2,aes(DateTime, as.numeric(currentRMS))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+
		scale_y_continuous(position = "right",limits=c(0,0.01))+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('RMS (current)'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=0.008, label= "d.", size=6)
	
ypos = min(flowdata$Burdekin,na.rm = TRUE) + 0.95*diff(range(flowdata$Burdekin,na.rm = TRUE))
xpos=c(min(tdata$timestamp), max(tdata$timestamp))
xpos=as.Date(xpos, format="%m/%d/%Y")

p.flow <- ggplot(flowdata, aes(Date, Burdekin)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
		theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+scale_y_continuous(position = "left")+
		scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0),limits = as.Date(c('2016-06-01','2020-03-23')))+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "g.", size=6)

		
	plot_grid(p.SSC,p.RMS, p.Speed, p.RMScurrent,p.Hs, p.Height,p.flow, align = "v", nrow = 7)	
	
	
	
png(paste(image.dir,"Middle Reef July 2020.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #		 
		
plot_grid(p.SSC,p.RMS, p.Speed, p.RMScurrent,p.Hs, p.Height,p.flow, align = "v", nrow = 7)	
dev.off()


##### plots for shorter periods
library(scales)
xpos=c(as.POSIXct("2016-10-01 12:00", format = "%Y-%m-%d %H:%M") ,as.POSIXct("2017-07-01 23:50", format = "%Y-%m-%d %H:%M"))

ypos = min(MG2$RMS,na.rm = TRUE) + 0.95*diff(range(MG2$RMS,na.rm = TRUE))


p.RMS <- ggplot(MG2, aes(DateTime, RMS)) + geom_point(stat="identity",colour="#4DAF4A", size=0.3) + theme_minimal() + theme_classic(base_size = 12)+scale_y_continuous(position = "right",limits=c(0,0.1))+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab("RMS (pressure)")+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M") , y=0.08, label= "b.", size=6)

ypos = min(MG2$SSC,na.rm = TRUE) + 0.95*diff(range(MG2$SSC,na.rm = TRUE))	

p.SSC <- ggplot(MG2,aes(DateTime, SSC)) + geom_point(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M"), y=40, label= "a.", size=6)+ylim(0,75)

ypos = min(MG2$NTUe,na.rm = TRUE) + 0.95*diff(range(MG2$NTUe,na.rm = TRUE))

p.NTUe <- ggplot(MG2,aes(DateTime, NTUe)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('NTUe'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 


ypos = min(as.numeric(MG2$Hs),na.rm = TRUE) + 0.95*diff(range(as.numeric(MG2$Hs),na.rm = TRUE))		
p.Hs <- ggplot(MG2,aes(DateTime, as.numeric(Hs))) + geom_point(stat="identity", colour="steelblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Sig wave Ht (m)'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-06 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 
		
ypos = min(as.numeric(MG2$Hmax),na.rm = TRUE) + 0.95*diff(range(as.numeric(MG2$Hmax),na.rm = TRUE))		
p.Hmax <- ggplot(MG2,aes(DateTime, as.numeric(Hmax))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Max wave Ht (m)'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2017-06-06 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 
		
ypos = min(MG2$Speed,na.rm = TRUE) + 0.95*diff(range(MG2$Speed,na.rm = TRUE))		

p.Speed <- ggplot(MG2,aes(DateTime, as.numeric(Speed))) + geom_point(stat="identity", colour="darkslategray3", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Current (m/s)'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M"), y=0.45, label= "c.", size=6)+ylim(0,0.3)
		
ypos = min(MG2$Height,na.rm = TRUE) + 0.95*diff(range(MG2$Height,na.rm = TRUE))		

p.Height <- ggplot(MG2,aes(DateTime, as.numeric(Height))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Tide height (m)'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "f.", size=6)#+ylim(0,0.8)
		
		
ypos = min(MG2$currentRMS,na.rm = TRUE) + 0.95*diff(range(MG2$currentRMS,na.rm = TRUE))		
		
p.RMScurrent <-ggplot(MG2,aes(DateTime, as.numeric(currentRMS))) + geom_point(stat="identity", colour="darkslateblue", size=0.3) + theme_classic(base_size = 12)+
		scale_y_continuous(position = "right",limits=c(0,0.0010))+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('RMS (current)'))+
		scale_x_datetime(expand=c(0,0),limits =xpos)+annotate(geom="text", x=as.POSIXct("2016-06-05 12:00", format = "%Y-%m-%d %H:%M"), y=0.002, label= "d.", size=6)
	
ypos = min(flowdata$Burdekin,na.rm = TRUE) + 0.95*diff(range(flowdata$Burdekin,na.rm = TRUE))

xpos=as.Date(xpos, format="%m/%d/%Y")

p.flow <- ggplot(flowdata, aes(Date, Burdekin)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "left",limits=c(0,800000))+geom_area(position = "identity",fill="#377EB8")+
		theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		scale_x_date(date_breaks=("2 week"),date_labels = "%d/%m/%Y",expand=c(0,0),limits = xpos)+annotate(geom="text", x=as.Date("2016-06-05"), y=40000, label= "g.", size=6)

	
plot_grid(p.SSC,p.RMS, p.Speed, p.RMScurrent,p.Hs, p.Height,p.flow, align = "v", nrow = 7)	
	
png(paste(image.dir,"Middle 2016 Oct-July 2017.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #		 
		
plot_grid(p.SSC,p.RMS, p.Speed, p.RMScurrent,p.Hs, p.Height,p.flow, align = "v", nrow = 7)	
dev.off()
