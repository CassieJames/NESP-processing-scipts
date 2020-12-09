#################################################################################
#### Scripts to explore current data

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(data.table)
library(openair)
library(readxl)
library(dplyr)
library(tidyr)



image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"


setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")
setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

####Dunk Island

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

tdata2=tdata[complete.cases(tdata$'speed (m/s)'), ]

colnames(tdata2)[9] <- "Speed"

tdata2=tdata2[with(tdata2, order(deploys, tdata2$Speed)),]
tdata2$timestamp=as.factor(tdata2$timestamp)

tdata3<- tdata2 %>%
  mutate(Speed) %>%
  group_by(deploys,Speed) %>%
  # 1. Remove grouping
  ungroup() %>%
  mutate(order = row_number())

tdata3$deploys=as.factor(tdata3$deploys)

tdata3_summary<- tdata2 %>%
  select(deploys,Speed) %>%
  group_by(tdata2$deploys) %>%
  summarise(mean_deployment=mean(Speed),quant95=quantile(Speed,probs=0.95))

tdata4=tdata3[tdata3$Speed<=0.025,]

facet_mean_labels=paste("Mean =", round(tdata3_summary$mean_deployment,3), sep="")
facet_95th_labels=paste("95th percentile =", round(tdata3_summary$quant95,3), sep="")
facet_info=paste(deploys$Date_in[1:8], deploys$Serial_no[1:8],sep=" ")
final_labels=c(paste(facet_info,facet_mean_labels,facet_95th_labels, sep=" "))

variable_labeller <- function(variable,value){
  return(final_labels[value])
}


library(ggpubr)

p1<-ggplot(tdata4, aes(x=order,y=Speed))+
  facet_wrap(~deploys,ncol=2,scales="free", labeller=variable_labeller)+geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_cor(label.y = 0.06) +
  stat_regline_equation(label.y = 0.05)+
  ylab("Speed (m/s)")+ylim(0,0.07)


png(paste(image.dir,"Dunk Island deployment vs current speed_0.025ms.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=16, bg='white')	
p1
dev.off()

####

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/Tide data/"; setwd(data.dir)
Tide_data<-fread("Tidal_amplitude_2016_2020.csv",sep=",")
Tide_data[,'Date'] <- as.POSIXct(Tide_data$"Date", format = "%d/%m/%Y")


tdata3$Date <-as.Date(tdata3$timestamp)
Tide_data$Date <-as.Date(Tide_data$Date)+1

MG<-merge(tdata3,Tide_data, by="Date", all.x=T)

MG$Year = lubridate::year(MG$Date)
MG$Month = lubridate::month(MG$Date)
MG<-MG[!is.na(MG$Amplitude),]
MG=MG[MG$Speed<=0.025,]
library(viridis)

p1<-ggplot(MG, aes(x=order,y=Speed))+
  facet_wrap(~deploys,ncol=2,scales="free", labeller=variable_labeller)+geom_point(aes(color=Amplitude))+
  scale_color_viridis(option = "D")+
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_cor(label.y = 0.04) +
  stat_regline_equation(label.y = 0.03)+
  ylab("Speed (m/s)")+ylim(0,0.05)


########################################################################################
####Orpheus

mydat.x1<- read_xlsx("Orpheus Island timeseries.xlsx", sheet=1, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x2<- read_xlsx("Orpheus Island timeseries.xlsx", sheet=2,col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x3<- read_xlsx("Orpheus Island timeseries.xlsx", sheet=3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x4<- read_xlsx("Orpheus Island timeseries.xlsx", sheet=4, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))


mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)

tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4)

#plot(tdata$timestamp,tdata$'speed (m/s)')

deployments<- read_xlsx("Deployment_dates.xlsx", sheet=1)
deploys=as.data.frame(deployments)
deploys=deploys[which(deploys$Site=="Orpheus"),]
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
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[11], deploys$Date_in[12])]<-11

tdata2=tdata[complete.cases(tdata$'speed (m/s)'), ]

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

tdata4=tdata3[tdata3$Speed<=0.05,]

facet_mean_labels=paste("Mean =", round(tdata3_summary$mean_deployment,3), sep="")
facet_95th_labels=paste("95th percentile =", round(tdata3_summary$quant95,3), sep="")
facet_info=paste(deploys$Date_in[1:11], deploys$Serial_no[1:11],sep=" ")
final_labels=c(paste(facet_info,facet_mean_labels,facet_95th_labels, sep=" "))

variable_labeller <- function(variable,value){
  return(final_labels[value])
}

library(ggpubr)

p1<-ggplot(tdata4, aes(x=order,y=Speed))+
  facet_wrap(~deploys,ncol=2,scales="free", labeller=variable_labeller)+geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_cor(label.y = 0.06) +
  stat_regline_equation(label.y = 0.05)+
  ylab("Speed (m/s)")+ylim(0,0.07)


png(paste(image.dir,"Orpheus Island deployment vs current speed_0.05ms.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=16, bg='white')	
p1
dev.off()

########################################################################################
#### Havannah


mydat.x1<- read_xlsx("Havannah Island timeseries.xlsx", sheet=1, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x2<- read_xlsx("Havannah Island timeseries.xlsx", sheet=2,col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x3<- read_xlsx("Havannah Island timeseries.xlsx", sheet=3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x4<- read_xlsx("Havannah Island timeseries.xlsx", sheet=4, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x5<- read_xlsx("Havannah Island timeseries.xlsx", sheet=5, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))

mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)
mydat.x5=as.data.frame(mydat.x5)


tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4, mydat.x5)

deployments<- read_xlsx("Deployment_dates.xlsx", sheet=1)
deploys=as.data.frame(deployments)
deploys=deploys[which(deploys$Site=="Havannah"),]
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
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[11], deploys$Date_in[12])]<-11
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[12], deploys$Date_in[13])]<-12
tdata$deploys[tdata$timestamp %between% c(deploys$Date_in[13], deploys$Date_in[14])]<-13


tdata2=tdata[complete.cases(tdata$'speed (m/s)'), ]

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

tdata4=tdata3[tdata3$Speed<=0.05,]

facet_mean_labels=paste("Mean =", round(tdata3_summary$mean_deployment,3), sep="")
facet_95th_labels=paste("95th percentile =", round(tdata3_summary$quant95,3), sep="")
facet_info=paste(deploys$Date_in[1:13], deploys$Serial_no[1:13],sep=" ")
final_labels=c(paste(facet_info,facet_mean_labels,facet_95th_labels, sep=" "))

variable_labeller <- function(variable,value){
  return(final_labels[value])
}

library(ggpubr)

p1<-ggplot(tdata4, aes(x=order,y=Speed))+
  facet_wrap(~deploys,ncol=2,scales="free", labeller=variable_labeller)+geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_cor(label.y = 0.08) +
  stat_regline_equation(label.y = 0.065)+
  ylab("Speed (m/s)")+ylim(0,0.09)


png(paste(image.dir,"Havannah Island deployment vs current speed_0.05ms.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=16, bg='white')	
p1
dev.off()



########################################################################################
#### Cleveland

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

coltypes=c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric")
mydat.x1<- read_xlsx("Cleveland Bay timeseries.xlsx", sheet=1, col_types = coltypes)
mydat.x2<- read_xlsx("Cleveland Bay timeseries.xlsx", sheet=2,col_types = coltypes)
mydat.x3<- read_xlsx("Cleveland Bay timeseries.xlsx", sheet=3,col_types = coltypes)
mydat.x4<- read_xlsx("Cleveland Bay timeseries.xlsx", sheet=4,col_types = coltypes)
mydat.x5<- read_xlsx("Cleveland Bay timeseries.xlsx", sheet=5,col_types = coltypes)

mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)
mydat.x5=as.data.frame(mydat.x5)

tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4, mydat.x5)

deployments<- read_xlsx("Deployment_dates.xlsx", sheet=1)
deploys=as.data.frame(deployments)
deploys=deploys[which(deploys$Site=="Cleveland"),]
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


tdata2=tdata[complete.cases(tdata$'speed (m/s)'), ]

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

tdata4=tdata3[tdata3$Speed<=0.05,]

facet_mean_labels=paste("Mean =", round(tdata3_summary$mean_deployment,3), sep="")
facet_95th_labels=paste("95th percentile =", round(tdata3_summary$quant95,3), sep="")
facet_info=paste(deploys$Date_in[1:11], deploys$Serial_no[1:11],sep=" ")
final_labels=c(paste(facet_info,facet_mean_labels,facet_95th_labels, sep=" "))

variable_labeller <- function(variable,value){
  return(final_labels[value])
}

library(ggpubr)



p1<-ggplot(tdata3, aes(x=order,y=Speed))+
  facet_wrap(~deploys,ncol=2,scales="free", labeller=variable_labeller)+geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_cor(label.y = ((max(tdata3$Speed)/100)*85)) +
  stat_regline_equation(label.y = ((max(tdata3$Speed)/100)*70))+
  ylab("Speed (m/s)")+ylim(0,1.25)


png(paste(image.dir,"Cleveland Bay deployment vs current speed.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=16, bg='white')	
p1
dev.off()

##### Comparison with wave rider buoy data @ cleveland bay

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
Cleve.2<-setkey(tdata5, timestamp)

MG <- TV.2[ Cleve.2, roll = "nearest",allow.cartesian=TRUE]



p2<-ggplot(MG, aes(x=as.numeric(RMS),y=as.numeric(Hs)))+
  facet_wrap(~deploys,ncol=2,scales="free")+geom_point()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Significant wave height (Hs)")#+ylim(0,0.4)


png(paste(image.dir,"Cleveland Bay deployment vs current speed.png",sep=''),width=20, height=20, units='cm', res=500, pointsize=16, bg='white')	
p1
dev.off()

#####
data.dir="C:/Users/jc246980/Documents/Current projects/NESP/NESP work with wave rider data etc/Townsville tidal data/"; setwd(data.dir)
TV_tide_ht<-fread("Townsville_tide_Ht_10Mins.csv",sep=",")
TV_tide_ht$DateTime<-as.POSIXct(TV_tide_ht$DateTime, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

TV_tide_ht<-setkey(TV_tide_ht, DateTime)
MG<-setkey(MG, DateTime)

MG2 <-TV_tide_ht[ MG, roll = "nearest",allow.cartesian=TRUE]

p2<-ggplot(MG2, aes(x=as.numeric(Height),y=as.numeric(SSC)))+
  facet_wrap(~deploys,ncol=2,scales="free")+geom_point()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Significant wave height (Hs)")#+ylim(0,0.4)