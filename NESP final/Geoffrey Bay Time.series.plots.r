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


image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"

####Geoffrey Bay
setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

mydat.x1<- read_xlsx("Geoffrey Bay timeseries.xlsx", sheet=1, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x2<- read_xlsx("Geoffrey Bay timeseries.xlsx", sheet=2,col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x3<- read_xlsx("Geoffrey Bay timeseries.xlsx", sheet=3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x4<- read_xlsx("Geoffrey Bay timeseries.xlsx", sheet=4, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))
mydat.x5<- read_xlsx("Geoffrey Bay timeseries.xlsx", sheet=5, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric"))

mydat.x1=as.data.frame(mydat.x1)
mydat.x2=as.data.frame(mydat.x2)
mydat.x3=as.data.frame(mydat.x3)
mydat.x4=as.data.frame(mydat.x4)
mydat.x5=as.data.frame(mydat.x5)


tdata=rbind(mydat.x1, mydat.x2, mydat.x3, mydat.x4, mydat.x5)

flowdata=data.frame(read.csv("All discharge data.csv"))

trapdata<- read_xlsx("Logger metadata_for import.xlsx", sheet="Geoffrey Bay")
trapdata=as.data.frame(trapdata)
trapdata=trapdata[,c(2,3,6)]
data_long=trapdata
data_long$Date.in= as.Date(data_long$Date.in, format="%d/%m/%Y") 
data_long$Date.out= as.Date(data_long$Date.out, format="%d/%m/%Y") 

tdata$timestamp<-as.POSIXct(tdata$timestamp, format = "%d/%m/%Y %H:%M") 
flowdata$Date <- as.Date(flowdata$Date, format="%d/%m/%Y") 

missinglightdata=tdata[,c(1,4)]
missinglightdata$noData = is.na(missinglightdata$light)
missinglightdata=missinglightdata[which(missinglightdata$noData ==TRUE),]


cols=brewer.pal(n = 9, name = "Set1")

ypos = min(tdata$RMS,na.rm = TRUE) + 0.95*diff(range(tdata$RMS,na.rm = TRUE))

p1.RMS <- ggplot(tdata, aes(timestamp, RMS)) + geom_line(stat="identity",colour="#4DAF4A", size=0.3) + theme_minimal() + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab("RMS (pressure)")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "c.", size=6) 

ypos = min(tdata$SSC,na.rm = TRUE) + 0.95*diff(range(tdata$SSC,na.rm = TRUE))
		
p2.SSC <- ggplot(tdata,aes(timestamp, SSC)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 

ypos = min(tdata$NTUe,na.rm = TRUE) + 0.95*diff(range(tdata$NTUe,na.rm = TRUE))

p2.NTUe <- ggplot(tdata,aes(timestamp, NTUe)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('NTUe'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 

ypos = min(tdata$light,na.rm = TRUE) + 0.95*diff(range(tdata$light,na.rm = TRUE))

p5.light <- ggplot(tdata,aes(timestamp, light), na.rm=FALSE) + geom_rect(data=tdata, aes(xmin = min(timestamp), xmax = max(timestamp), ymin=0, ymax=+Inf), fill='grey')+ 
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(limits = c(0, 1500),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_vline(data = missinglightdata, aes( xintercept = timestamp,linetype="dotted", colour="pink"), alpha=0.4, size=1)+
		theme(legend.position = "none")+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)

ypos = min(data_long$Trap,na.rm = TRUE) + 0.95*diff(range(data_long$Trap,na.rm = TRUE))
		
p3.trap <- ggplot(data_long, aes(colour = "chocolate4", x = data_long$Date.in, xend = data_long$Date.out, y = data_long$Trap, yend = data_long$Trap)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.95))+ 
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4","chocolate2"))+scale_y_continuous(limits = c(0, 50),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "a.", size=6) + theme(legend.position = "none")
	
ypos = min(flowdata$Burdekin,na.rm = TRUE) + 0.95*diff(range(flowdata$Burdekin,na.rm = TRUE))
xpos=c(min(tdata$timestamp), max(tdata$timestamp))
xpos=as.Date(xpos, format="%m/%d/%Y")

p4.flow <- ggplot(flowdata, aes(Date, Burdekin)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
		theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0),limits=xpos)+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "e.", size=6)

#ypos = min(tdata$u.current,na.rm = TRUE) + 0.95*diff(range(tdata$u.current,na.rm = TRUE))

ypos=0.475
cols=brewer.pal(n = 8, name = "Set2")
		
		 
png(paste(image.dir,"Geoffrey Bay May 2020.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.SSC,p4.flow, align = "v", nrow = 5)
dev.off()

#######################################################################################################################################
# zoom in plots

library(lubridate)

date1 <- as.POSIXct("2017-12-01 00:00:00")
date2 <- as.POSIXct("2018-06-30 00:00:00")
int <- interval(date1, date2)

tdata.zoom1 = tdata[tdata$Date_TimeV2 %within% int,]
trapdata.zoom1 = trapdata[trapdata$Date.Out %within% int,]
trapdata.zoom2 = trapdata[trapdata$Date.In %within% int,]
trapdata.zoom = rbind(trapdata.zoom1,trapdata.zoom2)
trapdata.zoom =distinct(trapdata.zoom ,Value, .keep_all= TRUE)
flowdata.zoom1=flowdata[flowdata$Date %within% int,]
missinglightdata.zoom1=missinglightdata[missinglightdata$Date_Time_In %within% int,]

ypos = min(tdata.zoom1$RMS,na.rm = TRUE) + 0.95*diff(range(tdata.zoom1$RMS,na.rm = TRUE))

p1.RMS <- ggplot(tdata.zoom1, aes(Date_TimeV2, RMS)) + geom_line(stat="identity",colour="#4DAF4A", size=0.3) + theme_minimal() + theme_classic(base_size = 12)+
		scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab("RMS (pressure)")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2017-12-05 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "c.", size=6) 

ypos = min(tdata.zoom1$SSC..mg.L.1.,na.rm = TRUE) + 0.95*diff(range(tdata.zoom1$SSC..mg.L.1.,na.rm = TRUE))
		
p2.SSC <- ggplot(tdata.zoom1,aes(Date_TimeV2, SSC..mg.L.1.)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+
		scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2017-12-05 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 

ypos = min(tdata.zoom1$light..uE.cm2.,na.rm = TRUE) + 0.95*diff(range(tdata.zoom1$light..uE.cm2.,na.rm = TRUE))


p5.light <- ggplot(tdata.zoom1,aes(Date_TimeV2, light..uE.cm2.)) + geom_rect(data=tdata.zoom1, aes(xmin = min(Date_TimeV2), xmax = max(Date_TimeV2), ymin=0, ymax=+Inf), fill='grey')+ 
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 10)+scale_y_continuous(limits = c(0, 450),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2017-12-05 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_rect(data = missinglightdata.zoom1, aes( x = NULL,y = NULL,xmin = missinglightdata.zoom1$Date_Time_In, xmax = missinglightdata.zoom1$Date_Time_Out, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4 )


ypos = min(trapdata.zoom1$Value,na.rm = TRUE) + 0.95*diff(range(trapdata.zoom1$Value,na.rm = TRUE))
		
p3.trap <- ggplot2::ggplot(data = trapdata.zoom, aes(colour = Trap, x = trapdata.zoom1$Date.In, xend = trapdata.zoom$Date.Out, y = trapdata.zoom$Value, yend = trapdata.zoom$Value)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.95))+
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4","chocolate2"))+scale_y_continuous(limits = c(0, 100),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2017-12-12"), y=c(95), label= "a.", size=6) 
	
ypos = min(flowdata.zoom1$Discharge..ML.day.,na.rm = TRUE) + 0.95*diff(range(flowdata.zoom1$Discharge..ML.day.,na.rm = TRUE))

p4.flow <- ggplot(flowdata.zoom1, aes(Date, Discharge..ML.day.)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 10)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
	theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		 scale_x_date(date_breaks=("1 month"),date_labels = "%d/%m/%Y",expand=c(0,0))+annotate(geom="text", x=as.Date("2017-12-05"), y=ypos, label= "e.", size=6) 


png(paste(image.dir,"Dunk Island zoom wet2018.png",sep=''),width=30, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.SSC,p4.flow, align = "v", nrow = 5)
dev.off()	 

