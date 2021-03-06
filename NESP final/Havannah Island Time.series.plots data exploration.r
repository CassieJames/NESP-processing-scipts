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

####Havannah Island
setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

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

flowdata=data.frame(read.csv("All discharge data.csv"))

trapdata<- read_xlsx("Logger metadata_for import.xlsx", sheet="Havannah Island")
trapdata=as.data.frame(trapdata)
trapdata=trapdata[,c(2,3,6,8,10)]
data_long <- gather(trapdata, Trap, Measurement, Havannah.1:Havannah.2,factor_key=TRUE)
data_long$Date.in= as.Date(data_long$Date.in, format="%d/%m/%Y") 
data_long$Date.out= as.Date(data_long$Date.out, format="%d/%m/%Y") 
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
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(limits = c(0, 500),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_vline(data = missinglightdata, aes( xintercept = timestamp,linetype="dotted", colour="pink"), alpha=0.4, size=1)+
		theme(legend.position = "none")

ypos = min(data_long$Measurement,na.rm = TRUE) + 0.95*diff(range(data_long$Measurement,na.rm = TRUE))
		
p3.trap <- ggplot(data_long, aes(colour = Trap, x = data_long$Date.in, xend = data_long$Date.out, y = data_long$Measurement, yend = data_long$Measurement)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.95))+ 
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4","chocolate2", "chocolate"))+scale_y_continuous(limits = c(0, 20),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=(20/100)*95
		, label= "a.", size=6)
	
ypos = min(flowdata$Burdekin,na.rm = TRUE) + 0.95*diff(range(flowdata$Burdekin,na.rm = TRUE))
xpos=c(min(tdata$timestamp), max(tdata$timestamp))
xpos=as.Date(xpos, format="%m/%d/%Y")

p4.flow <- ggplot(flowdata, aes(Date, Burdekin)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
		theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0),limits=xpos)+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "e.", size=6)

#ypos = min(tdata$u.current,na.rm = TRUE) + 0.95*diff(range(tdata$u.current,na.rm = TRUE))

ypos=0.475
cols=brewer.pal(n = 8, name = "Set2")
		
		 
png(paste(image.dir,"Havannah Island May 2020.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.SSC,p4.flow, align = "v", nrow = 5)
dev.off()


#######################################################################################################################################
tdata$u_comp_current<- tdata$'speed (m/s)'*sin((tdata$'heading (degrees CW from North)')*pi/180) # u-component, x-axis, E-W component
tdata$v_comp_current<- tdata$'speed (m/s)'*cos((tdata$'heading (degrees CW from North)')*pi/180) # u-component, x-axis, E-W component


data.dir="C:/Users/jc246980/Documents/Current projects/NESP/Tide data/"; setwd(data.dir)
Tide_data<-fread("Tidal_amplitude_2016_2020.csv",sep=",")
Tide_data[,'Date'] <- as.POSIXct(Tide_data$"Date", format = "%d/%m/%Y")


tdata$Date <-as.Date(tdata$timestamp)
Tide_data$Date <-as.Date(Tide_data$Date)+1

MG<-merge(tdata,Tide_data, by="Date", all.x=T)

MG$Year = lubridate::year(MG$Date)
MG$Month = lubridate::month(MG$Date)
MG<-MG[!is.na(MG$Amplitude),]
MGSpring<-MG[MG$Amplitude>=3.5,]
MGNeap<-MG[MG$Amplitude<1,]


png(paste(image.dir,"Havannah Island Tide amplitude vs current data.png",sep=''),width=20, height=10, units='cm', res=500, pointsize=10, bg='white')

par(mfrow=c(1,2))
plot(MG$u_comp_current,MG$v_comp_current,ylim=c(-0.4,0.4), xlim=c(-0.4,0.4),col='grey',ylab="V component",xlab="U component",main="Havannah Island")
plot(MGSpring$u_comp_current,MGSpring$v_comp_current,ylim=c(-0.4,0.4), xlim=c(-0.4,0.4),col='blue',ylab="V component",xlab="U component")
points(MGNeap$u_comp_current,MGNeap$v_comp_current, col='red')
legend(-0.2, 0.25, legend=c("Tide amplitude >3.5m", "Tide amplitude <1m"),
       col=c("blue", "red"), lty=1, cex=0.8)
dev.off() 
	  
png(paste(image.dir,"Havannah Island Tide amplitude vs current data by year.png",sep=''),width=30, height=30, units='cm', res=500, pointsize=10, bg='white')		  
	  
P1<-ggplot(MG,aes(u_comp_current,v_comp_current), na.rm=FALSE) + geom_point(colour="grey")+facet_wrap(MG$Year, nrow=1)+xlim(-0.4,0.4)+ylim(-0.4,0.4)+ggtitle("Havannah Island")
P2<-ggplot(MGSpring,aes(u_comp_current,v_comp_current), na.rm=TRUE) + geom_point(colour="blue",na.rm = TRUE)+facet_wrap(MGSpring$Year, nrow=1)+xlim(-0.4,0.4)+ylim(-0.4,0.4)+ggtitle("Tide amplitude >=3.5")
P3<-ggplot(MGNeap,aes(u_comp_current,v_comp_current), na.rm=TRUE) + geom_point(colour="red")+facet_wrap(MGNeap$Year, nrow=1)+xlim(-0.4,0.4)+ylim(-0.4,0.4)+ggtitle("Tide amplitude <1")

plot_grid(P1,P2, P3, align = "v", nrow = 3)

dev.off()

MG2018<-MG[MG$Year==2018,]

png(paste(image.dir,"Havannah Island Tide amplitude vs current data by month 2018.png",sep=''),width=30, height=30, units='cm', res=500, pointsize=10, bg='white')	
P4<-ggplot(MG2018,aes(u_comp_current,v_comp_current), na.rm=FALSE) + geom_point(colour="grey")+facet_wrap(MG2018$Month, nrow=2)+xlim(-0.4,0.4)+ylim(-0.4,0.4)+ggtitle("Havannah Island Monthly data for 2018")
P4
dev.off()

MG2019<-MG[MG$Year==2019,]

png(paste(image.dir,"Havannah Island Tide amplitude vs current data by month 2019.png",sep=''),width=30, height=30, units='cm', res=500, pointsize=10, bg='white')	
P5<-ggplot(MG2019,aes(u_comp_current,v_comp_current), na.rm=FALSE) + geom_point(colour="grey")+facet_wrap(MG2019$Month, nrow=2)+xlim(-0.4,0.4)+ylim(-0.4,0.4)+ggtitle("Havannah Island Monthly data for 2019")
P5
dev.off()

png(paste(image.dir,"Havannah Island Date vs current speed 2018.png",sep=''),width=30, height=30, units='cm', res=500, pointsize=10, bg='white')	
plot(MG2018$'Date', MG2018$'speed (m/s)')
dev.off()

png(paste(image.dir,"Havannah Island Date vs current speed Full Record.png",sep=''),width=30, height=30, units='cm', res=500, pointsize=10, bg='white')	
plot(MG$'Date', MG$'speed (m/s)')
dev.off()

#######################################################################################################################################
# Turbidity statistics for Zoe

quantile(tdata$NTUe, probs = c(0.1,0.5,0.9), na.rm=TRUE)
mean(tdata$NTUe,na.rm=TRUE)
sd(tdata$NTUe, na.rm=TRUE)/mean(tdata$NTUe, na.rm=TRUE)*100	 
NTUe10_90 = subset(tdata$NTUe, tdata$NTUe>=quants[[1]] &  tdata$NTUe<=quants[[3]])
hist(NTUe10_90)
cv=sd(NTUe10_90, na.rm=TRUE)/mean(NTUe10_90, na.rm=TRUE)*100	 
mean(NTUe10_90)
cv
length(tdata$NTUe[!is.na(tdata$NTUe)])