#################################################################################
#### Steve Lewis Time Series plots

library(ggplot2)
library(binhf)
library(RColorBrewer)
library(cowplot)
library(data.table)
library(openair)
data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/"; setwd(data.dir)
image.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/"

####Dunk Island


tdata=data.frame(read.csv("Dunk_Island.csv"))
flowdata=data.frame(read.csv("Tully discharge data.csv"))

trapdata=data.frame(read.csv("Dunk trap accumulationAug2019.csv"))


tdata$timestamp<-as.POSIXct(tdata$timestamp, format = "%d/%m/%Y %H:%M") 
flowdata$Date <- as.Date(flowdata$Date, format="%d/%m/%Y") 
trapdata$Date.In <- as.Date(trapdata$Date.In, format="%d/%m/%Y") 
trapdata$Date.Out <- as.Date(trapdata$Date.Out, format="%d/%m/%Y") 

missinglightdata=data.frame(read.csv("Missing_light.csv"))
missinglightdata$Date.In <- as.POSIXct(missinglightdata$Date.In, format="%d/%m/%Y") 
missinglightdata$Time.In <- format(missinglightdata$Time.In, format = "%H:%M:%S") 
missinglightdata$Date_Time_In<-as.POSIXct(paste(missinglightdata$Date.In, missinglightdata$Time.In), format="%Y-%m-%d %H:%M")
missinglightdata$Date.Out <- as.POSIXct(missinglightdata$Date.Out, format="%d/%m/%Y") 
missinglightdata$Time.Out <- format(missinglightdata$Time.Out, format = "%H:%M:%S") 
missinglightdata$Date_Time_Out<-as.POSIXct(paste(missinglightdata$Date.Out, missinglightdata$Time.Out), format="%Y-%m-%d %H:%M")



aimsdata$SAMPLE_DAY<-as.POSIXct(aimsdata$SAMPLE_DAY, format = "%d/%m/%Y")
aimsdata$SAMPLE_TIME<-format(aimsdata$SAMPLE_TIME, format = "%H:%M:%S")



Dunk_data=tdata
# merge wind data

data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/covariates/"; setwd(data.dir)

# import lucinda
winddata=read.csv("WIND_LUCIND_sorted.csv") # load data I did initial sorting in excel but data still needs to be sorted
winddata$Date_TimeV2<-as.POSIXct(winddata$Date_Time, format = "%d-%m-%Y %H:%M:%S")
winddata<-winddata[ order(winddata$Date_TimeV2 , decreasing = FALSE),]

Dunk_data <-data.table(Dunk_data)
winddata <-data.table(winddata)

setkey(Dunk_data, timestamp)
setkey(winddata, Date_TimeV2)
combined <- winddata[ Dunk_data, roll = "nearest"]

# import innisfail 
winddata=read.csv("WIND_INNISFAIL_sorted.csv") # load data I did initial sorting in excel but data still needs to be sorted
winddata$Date_TimeV2<-as.POSIXct(winddata$datetime, format = "%d-%m-%Y %H:%M:%S")
winddata<-winddata[ order(winddata$Date_TimeV2 , decreasing = FALSE),]
winddata <-data.table(winddata)

combined$Date_TimeV2<-as.POSIXct(combined$Date_TimeV2, format = "%d/%m/%Y %H:%M") 
setkey(combined, Date_TimeV2)
setkey(winddata, Date_TimeV2)

combinedV2 <- winddata[ combined, roll = "nearest"]

combinedV2$u.current <- combinedV2$speed.upper..m.s. * sin(2 * pi * combinedV2$heading..degrees.CW.from.North./360)
combinedV2$v.current <- combinedV2$speed.upper..m.s. * cos(2 * pi * combinedV2$heading..degrees.CW.from.North./360)

######################################################################################################################
# Import aims data - note that AIMs data is not continuous so needs to be merged properly and also has some different time sequences - goes from 10 minute intervals
# to 7 second intervals for a period of time mid record!

data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/"; setwd(data.dir)
aimsdata=read.csv("AIMs_data_NTU.csv") 
aimsdata$SAMPLE_DAY<-as.POSIXct(aimsdata$SAMPLE_DAY, format = "%d/%m/%Y")
aimsdata$SAMPLE_TIME<-format(aimsdata$SAMPLE_TIME, format = "%H:%M:%S")
aimsdata$Date_TimeV2<-as.POSIXct(paste(aimsdata$SAMPLE_DAY, aimsdata$SAMPLE_TIME), format="%Y-%m-%d %H:%M")
aimsdata=aimsdata[which(aimsdata$LOCATION=="Dunk"),]


require(dplyr)
aims10 <-aimsdata %>%
  group_by(Date_TimeV2 = cut(Date_TimeV2, breaks="10 min")) %>%
  summarize(NTU_QA = mean(NTU_QA))
  
aims10$Date_TimeV2<-as.POSIXct(aims10$Date_TimeV2, format="%Y-%m-%d %H:%M")

setDT(combinedV2)
setDT(aims10)

combinedV3=aims10[combinedV2, on = c('Date_TimeV2')]

write.csv(combinedV3, file = "File for correlations between Aims and TropWATER data.csv") # save data out

tdata=combinedV2

cols=brewer.pal(n = 9, name = "Set1")

ypos = min(tdata$RMS,na.rm = TRUE) + 0.95*diff(range(tdata$RMS,na.rm = TRUE))

p1.RMS <- ggplot(tdata, aes(Date_TimeV2, RMS)) + geom_line(stat="identity",colour="#4DAF4A", size=0.3) + theme_minimal() + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab("RMS (pressure)")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "c.", size=6) 

ypos = min(tdata$SSC..mg.L.1.,na.rm = TRUE) + 0.95*diff(range(tdata$SSC..mg.L.1.,na.rm = TRUE))
		
p2.SSC <- ggplot(tdata,aes(Date_TimeV2, SSC..mg.L.1.)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 

ypos = min(tdata$NTUe,na.rm = TRUE) + 0.95*diff(range(tdata$NTUe,na.rm = TRUE))

p2.NTUe <- ggplot(tdata,aes(Date_TimeV2, NTUe)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('NTUe'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 

ypos = min(aimsdata$NTU_QA,na.rm = TRUE) + 0.95*diff(range(aimsdata$NTU_QA,na.rm = TRUE))

p2.NTUAims <- ggplot(aimsdata,aes(Date_TimeV2, NTU_QA)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab('NTU')+
		scale_x_datetime(expand=c(0,0), limits=c(as.POSIXct(min(tdata$Date_TimeV2)),as.POSIXct(max(tdata$Date_TimeV2))))+
		annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 

ypos = min(tdata$light..uE.cm2.,na.rm = TRUE) + 0.95*diff(range(tdata$light..uE.cm2.,na.rm = TRUE))


p5.light <- ggplot(tdata,aes(Date_TimeV2, light..uE.cm2.)) + geom_rect(data=tdata, aes(xmin = min(Date_TimeV2), xmax = max(Date_TimeV2), ymin=0, ymax=+Inf), fill='grey')+ 
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(limits = c(0, 450),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_rect(data = missinglightdata, aes( x = NULL,y = NULL,xmin = missinglightdata$Date_Time_In, xmax = missinglightdata$Date_Time_Out, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4 )

		



ypos = min(trapdata$Value,na.rm = TRUE) + 0.95*diff(range(trapdata$Value,na.rm = TRUE))
		
p3.trap <- ggplot2::ggplot(data = trapdata, aes(colour = Trap, x = trapdata$Date.In, xend = trapdata$Date.Out, y = trapdata$Value, yend = trapdata$Value)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.95))+
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4","chocolate2"))+scale_y_continuous(limits = c(0, 100),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=c(95), label= "a.", size=6) 
	
ypos = min(flowdata$Discharge..ML.day.,na.rm = TRUE) + 0.95*diff(range(flowdata$Discharge..ML.day.,na.rm = TRUE))

p4.flow <- ggplot(flowdata, aes(Date, Discharge..ML.day.)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+geom_area(position = "identity",fill="#377EB8")+
	theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		 scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "f.", size=6) 

#ypos = min(tdata$u.current,na.rm = TRUE) + 0.95*diff(range(tdata$u.current,na.rm = TRUE))

ypos=0.475
cols=brewer.pal(n = 8, name = "Set2")
		
p6.ucurrent <- ggplot(tdata,aes(Date_TimeV2, u.current)) + geom_line(stat="identity", colour=cols[1], size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+coord_cartesian(ylim = c(-0.25, 0.25))+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=11))+ylab("u current")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=5) 		 
		 
#ypos = min(tdata$v.current,na.rm = TRUE) + 0.95*diff(range(tdata$v.current,na.rm = TRUE))
		
p7.vcurrent <- ggplot(tdata,aes(Date_TimeV2, v.current)) + geom_line(stat="identity", colour=cols[2], size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+coord_cartesian(ylim = c(-0.25, 0.25))+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=11))+ylab("v current")+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "f.", size=5) 	

		# 
colnames(tdata)[6]="ws"
colnames(tdata)[7]="wd"
colnames(tdata)[4]="date"
data.luc <- timeAverage(tdata, avg.time = 'week')	
data.luc=as.data.frame(data.luc <- timeAverage(tdata, avg.time = 'week'))
data.luc$wd=round(data.luc$wd,0)

ypos = 0.95

library(circular)
	
col.circ=	circular.colors(360, m = 0, M = 2 * pi)
col.circ=as.data.frame(col.circ)
col.circ$degrees=rownames(col.circ)

data.luc2=merge(data.luc, col.circ,  by.x="wd", by.y="degrees")
	
p8 <-
 ggplot(data.luc2,aes(date, 0)) + geom_text(aes(angle=-wd+90), label="→",colour=data.luc2$col.circ, size=10)+scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "g.", size=5)+ 	
coord_cartesian(ylim = c(-1, 1))+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y = element_text(colour="grey20",size=11), axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank())+ylab("Wind direction")

ypos = min(tdata$ws,na.rm = TRUE) + 0.95*diff(range(tdata$ws,na.rm = TRUE))
		
p9 <- ggplot(tdata,aes(date, ws)) + geom_line(stat="identity", colour=cols[8], size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=11))+ylab(bquote('Wind speed' ~(km.^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "h.", size=5) 			
		 
png(paste(image.dir,"Dunk Island August2019V3.png",sep=''),width=40, height=(40/3)*2, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.NTUAims,p2.SSC,p4.flow, align = "v", nrow = 6)
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

# zoom in plots

library(lubridate)
library(dplyr)

date1 <- as.POSIXct("2016-12-01 00:00:00")
date2 <- as.POSIXct("2017-06-30 00:00:00")
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
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-12-05 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "c.", size=6) 

ypos = min(tdata.zoom1$SSC..mg.L.1.,na.rm = TRUE) + 0.95*diff(range(tdata.zoom1$SSC..mg.L.1.,na.rm = TRUE))
		
p2.SSC <- ggplot(tdata.zoom1,aes(Date_TimeV2, SSC..mg.L.1.)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+
		scale_y_continuous(position = "right")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('SSC' ~(mg.L^-1)))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-12-05 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "d.", size=6) 

ypos = min(tdata.zoom1$light..uE.cm2.,na.rm = TRUE) + 0.95*diff(range(tdata.zoom1$light..uE.cm2.,na.rm = TRUE))


p5.light <- ggplot(tdata.zoom1,aes(Date_TimeV2, light..uE.cm2.)) + geom_rect(data=tdata.zoom1, aes(xmin = min(Date_TimeV2), xmax = max(Date_TimeV2), ymin=0, ymax=+Inf), fill='grey')+ 
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 10)+scale_y_continuous(limits = c(0, 450),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-12-05 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_rect(data = missinglightdata.zoom1, aes( x = NULL,y = NULL,xmin = missinglightdata.zoom1$Date_Time_In, xmax = max(tdata.zoom1$Date_TimeV2), ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4 )


ypos = min(trapdata.zoom$Value,na.rm = TRUE) + 0.95*diff(range(trapdata.zoom$Value,na.rm = TRUE))
		
p3.trap <- ggplot2::ggplot(data = trapdata.zoom, aes(colour = Trap, x = trapdata.zoom$Date.In, xend = trapdata.zoom$Date.Out, y = trapdata.zoom$Value, yend = trapdata.zoom$Value)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.95))+
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=10))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4","chocolate2"))+scale_y_continuous(limits = c(0, 100),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2016-11-28"), y=c(95), label= "a.", size=6) 
	
ypos = min(flowdata.zoom1$Discharge..ML.day.,na.rm = TRUE) + 0.95*diff(range(flowdata.zoom1$Discharge..ML.day.,na.rm = TRUE))

p4.flow <- ggplot(flowdata.zoom1, aes(Date, Discharge..ML.day.)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 10)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
	theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		 scale_x_date(date_breaks=("1 month"),date_labels = "%d/%m/%Y",expand=c(0,0))+annotate(geom="text", x=as.Date("2016-12-05"), y=ypos, label= "e.", size=6) 


png(paste(image.dir,"Dunk Island zoom wet2017.png",sep=''),width=30, height=20, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.SSC,p4.flow, align = "v", nrow = 5)
dev.off()	 