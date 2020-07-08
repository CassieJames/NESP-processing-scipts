#################################################################################
#### Steve Lewis Time Series plots

library(ggplot2)
library(binhf)
library(RColorBrewer)
library(cowplot)
library(data.table)
library(openair)
data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Cleveland Bay/"; setwd(data.dir)
image.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Cleveland Bay/"

####Dunk Island


tdata=data.frame(read.csv("Cleveland.csv"))
flowdata=data.frame(read.csv("Burdekin discharge.csv"))

trapdata=data.frame(read.csv("Cleveland trap accumulation.csv"))


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
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 

ypos = min(tdata$NTUe,na.rm = TRUE) + 0.95*diff(range(tdata$NTUe,na.rm = TRUE))

p2.NTUe <- ggplot(tdata,aes(timestamp, NTUe)) + geom_line(stat="identity", colour="#FF7F00", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+
		theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('NTUe'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M"), y=ypos, label= "e.", size=6) 

ypos = min(tdata$light,na.rm = TRUE) + 0.95*diff(range(tdata$light,na.rm = TRUE))

p5.light <- ggplot(tdata,aes(timestamp, light)) + geom_rect(data=tdata, aes(xmin = min(timestamp), xmax = max(timestamp), ymin=0, ymax=+Inf), fill='grey')+ 
		geom_line(stat="identity", colour="white", size=0.3) + theme_classic(base_size = 12)+scale_y_continuous(limits = c(0, 550),position = "right")+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Light ('*mu~E/cm^2*')'))+
		scale_x_datetime(expand=c(0,0))+annotate(geom="text", x=as.POSIXct("2016-06-25 12:00", format = "%Y-%m-%d %H:%M") , y=ypos, label= "b.", size=6)+
		geom_rect(data = missinglightdata, aes( x = NULL,y = NULL,xmin = missinglightdata$Date_Time_In, xmax = missinglightdata$Date_Time_Out, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4 )

		
ypos = min(trapdata$Value,na.rm = TRUE) + 0.95*diff(range(trapdata$Value,na.rm = TRUE))
		
p3.trap <- ggplot2::ggplot(data = trapdata, aes(colour = Trap, x = trapdata$Date.In, xend = trapdata$Date.Out, y = trapdata$Value, yend = trapdata$Value)) +theme_classic(base_size = 12)+
    geom_segment(lwd=1.5)+ theme(legend.position = c(0.95,0.90))+
	theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Trap acc.' ~(mg.cm^2~d^-1)))+
		scale_color_manual(values=c("chocolate4"))+scale_y_continuous(limits = c(0, 100),position = "left")+
		scale_x_date(expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=95, label= "a.", size=6) 
	
ypos = min(flowdata$Discharge..ML.Day.,na.rm = TRUE) + 0.95*diff(range(flowdata$Discharge..ML.Day.,na.rm = TRUE))

p4.flow <- ggplot(flowdata, aes(Date, Discharge..ML.Day.)) + geom_line(colour = "#377EB8") + theme_classic(base_size = 12)+scale_y_continuous(position = "left")+geom_area(position = "identity",fill="#377EB8")+
	theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="grey20",size=12))+ylab(bquote('Discharge' ~(ML.d^-1)))+
		 scale_x_date(date_breaks=("3 month"),date_labels = "%d/%m/%Y",expand=c(0,0))+annotate(geom="text", x=as.Date("2016-06-25"), y=ypos, label= "f.", size=6) 

		
		 
png(paste(image.dir,"Cleveland September02.png",sep=''),width=40, height=22, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #
		
plot_grid(p3.trap,p5.light, p1.RMS,p2.SSC,p4.flow, align = "v", nrow = 5)
dev.off()

