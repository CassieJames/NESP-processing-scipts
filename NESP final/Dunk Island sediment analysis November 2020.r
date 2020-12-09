##################################################################################
#### Gam models of resuspension including terms to account of temporal autocorrelation of data and differences in instruments between deployments
#### Cassie James TropWATER
#### 

library(mgcv)
library(ggplot2)
library(gratia)
library(itsadug)
library(wesanderson)
library(data.table)

setwd("C:/Users/jc246980/Documents/Current projects/NESP/Data sent to Murray Logan AIMS/")
MG2=data.frame(read.csv("Dunkdata.csv"))

MG2$DateTime<-as.POSIXct(MG2$DateTime, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")
MG2$Date=as.Date(MG2$DateTime)

lower_bound <- quantile(MG2$currRMS, 0.01, na.rm=TRUE)
upper_bound <- quantile(MG2$currRMS, 0.995, na.rm=TRUE)

MG2=MG2[MG2$currRMS>lower_bound,]
MG2=MG2[MG2$currRMS<upper_bound,]

MG2=MG2[complete.cases(MG2),]
MG2=MG2[MG2$NTUe>0,]

# identify start of different deployment time series
MG2 <-start_event(MG2, column = "DateTime", event = "deploys", label = "start.event",label.event = NULL, order = TRUE)
MG2$deploys=as.factor(MG2$deploys)

# naive model to explore autocorrelation
m1 <- bam(log(NTUe) ~ s(log(currRMS), k=-1)+s(deploys, bs="re"),data=MG2,method = "fREML", discrete=TRUE, family="scat", AR.start=MG2$start.event)
r1=acf(resid(m1), plot=FALSE)$acf[2]

# 41% deviance expained using 
m1AR1 <- bam(log(NTUe) ~ s(log(currRMS), k=-1)+s(deploys, bs="re"),data=MG2,rho=r1,method = "fREML", discrete=TRUE, family="scat",AR.start=MG2$start.event)
summary(m1AR1)

pal=wes_palette(n=5,name="Zissou1")

P1<-appraise(m1AR1)
P2<-draw(m1AR1)

myres=residuals(m1AR1, type = "response") # extract residuals

MG2$Residuals=exp(myres) # back transform residuals (rem log)

pal=wes_palette(n=5,name="Zissou1")

###### Plot residuals against river flow (note river flow is daily data so this is purely visual

 p <- ggplot(data = MG2, aes(Date,Residuals, colour="Residuals"))+ geom_point(size = 0.8)+theme_bw()
 p <- p+geom_line(data=MG2,aes(x=Date,y=Tully/2000,colour="Tully"),size = 0.8)
 p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2000, name = "Tully Discharge"))
 p <- p + labs(y = "Fitted Residuals",
                x = "Date",
                colour = "RMS current")
 p <- p + theme(legend.position = c(0.1, 0.8))+coord_cartesian(ylim = c(0, 60)) 
 p <- p+scale_color_manual(values=c(pal[5], pal[1]))+ scale_x_date(date_breaks = "2 month") 
 
 
#### Compare fitted with actual NTUe data

MG2=as.data.table(MG2)
xpos=c(as.POSIXct("2016-06-01 12:00", format = "%Y-%m-%d %H:%M") ,as.POSIXct("2016-12-01 23:50", format = "%Y-%m-%d %H:%M"))
xpos=as.Date(xpos)

datas <- rbindlist(list(MG2[, .(NTUe, Date)],
                        data.table(value = exp(m1AR1$fitted.values),
                                   DateTime = MG2[, Date])))
datas[, type := c(rep("Real", nrow(MG2)), rep("Fitted", nrow(MG2)))] 
p2<-ggplot(data = datas, aes(Date,(NTUe),colour = type)) + geom_point(size = 0.8) +
    theme_bw() +facet_wrap(~ type, nrow=2)+
    labs(x = "Time", y = "NTUe",
    title = "Dunk Island deployments 1 and 2")+ scale_x_date(date_breaks = "3 month", limits=c(xpos),date_labels = "%d/%m/%Y",expand=c(0,0)) 

