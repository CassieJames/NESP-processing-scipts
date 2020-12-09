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


image.dir="C:/Users/jc246980/Documents/Current projects/NESP/Plots/"


setwd("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/NESP/Time series")
setwd("C:/Users/jc246980/Documents/Current projects/NESP/Time series/")
home<-getwd()

########################################################################################
# note: RMScurrent data has duplicates where timeframes between deployments overlap so need to identify these periods if RMS recalculated
df <- a2[duplicated(a2),]
df <- df[!duplicated(df[c('date')]),]

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

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/RMS calculations/Dunk/"; setwd(data.dir)
RMScurrent<-fread("Dunk_RMS_ALL.csv",sep=",")
RMScurrent$Ten_mins<-as.POSIXct(RMScurrent$Ten_mins, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")

MG2=merge(MG2, RMScurrent, by.x="DateTime", by.y="Ten_mins", all.x=TRUE, all.y=FALSE)

#write.csv(MG2,paste('Dunk_deploys1_2_data2model.csv',sep=''),row.names=F) # save as CSV file



#############################################################################################
#### Linear models relatig SSC to predictors
#############################################################################################

library(car)
library(relaimpo)
library(PerformanceAnalytics)

##### Deployment 1
MG2_d1=MG2[which(MG2$deploys %in% c(1,2)),c("DateTime","SSC", "RMS", "Speed", "currentRMS", "Hs", "Height")] 
MG2_d1=MG2_d1[complete.cases(MG2_d1),]
MG2_d1$Hs=as.numeric(MG2_d1$Hs)
MG2_d1=MG2_d1[MG2_d1$RMS>0.000000001,]# remove very small values=outliers
MG2_d1=MG2_d1[MG2_d1$Speed<0.2,]
MG2_d1=MG2_d1[MG2_d1$currentRMS<0.01,]

# create some lags in data
#30 mins
MG3 = MG2_d1 %>%
  arrange(DateTime) %>%
  mutate(currRMS_30 = rollmean(x = currentRMS, 3, align = "right", fill = NA))
head(MG3)

#1 hour
MG3 = MG3 %>%
  arrange(DateTime) %>%
  mutate(currRMS_60 = rollmean(x = currentRMS, 6, align = "right", fill = NA))
head(MG3)

# 3 hours
MG3 = MG3 %>%
  arrange(DateTime) %>%
  mutate(currRMS_3hr = rollmean(x = currentRMS, 18, align = "right", fill = NA))
head(MG3)

# 3 hours
MG3 = MG3 %>%
  arrange(DateTime) %>%
  mutate(currRMS_12hr = rollmean(x = currentRMS, 72, align = "right", fill = NA))
head(MG3)

png(paste(image.dir,"Dunk Island predictor correlations august 2020.png",sep=''),width=25, height=25, units='cm', res=500, pointsize=10, bg='white')
        par(mar=c(4,4,1,1),cex=1,oma=c(2,0,1,0.5)) #		
chart.Correlation(MG2_d1[,2:7])

dev.off()

trial1 <- lm(log(SSC)~log(RMS+1)+log(Speed+1)+log(currentRMS+1)+Hs+Height, data=MG2_d1)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")


cutoff <- 4/((nrow(MG2_d1)-length(trial1$coefficients)-2))
plot(trial1, which=4, cook.levels=cutoff) # outliers identified 
library(MASS)
sresid <- studres(trial1)
hist(sresid, freq=FALSE,
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

ncvTest(trial1)
spreadLevelPlot(trial1)# plot studentized residuals vs. fitted values

MG2_d1_sub=MG2_d1[-c(8080,5671,5670,5669),]

MG2_d1_sub=MG2_d1_sub[-c(967,664,663),]

trial1 <- lm(log(SSC)~(RMS)+(Speed)+(currentRMS)+Hs+Height, data=MG2_d1_sub)
par(mfrow=c(2, 2))
plot(trial1)

#### models using mgvc gams

library(mgcv)

m0 <- bam(log(SSC) ~ s(currentRMS)+s(Height)+s(Hs),data=MG2_d1,method = "fREML")

r0=acf(resid(m0), plot=FALSE)$acf[2]
m0 <- bam(log(SSC) ~ s(currentRMS)+s(Height)+s(Hs),data=MG2_d1,method = "fREML",rho=r0)
datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m0$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS Fit from Dunk Island deployments 1 and 2")



#####
m1 <- bam(log(SSC) ~ s(currentRMS),data=MG2_d1, method = "fREML")
r1=acf(resid(m1), plot=FALSE)$acf[2]
m1 <- bam(log(SSC) ~ s(currentRMS),data=MG2_d1,rho=r1)
datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m1$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS Fit from Dunk Island deployments 1 and 2")

####
m2 <- bam(log(SSC) ~ s(RMS),data=MG2_d1, method = "fREML")
r2=acf(resid(m2), plot=FALSE)$acf[2]
m2 <- bam(log(SSC) ~ s(RMS),data=MG2_d1,rho=r2)
datas <- rbindlist(list(MG2_d1[, .(SSC, DateTime)],
                        data.table(value = exp(m2$fitted.values),
                                   DateTime = MG2_d1[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS Fit from Dunk Island deployments 1 and 2")
  
  
#### models with lags

chart.Correlation(MG3[,2:11])
MG3=MG3[complete.cases(MG3),]

m2 <- bam(log(SSC) ~ s(curRMS_30),data=MG3, method = "fREML")
r2=acf(resid(m2), plot=FALSE)$acf[2]
m2 <- bam(log(SSC) ~ s(RMS),data=MG3,rho=r2)
datas <- rbindlist(list(MG31[, .(SSC, DateTime)],
                        data.table(value = exp(m2$fitted.values),
                                   DateTime = MG3[, DateTime])))
datas[, type := c(rep("Real", nrow(MG2_d1)), rep("Fitted", nrow(MG2_d1)))] 
ggplot(data = datas, aes(DateTime,(SSC),colour = type)) +
  geom_point(size = 0.8) +
  theme_bw() +facet_wrap(~ type, nrow=2)+
  labs(x = "Time", y = "SSC",
  title = "Current RMS Fit from Dunk Island deployments 1 and 2")