##############################################################################################
### NESP analysis
### C James TropWATER, JCU
### 14 June 2019

library(data.table)

data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/"; setwd(data.dir)
Dunk_data=read.csv("Dunk_Island.csv") # load data
Dunk_data$timestampV2<-as.POSIXct(Dunk_data$timestamp, format = "%d/%m/%Y %H:%M") # make sure r recongises datetime stampdata.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/Covariates/"; setwd(data.dir)

# import lucinda wind data
data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Dunk Island/covariates/"; setwd(data.dir)
winddata=read.csv("WIND_LUCIND_sorted.csv") # load data I did initial sorting in excel but data still needs to be sorted
winddata$Date_TimeV2<-as.POSIXct(winddata$Date_Time, format = "%d-%m-%Y %H:%M:%S")
winddata<-winddata[ order(winddata$Date_TimeV2 , decreasing = FALSE),]

# Sorting out wind data to match Dunk_island data time series

Dunk_dataV2 <-data.table(Dunk_data)
winddataV2 <-data.table(winddata)

setkey(Dunk_dataV2, timestampV2)
setkey(winddataV2, Date_TimeV2)
combined <- winddataV2[ Dunk_dataV2, roll = "nearest"]

# import innisfail wind data
winddata=read.csv("WIND_INNISFAIL_sorted.csv") # load data I did initial sorting in excel but data still needs to be sorted
winddata$Date_TimeV2<-as.POSIXct(winddata$datetime, format = "%d-%m-%Y %H:%M:%S")
winddata<-winddata[ order(winddata$Date_TimeV2 , decreasing = FALSE),]
winddataV2 <-data.table(winddata)

combined$timestampV2<-as.POSIXct(combined$timestamp, format = "%d/%m/%Y %H:%M") 
setkey(combined, timestampV2)
setkey(winddataV2, Date_TimeV2)

combinedV2 <- winddataV2[ combined, roll = "nearest"]


##################################################################################################
# Wind data transformations
# plot data to check
library(openair)

 plot(combined$Date_TimeV2,combined$Wind_Speed,type="n", main=heading)
  lines(combined$Date_TimeV2,combined$Wind_Speed, type=l) 
 
#################################### 
# Calculate the u and v wind components  - this is just a break down of what the openair functions do internally so these steps are just to demonstrate internal functions
#combined$u.wind <- combined$Wind_Speed * sin(2 * pi * combined$Wind_Direction/360)
#combined$v.wind <- combined$Wind_Speed * cos(2 * pi * combined$Wind_Direction/360)
# mean.u <- mean(combined$u.wind, na.rm = T)
# mean.v <- mean(combined$v.wind, na.rm = T)
# Calculate the resultant vector average wind direction
# wd.average <- (atan2(mean.u, mean.v) * 360/2/pi) + 180

# function needs ws for wind speed and wd for wind direction - this calculates summary metrics for dataset with only Lucinda data
colnames(combined)[2]="ws"
colnames(combined)[3]="wd"
colnames(combined)[4]="date"

data.luc <- timeAverage(combined, avg.time = 'hour')
data.luc <-as.data.frame(data.luc)
data.luc$ws<-round(data.luc$ws,2)

png(paste(data.dir,'Prevailing Wind direction Lucinda.png',sep=''), width=3000, height=3000, units="px", res=200)
windRose(data.luc,paddle=F,type = c("month"),breaks=c(0,1.1,12.6,19.8,28.8,38.9,50,61.9), hemisphere = "southern") # breaks taken f
dev.off()

# function needs ws for wind speed and wd for wind direction - this calculates summary metrics for dataset with both wind datasets
colnames(combinedV2)[2]="ws"
colnames(combinedV2)[3]="wd"
colnames(combinedV2)[4]="date"

data.inis <- timeAverage(combinedV2, avg.time = 'hour')
data.inis <-as.data.frame(data.inis)
data.inis$ws<-round(data.inis$ws,2)

png(paste(data.dir,'Prevailing Wind direction Inisfail.png',sep=''), width=3000, height=3000, units="px", res=200)
windRose(data.inis,paddle=F,type = c("monthyear"),breaks=c(0,1.1,12.6,19.8,28.8,38.9,50,61.9), hemisphere = "southern") 
dev.off()

# Comparing lucinda and innisfail
colnames(combinedV2)[2]="ws_inis"
colnames(combinedV2)[3]="wd_inis"
colnames(combinedV2)[4]="date"
colnames(combinedV2)[6]="ws_luc"
colnames(combinedV2)[7]="wd_luc"

pollutionRose(combinedV2,ws="ws_inis", wd="wd_inis",ws2="ws_luc", wd2="wd_luc",breaks=c(0,1.1,12.6,19.8,28.8,38.9,50,61.9)) 

windRose(data.luc,paddle=F,breaks=c(0,1.1,12.6,19.8,28.8,38.9,50,61.9), hemisphere = "southern") 

####################################################################################################################################
# Working out where SSC data breakpoints occurs to subset baseline from extremes
# check web page for method details: https://rpubs.com/MarkusLoew/12164
library(ggplot2)
library(segmented)

combinedV3<-combinedV2[ order(combinedV2$SSC..mg.L.1. , decreasing = FALSE),] # sort SSC data
plot(combinedV3$SSC..mg.L.1.)

SSCdata=d <- combinedV3$SSC..mg.L.1.[!is.na(combinedV3$SSC..mg.L.1.)]
x=1:length(SSCdata)
mydata=cbind(x,SSCdata)
mydata=as.data.frame(mydata)

out.lm <- lm(SSCdata ~ x)
out.seg<-segmented(out.lm,seg.Z = ~x, psi = list(x = c( 128000)),npsi=2)

my.fitted <- fitted(out.seg)
my.model <- data.frame(time = x, SSC = my.fitted)
my.lines <- out.seg$psi[, 2]

# plot the fitted model
ggplot(my.model, aes(x = x, y = SSC)) + geom_line()+geom_point(data=mydata,aes(x=x,y=SSCdata))+ geom_vline(xintercept = my.lines, linetype = "dashed")

# break line = 126561 == SSC == 19.85483


my.slopes <- coef(out.seg)
b0 <- coef(out.seg)[[1]]
b1 <- coef(out.seg)[[2]]

c1 <- coef(out.seg)[[2]] + coef(out.seg)[[3]]
break1 <- out.seg$psi[[3]]

####################################################################################################################################
# Calculate current vectors

combinedV2$u.current <- combinedV2$speed.upper..m.s. * sin(2 * pi * combinedV2$heading..degrees.CW.from.North./360)
combinedV2$v.current <- combinedV2$speed.upper..m.s. * cos(2 * pi * combinedV2$heading..degrees.CW.from.North./360)

####################################################################################################################################
# plot data to explore drivers
library(relaimpo)
library(car)
library(effects)
library(gridExtra)
library(Hmisc)
library(ggplot2)
library(RColorBrewer)

mydata= combinedV2[!is.na(combinedV2$SSC..mg.L.1.),]
mydata= mydata[!is.na(mydata$v.current),]
mydata= mydata[!is.na(mydata$RMS),]
colnames(mydata)[11] <-"SSC"

mydatalow= mydata[mydata$SSC<=19.85483,]

trial1 <- lm((SSC)~log(RMS)+u.current+v.current, data=mydatalow)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")


summary(fmp <- glm(SSC ~ HEIGHT*YEAR, family=poisson))

cutoff <- 4/((nrow(fwdata)-length(trial1$coefficients)-2))
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

# remove outliers

fwdata_sub=fwdata[-c(58,61,64,66,67,213,232),]
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Freshwater point results", out, file="summary_Freshwater point.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Sandy", "wind_NESW", "wind_NWSE", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot<-ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Freshwater point 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 

eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Freshwater point 2015-16 Partial effects.png',sep=''), width=3500, height=2000, units="px", res=300)
par(mfrow=c(3,2))
axis.text <- trellis.par.get("axis.text") # text to set lattice graphicsqphics parameters....
par.ylab.text <- trellis.par.get("par.ylab.text")
par.xlab.text <- trellis.par.get("par.xlab.text")
axis.text$cex <- 0.7
par.ylab.text$cex <- 1
par.xlab.text$cex <- 1
trellis.par.set("axis.text", axis.text)
trellis.par.set("par.ylab.text", par.ylab.text)
trellis.par.set("par.xlab.text", par.xlab.text)
plot(eff.pres, main="", ylab="Ln(SSC) (mg/L)")
dev.off()


