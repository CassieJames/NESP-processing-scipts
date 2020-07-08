##############################################################################################
### Site specific analysis of relationship between SSC and various predictors
### Haypoint project
### C James TropWATER, JCU
### 24 October 2016

library(relaimpo)
library(car)
library(effects)
library(gridExtra)
library(Hmisc)
library(ggplot2)

# Import data

data.dir="C:/Users/jc246980/Documents/Documents (2)/Current projects/Haypoint project/"; setwd(data.dir)
data.matrix.env.data=read.csv("Data 2015-2016.csv") # load data
envdata=data.matrix.env.data[,c("Site","SSC", "RMS", "wind_NESW", "wind_NWSE", "Amplitude","Pioneer","Sandy")]

### Following code works out the absolute difference between two consecutive tide readings and returns the maximum for each date - does not need to be rerun
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

tide.data=read.csv("Tides_15_16.csv") # load tide data which has high and low tides as separate columns 

tide.data <- within(tide.data, datetimeLT1 <- as.POSIXlt(paste(datestamp, time_LT1),
                                          format = "%d/%m/%Y %H:%M:%S")) # merge date and time stamp so that tides can be ordered consecutively
tide.data <- within(tide.data, datetimeHT1 <- as.POSIXlt(paste(datestamp, time_HT1),
                                          format = "%d/%m/%Y %H:%M:%S"))
tide.data <- within(tide.data, datetimeLT2 <- as.POSIXlt(paste(datestamp, time_LT2),
                                          format = "%d/%m/%Y %H:%M:%S"))
tide.data <- within(tide.data, datetimeHT2 <- as.POSIXlt(paste(datestamp, time_HT2),
                                          format = "%d/%m/%Y %H:%M:%S"))										  
									  
tide.data.LT1=tide.data[,c("datetimeLT1", "LT1")]	# rename so that data can be merged into single 
tide.data.HT1=setNames(tide.data[,c("datetimeHT1", "HT1")], names(tide.data.LT1))
tide.data.LT2=setNames(tide.data[,c("datetimeLT2", "LT2")], names(tide.data.LT1))		
tide.data.HT2=setNames(tide.data[,c("datetimeHT2", "HT2")], names(tide.data.LT1))	

tide.data.all=rbind(tide.data.LT1,tide.data.HT1,tide.data.LT2,tide.data.HT2)
tide.data.all$datetimeLT1 <- as.POSIXct( tide.data.all$datetimeLT1 , format = "%d-%m-%y %H:%M:%S")

tide.data.all=tide.data.all[ order(tide.data.all$datetimeLT1),]
tide.data.all$amplitude=abs(tide.data.all$LT1-rowShift(tide.data.all$LT1,-1))
tide.data.all=tide.data.all[complete.cases(tide.data.all),] # ignore the NA's - these were simply dates when 2 low or high tides did not occur
tide.data.all$Date =as.Date(tide.data.all$datetimeLT1) # provide date as a grouping variable removing the time stamp
tide.MAX.AMP=aggregate(tide.data.all$amplitude,by = list(Group.date = tide.data.all$Date), FUN = max)
write.csv(tide.MAX.AMP,paste('tide.max.amp.csv',sep=''),row.names=F) # save as CSV file

######################################################################
# fit model to freshwater data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Freshwater"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

######################################################################
# fit model to Hay Reef data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Hay Reef"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+NESW+NWSE+Con_Amp, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata[-c(69,68,285),] # remove outliers
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+wind_NESW+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Hay Reef results", out, file="summary_Hay_Reef_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Pioneer", "wind_NESW", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Hay Reef 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Hay Reef 2015-16 Partial effects.png',sep=''), width=(3500/3)*2, height=2000, units="px", res=300)
par(mfrow=c(2,2))
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


######################################################################
# fit model to Keswick data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Keswick"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata[-c(272,273,274,275,276,277,278,299,300,301,304),] # remove outliers
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NWSE+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Keswick results", out, file="summary_Keswick_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Pioneer", "Sandy", "wind_NWSE", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Keswick 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Keswick 2015-16 Partial effects.png',sep=''), width=3500, height=2000, units="px", res=300)
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

######################################################################
# fit model to Roundtop Island data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Roundtop Island"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata[-c(69,149,166,68,76,164,79,207,78),] # remove outliers
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Roundtop results", out, file="summary_Roundtop_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Pioneer",  "wind_NESW", "wind_NWSE", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Roundtop 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Roundtop 2015-16 Partial effects.png',sep=''), width=3500, height=2000, units="px", res=300)
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

######################################################################
# fit model to Slade Island data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Slade Island"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1), data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Slade results", out, file="summary_Slade_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Pioneer","Sandy")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Slade 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Slade 2015-16 Partial effects.png',sep=''), width=(3500/3)*2, height=2000, units="px", res=300)
par(mfrow=c(2,2))
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

######################################################################
# fit model to Relocation ground data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Relocation ground"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Sandy+1)+wind_NESW+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Relocation", out, file="summary_Relocation_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS","Sandy",  "wind_NESW", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Relocation 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Relocation 2015-16 Partial effects.png',sep=''), width=(3500/3)*2, height=2000, units="px", res=300)
par(mfrow=c(2,2))
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

######################################################################
# fit model to Victor data and check for autocorrelation and outliers
fwdata=envdata[which(envdata$Site=="Victor"),] # not sure why but SSC is reading as a factor
fwdata$SSC=as.numeric(levels(fwdata$SSC))[fwdata$SSC]
fwdata=fwdata[which(fwdata$SSC>=0.001),] # remove very low values of SSC as look wrong - need to check with nathan
row.names(fwdata) <- 1:nrow(fwdata) # renumber for ease when indexing rows later on to remove outliers

trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata)
outlierTest(trial1)
vif(trial1)# variance inflation values - none 
sqrt(vif(trial1))
qqPlot(trial1, main="QQ Plot")

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

# remove outliers and rerun model

fwdata_sub=fwdata[-c(68,121,157,291),] # remove outliers
fwdata_sub=fwdata_sub[which(fwdata_sub$Pioneer<=40000),] # remove extreme values for Pioneer flow
trial1 <- lm(log(SSC)~log(RMS)+log(Pioneer+1)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
trial1 <- lm(log(SSC)~log(RMS)+log(Sandy+1)+wind_NESW+wind_NWSE+Amplitude, data=fwdata_sub)
out <- capture.output(summary(trial1))
cat("Victor", out, file="summary_Victor_model.txt", sep="n", append=TRUE) # save out results of model

# Run boot strapping

calc.relimp(trial1, type = c("lmg", "pmvd"))
bootresult <- boot.relimp(trial1, b = 1000, type = c("lmg", "pmvd", "last","first"), fixed = FALSE, rela=TRUE)
boot.result=booteval.relimp(bootresult, typesel = c("lmg"), level = 0.95,bty = "perc", nodiff = TRUE)

CI.lower=as.data.frame(t(boot.result$lmg.lower)) # extract results to create own plot
CI.upper=as.data.frame(t(boot.result$lmg.upper))
lmg.result=as.data.frame((boot.result$lmg))
fw.boots=cbind(lmg.result, CI.lower, CI.upper)
fw.boots$predictors=c("RMS", "Sandy",  "wind_NESW", "wind_NWSE", "Amplitude")
colnames(fw.boots)=c("lmg", "lower", "upper", "predictors")
fw.boots$predictors<- factor(fw.boots$predictors, levels = fw.boots$predictors[order(fw.boots$lmg, decreasing=TRUE)])


myplot <- ggplot(fw.boots, aes(x=fw.boots[,4],y=fw.boots[,1]))+
geom_bar(position=position_dodge(), stat="identity", fill = "#C1CDCD") +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
				  ylab("% of R2") +
				  xlab("")+
				  theme_bw()
 
png(paste(data.dir,'Victor 2015-16 Variable importance.png',sep=''), width=3000, height=2000, units="px", res=500)
myplot
dev.off()
 
eff.pres <- allEffects(trial1, xlevels=50)

png(paste(data.dir,'Victor 2015-16 Partial effects.png',sep=''), width=3500, height=2000, units="px", res=300)
par(mfrow=c(3,2))
axis.text <- trellis.par.get("axis.text") # text to set lattice graphic parameters....
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

#####################################################################################
##

wind.dat=read.csv("windrose.csv") # load data
windRose(wind.dat, breaks=c(0,6,12,18,24), paddle=FALSE, cols=c("#FFF68F","#66CD00","#BCD2EE","#607B8B","#8470FF"), annotate=FALSE,offset=0,
key = list(header="Wind Speed (km/hr)",footer="",
              fit = "all", height = 0.5,width=2,
              space = "left", labels=c("<6","6-12", "12-18","18-24", ">24")) )
			  
			  
png(paste(data.dir,'Wind plot.png',sep=''), width=3000, height=2000, units="px", res=500)

windRose(wind.dat, breaks=c(0,6,12,18,24), paddle=FALSE, cols=c("#FFF68F","#66CD00","#BCD2EE","#607B8B","#8470FF"), annotate=FALSE,offset=0,
key = list(header="Wind Speed (km/hr)",footer="",
              fit = "all", height = 0.5,width=2,
              space = "left", labels=c("<6","6-12", "12-18","18-24", ">24")) )
dev.off()