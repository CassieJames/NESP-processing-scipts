 # script to determine significant wave height
 
 library(oceanwaves)
 
 data.dir="C:/Users/jc246980/Documents/Current projects/Additional work/Nesp analysis/Dunk Island/"; setwd(data.dir) # Set data directory
 
 C:\Users\jc246980\Documents\Current projects\Additional work\NESP analysis\Dunk Island
 
DunkDat= read.csv("Dunk_DepthData.csv") # read in data

for(i in 1:nrow(DunkDat){

mydat=DunkDat[i,]
mydat=as.data.frame(t(mydat[2:11]))
mydat=cbind(c(1:10), mydat)
colnames(mydat)=c("Hz", "Depth")

DunkDat$DateTime<-as.POSIXct(DunkDat$DateTime, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")
data_long <- gather(DunkDat, Depth, measurement, X1:X10)
data_long<-data_long[ order(data_long$DateTime, decreasing = FALSE ),]

tada<-spectrum(data_long$measurement[1:500],method = c("pgram", "ar"))

par(mfrow = c(2,2))
spectrum(data_long$measurement[1:10])
spectrum(data_long$measurement[1:10], spans = 3)
spectrum(data_long$measurement[1:10], spans = c(3,3))
spectrum(data_long$measurement[1:10], spans = c(3,5))

stats=waveStatsSP(data_long$measurement, Fs = 1, plot = TRUE)

#### Orchard Rocks comparison between RMS data and wave data 
C:\Users\jc246980\Documents\Current projects\NESP\NESP work with wave rider data etc

data.dir="C:/Users/jc246980/Documents/Current projects/NESP/NESP work with wave rider data etc/"; setwd(data.dir) # Set data directory
library(data.table)
OR<-fread("Orchard_Rocks_Jan2020.csv",sep=",")
OR[,'DateTime'] <- as.POSIXct(OR$DateTime, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")
head(OR)

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
OR.2<-setkey(OR, DateTime )

MG <- TV.2[ OR.2, roll = "nearest",allow.cartesian=TRUE]

par(mfrow=c(2,2))
plot((MG$SSC)~MG$RMS)
plot((MG$SSC)~MG$Hmax)
plot((MG$SSC)~MG$Hs)
plot((MG$RMS)~MG$Hs)




  # Wind data decomposition.
  # Adding components u and v to wind data table (angles must be in radians, so times pi/180)
  TV$u_comp_EW<-(TV$Speed*3.6)*cos((TV$Heading)*pi/180) # u-component, x-axis, E-W component
  TV$v_comp_NS<-(TV$Speed*3.6)*sin((TV$Heading)*pi/180) # v-component, y-axis, N-S component
  

# import burdekin discharge data

discharge.dir="C:/Users/jc246980/Documents/Current projects/Additional work/Steve Lewis time series plots/Orchard Rocks/"; setwd(discharge.dir) # Set data directory

flow<-fread("Burdekin flow 2016-2019.csv",sep=",")

flow[,'DateTime'] <- as.POSIXct(flow$"Date/Time", format = "%d/%m/%Y %H:%M")
flow[,'Date/Time'] <- NULL # so we don't imply Date/Time!
flow.2<-setkey(flow, DateTime )

# joins but attributes hydrological data to actual day
MG.2 <- flow.2[ MG, roll = T,allow.cartesian=TRUE]

MG.2$Hs <-as.numeric(MG.2$Hs)
MG.2$Hmax <-as.numeric(MG.2$Hmax)

ggplot(MG.2, aes(x=RMS, y=SSC,  size=Discharge)) +
  geom_point()

# Import tide amplitude data
Tide.dir="C:/Users/jc246980/Documents/Current projects/Additional work/NESP analysis/Townsville tidal data/"; setwd(Tide.dir) # Set data directory
Tide<-fread("Townsville_tide_amplitude_2016-2019.csv",sep=",")

Tide[,'Date'] <- as.POSIXct(Tide$"Date", format = "%d/%m/%Y")
Tide.2<-setkey(Tide, Date )

MG.3 <- Tide.2[ MG.2, roll = T,allow.cartesian=TRUE]

TideHt<-fread("Townsville_tide_Ht_10Mins.csv",sep=",")

TideHt[,'DateTime'] <- as.POSIXct(TideHt$"DateTime", format = "%d/%m/%Y %H:%M")
TideHt.2<-setkey(TideHt, DateTime )
MG.4 <- TideHt.2[ MG.3, roll = T,allow.cartesian=TRUE]