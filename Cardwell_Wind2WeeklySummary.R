#---
#  title: "Wind summarised to weekly data"
#  author: C James
#  date: "22 July 2020"

#  This code gets wind data from the database as hourly readings with directon and quality coentrol, and output it into the data base as decomposed wind into componets u (x-axis, E-w) and v (y-axis, N-S).
#  Wind data is collected every three hours from midnight till 9 pm, so at 00:00, 03:00, 06:00, 09:00, 12:00, 15:00, 18:00, and 21:00. If no unwanted hours is selected, a daily u and v components are calculated.
#  If any hour must be excluded, they must be speficied below:
  
#  Hours to be excluded:

exclude<-c('03','06','18','21')  # in this example wind data colected at 03:00, 06:00, 18:00 and 21:00 will be excluded from the mean daily wind components. And comment out the next command line.
# If there is no need for exclusion, use any time not in the normal sampling time, e.g., 22.
exclude<-c('22')


#Selecting Wind Station of interest and period for data analysis
#All possible stations are:
  
#  Station_Number   Station_Name           Closest_River                     commence
#  31209            COOKTOWN AIRPORT       Daintree                          2000-06-01
#  31011            CAIRNS AERO            Barron                            1941-05-01
#  32040            TOWNSVILLE AERO        Cleveland Bay                     1940-01-01
#  33119            MACKAY M.O             Pioneer                           1959-01-01
  

# Station_Number:
WdStn<-c('32004')

# Start date and End date:
datei='2016-06-01'
datef='2020-10-26'



library(lubridate)
# Specify start of the week
  # ____________________________________________________________________
  # Sunday: =6
  # Saturday: =5
  # Friday: =4
  # Thursday: =3
  # Wedndesday: =2
  # Tuesday: =1
  # Monday: = 7
 wday(datei,label=TRUE) # provides day of the week for 1st of December for years of interest
 
 sat=7

#Getting WIND data from our data base and melting table

library(reshape)
library(RODBC)
library(plyr)
library(dplyr)
library(lubridate)
library(doBy)
library(data.table)
library(rCAT)

ch<-odbcConnectAccess("C:/Users/jc246980/OneDrive - James Cook University/Current Projects/MMP/DATA_BASE/ACTFR.mdb") 

for (j in 1:length(WdStn)){
  w1<-sqlQuery(ch,paste("SELECT Station_Number, datestamp, Wind_speed_at_00_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_00_hours_Local_Time, Wind_speed_at_03_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_03_hours_Local_Time, Wind_speed_at_06_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_06_hours_Local_Time, Wind_speed_at_09_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_09_hours_Local_Time, Wind_speed_at_12_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_12_hours_Local_Time, Wind_speed_at_15_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_15_hours_Local_Time, Wind_speed_at_18_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_18_hours_Local_Time, Wind_speed_at_21_hours_Local_Time_measured_in_km_h, Quality_of_wind_speed_at_21_hours_Local_Time, Wind_direction_at_00_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_00_hours_Local_Time, Wind_direction_at_03_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_03_hours_Local_Time, Wind_direction_at_06_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_06_hours_Local_Time, Wind_direction_at_09_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_09_hours_Local_Time, Wind_direction_at_12_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_12_hours_Local_Time, Wind_direction_at_15_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_15_hours_Local_Time, Wind_direction_at_18_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_18_hours_Local_Time, Wind_direction_at_21_hours_Local_Time_measured_in_degrees, Quality_of_wind_direction_at_21_hours_Local_Time FROM wind_data WHERE Station_Number = '",WdStn[j],"' AND datestamp >= #",datei,"# AND datestamp <= #",datef,"# ORDER BY datestamp",sep=''))
  w2<-sqlQuery(ch,paste("SELECT Station_Number, Station_Name, Lat_DD, Log_DD FROM wind_info WHERE Station_Number = '",WdStn[j],"'",sep=''))
  wSPEED<-reshape2::melt(w1, id=1:2, measure.var=seq(3,17,2), var='speed_km_h')
  colnames(wSPEED)[3:4]<-c('time','speed_km_h')
  wSPEED$time<-substr(wSPEED$time,15,16)
  wSPEEDqc<-reshape2::melt(w1, id=1:2, measure.var=seq(4,18,2), var='QC_speed')
  colnames(wSPEEDqc)[3:4]<-c('time','QC_speed')
  wSPEEDqc$time<-substr(wSPEEDqc$time,26,27)
  wDIR<-reshape2::melt(w1, id=1:2, measure.var=seq(19,33,2), var='dir_degrees')
  colnames(wDIR)[3:4]<-c('time','dir_degrees')
  wDIR$time<-substr(wDIR$time,19,20)
  wDIRqc<-reshape2::melt(w1, id=1:2, measure.var=seq(20,34,2), var='QC_direction')
  colnames(wDIRqc)[3:4]<-c('time','QC_direction')
  wDIRqc$time<-substr(wDIRqc$time,30,31)
  ws<-merge(wSPEED,wSPEEDqc,by=c('Station_Number','datestamp','time'))
  wd<-merge(wDIR,wDIRqc,by=c('Station_Number','datestamp','time'))
  ww<-merge(ws,wd,by=c('Station_Number','datestamp','time'))
  
  #Checking wind data based on their QC criteria:
  # QUALITY FLAG DESCRIPTIONS
  
  # ____________________________________________________________________
  # Y: quality controlled and acceptable
  # N: not quality controlled
  # S: quality controlled and considered suspect
  # I: quality controlled and inconsistent with other known information
  # blank (X): no quality information available
  # ____________________________________________________________________
  
  #The only data excluded is that in category I
  head(ww$QC_speed)
  head(ww$QC_direction)
  if(length(which(ww$QC_speed=='I'))>0){ww<-ww[-c(which(ww$QC_speed=='I')),]}
  if(length(which(ww$QC_direction=='I'))>0){ww<-ww[-c(which(ww$QC_direction=='I')),]}
  if(length(which(ww$QC_speed=='S'))>0){ww<-ww[-c(which(ww$QC_speed=='S')),]}
  if(length(which(ww$QC_direction=='S'))>0){ww<-ww[-c(which(ww$QC_direction=='S')),]}
  
  # Wind data decomposition.
  # Adding components u and v to wind data table (angles must be in radians, so times pi/180)
  ww$u_comp_EW<- ww$speed_km_h*cos((ww$dir_degrees)*pi/180) # u-component, x-axis, E-W component
  ww$v_comp_NS<- ww$speed_km_h*sin((ww$dir_degrees)*pi/180) # v-component, y-axis, N-S component
  
  # Calculating mean wind data by simply averaging values within a day. This works better for the wind components
  wwU<-summaryBy(u_comp_EW ~ datestamp, data = ww, FUN = function(x) {c(Mean = mean(x, na.rm=T), N = length(which(!is.na(x))))})
  wwV<-summaryBy(v_comp_NS ~ datestamp, data = ww, FUN = function(x) {c(Mean = mean(x, na.rm=T), N = length(which(!is.na(x))))})
  
  
  ww <- merge(ww,wwU, by='datestamp')
  ww <- merge(ww,wwV, by='datestamp')
  
  # summarise to weekly
  tada=ww %>%group_by(Week = ceiling_date(datestamp, unit="week",week_start = sat))
  tada=tada%>%summarise(speed = mean(speed_km_h, na.rm=T),direction = mean(dir_degrees,na.rm=T), u_comp = mean(u_comp_EW,na.rm=T),v_comp = mean(v_comp_NS,na.rm=T),min_date = min(datestamp), max_date = max(datestamp))%>% mutate(weekdays(min_date), weekdays(max_date))
   
  tada=as.data.frame(tada)

  tada$vect_direction <- atan2(tada$v_comp,tada$u_comp)
  tada$vect_direction <- rad2deg(tada$vect_direction)
  convert_360 <- function(x) { 
  x<-x[!is.na(x)]
  x[x < 0] <- 360 + x[x < 0]
  return(x)
  }
  tada$vect_direction=lapply(X=tada$vect_direction,convert_360)
 
  # Sort out decimal places
  tada$speed <- round(tada$speed,4)
  tada$direction <- round(tada$direction,4)
  
  # Fixing column names
  colnames(ww)[c(5,7)]<-c("u_comp_EW","v_comp_NS")
  
  # Adding Lat & Long plus Stn Name and Number
  tada$Lat_DD<-rep(w2$Lat_DD,length(tada[,1]))
  tada$Long_DD<-rep(w2$Log_DD,length(tada[,1]))
  tada$StnNum<-rep(w2$Station_Number,length(tada[,1]))
  tada$StnName<-rep(w2$Station_Name,length(tada[,1]))
  
  if (j==1){
    WW<-tada
  } else {
    WW<-rbind(WW,tada)
  }
}


NESP.dir="C:/Users/jc246980/Documents/Current projects/NESP/Wind"
fwrite(WW,paste(NESP.dir,"Weekly_wind_data_CARDWELL.csv",sep=""),dateTimeAs="write.csv") 

