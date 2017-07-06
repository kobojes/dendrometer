# # Load Dendrometer data(cleaned hourly values)
### split growth and WB components of increment variation according to Zweifel (xxxx)
### Comparison of Sites
### 
# Starting dates: 
# SF1 2012-03-23 17:00:00
# SF2 2012-03-22 19:00:00
# SF3 2012-05-23 16:00:00
# SF4 2012-05-24 17:00:00
# SF5 2012-05-25 13:00:00

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#loads required R-packages
library(zoo)
library(pastecs)
library(plyr)
library(ggplot2)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Clears workspace
#Sets working directory
setwd("H:/Data/Matsch_Catchment/SF/dendro files for R")
remove(list = ls())
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show content 
list.files(path = "H:/Data/Matsch_Catchment/SF/dendro files for R")

# Load manualy cleaned dendrometer
data_dendro_SF1 <- read.csv("Dendro1_2012_2013_hourly_cleaned.csv",header = T,sep=",")
data_dendro_SF2 <- read.csv("Dendro2_2012_2013_hourly_cleaned.csv",header = T,sep=",")
data_dendro_SF3 <- read.csv("Dendro3_2012_2013_hourly_cleaned.csv",header = T,sep=",")
data_dendro_SF4 <- read.csv("Dendro4_2012_2013_hourly_cleaned.csv",header = T,sep=",")
data_dendro_SF5 <- read.csv("Dendro5_2012_2013_hourly_cleaned.csv",header = T,sep=",")

# assign shorter column names 
colnames(data_dendro_SF1) <- c("SF1_Date_Time","SF1_Lade2_INCacc","SF1_Lade4_INCacc","SF1_Lade5_INCacc","SF1_Lade6_INCacc",
                              "SF1_Lade2_INCsv","SF1_Lade4_INCsv","SF1_Lade5_INCsv","SF1_Lade6_INCsv",
                              "SF1_Lade2_Td","SF1_Lade4_Td","SF1_Lade5_Td","SF1_Lade6_Td")  

colnames(data_dendro_SF2) <- c("SF2_Date_Time","SF2_Lade1_INCacc","SF2_Lade4_INCacc","SF2_Lade5_INCacc","SF2_Lade7_INCacc",
                              "SF2_Lade1_INCsv","SF2_Lade4_INCsv","SF2_Lade5_INCsv","SF2_Lade7_INCsv",
                              "SF2_Lade1_Td","SF2_Lade4_Td","SF2_Lade5_Td","SF2_Lade7_Td")  

colnames(data_dendro_SF3) <- c("SF3_Date_Time","SF3_Lade3_INCacc","SF3_Lade4_INCacc","SF3_Lade5_INCacc","SF3_Lade6_INCacc",
                              "SF3_Lade3_INCsv","SF3_Lade4_INCsv","SF3_Lade5_INCsv","SF3_Lade6_INCsv",
                              "SF3_Lade3_Td","SF3_Lade4_Td","SF3_Lade5_Td","SF3_Lade6_Td")  

colnames(data_dendro_SF4) <- c("SF4_Date_Time","SF4_Lade1_INCacc","SF4_Pice1_INCacc","SF4_Lade2_INCacc","SF4_Pice2_INCacc",
                               "SF4_Lade3_INCacc","SF4_Pice3_INCacc","SF4_Lade4_INCacc","SF4_Pice4_INCacc",
                               "SF4_Lade1_INCsv","SF4_Pice1_INCsv","SF4_Lade2_INCsv","SF4_Pice2_INCsv",
                               "SF4_Lade3_INCsv","SF4_Pice3_INCsv","SF4_Lade4_INCsv","SF4_Pice4_INCsv")

colnames(data_dendro_SF5) <- c("SF5_Date_Time","SF5_Lade1_INCacc","SF5_Lade3_INCacc","SF5_Lade4_INCacc","SF5_Pice4_INCacc",
                               "SF5_Pice1_INCacc","SF5_Pice2_INCacc","SF5_Lade2_INCacc","SF5_Pice3_INCacc",
                               "SF5_Lade1_INCsv","SF5_Lade3_INCsv","SF5_Lade4_INCsv","SF5_Pice4_INCsv",
                               "SF5_Pice1_INCsv","SF5_Pice2_INCsv","SF5_Lade2_INCsv","SF5_Pice3_INCsv")

# convert dates to POSIXct-format
data_dendro_SF1$Date_Time_px <- as.POSIXct(data_dendro_SF1$SF1_Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") ;
data_dendro_SF2$Date_Time_px <- as.POSIXct(data_dendro_SF2$SF2_Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") ;
data_dendro_SF3$Date_Time_px <- as.POSIXct(data_dendro_SF3$SF3_Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") ;
data_dendro_SF4$Date_Time_px <- as.POSIXct(data_dendro_SF4$SF4_Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") ;
data_dendro_SF5$Date_Time_px <- as.POSIXct(data_dendro_SF5$SF5_Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") ;

# summary(data_dendro_SF3)
start.SF1 <- data_dendro_SF1$Date_Time_px[1]
start.SF2 <- data_dendro_SF2$Date_Time_px[1]
start.SF3 <- data_dendro_SF3$Date_Time_px[1]
start.SF4 <- data_dendro_SF4$Date_Time_px[1]
start.SF5 <- data_dendro_SF5$Date_Time_px[1]


# Merge data
data_dendro_all <- merge(data_dendro_SF1,data_dendro_SF2,by="Date_Time_px",all=TRUE)
data_dendro_all <- merge(data_dendro_all,data_dendro_SF3,by="Date_Time_px",all=TRUE)
data_dendro_all <- merge(data_dendro_all,data_dendro_SF4,by="Date_Time_px",all=TRUE)
data_dendro_all <- merge(data_dendro_all,data_dendro_SF5,by="Date_Time_px",all=TRUE)

# Creates DOY (including hours and minutes as numbers from 0 to 1)
data_dendro_all$doy <- strptime(data_dendro_all$Date_Time_px, "%Y-%m-%d %H:%M")$yday+1
hours <- strptime(data_dendro_all$Date_Time_px, "%Y-%m-%d %H:%M")$hour/24
min <- strptime(data_dendro_all$Date_Time_px, "%Y-%m-%d %H:%M")$min/60/24
data_dendro_all$doytime <- data_dendro_all$doy+hours+min

# Save data_dendro_all dataset
# outfile.c <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_circumference.csv")  
# write.csv(data_dendro_all,file=outfile.c)

# Load data_dendro_all dataset
data_dendro_all <- read.csv("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_circumference.csv",header = T,sep=",")
data_dendro_all <- data_dendro_all [-1]
data_dendro_all$Date_Time_px <- as.POSIXct(data_dendro_all$Date_Time_px,format="%Y-%m-%d %H:%M", tz="GMT")

names(data_dendro_r_all)
str(data_dendro_all)

data_dendro_r_all <- data_dendro_all[c(3:10,16:23,29:36,42:57,59:74)]/(2*pi)     # transforms circumference to radius
data_dendro_r_all <- cbind(data_dendro_r_all,data_dendro_all[c(1,2,11:15,24:28,37:41,58,75:76)]) 

# Save data_dendro_r_all dataset
# outfile.r <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius.csv")  
# write.csv(data_dendro_r_all,file=outfile.r)

# Load data_dendro_r_all dataset
data_dendro_r_all <- read.csv("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius.csv",header = T,sep=",")
data_dendro_r_all <- data_dendro_r_all [-1]
data_dendro_r_all$Date_Time_px <- as.POSIXct(data_dendro_r_all$Date_Time_px,format="%Y-%m-%d %H:%M", tz="GMT")

### Figures dendrometer-data hourly (starting circumference increment-values set to 0) ####
# str(data_dendro_all) 

# Min- and max-dates for x-axis (earliest measurements: 2012-03-22)
range(data_dendro_all$Date_Time_px,na.rm=TRUE)
start_date <- as.POSIXct(as.Date("2012-03-20", format="%Y-%m-%d"))
end_date <- as.POSIXct(as.Date("2013-11-15", format="%Y-%m-%d"))

jan1_2013 <- as.POSIXct(as.Date("2013-01-01", format="%Y-%m-%d"))
start_date.axis <- as.POSIXct(as.Date("2012-04-01", format="%Y-%m-%d"))

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5) ####
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_circ_SF1-3.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF1
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF1_Lade2_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF1_Lade4_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF1_Lade5_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF1_Lade6_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
legend("topleft",c("L2 (DBH 24.5 cm)","L4 (DBH 35.3 cm)","L5 (DBH 20.7 cm)","L6 (DBH 53.5 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF1")
title("Dendrometer (hourly) SF1")

# Plot hourly dendrometer-data Site SF2
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF2_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF2_Lade4_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF2_Lade5_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF2_Lade7_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 49.2 cm)","L4 (DBH 37.2 cm)","L5 (DBH 43.1 cm)","L7 (DBH 29.9 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF2")
title("Dendrometer (hourly) SF2")

# Plot hourly dendrometer-data Site SF3
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF3_Lade3_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF3_Lade4_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF3_Lade5_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF3_Lade6_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L3 (DBH 56.0 cm)","L4 (DBH 44.6 cm)","L5 (DBH 36.0 cm)","L6 (DBH 31.2 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF3")
title("Dendrometer (hourly) SF3")

# dev.off()

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5)
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_circ_SF4-5.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF4 Larix
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Lade2_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Lade3_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Lade4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 65.3 cm)","L2 (DBH 42.3 cm)","L3 (DBH 28.3 cm)","L4 (DBH 20.8 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF4")
title("Dendrometer (hourly) SF4 Larix")

# Plot hourly dendrometer-data Site SF5 Larix
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Lade2_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Lade3_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Lade4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 37.9 cm)","L2 (DBH 33.1 cm)","L3 (DBH 23.1 cm)","L4 (DBH 58.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF5")
title("Dendrometer (hourly) SF5 Larix")

# Plot hourly dendrometer-data Site SF4 P.cembra
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Pice1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Pice2_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Pice3_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF4_Pice4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (DBH 30.2 cm)","Pc2 (DBH 36.0 cm)","Pc3 (DBH 31.2 cm)","Pc4 (DBH 23.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF4")
title("Dendrometer (hourly) SF4 P.cembra")

# Plot hourly dendrometer-data Site SF5 P.cembra
plot(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Pice1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,40))
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Pice2_INCacc,col=2)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Pice3_INCacc,col=3)
lines(data_dendro_all$Date_Time_px,data_dendro_all$SF5_Pice4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (DBH 25.8 cm)","Pc2 (DBH 57.3 cm)","Pc3 (DBH 36.3 cm)","Pc4 (DBH 19.4 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF5")
title("Dendrometer (hourly) SF5 P.cembra")

# dev.off()


# figures Radius increment #####
summary(data_dendro_r_all)

attach(data_dendro_r_all)

# Save following Figures (1st page SF1-SF3, 2nd page SF4-SF5)
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_radius_SF1-3.emf", height=8, width=12, pointsize=15)
# par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF1
plot(Date_Time_px,SF1_Lade2_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF1_Lade4_INCacc,col=2)
lines(Date_Time_px,SF1_Lade5_INCacc,col=3)
lines(Date_Time_px,SF1_Lade6_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
legend("topleft",c("L2 (DBH 24.5 cm)","L4 (DBH 35.3 cm)","L5 (DBH 20.7 cm)","L6 (DBH 53.5 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF1")

# Plot hourly dendrometer-data Site SF2
plot(Date_Time_px,SF2_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF2_Lade4_INCacc,col=2)
lines(Date_Time_px,SF2_Lade5_INCacc,col=3)
lines(Date_Time_px,SF2_Lade7_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 49.2 cm)","L4 (DBH 37.2 cm)","L5 (DBH 43.1 cm)","L7 (DBH 29.9 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF2")

# Plot hourly dendrometer-data Site SF3
plot(Date_Time_px,SF3_Lade3_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF3_Lade4_INCacc,col=2)
lines(Date_Time_px,SF3_Lade5_INCacc,col=3)
lines(Date_Time_px,SF3_Lade6_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L3 (DBH 56.0 cm)","L4 (DBH 44.6 cm)","L5 (DBH 36.0 cm)","L6 (DBH 31.2 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF3")

# dev.off()

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5)
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_radius_SF4-5.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF4 Larix
plot(Date_Time_px,SF4_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF4_Lade2_INCacc,col=2)
lines(Date_Time_px,SF4_Lade3_INCacc,col=3)
lines(Date_Time_px,SF4_Lade4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 65.3 cm)","L2 (DBH 42.3 cm)","L3 (DBH 28.3 cm)","L4 (DBH 20.8 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF4 Larix")

# Plot hourly dendrometer-data Site SF5 Larix
plot(Date_Time_px,SF5_Lade1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF5_Lade2_INCacc,col=2)
lines(Date_Time_px,SF5_Lade3_INCacc,col=3)
lines(Date_Time_px,SF5_Lade4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (DBH 37.9 cm)","L2 (DBH 33.1 cm)","L3 (DBH 23.1 cm)","L4 (DBH 58.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF5 Larix")

# Plot hourly dendrometer-data Site SF4 P.cembra
plot(Date_Time_px,SF4_Pice1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF4_Pice2_INCacc,col=2)
lines(Date_Time_px,SF4_Pice3_INCacc,col=3)
lines(Date_Time_px,SF4_Pice4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (DBH 30.2 cm)","Pc2 (DBH 36.0 cm)","Pc3 (DBH 31.2 cm)","Pc4 (DBH 23.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF4 P.cembra")

# Plot hourly dendrometer-data Site SF5 P.cembra
plot(Date_Time_px,SF5_Pice1_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_date,end_date),ylim=c(-1,6))
lines(Date_Time_px,SF5_Pice2_INCacc,col=2)
lines(Date_Time_px,SF5_Pice3_INCacc,col=3)
lines(Date_Time_px,SF5_Pice4_INCacc,col=4)
abline(h=0); abline(v=jan1_2013,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (DBH 25.8 cm)","Pc2 (DBH 57.3 cm)","Pc3 (DBH 36.3 cm)","Pc4 (DBH 19.4 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF5 P.cembra")

# dev.off()
detach(data_dendro_r_all)

######################################################################################################################################

##  subset both growth periods/years, set all dates ######
# str(data_dendro_r_all); tail(data_dendro_r_all$Date_Time_px,3)
# names(data_dendro_r_all)

start_2012.1<-as.POSIXct(as.Date("2012-03-23 0:10", format="%Y-%m-%d %H:%M"))
start_2012.2<-as.POSIXct(as.Date("2012-05-26 0:10", format="%Y-%m-%d %H:%M"))
start_2013<-as.POSIXct(as.Date("2013-04-10 0:10", format="%Y-%m-%d %H:%M"))

end_2012<-as.POSIXct(as.Date("2012-11-15 0:00", format="%Y-%m-%d %H:%M"))
end_2013<-as.POSIXct(as.Date("2013-11-15 0:00", format="%Y-%m-%d %H:%M"))

match(start_2012.1,data_dendro_r_all$Date_Time_px)
match(start_2012.2,data_dendro_r_all$Date_Time_px)
match(start_2013,data_dendro_r_all$Date_Time_px)
match(end_2012,data_dendro_r_all$Date_Time_px)
match(end_2013,data_dendro_r_all$Date_Time_px)

# data_dendro2012.1 <- data_dendro_r_all[6:5693,c(3:6,16:19,29:32,42:49,59:66)] # column numbers 
data_dendro2012.1 <- data_dendro_r_all[6:5693,c(1:4,9:12,17:20,25:32,41:48)]
date2012.1 <- data_dendro_r_all[6:5693,57]
doy2012.1 <- data_dendro_r_all[6:5693,75]
# data_dendro2012.2 <- data_dendro_r_all[1542:5693,c(3:6,16:19,29:32,42:49,59:66)]
data_dendro2012.2 <- data_dendro_r_all[1542:5693,c(1:4,9:12,17:20,25:32,41:48)]
date2012.2 <- data_dendro_r_all[1542:5693,57]
doy2012.2 <- data_dendro_r_all[1542:5693,75]
# data_dendro2013 <- data_dendro_r_all[9198:14453,c(3:6,16:19,29:32,42:49,59:66)]
data_dendro2013 <- data_dendro_r_all[9198:14453,c(1:4,9:12,17:20,25:32,41:48)]
date2013 <- data_dendro_r_all[9198:14453,57]
doy2013 <- data_dendro_r_all[9198:14453,75]

# dataset data_dendro2012.1: set starting values to 0 
data_dendro2012.1[1,]; 
data_dendro2012.1[1,c(1:4,9:28)] <- 0 
data_dendro2012.1.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2012.1)   # substract starting value from rest of the column
data_dendro2012.1[1,c(1:4,9:28)] <- NA 
data_dendro2012.1.0[1,c(1:4,9:28)] <- NA 
data_dendro2012.1.0 <- data.frame(cbind(data_dendro2012.1.0,doy2012.1)); colnames(data_dendro2012.1.0)[29] <- "DOY"
# zoo_data_dendro2012.1.0 <- zoo(data_dendro2012.1.0,date2012.1)
data_dendro2012.1.0 <- data.frame(cbind(data_dendro2012.1.0,date2012.1))
colnames(data_dendro2012.1.0)[30] <- "Date_Time_px"

# Comparison of dendrometerdata with same starting point
# dataset data_dendro2012.2: set starting values to 0 and plot selection of trees from all sites
data_dendro2012.2[1,]; 
data_dendro2012.2[1,13:16] <- 0 
data_dendro2012.2.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2012.2)   # substract starting value from rest of the column
data_dendro2012.2[1,13:16] <- NA 
data_dendro2012.2.0[1,13:16] <- NA 
data_dendro2012.2.0 <- data.frame(cbind(data_dendro2012.2.0,doy2012.2)); colnames(data_dendro2012.2.0)[29] <- "DOY"
# zoo_data_dendro2012.2.0 <- zoo(data_dendro2012.2.0,date2012.2)
data_dendro2012.2.0 <- data.frame(cbind(data_dendro2012.2.0,date2012.2))
colnames(data_dendro2012.2.0)[30] <- "Date_Time_px"

# dataset data_dendro2013: set starting values to 0 and plot selection of trees from all sites
data_dendro2013[1,]; 
data_dendro2013.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2013)   # substract starting value from rest of the column
data_dendro2013.0 <- data.frame(cbind(data_dendro2013.0,doy2013)); colnames(data_dendro2013.0)[29] <- "DOY"
# zoo_data_dendro2013.0 <- zoo(data_dendro2013.0,data_dendro2013.date)
data_dendro2013.0 <- data.frame(cbind(data_dendro2013.0,date2013))
colnames(data_dendro2013.0)[30] <- "Date_Time_px"

# names(data_dendro2012.1)
# summary(data_dendro2012.1.0)
# str(data_dendro2012.1.0)

# Figures radius increment yearly all trees ####
# Save following Figures 
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_yearly_radius.emf", height=8, width=12, pointsize=15)
# par(mfcol=c(1,3), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot variables of hourly dendrometer-data data_dendro2012.1.0 (2012 values starting at 2012-03-23, starting at 0)

plot(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF1_Lade2_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_2012.1,end_2012),ylim=c(-1,4.5))
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF1_Lade4_INCacc,col=1,lty=2)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF1_Lade5_INCacc,col=1,lty=3)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF1_Lade6_INCacc,col=1,lty=4)

lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF2_Lade1_INCacc,col=2,lty=1)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF2_Lade4_INCacc,col=2,lty=2)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF2_Lade5_INCacc,col=2,lty=3)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF2_Lade7_INCacc,col=2,lty=4)

lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF3_Lade3_INCacc,col=3,lty=1)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF3_Lade4_INCacc,col=3,lty=2)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF3_Lade5_INCacc,col=3,lty=3)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF3_Lade6_INCacc,col=3,lty=4)

lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF4_Lade1_INCacc,col=4,lty=1)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF4_Lade2_INCacc,col=4,lty=2)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF4_Lade3_INCacc,col=4,lty=3)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF4_Lade4_INCacc,col=4,lty=4)

lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF5_Lade1_INCacc,col=5,lty=1)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF5_Lade2_INCacc,col=5,lty=2)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF5_Lade3_INCacc,col=5,lty=3)
lines(data_dendro2012.1.0$Date_Time_px,data_dendro2012.1.0$SF5_Lade4_INCacc,col=5,lty=4)

abline(h=0)
axis.POSIXct(1, at=seq(start_2012.2,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3","SF4","SF5"),
       pch=15,col=c(1,2,3,4,5),cex=0.8,bty="n",title="Site")
title("Dendrometer (hourly) 2012 all trees + sites")


# Plot variables of hourly dendrometer-data data_dendro2012.2.0 (2012 values same starting point in May for better comparability)

plot(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF1_Lade2_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_2012.2,end_2012),ylim=c(-1,4.5))
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF1_Lade4_INCacc,col=1,lty=2)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF1_Lade5_INCacc,col=1,lty=3)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF1_Lade6_INCacc,col=1,lty=4)

lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF2_Lade1_INCacc,col=2,lty=1)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF2_Lade4_INCacc,col=2,lty=2)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF2_Lade5_INCacc,col=2,lty=3)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF2_Lade7_INCacc,col=2,lty=4)

lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF3_Lade3_INCacc,col=3,lty=1)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF3_Lade4_INCacc,col=3,lty=2)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF3_Lade5_INCacc,col=3,lty=3)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF3_Lade6_INCacc,col=3,lty=4)

lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF4_Lade1_INCacc,col=4,lty=1)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF4_Lade2_INCacc,col=4,lty=2)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF4_Lade3_INCacc,col=4,lty=3)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF4_Lade4_INCacc,col=4,lty=4)

lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF5_Lade1_INCacc,col=5,lty=1)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF5_Lade2_INCacc,col=5,lty=2)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF5_Lade3_INCacc,col=5,lty=3)
lines(data_dendro2012.2.0$Date_Time_px,data_dendro2012.2.0$SF5_Lade4_INCacc,col=5,lty=4)

abline(h=0)
axis.POSIXct(1, at=seq(start_2012.2,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3","SF4","SF5"),
       pch=15,col=c(1,2,3,4,5),cex=0.8,bty="n",title="Site")
title("Dendrometer (hourly) 2012 all trees + sites (same starting point)")


# Plot variables of hourly dendrometer-data data_dendro2013.0 (2013 values starting at 2013-04-10)

plot(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF1_Lade2_INCacc,type="l", col=1, xaxt="n",
     xlab="Date",ylab="radius variation (mm)", xlim=c(start_2013,end_2013),ylim=c(-1,4.5))
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF1_Lade4_INCacc,col=1,lty=2)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF1_Lade5_INCacc,col=1,lty=3)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF1_Lade6_INCacc,col=1,lty=4)

lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF2_Lade1_INCacc,col=2,lty=1)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF2_Lade4_INCacc,col=2,lty=2)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF2_Lade5_INCacc,col=2,lty=3)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF2_Lade7_INCacc,col=2,lty=4)

lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF3_Lade3_INCacc,col=3,lty=1)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF3_Lade4_INCacc,col=3,lty=2)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF3_Lade5_INCacc,col=3,lty=3)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF3_Lade6_INCacc,col=3,lty=4)

lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF4_Lade1_INCacc,col=4,lty=1)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF4_Lade2_INCacc,col=4,lty=2)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF4_Lade3_INCacc,col=4,lty=3)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF4_Lade4_INCacc,col=4,lty=4)

lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF5_Lade1_INCacc,col=5,lty=1)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF5_Lade2_INCacc,col=5,lty=2)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF5_Lade3_INCacc,col=5,lty=3)
lines(data_dendro2013.0$Date_Time_px,data_dendro2013.0$SF5_Lade4_INCacc,col=5,lty=4)

abline(h=0)
axis.POSIXct(1, at=seq(start_2013,end_2013, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3","SF4","SF5"),
       pch=15,col=c(1,2,3,4,5),cex=0.8,bty="n",title="Site")
title("Dendrometer (hourly) 2013 all trees + sites")

# dev.off()


# Save yearly datasets #####
# outfile.2012.1.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2012_1_0.csv")  
# write.csv(data_dendro2012.1.0,file=outfile.2012.1.0)
# 
# outfile.2012.2.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2012_2_0.csv")  
# write.csv(data_dendro2012.2.0,file=outfile.2012.2.0)
# 
# outfile.2013.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2013_0.csv")  
# write.csv(data_dendro2013.0,file=outfile.2013.0)

#######################################################################################################################

# 2 weekly files of accumulated dendrometer-radius data 
# max values: SF1: L6, SF2: L4+L5, SF3: L3, SF4: L3, SF5: L2+4

# head(data_dendro_r_all$Date_Time_px,10)
# tail(data_dendro_r_all$Date_Time_px,10)
start_date.axis2 <- as.POSIXct(as.Date("2012-03-20", format="%Y-%m-%d"))

# create a 2-weekly numbering variable for later loops
n_2weekly <- trunc(length(data_dendro_r_all$Date_Time_px)/14/24)       # number of 2 week periods
seq_2weekly <- seq(1,n_2weekly,by=1)                                   # vector 1 to n_2weekly
idx_2week <- rep(seq_2weekly,each=(24*14))                             # create vector fitting to date/time column
assign("idx_2week",c(1,1,1,1,1,1,idx_2week))                         
diff <- length(data_dendro_r_all$Date_Time_px)-length(idx_2week)       # difference between date/time column and new vector
r_2weekly <- rep(max(idx_2week+1), each=diff)                          # create vector for rediual days/time 
assign("idx_2week",c(idx_2week,r_2weekly))                             # add residual vector

data_dendro_r_all <- cbind(data_dendro_r_all,idx_2week)

# names(data_dendro_r_all)

all2weeks <- unique(data_dendro_r_all$idx_2week)

# 2-weekly figures for site SF1 ####
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF1")
for (i in 1:44){
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF1_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=14)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF1_Lade6_INCacc,type="l",col=4,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(1:4)],na.rm = TRUE),max(data_dendro_r_all.i[,c(1:4)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF1_Lade2_INCacc,col=1)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF1_Lade4_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF1_Lade5_INCacc,col=3)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L2","L4","L5","L6"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF1 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF2 ####
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF2")
for (i in 1:44){
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF2_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF2_Lade1_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(9:12)],na.rm = TRUE),max(data_dendro_r_all.i[,c(9:12)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF2_Lade4_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF2_Lade5_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF2_Lade7_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L4","L5","L7"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF2 N°",idx_2week.i,sep=""))
  dev.off()
}

data_dendro_r_all$idx_2week[match(start.SF3,data_dendro_r_all$Date_Time_px)]

# 2-weekly figures for site SF3 ####                                             => check min/max for SF3 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF3")
for (i in 5:43){                                                                   # i 5-43 for later start in 2012 and earlier end in 2013
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF3_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF3_Lade3_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(17:20)],na.rm = TRUE),max(data_dendro_r_all.i[,c(17:20)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF3_Lade4_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF3_Lade5_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF3_Lade6_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L2","L4","L5","L6"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF3 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF4 ####                                             => check min/max for SF4 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF4")           # => exclude i = 1-4,24,>42
for (i in 25:41){                                                               
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Lade1_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(25,27,29,31)],na.rm = TRUE),max(data_dendro_r_all.i[,c(25,27,29,31)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Lade2_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Lade3_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Lade4_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L2","L3","L4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 5:18){                                                                
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Lade1_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(41:43,47)],na.rm = TRUE),max(data_dendro_r_all.i[,c(41:43,47)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Lade2_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Lade3_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Lade4_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L2","L3","L4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF5 N°",idx_2week.i,sep=""))
  dev.off()
}



# site 4 and 5 for Pinus cembra
# 2-weekly figures for site SF4 ####                                             => check min/max for SF4 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF4")           # => exclude i = 1-4,24,>42
for (i in 25:41){                                                               
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_acc_Pice_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Pice1_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(26,28,30,32)],na.rm = TRUE),max(data_dendro_r_all.i[,c(26,28,30,32)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Pice2_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Pice3_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Pice4_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Pice1","Pice2","Pice3","Pice4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 20:23){                                                                
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_acc_Pice_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Pice1_INCacc,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(44:46,48)],na.rm = TRUE),max(data_dendro_r_all.i[,c(44:46,48)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Pice2_INCacc,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Pice3_INCacc,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Pice4_INCacc,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Pice1","Pice2","Pice3","Pice4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF5 N°",idx_2week.i,sep=""))
  dev.off()
}



# 2 weekly files of dendrometer-radius data(hourly differences) ####
# max values: SF1: L6, SF2: L4+L5, SF3: L3, SF4: L3, SF5: L2+4

# 2-weekly figures for site SF1 ####
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF1")
for (i in 1:44){
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF1_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=14)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF1_Lade6_INCsv,type="l",col=4,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(5:8)],na.rm = TRUE),max(data_dendro_r_all.i[,c(5:8)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF1_Lade2_INCsv,col=1)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF1_Lade4_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF1_Lade5_INCsv,col=3)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L2","L4","L5","L6"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF1 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF2 ####
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF2")
for (i in 1:44){
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF2_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF2_Lade1_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(13:16)],na.rm = TRUE),max(data_dendro_r_all.i[,c(13:16)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF2_Lade4_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF2_Lade5_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF2_Lade7_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L4","L5","L7"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF2 N°",idx_2week.i,sep=""))
  dev.off()
}

# data_dendro_r_all$idx_2week[match(start.SF3,data_dendro_r_all$Date_Time_px)]

# 2-weekly figures for site SF3 ####                                             => check min/max for SF3 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF3")
for (i in 5:43){                                                                   # i 5-43 for later start in 2012 and earlier end in 2013
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF3_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF3_Lade3_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(21:24)],na.rm = TRUE),max(data_dendro_r_all.i[,c(21:24)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF3_Lade4_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF3_Lade5_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF3_Lade6_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L2","L4","L5","L6"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF3 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF4 ####                                             => check min/max for SF4 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF4")           # => exclude i = 1-4,24,>42
for (i in 25:41){                                                               
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Lade1_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(33,35,37,39)],na.rm = TRUE),max(data_dendro_r_all.i[,c(33,35,37,39)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Lade2_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Lade3_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Lade4_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L2","L3","L4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 26:41){                                                                
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Lade1_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(49:51,55)],na.rm = TRUE),max(data_dendro_r_all.i[,c(49:51,55)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Lade2_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Lade3_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Lade4_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L2","L3","L4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF5 N°",idx_2week.i,sep=""))
  dev.off()
}


# site 4 and 5 for Pinus cembra ####
# 2-weekly figures for site SF4                                              => check min/max for SF4 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF4")           # => exclude i = 1-4,24,>42
for (i in 25:41){                                                               
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_diff_Pice_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Pice1_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(34,36,38,40)],na.rm = TRUE),max(data_dendro_r_all.i[,c(34,36,38,40)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF4_Pice2_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Pice3_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF4_Pice4_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Pice1","Pice2","Pice3","Pice4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 26:41){                                                                
  idx_2week.i <- all2weeks[i]
  data_dendro_r_all.i <- data_dendro_r_all[data_dendro_r_all$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_diff_Pice_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Pice1_INCsv,type="l",col=1,xaxt="n",
       xlab="Date",ylab="radius variation (mm)",
       ylim=c(min(data_dendro_r_all.i[,c(52:54,56)],na.rm = TRUE),max(data_dendro_r_all.i[,c(52:54,56)],na.rm = TRUE)))
  lines(x=data_dendro_r_all.i$Date_Time_px,y=data_dendro_r_all.i$SF5_Pice2_INCsv,col=2)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Pice3_INCsv,col=3)
  lines(data_dendro_r_all.i$Date_Time_px,data_dendro_r_all.i$SF5_Pice4_INCsv,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Pice1","Pice2","Pice3","Pice4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF5 N°",idx_2week.i,sep=""))
  dev.off()
}

# Overall and seasonal plots with ggplot2 ####
# Order Trees at Sites according to radius diameter
# MHD (diameter at measuring height): (D)... with dendrometer, => order largest to smallest
# SF1: L1 34.4, L2(D) 24.5, L3 38.8, L4(D) 35.3, L5(D) 20.7, L6(D) 53.5 => L6 - L4 - L2 - L5
# SF2: L1(D) 49.2, L2 43.9, L3 39.8, L4(D) 37.2, L5(D) 43.1, L7(D) 29.9 => L1 - L5 - L4 - L7
# SF3: L1 32.1, L2 54.1, L3(D) 56.0, L4(D) 44.6, L5(D) 36.0, L6(D) 31.2 => L3 - L4 - L5 - L6
# SF4_L (alle D): L1 65.3, L2 42.3, L3 28.3, L4 20.8                    => L1 - L2 - L3 - L4
# SF5_L (alle D): L1 37.9, L2 33.1, L3 23.2, L4 58.6                    => L4 - L1 - L2 - L3  

# SF4_Z (alle D): Z1 30.2, Z2 36.0, Z3 31.2, Z4 23.6                    => Z2 - Z3 - Z1 - Z4
# SF5_Z (alle D): Z1 25.8, Z2 57.3, Z3 36.3, Z4 19.4                    => Z2 - Z3 - Z1 - Z4  

names(data_dendro_r_all)
# renaming trees: L...Larch, Z...Pinus cembra, dia1...largest diameter at site, dia4...smallest diameter at site

wide_dendro_L_all <- data_dendro_r_all[c(57,4,9,17,25,43,2,11,18,27,41,1,10,19,29,47,3,12,20,31,42)] 
long_dendro_L_all <- reshape(wide_dendro_L_all,varying=c(2:6,7:11,12:16,17:21),v.names="Rad_inc",direction="long")

wide_dendro_all <- data_dendro_r_all[c(57,4,9,17,25,43,2,11,18,27,41,1,10,19,29,47,3,12,20,31,42,28,46,30,48,26,45,32,44)] 
long_dendro_all <- reshape(wide_dendro_all,varying=c(2:6,7:11,12:16,17:21,22:23,24:25,26:27,28:29),v.names="Rad_inc",direction="long")

long_dendro_L_all$Site <- rep(1:5, each = 14544,times = 4)
long_dendro_L_all$Tree_dia <- rep(1:4, each = 72720)

Site_Z1 <- rep(1:5, each = 14544,times = 4)
Site_Z2 <- rep(4:5, each = 14544,times = 4)
long_dendro_all$Site <- c(Site_Z1,Site_Z2)

Tree_dia_Z1 <- rep(1:4, each = 72720)
Tree_dia_Z2 <- rep(1:4, each = 29088)
long_dendro_all$Tree_dia <- c(Tree_dia_Z1,Tree_dia_Z2)

long_dendro_all$Treesp[1:407232] <- "0"
long_dendro_all$Treesp[1:290880] <- "Larix decidua" 
long_dendro_all$Treesp[290881:407232] <- "Pinus cembra" 

# Test
# long_dendro_L_all[218160:218190,]
# long_dendro_all[290875:290885,]

# figure using ggplot
names(long_dendro_L_all)
long_dendro_L_all$fSite <- factor(long_dendro_L_all$Site)
long_dendro_L_all$fTree_dia <- factor(long_dendro_L_all$Tree_dia)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/seasonal_min_mean_max_figures/gg_hourly2012_2013.emf", height=16, width=12, pointsize=14)
qplot(Date_Time_px,Rad_inc, geom="line",facets=fSite~.,data=long_dendro_L_all, colour=fTree_dia, main="Radius variation: hourly 2012+2013")+
  theme_bw() + scale_y_continuous("Radius variation (mm)") + labs(colour = "Tree: (1=thickest, 4=thinnest/site):")+
  theme(legend.position="bottom")+geom_line(size=1)     # => to-do: change x-axis title
dev.off()       

