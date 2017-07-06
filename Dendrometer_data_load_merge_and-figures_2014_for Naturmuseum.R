# # Load Dendrometer data(cleaned hourly values)
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
library(reshape2)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Clears workspace
#Sets working directory
setwd("H:/Data/Matsch_Catchment/SF/dendro files for R")
remove(list = ls())
Sys.setenv(TZ = "GMT") # sets the system time to GMT

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show content 
list.files(path = "H:/Data/Matsch_Catchment/SF/dendro files for R")

# Load cleaned dendrometer files (from script)
dendro_abs <- read.csv("dendro_all_cf_abs.csv",header = T,sep=",")
dendro_diff <- read.csv("dendro_all_cf_diff.csv",header = T,sep=",")
dendro_temp <- read.csv("dendro_all_temp.csv",header = T,sep=",")
summary(dendro_abs)

# convert dates to POSIXct-format
dendro_abs$Date_Time.px <- as.POSIXct(dendro_abs$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_diff$Date_Time.px <- as.POSIXct(dendro_diff$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT")
dendro_temp$Date_Time.px <- as.POSIXct(dendro_temp$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT")

# Creates DOY (including hours and minutes as numbers from 0 to 1)
dendro_abs$doy <- strptime(dendro_abs$Date_Time.px, "%Y-%m-%d %H:%M")$yday+1
hours <- strptime(dendro_abs$Date_Time.px, "%Y-%m-%d %H:%M")$hour/24
min <- strptime(dendro_abs$Date_Time.px, "%Y-%m-%d %H:%M")$min/60/24
dendro_abs$doytime <- dendro_abs$doy+hours+min

# Transform circumference to radius (absolut and difference values)
names(dendro_diff)
dendro_r_abs <- dendro_abs; dendro_r_abs[c(3:30)] <- dendro_abs[c(3:30)]/(2*pi)     
dendro_r_diff <- dendro_diff; dendro_r_diff[c(3:30)] <- dendro_diff[c(3:30)]/(2*pi)     
dendro_r <- merge(dendro_r_abs,dendro_r_diff,by="Date_Time.px",all=TRUE)
dendro_r <- merge(dendro_r,dendro_temp,by="Date_Time.px",all=TRUE)

# Save data_dendro_r dataset
# outfile.r <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius_2014.csv")  
# write.csv(dendro_r,file=outfile.r)

# Load dendro_r dataset
# dendro_r <- read.csv("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius.csv",header = T,sep=",")
# dendro_r <- dendro_r [-1]
# dendro_r$Date_Time.px <- as.POSIXct(dendro_r$Date_Time.px,format="%Y-%m-%d %H:%M", tz="GMT")

## Figures dendrometer-data hourly (starting circumference increment-values set to 0) ####
# str(dendro_abs) 

# Min- and max-dates for x-axis (earliest measurements: 2012-03-22)
range(dendro_abs$Date_Time.px,na.rm=TRUE)
start_date <- as.POSIXct(as.Date("2013-05-01", format="%Y-%m-%d"))
end_date <- as.POSIXct(as.Date("2013-11-01", format="%Y-%m-%d"))

jan1_2013 <- as.POSIXct(as.Date("2013-01-01", format="%Y-%m-%d"))
jan1_2014 <- as.POSIXct(as.Date("2014-01-01", format="%Y-%m-%d"))
start_date.axis <- as.POSIXct(as.Date("2013-05-01", format="%Y-%m-%d"))

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5) ####
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_circ_SF1-3.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF1
plot(dendro_abs$Date_Time.px,dendro_abs$SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF1_L4_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF1_L5_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF1_L6_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
legend("topleft",c("L2 (BHD 24.5 cm)","L4 (BHD 35.3 cm)","L5 (BHD 20.7 cm)","L6 (BHD 53.5 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF1")
title("Dendrometer (hourly) SF1")

# Plot hourly dendrometer-data Site SF2
plot(dendro_abs$Date_Time.px,dendro_abs$SF2_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF2_L4_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF2_L5_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF2_L7_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 49.2 cm)","L4 (BHD 37.2 cm)","L5 (BHD 43.1 cm)","L7 (BHD 29.9 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF2")
title("Dendrometer (hourly) SF2")

# Plot hourly dendrometer-data Site SF3
plot(dendro_abs$Date_Time.px,dendro_abs$SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF3_L4_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF3_L5_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF3_L6_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L3 (BHD 56.0 cm)","L4 (BHD 44.6 cm)","L5 (BHD 36.0 cm)","L6 (BHD 31.2 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF3")
title("Dendrometer (hourly) SF3")


# dev.off()

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5)
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_circ_SF4-5.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF4 Larix
plot(dendro_abs$Date_Time.px,dendro_abs$SF4_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_L2_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_L3_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_L4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 65.3 cm)","L2 (BHD 42.3 cm)","L3 (BHD 28.3 cm)","L4 (BHD 20.8 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF4")
title("Dendrometer (hourly) SF4 Larix")

# Plot hourly dendrometer-data Site SF5 Larix
plot(dendro_abs$Date_Time.px,dendro_abs$SF5_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_L2_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_L3_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_L4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 37.9 cm)","L2 (BHD 33.1 cm)","L3 (BHD 23.1 cm)","L4 (BHD 58.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF5")
title("Dendrometer (hourly) SF5 Larix")

# Plot hourly dendrometer-data Site SF4 P.cembra
plot(dendro_abs$Date_Time.px,dendro_abs$SF4_Z1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_Z2_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_Z3_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF4_Z4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (BHD 30.2 cm)","Pc2 (BHD 36.0 cm)","Pc3 (BHD 31.2 cm)","Pc4 (BHD 23.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF4")
title("Dendrometer (hourly) SF4 P.cembra")

# Plot hourly dendrometer-data Site SF5 P.cembra
plot(dendro_abs$Date_Time.px,dendro_abs$SF5_Z1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="circumference var. (mm)", xlim=c(start_date,end_date),ylim=c(-6,60))
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_Z2_dia.c,col=2)
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_Z3_dia.c,col=3)
lines(dendro_abs$Date_Time.px,dendro_abs$SF5_Z4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (BHD 25.8 cm)","Pc2 (BHD 57.3 cm)","Pc3 (BHD 36.3 cm)","Pc4 (BHD 19.4 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Site SF5")
title("Dendrometer (hourly) SF5 P.cembra")

# dev.off()


# figures Radius increment #####
summary(dendro_r)

attach(dendro_r)

# Save following Figures (1st page SF1-SF3, 2nd page SF4-SF5)
# Plot hourly dendrometer-data Site SF1

# Min- and max-dates for x-axis (earliest measurements: 2012-03-22)
range(dendro_abs$Date_Time.px,na.rm=TRUE)
start_date <- as.POSIXct(as.Date("2013-05-01", format="%Y-%m-%d"))
end_date <- as.POSIXct(as.Date("2013-11-01", format="%Y-%m-%d"))

jan1_2013 <- as.POSIXct(as.Date("2013-01-01", format="%Y-%m-%d"))
jan1_2014 <- as.POSIXct(as.Date("2014-01-01", format="%Y-%m-%d"))
start_date.axis <- as.POSIXct(as.Date("2013-05-01", format="%Y-%m-%d"))

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_SF1_L6_2013.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF1_L6_dia.c,type="l", col=4, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(start_date,end_date),ylim=c(0,3),lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_SF3_L4_2013.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF3_L4_dia.c,type="l", col=2, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(start_date,end_date),ylim=c(0,3),lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
dev.off()

d.start_date <- as.POSIXct(as.Date("2013-08-01", format="%Y-%m-%d"))
d.end_date <- as.POSIXct(as.Date("2013-08-05", format="%Y-%m-%d"))
d.start_date.axis <- as.POSIXct(as.Date("2013-08-01", format="%Y-%m-%d"))

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_SF3_L4_daily.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF3_L4_dia.c,type="l", col=2, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(d.start_date,d.end_date),ylim=c(2.1,2.2),lwd=2)
axis.POSIXct(1, at=seq(d.start_date.axis,d.end_date, by="days"), format="%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
dev.off()

w.start_date <- as.POSIXct(as.Date("2013-07-25", format="%Y-%m-%d"))
w.end_date <- as.POSIXct(as.Date("2013-08-10", format="%Y-%m-%d"))
w.start_date.axis <- as.POSIXct(as.Date("2013-07-25", format="%Y-%m-%d"))

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_SF1_L6_period.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF1_L6_dia.c,type="l", col=4, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(w.start_date,w.end_date),ylim=c(1.9,2.4),lwd=2)
axis.POSIXct(1, at=seq(w.start_date.axis,w.end_date, by="days"), format="%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
# legend("topleft",c("L2 (BHD 24.5 cm)","L4 (BHD 35.3 cm)","L5 (BHD 20.7 cm)","L6 (BHD 53.5 cm)"),
#        pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
# title("SF1")
dev.off()


win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_radius_SF1.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(start_date,end_date),ylim=c(-1,8),lwd=2)
lines(Date_Time.px,SF1_L4_dia.c,col=2,lwd=2)
lines(Date_Time.px,SF1_L5_dia.c,col=3,lwd=2)
lines(Date_Time.px,SF1_L6_dia.c,col=4,lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
legend("topleft",c("L2 (BHD 24.5 cm)","L4 (BHD 35.3 cm)","L5 (BHD 20.7 cm)","L6 (BHD 53.5 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("SF1")
dev.off()

# Plot hourly dendrometer-data Site SF2
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_radius_SF2_.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF2_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="", xlim=c(start_date,end_date),ylim=c(-1,8),lwd=2)
lines(Date_Time.px,SF2_L4_dia.c,col=2,lwd=2)
lines(Date_Time.px,SF2_L5_dia.c,col=3,lwd=2)
lines(Date_Time.px,SF2_L7_dia.c,col=4,lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 49.2 cm)","L4 (BHD 37.2 cm)","L5 (BHD 43.1 cm)","L7 (BHD 29.9 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("SF2")
dev.off()

# Plot hourly dendrometer-data Site SF3
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_radius_SF3_.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="", xlim=c(start_date,end_date),ylim=c(-1,8),lwd=2)
lines(Date_Time.px,SF3_L4_dia.c,col=2,lwd=2)
lines(Date_Time.px,SF3_L5_dia.c,col=3,lwd=2)
lines(Date_Time.px,SF3_L6_dia.c,col=4,lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L3 (BHD 56.0 cm)","L4 (BHD 44.6 cm)","L5 (BHD 36.0 cm)","L6 (BHD 31.2 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("SF3")
dev.off()

start_date <- as.POSIXct(as.Date("2013-04-01", format="%Y-%m-%d"))
end_date <- as.POSIXct(as.Date("2013-11-01", format="%Y-%m-%d"))

jan1_2013 <- as.POSIXct(as.Date("2013-01-01", format="%Y-%m-%d"))
jan1_2014 <- as.POSIXct(as.Date("2014-01-01", format="%Y-%m-%d"))
start_date.axis <- as.POSIXct(as.Date("2012-03-01", format="%Y-%m-%d"))

# Plot hourly dendrometer-data Site SF3
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_radius_SF1.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF1_L6_dia.c,type="l", col=4, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(start_date,end_date),ylim=c(-1,8),lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
legend("topleft",c("L2 (BHD 24.5 cm)","L4 (BHD 35.3 cm)","L5 (BHD 20.7 cm)","L6 (BHD 53.5 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("SF1")
dev.off()

# dev.off()

# Save following Figures (1st page SF4-SF5, 2nd page SF4-SF5)
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2013_radius_SF4-5.emf", height=8, width=12, pointsize=14)
# par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF4 Larix
plot(Date_Time.px,SF4_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_date,end_date),ylim=c(-1,10))
lines(Date_Time.px,SF4_L2_dia.c,col=2)
lines(Date_Time.px,SF4_L3_dia.c,col=3)
lines(Date_Time.px,SF4_L4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 65.3 cm)","L2 (BHD 42.3 cm)","L3 (BHD 28.3 cm)","L4 (BHD 20.8 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF4 Larix")

# Plot hourly dendrometer-data Site SF5 Larix
plot(Date_Time.px,SF5_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_date,end_date),ylim=c(-1,10))
lines(Date_Time.px,SF5_L2_dia.c,col=2)
lines(Date_Time.px,SF5_L3_dia.c,col=3)
lines(Date_Time.px,SF5_L4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L1 (BHD 37.9 cm)","L2 (BHD 33.1 cm)","L3 (BHD 23.1 cm)","L4 (BHD 58.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF5 Larix")

# Plot hourly dendrometer-data Site SF4 P.cembra
plot(Date_Time.px,SF4_Z1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_date,end_date),ylim=c(-1,10))
lines(Date_Time.px,SF4_Z2_dia.c,col=2)
lines(Date_Time.px,SF4_Z3_dia.c,col=3)
lines(Date_Time.px,SF4_Z4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (BHD 30.2 cm)","Pc2 (BHD 36.0 cm)","Pc3 (BHD 31.2 cm)","Pc4 (BHD 23.6 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF4 P.cembra")

# Plot hourly dendrometer-data Site SF5 P.cembra
plot(Date_Time.px,SF5_Z1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_date,end_date),ylim=c(-1,10))
lines(Date_Time.px,SF5_Z2_dia.c,col=2)
lines(Date_Time.px,SF5_Z3_dia.c,col=3)
lines(Date_Time.px,SF5_Z4_dia.c,col=4)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("Pc1 (BHD 25.8 cm)","Pc2 (BHD 57.3 cm)","Pc3 (BHD 36.3 cm)","Pc4 (BHD 19.4 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("Dendrometer (hourly) SF5 P.cembra")

# dev.off()
detach(dendro_r)

######################################################################################################################################

##  subset both growth periods/years, set all dates ######
# str(dendro_r_abs); tail(dendro_r_abs$Date_Time.px,3)
# names(dendro_r_abs)

start_2012.1<-as.POSIXct(as.Date("2012-03-23 0:10", format="%Y-%m-%d %H:%M"))
start_2012.2<-as.POSIXct(as.Date("2012-05-26 0:10", format="%Y-%m-%d %H:%M"))
start_2013<-as.POSIXct(as.Date("2013-03-23 0:10", format="%Y-%m-%d %H:%M"))
start_2014<-as.POSIXct(as.Date("2014-03-23 0:10", format="%Y-%m-%d %H:%M"))

end_2012<-as.POSIXct(as.Date("2012-11-1 0:00", format="%Y-%m-%d %H:%M"))
end_2013<-as.POSIXct(as.Date("2013-11-1 0:00", format="%Y-%m-%d %H:%M"))
end_2014<-as.POSIXct(as.Date("2014-8-29 0:00", format="%Y-%m-%d %H:%M"))
end_2014.2<-as.POSIXct(as.Date("2014-11-1 0:00", format="%Y-%m-%d %H:%M"))

axis_2012<-as.POSIXct(as.Date("2012-04-1", format="%Y-%m-%d"))
axis_2013<-as.POSIXct(as.Date("2013-04-1", format="%Y-%m-%d"))
axis_2014<-as.POSIXct(as.Date("2014-04-1", format="%Y-%m-%d"))

m2.1 <- match(start_2012.1,dendro_r_abs$Date_Time.px)
m2.1.1 <- match(start_2012.2,dendro_r_abs$Date_Time.px)
m3.1 <- match(start_2013,dendro_r_abs$Date_Time.px)
m4.1 <- match(start_2014,dendro_r_abs$Date_Time.px)

m2.2 <- match(end_2012,dendro_r_abs$Date_Time.px)
m3.2 <- match(end_2013,dendro_r_abs$Date_Time.px)
m4.2 <- match(end_2014,dendro_r_abs$Date_Time.px)

data_dendro2012.1 <- dendro_r_abs[m2.1:m2.2,3:30]
date2012.1 <- dendro_r_abs[m2.1:m2.2,31]
doy2012.1 <- dendro_r_abs[m2.1:m2.2,32]

data_dendro2012.2 <- dendro_r_abs[m2.1.1:m2.2,3:30]
date2012.2 <- dendro_r_abs[m2.1.1:m2.2,31]
doy2012.2 <- dendro_r_abs[m2.1.1:m2.2,32]

data_dendro2013 <- dendro_r_abs[m3.1:m3.2,3:30]
date2013 <- dendro_r_abs[m3.1:m3.2,31]
doy2013 <- dendro_r_abs[m3.1:m3.2,32]

data_dendro2014 <- dendro_r_abs[m4.1:m4.2,3:30]
date2014 <- dendro_r_abs[m4.1:m4.2,31]
doy2014 <- dendro_r_abs[m4.1:m4.2,32]

# dataset data_dendro2012.1: set starting values to 0 
data_dendro2012.1[1,]
data_dendro2012.1[1,c(1,2,9:28)] <- 0 # set NA's in first row to 0
data_dendro2012.1.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2012.1)   # substract starting value from rest of the column
data_dendro2012.1[1,c(9:28)] <- NA 
data_dendro2012.1.0[1,c(9:28)] <- NA 
data_dendro2012.1.0 <- data.frame(cbind(data_dendro2012.1.0,doy2012.1)); colnames(data_dendro2012.1.0)[29] <- "DOY"
data_dendro2012.1.0 <- data.frame(cbind(data_dendro2012.1.0,date2012.1))
colnames(data_dendro2012.1.0)[30] <- "Date_Time.px"

# Comparison of dendrometerdata with same starting point
# dataset data_dendro2012.2: set starting values to 0 and plot selection of trees from all sites
data_dendro2012.2[1,]
data_dendro2012.2[1,c(13,14,17:18)] <- 0 # set NA's in first row to 0
data_dendro2012.2.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2012.2)   # substract starting value from rest of the column
data_dendro2012.2[1,c(9:28)] <- NA 
data_dendro2012.2.0[1,c(9:28)] <- NA 
data_dendro2012.2.0 <- data.frame(cbind(data_dendro2012.2.0,doy2012.2)); colnames(data_dendro2012.2.0)[29] <- "DOY"
data_dendro2012.2.0 <- data.frame(cbind(data_dendro2012.2.0,date2012.2))
colnames(data_dendro2012.2.0)[30] <- "Date_Time.px"

# dataset data_dendro2013: set starting values to 0 and plot selection of trees from all sites
summary(data_dendro2013); names(data_dendro2013)
data_dendro2013[1,] 
data_dendro2013.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2013)   # substract starting value from rest of the column
data_dendro2013.0 <- data.frame(cbind(data_dendro2013.0,doy2013)); colnames(data_dendro2013.0)[29] <- "DOY"
data_dendro2013.0 <- data.frame(cbind(data_dendro2013.0,date2013))
colnames(data_dendro2013.0)[30] <- "Date_Time.px"

# dataset data_dendro2014: set starting values to 0 and plot selection of trees from all sites
data_dendro2014[1,] 
data_dendro2014.0 <- colwise(.fun = function(x) {x - x[1]})(data_dendro2014)   # substract starting value from rest of the column
data_dendro2014.0 <- data.frame(cbind(data_dendro2014.0,doy2014)); colnames(data_dendro2014.0)[29] <- "DOY"
data_dendro2014.0 <- data.frame(cbind(data_dendro2014.0,date2014))
colnames(data_dendro2014.0)[30] <- "Date_Time.px"

# names(data_dendro2013.0)
# summary(data_dendro2012.1.0)
# str(data_dendro2012.1.0)

# Figures radius increment yearly all trees ####
# Save following Figures 
# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro_yearly_radius.emf", height=8, width=12, pointsize=15)
# par(mfcol=c(1,3), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot hourly dendrometer-data Site SF3
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_radius_SF3.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(Date_Time.px,SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)", xlim=c(start_date,end_date),ylim=c(-1,8),lwd=2)
lines(Date_Time.px,SF3_L4_dia.c,col=2,lwd=2)
lines(Date_Time.px,SF3_L5_dia.c,col=3,lwd=2)
lines(Date_Time.px,SF3_L6_dia.c,col=4,lwd=2)
abline(h=0); abline(v=jan1_2013,lty=2);abline(v=jan1_2014,lty=2)
axis.POSIXct(1, at=seq(start_date.axis,end_date, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("L3 (BHD 56.0 cm)","L4 (BHD 44.6 cm)","L5 (BHD 36.0 cm)","L6 (BHD 31.2 cm)"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n")
title("SF3")
dev.off()


# Plot variables of hourly dendrometer-data data_dendro2012.1.0 (2012 values starting at 2012-03-23, starting at 0)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012_radius_SF1-3.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_2012.1,end_2012),ylim=c(-1,4.5),lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L4_dia.c,col=1,lty=2,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L5_dia.c,col=1,lty=3,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L6_dia.c,col=1,lty=4,lwd=2)

lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L1_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L4_dia.c,col=2,lty=2,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L5_dia.c,col=2,lty=3,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L7_dia.c,col=2,lty=4,lwd=2)

lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L3_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L4_dia.c,col=3,lty=2,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L5_dia.c,col=3,lty=3,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L6_dia.c,col=3,lty=4,lwd=2)

abline(h=0)
axis.POSIXct(1, at=seq(axis_2012,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3"),
       pch=15,col=c(1,2,3),cex=0.8,bty="n",title="Site")
title("2012")
dev.off()


# Plot variables of hourly dendrometer-data data_dendro2012.2.0 (2012 values same starting point in May for better comparability)
length(data_dendro2012.2.0$Date_Time.px)
plot(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="Radiusänderung(mm)", xlim=c(start_2012.2,end_2012),ylim=c(-1,4.5))
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF1_L4_dia.c,col=1,lty=2)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF1_L5_dia.c,col=1,lty=3)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF1_L6_dia.c,col=1,lty=4)

lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF2_L1_dia.c,col=2,lty=1)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF2_L4_dia.c,col=2,lty=2)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF2_L5_dia.c,col=2,lty=3)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF2_L7_dia.c,col=2,lty=4)

lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF3_L3_dia.c,col=3,lty=1)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF3_L4_dia.c,col=3,lty=2)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF3_L5_dia.c,col=3,lty=3)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF3_L6_dia.c,col=3,lty=4)

lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF4_L1_dia.c,col=4,lty=1)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF4_L2_dia.c,col=4,lty=2)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF4_L3_dia.c,col=4,lty=3)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF4_L4_dia.c,col=4,lty=4)

lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF5_L1_dia.c,col=5,lty=1)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF5_L2_dia.c,col=5,lty=2)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF5_L3_dia.c,col=5,lty=3)
lines(data_dendro2012.2.0$Date_Time.px,data_dendro2012.2.0$SF5_L4_dia.c,col=5,lty=4)

abline(h=0)
axis.POSIXct(1, at=seq(start_2012.2,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3","SF4","SF5"),
       pch=15,col=c(1,2,3,4,5),cex=0.8,bty="n",title="Site")
title("Dendrometer (hourly) 2012 all trees + sites (same starting point)")


# Plot variables of hourly dendrometer-data data_dendro2013.0 (2013 values starting at 2013-04-10)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2013_radius_SF1-3.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2013,end_2013),ylim=c(-1,4.5),lwd=2)   # ylab="Radiusänderung(mm)", 
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L4_dia.c,col=1,lty=2,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L5_dia.c,col=1,lty=3,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L6_dia.c,col=1,lty=4,lwd=2)

lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L1_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L4_dia.c,col=2,lty=2,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L5_dia.c,col=2,lty=3,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L7_dia.c,col=2,lty=4,lwd=2)

lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L3_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L4_dia.c,col=3,lty=2,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L5_dia.c,col=3,lty=3,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L6_dia.c,col=3,lty=4,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2013,end_2013, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3"),
       pch=15,col=c(1,2,3),cex=0.8,bty="n",title="Site")
title("2013")
dev.off()

# Plot variables of hourly dendrometer-data data_dendro2014.0 (2014 values starting at 2014-04-10)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2014_radius_SF1-3.emf", height=5, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L2_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2014,end_2014.2),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L4_dia.c,col=1,lty=2,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L5_dia.c,col=1,lty=3,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L6_dia.c,col=1,lty=4,lwd=2)

lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L1_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L4_dia.c,col=2,lty=2,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L5_dia.c,col=2,lty=3,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L7_dia.c,col=2,lty=4,lwd=2)

lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L3_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L4_dia.c,col=3,lty=2,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L5_dia.c,col=3,lty=3,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L6_dia.c,col=3,lty=4,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2014,end_2014.2, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("SF1","SF2","SF3"),
       pch=15,col=c(1,2,3),cex=0.8,bty="n",title="Site")
title("2014")
dev.off()


# Plot variables of hourly dendrometer-data data_dendro2012.1.0 (2012_1 values starting at 2012.1-04-10)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012_1_radius_SF1.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2)  # 
plot(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L2_dia.c,type="l", col=3, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)",xlim=c(start_2012.1,end_2012),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L5_dia.c,col=4,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF1_L6_dia.c,col=1,lty=1,lwd=2)
axis.POSIXct(1, at=seq(axis_2012,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
abline(h=0)
legend("topleft",c("54","35","25","21"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF1-2012")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012_1_radius_SF2.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2012.1,end_2012),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L4_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L5_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF2_L7_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2012,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("49","43","37","30"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF2-2012")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012_1_radius_SF3.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2012.1,end_2012),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L5_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2012.1.0$Date_Time.px,data_dendro2012.1.0$SF3_L6_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2012,end_2012, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("56","45","36","31"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF3-2012")
dev.off()

# Plot variables of hourly dendrometer-data data_dendro2013.0 (2013 values starting at 2013-04-10)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2013_radius_SF1.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2)  # 
plot(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L2_dia.c,type="l", col=3, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)",xlim=c(start_2013,end_2013),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L5_dia.c,col=4,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF1_L6_dia.c,col=1,lty=1,lwd=2)
axis.POSIXct(1, at=seq(axis_2013,end_2013, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
abline(h=0)
legend("topleft",c("54","35","25","21"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF1-2013")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2013_radius_SF2.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2013,end_2013),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L4_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L5_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF2_L7_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2013,end_2013, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("49","43","37","30"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF2-2013")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2013_radius_SF3.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2013,end_2013),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L5_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2013.0$Date_Time.px,data_dendro2013.0$SF3_L6_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2013,end_2013, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("56","45","36","31"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF3-2013")
dev.off()

# 2014
# Plot variables of hourly dendrometer-data data_dendro2014.0 (2014 values starting at 2014-04-10)
win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2014_radius_SF1.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2)  # 
plot(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L2_dia.c,type="l", col=3, xaxt="n",
     xlab="Date",ylab="Radiusänderung (mm)",xlim=c(start_2014,end_2014.2),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L5_dia.c,col=4,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF1_L6_dia.c,col=1,lty=1,lwd=2)
axis.POSIXct(1, at=seq(axis_2014,end_2014.2, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
abline(h=0)
legend("topleft",c("54","35","25","21"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF1-2014")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2014_radius_SF2.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L1_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2014,end_2014.2),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L4_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L5_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF2_L7_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2014,end_2014.2, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("49","43","37","30"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF2-2014")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2014_radius_SF3.emf", height=3, width=4, pointsize=15)
par(mfcol=c(1,1), mar=c(1,3,1,0),oma=c(1,1,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.2) # 
plot(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L3_dia.c,type="l", col=1, xaxt="n",
     xlab="Date",ylab="",xlim=c(start_2014,end_2014.2),ylim=c(-1,4.5),lwd=2) # ylab="Radiusänderung(mm)", 
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L4_dia.c,col=2,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L5_dia.c,col=3,lty=1,lwd=2)
lines(data_dendro2014.0$Date_Time.px,data_dendro2014.0$SF3_L6_dia.c,col=4,lty=1,lwd=2)
abline(h=0)
axis.POSIXct(1, at=seq(axis_2014,end_2014.2, by="months"), format="%Y-%m",cex.axis=0.8) #label the x axis by months
legend("topleft",c("56","45","36","31"),
       pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="BHD (cm)")
title("SF3-2014")
dev.off()





# Save yearly datasets #####
outfile.2012.1.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2012_1_0.csv")  
write.csv(data_dendro2012.1.0,file=outfile.2012.1.0)

outfile.2012.2.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2012_2_0.csv")  
write.csv(data_dendro2012.2.0,file=outfile.2012.2.0)

outfile.2013.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2013_0.csv")  
write.csv(data_dendro2013.0,file=outfile.2013.0)

outfile.2014.0 <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_radius2014_0.csv")  
write.csv(data_dendro2014.0,file=outfile.2014.0)

#######################################################################################################################

# 2 weekly files of accumulated dendrometer-radius data => not changed yet
# max values: SF1: L6, SF2: L4+L5, SF3: L3, SF4: L3, SF5: L2+4

# head(dendro_r$Date_Time.px,10)
# tail(dendro_r$Date_Time.px,10)
start_date.axis2 <- as.POSIXct(as.Date("2012-03-20", format="%Y-%m-%d"))

# create a 2-weekly numbering variable for later loops
n_2weekly <- trunc(length(dendro_r$Date_Time.px)/14/24)       # number of 2 week periods
seq_2weekly <- seq(1,n_2weekly,by=1)                                   # vector 1 to n_2weekly
idx_2week <- rep(seq_2weekly,each=(24*14))                             # create vector fitting to date/time column
assign("idx_2week",c(1,1,1,1,1,1,idx_2week))                         
diff <- length(dendro_r$Date_Time.px)-length(idx_2week)       # difference between date/time column and new vector
r_2weekly <- rep(max(idx_2week+1), each=diff)                          # create vector for rediual days/time 
assign("idx_2week",c(idx_2week,r_2weekly))                             # add residual vector

dendro_r <- cbind(dendro_r,idx_2week)

# names(dendro_r)

all2weeks <- unique(dendro_r$idx_2week)

# 2-weekly figures for site SF1 ####
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF1")
for (i in 1:44){
  idx_2week.i <- all2weeks[i]
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF1_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=14)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF1_L6_dia.c,type="l",col=4,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(1:4)],na.rm = TRUE),max(dendro_r.i[,c(1:4)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF1_L2_dia.c,col=1)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF1_L4_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF1_L5_dia.c,col=3)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF2_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF2_L1_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(9:12)],na.rm = TRUE),max(dendro_r.i[,c(9:12)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF2_L4_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF2_L5_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF2_L7_dia.c,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L4","L5","L7"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF2 N°",idx_2week.i,sep=""))
  dev.off()
}

dendro_r$idx_2week[match(start.SF3,dendro_r$Date_Time.px)]

# 2-weekly figures for site SF3 ####                                             => check min/max for SF3 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF3")
for (i in 5:43){                                                                   # i 5-43 for later start in 2012 and earlier end in 2013
  idx_2week.i <- all2weeks[i]
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF3_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF3_L3_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(17:20)],na.rm = TRUE),max(dendro_r.i[,c(17:20)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF3_L4_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF3_L5_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF3_L6_dia.c,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_L1_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(25,27,29,31)],na.rm = TRUE),max(dendro_r.i[,c(25,27,29,31)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_L2_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_L3_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_L4_dia.c,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_acc_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_L1_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(41:43,47)],na.rm = TRUE),max(dendro_r.i[,c(41:43,47)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_L2_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_L3_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_L4_dia.c,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_acc_Z_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_Z1_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(26,28,30,32)],na.rm = TRUE),max(dendro_r.i[,c(26,28,30,32)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_Z2_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_Z3_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_Z4_dia.c,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Z1","Z2","Z3","Z4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (acc./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 20:23){                                                                
  idx_2week.i <- all2weeks[i]
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_acc_Z_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_Z1_dia.c,type="l",col=1,xaxt="n",
       xlab="Date",ylab="acc. Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(44:46,48)],na.rm = TRUE),max(dendro_r.i[,c(44:46,48)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_Z2_dia.c,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_Z3_dia.c,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_Z4_dia.c,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Z1","Z2","Z3","Z4"),horiz=TRUE,
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF1_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=14)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF1_L6_dia_diff,type="l",col=4,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(5:8)],na.rm = TRUE),max(dendro_r.i[,c(5:8)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF1_L2_dia_diff,col=1)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF1_L4_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF1_L5_dia_diff,col=3)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF2_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF2_L1_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(13:16)],na.rm = TRUE),max(dendro_r.i[,c(13:16)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF2_L4_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF2_L5_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF2_L7_dia_diff,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("L1","L4","L5","L7"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF2 N°",idx_2week.i,sep=""))
  dev.off()
}

# dendro_r$idx_2week[match(start.SF3,dendro_r$Date_Time.px)]

# 2-weekly figures for site SF3 ####                                             => check min/max for SF3 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF3")
for (i in 5:43){                                                                   # i 5-43 for later start in 2012 and earlier end in 2013
  idx_2week.i <- all2weeks[i]
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF3_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF3_L3_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(21:24)],na.rm = TRUE),max(dendro_r.i[,c(21:24)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF3_L4_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF3_L5_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF3_L6_dia_diff,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_L1_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(33,35,37,39)],na.rm = TRUE),max(dendro_r.i[,c(33,35,37,39)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_L2_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_L3_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_L4_dia_diff,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_diff_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_L1_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(49:51,55)],na.rm = TRUE),max(dendro_r.i[,c(49:51,55)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_L2_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_L3_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_L4_dia_diff,col=4)
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
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF4_diff_Z_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_Z1_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(34,36,38,40)],na.rm = TRUE),max(dendro_r.i[,c(34,36,38,40)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF4_Z2_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_Z3_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF4_Z4_dia_diff,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Z1","Z2","Z3","Z4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF4 N°",idx_2week.i,sep=""))
  dev.off()
}


# 2-weekly figures for site SF5 ####                                             => check min/max for SF5 pointsize
setwd("H:/Data/Matsch_Catchment/SF/figures_dendro/2_week_figures_SF5")          # => exclude i = 1-4,19,24,25,>42
for (i in 26:41){                                                                
  idx_2week.i <- all2weeks[i]
  dendro_r.i <- dendro_r[dendro_r$idx_2week == idx_2week.i, ]
  filename2w <- paste("SF5_diff_Z_",idx_2week.i,".emf",sep="")
  win.metafile(file=filename2w, height=12, width=14, pointsize=16)
  plot(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_Z1_dia_diff,type="l",col=1,xaxt="n",
       xlab="Date",ylab="Radiusänderung(mm)",
       ylim=c(min(dendro_r.i[,c(52:54,56)],na.rm = TRUE),max(dendro_r.i[,c(52:54,56)],na.rm = TRUE)))
  lines(x=dendro_r.i$Date_Time.px,y=dendro_r.i$SF5_Z2_dia_diff,col=2)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_Z3_dia_diff,col=3)
  lines(dendro_r.i$Date_Time.px,dendro_r.i$SF5_Z4_dia_diff,col=4)
  axis.POSIXct(1, at=seq(start_date.axis2,end_date, by="days"), format="%Y-%m-%d",cex.axis=0.8) #label the x axis by months (day of tick depends on starting date)
  legend("bottom",c("Z1","Z2","Z3","Z4"),horiz=TRUE,
         pch=15,col=c(1,2,3,4),cex=0.8,bty="n",title="Tree")
  title(paste("Dendrometer (diff./hourly) SF5 N°",idx_2week.i,sep=""))
  dev.off()
}

########################################################################################################

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

names(dendro_r)
# renaming trees: L...Larch, Z...Pinus cembra, dia1...largest diameter at site, dia4...smallest diameter at site

wide_dendro_L_all <- dendro_r[c(57,4,9,17,25,43,2,11,18,27,41,1,10,19,29,47,3,12,20,31,42)] 
long_dendro_L_all <- reshape(wide_dendro_L_all,varying=c(2:6,7:11,12:16,17:21),v.names="Rad_inc",direction="long")

wide_dendro_all <- dendro_r[c(57,4,9,17,25,43,2,11,18,27,41,1,10,19,29,47,3,12,20,31,42,28,46,30,48,26,45,32,44)] 
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
qplot(Date_Time.px,Rad_inc, geom="line",facets=fSite~.,data=long_dendro_L_all, colour=fTree_dia, main="Radius variation: hourly 2012+2013")+
  theme_bw() + scale_y_continuous("Radiusänderung(mm)") + labs(colour = "Tree: (1=thickest, 4=thinnest/site):")+
  theme(legend.position="bottom")+geom_line(size=1)     # => to-do: change x-axis title
dev.off()       

