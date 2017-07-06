# Load and clean raw dendrometer data   => corrected calculation for SF5 L2-L4, check SF1-L2
# hourly data; bis zu 5 NAs bei Mittelwert-Berechnung in EMS-Mini32 erlaubt (betrifft v.a. Stem temperatures), 
# Treshold for cleaning dendro data: diff > 1 & < -1 
# export of circumference change, no calculation of radius/diameter
#loads required R-packages
library(zoo)
library(pastecs)
library(plyr)
library(ggplot2)
library(reshape2)

#Clears workspace
#Sets working directory
Sys.setenv(TZ = "GMT") # sets the system time to GMT
setwd("H:/Data/Matsch_Catchment/SF/dendro files for R")
remove(list = ls())

# Load raw hourly dendro files 
dendro_SF1_raw <- read.csv("Matsch_P1_dendro_hourly_5mv.csv",header = T,sep=",")
dendro_SF2_raw <- read.csv("Matsch_P2_dendro_hourly_5mv.csv",header = T,sep=",")
dendro_SF3_raw <- read.csv("Matsch_P3_dendro_hourly_5mv.csv",header = T,sep=",")
# dendro_SF4_raw <- read.csv("Matsch_P4_SF_all_raw_hourly.csv",header = T,sep=",")[,c(1,10:13,27:30)]
dendro_SF4_raw <- read.csv("Matsch_P4_SFbaseline_all_hourly_5mv.csv",header = T,sep=",")[,c(1,10:13,22:25)]
# dendro_SF5_raw <- read.csv("Matsch_P5_SF_all_raw_hourly.csv",header = T,sep=",")[,c(1,10:13,27:30)]
dendro_SF5_raw <- read.csv("Matsch_P5_SFbaseline_all_hourly_man_corr_5mv.csv",header = T,sep=",")[,c(1,10:13,22:25)]

colnames(dendro_SF1_raw) <- c("Date_Time","SF1_L2_dia_raw","SF1_L2_temp","SF1_L4_dia_raw","SF1_L4_temp",
                              "SF1_L5_dia_raw","SF1_L5_temp","SF1_L6_dia_raw","SF1_L6_temp")
colnames(dendro_SF2_raw) <- c("Date_Time","SF2_L1_dia_raw","SF2_L1_temp","SF2_L4_dia_raw","SF2_L4_temp",
                         "SF2_L5_dia_raw","SF2_L5_temp","SF2_L7_dia_raw","SF2_L7_temp")
colnames(dendro_SF3_raw) <- c("Date_Time","SF3_L3_dia_raw","SF3_L3_temp","SF3_L4_dia_raw","SF3_L4_temp",
                         "SF3_L5_dia_raw","SF3_L5_temp","SF3_L6_dia_raw","SF3_L6_temp")
colnames(dendro_SF4_raw) <- c("Date_Time","SF4_L1_dia_raw","SF4_Z1_dia_raw","SF4_L2_dia_raw","SF4_Z2_dia_raw",
                         "SF4_L3_dia_raw","SF4_Z3_dia_raw","SF4_L4_dia_raw","SF4_Z4_dia_raw")
colnames(dendro_SF5_raw) <- c("Date_Time","SF5_L1_dia_raw","SF5_L3_dia_raw","SF5_L4_dia_raw","SF5_Z4_dia_raw",
                         "SF5_Z1_dia_raw","SF5_Z2_dia_raw","SF5_L2_dia_raw","SF5_Z3_dia_raw")
# convert dates to POSIXct-format
dendro_SF1_raw$Date_Time.px <- as.POSIXct(dendro_SF1_raw$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF2_raw$Date_Time.px <- as.POSIXct(dendro_SF2_raw$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF3_raw$Date_Time.px <- as.POSIXct(dendro_SF3_raw$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF4_raw$Date_Time.px <- as.POSIXct(dendro_SF4_raw$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF5_raw$Date_Time.px <- as.POSIXct(dendro_SF5_raw$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 

summary(dendro_SF5_raw)
# plot raw value, calculate and plot differences  ####
# Treshold for cleaning dendro data: diff > 1 & < -1 (formerly 0.4/-0.4) are deleted => check again with literature!!!
# SF1_L2
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L2_dia_diff <- diff(dendro_SF1_raw$SF1_L2_dia_raw)
plot(SF1_Date_diff.px,SF1_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L2_dia_diff,main="SF1_L2")
boxplot(SF1_L2_dia_diff,ylim=c(-1,1),main="SF1_L2")
sort(SF1_L2_dia_diff, TRUE)[1:10]
sort(SF1_L2_dia_diff, FALSE)[1:10]
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[1])]; sort(SF1_L2_dia_diff, TRUE)[1] # to be removed
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[3])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[4])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[5])]
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, FALSE)[1])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, FALSE)[2])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, FALSE)[3])]
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, FALSE)[4])]
dendro_SF1_raw$Date_Time.px[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, FALSE)[5])]
SF1_L2_NA <- is.na(dendro_SF1_raw$SF1_L2_dia_raw)
SF1_L2_NAdiff <- SF1_L2_NA[-1]
SF1_L2_noNA <- na.locf(dendro_SF1_raw$SF1_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L2_noNA,type="l", col=2)
SF1_L2_dia_diff_noNA <- diff(SF1_L2_noNA)
SF1_L2_dia_diff.c <-ifelse((SF1_L2_dia_diff_noNA > 0.21 & SF1_L2_NAdiff == FALSE) | (SF1_L2_dia_diff_noNA < -0.2 & SF1_L2_NAdiff == FALSE),NA,SF1_L2_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L2_dia_diff.c,type="l", col=1)
boxplot(SF1_L2_dia_diff.c,main="SF1_L2")
summary(SF1_L2_dia_diff.c)
SF1_L2_dia_diff.0 <- na.fill(SF1_L2_dia_diff.c,0);summary(SF1_L2_dia_diff.0)
SF1_L2_dia.c <- diffinv(SF1_L2_dia_diff.0)
SF1_L2_dia.NA <- is.na(dendro_SF1_raw$SF1_L2_dia_raw)
SF1_L2_dia.c <- ifelse(SF1_L2_dia.NA == TRUE, NA, SF1_L2_dia.c); summary(SF1_L2_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L2_dia.c,type="l", col=1)
# plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1, ylim=c(40,50))
SF1_L2_dia_diff.c[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[1])]
SF1_L2_dia_diff.c[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[1])]
SF1_L2_dia_diff.c[which(SF1_L2_dia_diff == sort(SF1_L2_dia_diff, TRUE)[1])]
summary (SF1_L2_dia.c)

# SF1_L4
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L4_dia_diff <- diff(dendro_SF1_raw$SF1_L4_dia_raw)
plot(SF1_Date_diff.px,SF1_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L4_dia_diff,main="SF1_L4")
boxplot(SF1_L4_dia_diff,ylim=c(-1,1),main="SF1_L4")
sort(SF1_L4_dia_diff, TRUE)[1:10]
sort(SF1_L4_dia_diff, FALSE)[1:10]
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[1])] # to be removed
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[3])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[4])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[5])]
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, FALSE)[1])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, FALSE)[2])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, FALSE)[3])]
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, FALSE)[4])]
dendro_SF1_raw$Date_Time.px[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, FALSE)[5])]
SF1_L4_NA <- is.na(dendro_SF1_raw$SF1_L4_dia_raw)
SF1_L4_NAdiff <- SF1_L4_NA[-1]
SF1_L4_noNA <- na.locf(dendro_SF1_raw$SF1_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L4_noNA,type="l", col=2)
SF1_L4_dia_diff_noNA <- diff(SF1_L4_noNA)
SF1_L4_dia_diff.c <-ifelse((SF1_L4_dia_diff_noNA > 0.5 & SF1_L4_NAdiff == FALSE) | (SF1_L4_dia_diff_noNA < -0.3 & SF1_L4_NAdiff == FALSE),NA,SF1_L4_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L4_dia_diff.c,type="l", col=1)
boxplot(SF1_L4_dia_diff.c,main="SF1_L4")
summary(SF1_L4_dia_diff.c)
SF1_L4_dia_diff.0 <- na.fill(SF1_L4_dia_diff.c,0);summary(SF1_L4_dia_diff.0)
SF1_L4_dia.c <- diffinv(SF1_L4_dia_diff.0)
SF1_L4_dia.NA <- is.na(dendro_SF1_raw$SF1_L4_dia_raw)
SF1_L4_dia.c <- ifelse(SF1_L4_dia.NA == TRUE, NA, SF1_L4_dia.c); summary(SF1_L4_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L4_dia.c,type="l", col=1)
# plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1, ylim=c(40,50))
SF1_L4_dia_diff.c[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[1])]
SF1_L4_dia_diff.c[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[1])]
SF1_L4_dia_diff.c[which(SF1_L4_dia_diff == sort(SF1_L4_dia_diff, TRUE)[1])]
summary (SF1_L4_dia.c)

# SF1_L5
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L5_dia_diff <- diff(dendro_SF1_raw$SF1_L5_dia_raw)
plot(SF1_Date_diff.px,SF1_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L5_dia_diff,main="SF1_L5")
boxplot(SF1_L5_dia_diff,ylim=c(-1,1),main="SF1_L5")
sort(SF1_L5_dia_diff, TRUE)[1:10]
sort(SF1_L5_dia_diff, FALSE)[1:10]
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[1])] # to be removed
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[3])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[4])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[5])]
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[1])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[3])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[4])]
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[5])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[6])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[7])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[8])] 
SF1_L5_NA <- is.na(dendro_SF1_raw$SF1_L5_dia_raw)
SF1_L5_NAdiff <- SF1_L5_NA[-1]
SF1_L5_noNA <- na.locf(dendro_SF1_raw$SF1_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L5_noNA,type="l", col=2)
SF1_L5_dia_diff_noNA <- diff(SF1_L5_noNA)
SF1_L5_dia_diff.c <-ifelse((SF1_L5_dia_diff_noNA > 0.2 & SF1_L5_NAdiff == FALSE) | (SF1_L5_dia_diff_noNA < -0.15 & SF1_L5_NAdiff == FALSE),NA,SF1_L5_dia_diff_noNA) 
SF1_L5_dia_diff.c[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[5])] <- NA
plot(SF1_Date_diff.px,SF1_L5_dia_diff.c,type="l", col=1)
boxplot(SF1_L5_dia_diff.c,main="SF1_L5")
summary(SF1_L5_dia_diff.c)
SF1_L5_dia_diff.0 <- na.fill(SF1_L5_dia_diff.c,0);summary(SF1_L5_dia_diff.0)
SF1_L5_dia.c <- diffinv(SF1_L5_dia_diff.0)
SF1_L5_dia.NA <- is.na(dendro_SF1_raw$SF1_L5_dia_raw)
SF1_L5_dia.c <- ifelse(SF1_L5_dia.NA == TRUE, NA, SF1_L5_dia.c); summary(SF1_L5_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L5_dia.c,type="l", col=1)
# plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1, ylim=c(40,50))
SF1_L5_dia_diff.c[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[1])]
SF1_L5_dia_diff.c[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[1])]
SF1_L5_dia_diff.c[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, TRUE)[1])]
summary (SF1_L5_dia.c)

# SF1_L6
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L6_dia_diff <- diff(dendro_SF1_raw$SF1_L6_dia_raw)
plot(SF1_Date_diff.px,SF1_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L6_dia_diff,main="SF1_L6")
boxplot(SF1_L6_dia_diff,ylim=c(-1,1),main="SF1_L6")
sort(SF1_L6_dia_diff, TRUE)[1:10]
sort(SF1_L6_dia_diff, FALSE)[1:10]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[1])] # to be removed
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[3])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[4])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[5])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[6])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[7])]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[8])]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[9])]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[10])]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[1])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[2])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[3])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[4])]
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[5])] #
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[6])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[7])] 
dendro_SF1_raw$Date_Time.px[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[8])] 
SF1_L6_NA <- is.na(dendro_SF1_raw$SF1_L6_dia_raw)
SF1_L6_NAdiff <- SF1_L6_NA[-1]
SF1_L6_noNA <- na.locf(dendro_SF1_raw$SF1_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L6_noNA,type="l", col=2)
SF1_L6_dia_diff_noNA <- diff(SF1_L6_noNA)
SF1_L6_dia_diff.c <-ifelse((SF1_L6_dia_diff_noNA > 0.6 & SF1_L6_NAdiff == FALSE) | (SF1_L6_dia_diff_noNA < -0.3 & SF1_L6_NAdiff == FALSE),NA,SF1_L6_dia_diff_noNA) 
SF1_L6_dia_diff.c[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[5])] <- NA
plot(SF1_Date_diff.px,SF1_L6_dia_diff.c,type="l", col=1)
boxplot(SF1_L6_dia_diff.c,main="SF1_L6")
summary(SF1_L6_dia_diff.c)
SF1_L6_dia_diff.0 <- na.fill(SF1_L6_dia_diff.c,0);summary(SF1_L6_dia_diff.0)
SF1_L6_dia.c <- diffinv(SF1_L6_dia_diff.0)
SF1_L6_dia.NA <- is.na(dendro_SF1_raw$SF1_L6_dia_raw)
SF1_L6_dia.c <- ifelse(SF1_L6_dia.NA == TRUE, NA, SF1_L6_dia.c); summary(SF1_L6_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L6_dia.c,type="l", col=1)
# plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1, ylim=c(40,50))
SF1_L6_dia_diff.c[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[1])]
SF1_L6_dia_diff.c[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[2])]
SF1_L6_dia_diff.c[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, TRUE)[3])]
summary (SF1_L6_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF1_corr_corr_05_2015.emf", height=12, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF1 (raw data)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=2)
lines(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=3)
lines(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=4)

# Plot all trees of SF1 (differences)
plot(SF1_Date_diff.px,SF1_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF1_Date_diff.px,SF1_L4_dia_diff,type="l", col=2)
lines(SF1_Date_diff.px,SF1_L5_dia_diff,type="l", col=3)
lines(SF1_Date_diff.px,SF1_L6_dia_diff,type="l", col=4)
legend("topleft",c("L2","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF1 (cleaned data)
plot(dendro_SF1_raw$Date_Time.px,SF1_L2_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw$Date_Time.px,SF1_L4_dia.c,type="l", col=2)
lines(dendro_SF1_raw$Date_Time.px,SF1_L5_dia.c,type="l", col=3)
lines(dendro_SF1_raw$Date_Time.px,SF1_L6_dia.c,type="l", col=4)
legend("topleft",c("L2","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF1_corr_05_2015.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw$Date_Time.px,SF1_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw$Date_Time.px,SF1_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw$Date_Time.px,SF1_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw$Date_Time.px,SF1_L6_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

summary(dendro_SF2_raw)
par(mfcol=c(1,1))
# SF2_L1 ####
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L1_dia_diff <- diff(dendro_SF2_raw$SF2_L1_dia_raw)
plot(SF2_Date_diff.px,SF2_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L1_dia_diff,main="SF2_L1")
boxplot(SF2_L1_dia_diff,ylim=c(-1,1),main="SF2_L1")
sort(SF2_L1_dia_diff, TRUE)[1:10]
sort(SF2_L1_dia_diff, FALSE)[1:10]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[1])] # to be removed
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[2])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[3])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[4])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[5])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[6])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[7])]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[8])]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[9])]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[10])]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[1])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[2])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[3])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[4])]
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[5])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[6])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[7])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[8])] 
SF2_L1_NA <- is.na(dendro_SF2_raw$SF2_L1_dia_raw)
SF2_L1_NAdiff <- SF2_L1_NA[-1]
SF2_L1_noNA <- na.locf(dendro_SF2_raw$SF2_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L1_noNA,type="l", col=2)
SF2_L1_dia_diff_noNA <- diff(SF2_L1_noNA)
SF2_L1_dia_diff.c <-ifelse((SF2_L1_dia_diff_noNA > 0.5 & SF2_L1_NAdiff == FALSE) | (SF2_L1_dia_diff_noNA < -0.21 & SF2_L1_NAdiff == FALSE),NA,SF2_L1_dia_diff_noNA) 
# SF2_L1_dia_diff.c[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, FALSE)[5])] <- NA
plot(SF2_Date_diff.px,SF2_L1_dia_diff.c,type="l", col=1)
boxplot(SF2_L1_dia_diff.c,main="SF2_L1")
summary(SF2_L1_dia_diff.c)
SF2_L1_dia_diff.0 <- na.fill(SF2_L1_dia_diff.c,0);summary(SF2_L1_dia_diff.0)
SF2_L1_dia.c <- diffinv(SF2_L1_dia_diff.0)
SF2_L1_dia.NA <- is.na(dendro_SF2_raw$SF2_L1_dia_raw)
SF2_L1_dia.c <- ifelse(SF2_L1_dia.NA == TRUE, NA, SF2_L1_dia.c); summary(SF2_L1_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L1_dia.c,type="l", col=1)
# plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1, ylim=c(40,50))
SF2_L1_dia_diff.c[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[1])]
SF2_L1_dia_diff.c[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[2])]
SF2_L1_dia_diff.c[which(SF2_L1_dia_diff == sort(SF2_L1_dia_diff, TRUE)[3])]
summary (SF2_L1_dia.c)

# SF2_L4
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L4_dia_diff <- diff(dendro_SF2_raw$SF2_L4_dia_raw)
plot(SF2_Date_diff.px,SF2_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L4_dia_diff,main="SF2_L4")
boxplot(SF2_L4_dia_diff,ylim=c(-1,1),main="SF2_L4")
sort(SF2_L4_dia_diff, TRUE)[1:10]
sort(SF2_L4_dia_diff, FALSE)[1:10]
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[1])] # to be removed
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[2])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[3])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[4])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[5])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[6])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[7])]
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[8])]
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[9])]
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[10])]
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[1])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[2])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[3])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[4])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[5])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[6])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[7])] #
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[8])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[9])] 
dendro_SF2_raw$Date_Time.px[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[10])] 
SF2_L4_NA <- is.na(dendro_SF2_raw$SF2_L4_dia_raw)
SF2_L4_NAdiff <- SF2_L4_NA[-1]
SF2_L4_noNA <- na.locf(dendro_SF2_raw$SF2_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L4_noNA,type="l", col=2)
SF2_L4_dia_diff_noNA <- diff(SF2_L4_noNA)
SF2_L4_dia_diff.c <-ifelse((SF2_L4_dia_diff_noNA > 0.1 & SF2_L4_NAdiff == FALSE) | (SF2_L4_dia_diff_noNA < -0.6 & SF2_L4_NAdiff == FALSE),NA,SF2_L4_dia_diff_noNA) 
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[4])] <- NA
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[5])] <- NA
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, FALSE)[7])] <- NA
plot(SF2_Date_diff.px,SF2_L4_dia_diff.c,type="l", col=1)
boxplot(SF2_L4_dia_diff.c,main="SF2_L4")
summary(SF2_L4_dia_diff.c)
SF2_L4_dia_diff.0 <- na.fill(SF2_L4_dia_diff.c,0);summary(SF2_L4_dia_diff.0)
SF2_L4_dia.c <- diffinv(SF2_L4_dia_diff.0)
SF2_L4_dia.NA <- is.na(dendro_SF2_raw$SF2_L4_dia_raw)
SF2_L4_dia.c <- ifelse(SF2_L4_dia.NA == TRUE, NA, SF2_L4_dia.c); summary(SF2_L4_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L4_dia.c,type="l", col=1)
# plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1, ylim=c(40,50))
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[1])]
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[2])]
SF2_L4_dia_diff.c[which(SF2_L4_dia_diff == sort(SF2_L4_dia_diff, TRUE)[3])]
summary (SF2_L4_dia.c)

# SF2_L5
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L5_dia_diff <- diff(dendro_SF2_raw$SF2_L5_dia_raw)
plot(SF2_Date_diff.px,SF2_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L5_dia_diff,main="SF2_L5")
boxplot(SF2_L5_dia_diff,ylim=c(-1,1),main="SF2_L5")
sort(SF2_L5_dia_diff, TRUE)[1:10]
sort(SF2_L5_dia_diff, FALSE)[1:10]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[1])]; sort(SF2_L5_dia_diff, TRUE)[1] # to be removed
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[2])]; sort(SF2_L5_dia_diff, TRUE)[2] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[3])]; sort(SF2_L5_dia_diff, TRUE)[3] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[4])]; sort(SF2_L5_dia_diff, TRUE)[4] 
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[5])]; sort(SF2_L5_dia_diff, TRUE)[5] 
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[6])]; sort(SF2_L5_dia_diff, TRUE)[6] 
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[7])]; sort(SF2_L5_dia_diff, TRUE)[7]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[8])]; sort(SF2_L5_dia_diff, TRUE)[8]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[9])]; sort(SF2_L5_dia_diff, TRUE)[9]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[10])]; sort(SF2_L5_dia_diff, TRUE)[10]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[1])]; sort(SF2_L5_dia_diff, FALSE)[1] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[2])]; sort(SF2_L5_dia_diff, FALSE)[2] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[3])]; sort(SF2_L5_dia_diff, FALSE)[3] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[4])]; sort(SF2_L5_dia_diff, FALSE)[4] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[5])]; sort(SF2_L5_dia_diff, FALSE)[5] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[6])]; sort(SF2_L5_dia_diff, FALSE)[6] 
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[7])]; sort(SF2_L5_dia_diff, FALSE)[7] #
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[8])]; sort(SF2_L5_dia_diff, FALSE)[8]
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[9])]; sort(SF2_L5_dia_diff, FALSE)[9] 
dendro_SF2_raw$Date_Time.px[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[10])]; sort(SF2_L5_dia_diff, FALSE)[10] 
SF2_L5_NA <- is.na(dendro_SF2_raw$SF2_L5_dia_raw)
SF2_L5_NAdiff <- SF2_L5_NA[-1]
SF2_L5_noNA <- na.locf(dendro_SF2_raw$SF2_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L5_noNA,type="l", col=2)
SF2_L5_dia_diff_noNA <- diff(SF2_L5_noNA)
SF2_L5_dia_diff.c <-ifelse((SF2_L5_dia_diff_noNA > 0.6 & SF2_L5_NAdiff == FALSE) | (SF2_L5_dia_diff_noNA < -0.1 & SF2_L5_NAdiff == FALSE),NA,SF2_L5_dia_diff_noNA) 
SF2_L5_dia_diff.c[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[7])] <- NA
plot(SF2_Date_diff.px,SF2_L5_dia_diff.c,type="l", col=1)
boxplot(SF2_L5_dia_diff.c,main="SF2_L5")
summary(SF2_L5_dia_diff.c)
SF2_L5_dia_diff.0 <- na.fill(SF2_L5_dia_diff.c,0);summary(SF2_L5_dia_diff.0)
SF2_L5_dia.c <- diffinv(SF2_L5_dia_diff.0)
SF2_L5_dia.NA <- is.na(dendro_SF2_raw$SF2_L5_dia_raw)
SF2_L5_dia.c <- ifelse(SF2_L5_dia.NA == TRUE, NA, SF2_L5_dia.c); summary(SF2_L5_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L5_dia.c,type="l", col=1)
# plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1, ylim=c(40,50))
SF2_L5_dia_diff.c[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[1])]
SF2_L5_dia_diff.c[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[2])]
SF2_L5_dia_diff.c[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, TRUE)[3])]
summary (SF2_L5_dia.c)

# SF2_L7
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L7_dia_diff <- diff(dendro_SF2_raw$SF2_L7_dia_raw)
plot(SF2_Date_diff.px,SF2_L7_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L7_dia_diff,main="SF2_L7")
boxplot(SF2_L7_dia_diff,ylim=c(-1,1),main="SF2_L7")
sort(SF2_L7_dia_diff, TRUE)[1:10]
sort(SF2_L7_dia_diff, FALSE)[1:10]
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[1])]; sort(SF2_L7_dia_diff, TRUE)[1] # to be removed
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[2])]; sort(SF2_L7_dia_diff, TRUE)[2] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[3])]; sort(SF2_L7_dia_diff, TRUE)[3] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[4])]; sort(SF2_L7_dia_diff, TRUE)[4] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[5])]; sort(SF2_L7_dia_diff, TRUE)[5] 
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[6])]; sort(SF2_L7_dia_diff, TRUE)[6] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[7])]; sort(SF2_L7_dia_diff, TRUE)[7]
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[8])]; sort(SF2_L7_dia_diff, TRUE)[8]
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[9])]; sort(SF2_L7_dia_diff, TRUE)[9]
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[10])]; sort(SF2_L7_dia_diff, TRUE)[10]
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[1])]; sort(SF2_L7_dia_diff, FALSE)[1] # 
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[2])]; sort(SF2_L7_dia_diff, FALSE)[2] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[3])]; sort(SF2_L7_dia_diff, FALSE)[3] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[4])]; sort(SF2_L7_dia_diff, FALSE)[4] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[5])]; sort(SF2_L7_dia_diff, FALSE)[5] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[6])]; sort(SF2_L7_dia_diff, FALSE)[6] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[7])]; sort(SF2_L7_dia_diff, FALSE)[7] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[8])]; sort(SF2_L7_dia_diff, FALSE)[8] 
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[9])]; sort(SF2_L7_dia_diff, FALSE)[9] #
dendro_SF2_raw$Date_Time.px[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[10])]; sort(SF2_L7_dia_diff, FALSE)[10] 
SF2_L7_NA <- is.na(dendro_SF2_raw$SF2_L7_dia_raw)
SF2_L7_NAdiff <- SF2_L7_NA[-1]
SF2_L7_noNA <- na.locf(dendro_SF2_raw$SF2_L7_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L7_noNA,type="l", col=2)
SF2_L7_dia_diff_noNA <- diff(SF2_L7_noNA)
SF2_L7_dia_diff.c <-ifelse((SF2_L7_dia_diff_noNA > 0.1 & SF2_L7_NAdiff == FALSE) | (SF2_L7_dia_diff_noNA < -0.14 & SF2_L7_NAdiff == FALSE),NA,SF2_L7_dia_diff_noNA) 
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[6])] <- NA
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[9])] <- NA
plot(SF2_Date_diff.px,SF2_L7_dia_diff.c,type="l", col=1)
boxplot(SF2_L7_dia_diff.c,main="SF2_L7")
summary(SF2_L7_dia_diff.c)
SF2_L7_dia_diff.0 <- na.fill(SF2_L7_dia_diff.c,0);summary(SF2_L7_dia_diff.0)
SF2_L7_dia.c <- diffinv(SF2_L7_dia_diff.0)
SF2_L7_dia.NA <- is.na(dendro_SF2_raw$SF2_L7_dia_raw)
SF2_L7_dia.c <- ifelse(SF2_L7_dia.NA == TRUE, NA, SF2_L7_dia.c); summary(SF2_L7_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L7_dia.c,type="l", col=1)
# plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1, ylim=c(40,50))
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[1])]
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[2])]
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, TRUE)[3])]
summary (SF2_L7_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF2_corr_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF2 (raw data)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=2)
lines(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=3)
lines(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=4)

# Plot all trees of SF2 (differences)
plot(SF2_Date_diff.px,SF2_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF2_Date_diff.px,SF2_L4_dia_diff,type="l", col=2)
lines(SF2_Date_diff.px,SF2_L5_dia_diff,type="l", col=3)
lines(SF2_Date_diff.px,SF2_L7_dia_diff,type="l", col=4)
legend("topleft",c("L1","L4","L5","L7"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF2 (cleaned data)
plot(dendro_SF2_raw$Date_Time.px,SF2_L1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw$Date_Time.px,SF2_L4_dia.c,type="l", col=2)
lines(dendro_SF2_raw$Date_Time.px,SF2_L5_dia.c,type="l", col=3)
lines(dendro_SF2_raw$Date_Time.px,SF2_L7_dia.c,type="l", col=4)
legend("topleft",c("L1","L4","L5","L7"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF2_comp_raw-cleared_corr_05_2015.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw$Date_Time.px,SF2_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw$Date_Time.px,SF2_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw$Date_Time.px,SF2_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw$Date_Time.px,SF2_L7_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

summary(dendro_SF3_raw)
# SF3_L3 ####
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L3_dia_diff <- diff(dendro_SF3_raw$SF3_L3_dia_raw)
plot(SF3_Date_diff.px,SF3_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L3_dia_diff,main="SF3_L3")
boxplot(SF3_L3_dia_diff,ylim=c(-1,1),main="SF3_L3")
sort(SF3_L3_dia_diff, TRUE)[1:10]
sort(SF3_L3_dia_diff, FALSE)[1:10]
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[1])]; sort(SF3_L3_dia_diff, TRUE)[1] # to be removed
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[2])]; sort(SF3_L3_dia_diff, TRUE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[3])]; sort(SF3_L3_dia_diff, TRUE)[3] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[4])]; sort(SF3_L3_dia_diff, TRUE)[4] 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[5])]; sort(SF3_L3_dia_diff, TRUE)[5] 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[6])]; sort(SF3_L3_dia_diff, TRUE)[6] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[7])]; sort(SF3_L3_dia_diff, TRUE)[7]
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[8])]; sort(SF3_L3_dia_diff, TRUE)[8]
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[9])]; sort(SF3_L3_dia_diff, TRUE)[9]
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[10])]; sort(SF3_L3_dia_diff, TRUE)[10]
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[1])]; sort(SF3_L3_dia_diff, FALSE)[1] # 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[2])]; sort(SF3_L3_dia_diff, FALSE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[3])]; sort(SF3_L3_dia_diff, FALSE)[3] 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[4])]; sort(SF3_L3_dia_diff, FALSE)[4] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[5])]; sort(SF3_L3_dia_diff, FALSE)[5] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[6])]; sort(SF3_L3_dia_diff, FALSE)[6] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[7])]; sort(SF3_L3_dia_diff, FALSE)[7] 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[8])]; sort(SF3_L3_dia_diff, FALSE)[8] #
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[9])]; sort(SF3_L3_dia_diff, FALSE)[9] 
dendro_SF3_raw$Date_Time.px[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[10])]; sort(SF3_L3_dia_diff, FALSE)[10] 
SF3_L3_NA <- is.na(dendro_SF3_raw$SF3_L3_dia_raw)
SF3_L3_NAdiff <- SF3_L3_NA[-1]
SF3_L3_noNA <- na.locf(dendro_SF3_raw$SF3_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L3_noNA,type="l", col=2)
SF3_L3_dia_diff_noNA <- diff(SF3_L3_noNA)
SF3_L3_dia_diff.c <-ifelse((SF3_L3_dia_diff_noNA > 0.75 & SF3_L3_NAdiff == FALSE) | (SF3_L3_dia_diff_noNA < -0.5 & SF3_L3_NAdiff == FALSE),NA,SF3_L3_dia_diff_noNA) 
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[6])] <- NA
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[c(4)])] <- NA
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[c(5)])] <- NA
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[c(6)])] <- NA
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, FALSE)[c(8)])] <- NA
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
boxplot(SF3_L3_dia_diff.c,main="SF3_L3")
summary(SF3_L3_dia_diff.c)
SF3_L3_dia_diff.0 <- na.fill(SF3_L3_dia_diff.c,0);summary(SF3_L3_dia_diff.0)
SF3_L3_dia.c <- diffinv(SF3_L3_dia_diff.0)
SF3_L3_dia.NA <- is.na(dendro_SF3_raw$SF3_L3_dia_raw)
SF3_L3_dia.c <- ifelse(SF3_L3_dia.NA == TRUE, NA, SF3_L3_dia.c); summary(SF3_L3_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c,type="l", col=1)
# plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1, ylim=c(40,50))
july27.2013 <- as.POSIXct("2013-07-27 00:00", format="%Y-%m-%d %H:%M")
aug07.2013 <- as.POSIXct("2013-08-07 12:00", format="%Y-%m-%d %H:%M")
SF3_L3_dia.c[c(match(july27.2013,dendro_SF3_raw$Date_Time.px):  
                 match(aug07.2013,dendro_SF3_raw$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c,type="l", col=1)
SF3_L3_dia_diff.c[c(match(july27.2013,dendro_SF3_raw$Date_Time.px):
                      match(aug07.2013,dendro_SF3_raw$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
summary(SF3_L3_dia.c); summary(SF3_L3_dia_diff.c)
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[1])]
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[2])]
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[3])]
summary (SF3_L3_dia.c)

# SF3_L4
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L4_dia_diff <- diff(dendro_SF3_raw$SF3_L4_dia_raw)
plot(SF3_Date_diff.px,SF3_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L4_dia_diff,main="SF3_L4")
boxplot(SF3_L4_dia_diff,ylim=c(-1,1),main="SF3_L4")
sort(SF3_L4_dia_diff, TRUE)[1:10]
sort(SF3_L4_dia_diff, FALSE)[1:10]
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[1])]; sort(SF3_L4_dia_diff, TRUE)[1] # to be removed
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[2])]; sort(SF3_L4_dia_diff, TRUE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[3])]; sort(SF3_L4_dia_diff, TRUE)[3] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[4])]; sort(SF3_L4_dia_diff, TRUE)[4] # removed even though not control dat (>1 mm)
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[5])]; sort(SF3_L4_dia_diff, TRUE)[5] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[6])]; sort(SF3_L4_dia_diff, TRUE)[6] # removed even though not control dat (>1 mm)
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[7])]; sort(SF3_L4_dia_diff, TRUE)[7]
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[8])]; sort(SF3_L4_dia_diff, TRUE)[8] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[9])]; sort(SF3_L4_dia_diff, TRUE)[9] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[10])]; sort(SF3_L4_dia_diff, TRUE)[10]
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[1])]; sort(SF3_L4_dia_diff, FALSE)[1] # 
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[2])]; sort(SF3_L4_dia_diff, FALSE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[3])]; sort(SF3_L4_dia_diff, FALSE)[3] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[4])]; sort(SF3_L4_dia_diff, FALSE)[4] 
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[5])]; sort(SF3_L4_dia_diff, FALSE)[5] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[6])]; sort(SF3_L4_dia_diff, FALSE)[6] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[7])]; sort(SF3_L4_dia_diff, FALSE)[7] #
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[8])]; sort(SF3_L4_dia_diff, FALSE)[8] 
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[9])]; sort(SF3_L4_dia_diff, FALSE)[9] 
dendro_SF3_raw$Date_Time.px[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[10])]; sort(SF3_L4_dia_diff, FALSE)[10] 
SF3_L4_NA <- is.na(dendro_SF3_raw$SF3_L4_dia_raw)
SF3_L4_NAdiff <- SF3_L4_NA[-1]
SF3_L4_noNA <- na.locf(dendro_SF3_raw$SF3_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L4_noNA,type="l", col=2)
SF3_L4_dia_diff_noNA <- diff(SF3_L4_noNA)
SF3_L4_dia_diff.c <-ifelse((SF3_L4_dia_diff_noNA > 1 & SF3_L4_NAdiff == FALSE) | (SF3_L4_dia_diff_noNA < -0.19 & SF3_L4_NAdiff == FALSE),NA,SF3_L4_dia_diff_noNA) 
SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[8])] <- NA
SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[9])] <- NA
# SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, FALSE)[9])] <- NA
plot(SF3_Date_diff.px,SF3_L4_dia_diff.c,type="l", col=1)
boxplot(SF3_L4_dia_diff.c,main="SF3_L4")
summary(SF3_L4_dia_diff.c)
SF3_L4_dia_diff.0 <- na.fill(SF3_L4_dia_diff.c,0);summary(SF3_L4_dia_diff.0)
SF3_L4_dia.c <- diffinv(SF3_L4_dia_diff.0)
SF3_L4_dia.NA <- is.na(dendro_SF3_raw$SF3_L4_dia_raw)
SF3_L4_dia.c <- ifelse(SF3_L4_dia.NA == TRUE, NA, SF3_L4_dia.c); summary(SF3_L4_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L4_dia.c,type="l", col=1)
# plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1, ylim=c(40,50))
SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[1])]
SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[2])]
SF3_L4_dia_diff.c[which(SF3_L4_dia_diff == sort(SF3_L4_dia_diff, TRUE)[3])]
summary (SF3_L4_dia.c)

# SF3_L5
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L5_dia_diff <- diff(dendro_SF3_raw$SF3_L5_dia_raw)
plot(SF3_Date_diff.px,SF3_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L5_dia_diff,main="SF3_L5")
boxplot(SF3_L5_dia_diff,ylim=c(-1,1),main="SF3_L5")
sort(SF3_L5_dia_diff, TRUE)[1:10]
sort(SF3_L5_dia_diff, FALSE)[1:10]
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[1])]; sort(SF3_L5_dia_diff, TRUE)[1] # to be removed
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[2])]; sort(SF3_L5_dia_diff, TRUE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[3])]; sort(SF3_L5_dia_diff, TRUE)[3] #
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[4])]; sort(SF3_L5_dia_diff, TRUE)[4] #
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[5])]; sort(SF3_L5_dia_diff, TRUE)[5] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[6])]; sort(SF3_L5_dia_diff, TRUE)[6] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[7])]; sort(SF3_L5_dia_diff, TRUE)[7] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[8])]; sort(SF3_L5_dia_diff, TRUE)[8]
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[9])]; sort(SF3_L5_dia_diff, TRUE)[9]
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[10])]; sort(SF3_L5_dia_diff, TRUE)[10]
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[1])]; sort(SF3_L5_dia_diff, FALSE)[1] # 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[2])]; sort(SF3_L5_dia_diff, FALSE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[3])]; sort(SF3_L5_dia_diff, FALSE)[3] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[4])]; sort(SF3_L5_dia_diff, FALSE)[4] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[5])]; sort(SF3_L5_dia_diff, FALSE)[5] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[6])]; sort(SF3_L5_dia_diff, FALSE)[6] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[7])]; sort(SF3_L5_dia_diff, FALSE)[7] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[8])]; sort(SF3_L5_dia_diff, FALSE)[8] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[9])]; sort(SF3_L5_dia_diff, FALSE)[9] 
dendro_SF3_raw$Date_Time.px[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[10])]; sort(SF3_L5_dia_diff, FALSE)[10] 
SF3_L5_NA <- is.na(dendro_SF3_raw$SF3_L5_dia_raw)
SF3_L5_NAdiff <- SF3_L5_NA[-1]
SF3_L5_noNA <- na.locf(dendro_SF3_raw$SF3_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L5_noNA,type="l", col=2)
SF3_L5_dia_diff_noNA <- diff(SF3_L5_noNA)
SF3_L5_dia_diff.c <-ifelse((SF3_L5_dia_diff_noNA > 0.1 & SF3_L5_NAdiff == FALSE) | (SF3_L5_dia_diff_noNA < -0.5 & SF3_L5_NAdiff == FALSE),NA,SF3_L5_dia_diff_noNA) 
# SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[6])] <- NA
# SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[9])] <- NA
plot(SF3_Date_diff.px,SF3_L5_dia_diff.c,type="l", col=1)
boxplot(SF3_L5_dia_diff.c,main="SF3_L5")
summary(SF3_L5_dia_diff.c)
SF3_L5_dia_diff.0 <- na.fill(SF3_L5_dia_diff.c,0);summary(SF3_L5_dia_diff.0)
SF3_L5_dia.c <- diffinv(SF3_L5_dia_diff.0)
SF3_L5_dia.NA <- is.na(dendro_SF3_raw$SF3_L5_dia_raw)
SF3_L5_dia.c <- ifelse(SF3_L5_dia.NA == TRUE, NA, SF3_L5_dia.c); summary(SF3_L5_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L5_dia.c,type="l", col=1)
# plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1, ylim=c(40,50))
SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[1])]
SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[2])]
SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, TRUE)[3])]
summary (SF3_L5_dia.c)

# SF3_L6
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L6_dia_diff <- diff(dendro_SF3_raw$SF3_L6_dia_raw)
plot(SF3_Date_diff.px,SF3_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L6_dia_diff,main="SF3_L6")
boxplot(SF3_L6_dia_diff,ylim=c(-1,1),main="SF3_L6")
sort(SF3_L6_dia_diff, TRUE)[1:10]
sort(SF3_L6_dia_diff, FALSE)[1:10]
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[1])]; sort(SF3_L6_dia_diff, TRUE)[1] # to be removed
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[2])]; sort(SF3_L6_dia_diff, TRUE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[3])]; sort(SF3_L6_dia_diff, TRUE)[3] #
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[4])]; sort(SF3_L6_dia_diff, TRUE)[4] #
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[5])]; sort(SF3_L6_dia_diff, TRUE)[5] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[6])]; sort(SF3_L6_dia_diff, TRUE)[6] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[7])]; sort(SF3_L6_dia_diff, TRUE)[7]
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[8])]; sort(SF3_L6_dia_diff, TRUE)[8]
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[9])]; sort(SF3_L6_dia_diff, TRUE)[9]
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[10])]; sort(SF3_L6_dia_diff, TRUE)[10]
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[1])]; sort(SF3_L6_dia_diff, FALSE)[1] # 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[2])]; sort(SF3_L6_dia_diff, FALSE)[2] #
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[3])]; sort(SF3_L6_dia_diff, FALSE)[3] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[4])]; sort(SF3_L6_dia_diff, FALSE)[4] #
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[5])]; sort(SF3_L6_dia_diff, FALSE)[5] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[6])]; sort(SF3_L6_dia_diff, FALSE)[6] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[7])]; sort(SF3_L6_dia_diff, FALSE)[7] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[8])]; sort(SF3_L6_dia_diff, FALSE)[8] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[9])]; sort(SF3_L6_dia_diff, FALSE)[9] 
dendro_SF3_raw$Date_Time.px[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[10])]; sort(SF3_L6_dia_diff, FALSE)[10] 
SF3_L6_NA <- is.na(dendro_SF3_raw$SF3_L6_dia_raw)
SF3_L6_NAdiff <- SF3_L6_NA[-1]
SF3_L6_noNA <- na.locf(dendro_SF3_raw$SF3_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L6_noNA,type="l", col=2)
SF3_L6_dia_diff_noNA <- diff(SF3_L6_noNA)
SF3_L6_dia_diff.c <-ifelse((SF3_L6_dia_diff_noNA > 0.4 & SF3_L6_NAdiff == FALSE) | (SF3_L6_dia_diff_noNA < -0.5 & SF3_L6_NAdiff == FALSE),NA,SF3_L6_dia_diff_noNA) 
# SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[6])] <- NA
SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[4])] <- NA
plot(SF3_Date_diff.px,SF3_L6_dia_diff.c,type="l", col=1)
boxplot(SF3_L6_dia_diff.c,main="SF3_L6")
summary(SF3_L6_dia_diff.c)
SF3_L6_dia_diff.0 <- na.fill(SF3_L6_dia_diff.c,0);summary(SF3_L6_dia_diff.0)
SF3_L6_dia.c <- diffinv(SF3_L6_dia_diff.0)
SF3_L6_dia.NA <- is.na(dendro_SF3_raw$SF3_L6_dia_raw)
SF3_L6_dia.c <- ifelse(SF3_L6_dia.NA == TRUE, NA, SF3_L6_dia.c); summary(SF3_L6_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L6_dia.c,type="l", col=1)
# plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1, ylim=c(40,50))
SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[1])]
SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[2])]
SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, TRUE)[3])]
summary (SF3_L6_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF3_cleared_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF3 (raw data)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1,ylim=c(0,65))
lines(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=2)
lines(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=3)
lines(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=4)

# Plot all trees of SF3 (differences)
plot(SF3_Date_diff.px,SF3_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF3_Date_diff.px,SF3_L4_dia_diff,type="l", col=2)
lines(SF3_Date_diff.px,SF3_L5_dia_diff,type="l", col=3)
lines(SF3_Date_diff.px,SF3_L6_dia_diff,type="l", col=4)
legend("topleft",c("L3","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF3 (cleaned data)
plot(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF3_raw$Date_Time.px,SF3_L4_dia.c,type="l", col=2)
lines(dendro_SF3_raw$Date_Time.px,SF3_L5_dia.c,type="l", col=3)
lines(dendro_SF3_raw$Date_Time.px,SF3_L6_dia.c,type="l", col=4)
legend("topleft",c("L3","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF3_comp_raw-cleared_05_2015.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw$Date_Time.px,SF3_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw$Date_Time.px,SF3_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF3_raw$Date_Time.px,SF3_L6_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

summary(dendro_SF4_raw)
# SF4_L1 ####
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L1_dia_diff <- diff(dendro_SF4_raw$SF4_L1_dia_raw)
plot(SF4_Date_diff.px,SF4_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L1_dia_diff,main="SF4_L1")
boxplot(SF4_L1_dia_diff,ylim=c(-1,1),main="SF4_L1")
sort(SF4_L1_dia_diff, TRUE)[1:10]
sort(SF4_L1_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[1])]; sort(SF4_L1_dia_diff, TRUE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[2])]; sort(SF4_L1_dia_diff, TRUE)[2] #
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[3])]; sort(SF4_L1_dia_diff, TRUE)[3] #
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[4])]; sort(SF4_L1_dia_diff, TRUE)[4] #
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[5])]; sort(SF4_L1_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[6])]; sort(SF4_L1_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[7])]; sort(SF4_L1_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[8])]; sort(SF4_L1_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[9])]; sort(SF4_L1_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[10])]; sort(SF4_L1_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[1])]; sort(SF4_L1_dia_diff, FALSE)[1] # 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[2])]; sort(SF4_L1_dia_diff, FALSE)[2] #
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[3])]; sort(SF4_L1_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[4])]; sort(SF4_L1_dia_diff, FALSE)[4] #
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[5])]; sort(SF4_L1_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[6])]; sort(SF4_L1_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[7])]; sort(SF4_L1_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[8])]; sort(SF4_L1_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[9])]; sort(SF4_L1_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[10])]; sort(SF4_L1_dia_diff, FALSE)[10] 
SF4_L1_NA <- is.na(dendro_SF4_raw$SF4_L1_dia_raw)
SF4_L1_NAdiff <- SF4_L1_NA[-1]
SF4_L1_noNA <- na.locf(dendro_SF4_raw$SF4_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L1_noNA,type="l", col=2)
SF4_L1_dia_diff_noNA <- diff(SF4_L1_noNA)
SF4_L1_dia_diff.c <-ifelse((SF4_L1_dia_diff_noNA > 1 & SF4_L1_NAdiff == FALSE) | (SF4_L1_dia_diff_noNA < -1 & SF4_L1_NAdiff == FALSE),NA,SF4_L1_dia_diff_noNA) 
# SF4_L1_dia_diff.c[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[6])] <- NA
# SF4_L1_dia_diff.c[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_L1_dia_diff.c,type="l", col=1)
boxplot(SF4_L1_dia_diff.c,main="SF4_L1")
summary(SF4_L1_dia_diff.c)
SF4_L1_dia_diff.0 <- na.fill(SF4_L1_dia_diff.c,0);summary(SF4_L1_dia_diff.0)
SF4_L1_dia.c <- diffinv(SF4_L1_dia_diff.0)
SF4_L1_dia.NA <- is.na(dendro_SF4_raw$SF4_L1_dia_raw)
SF4_L1_dia.c <- ifelse(SF4_L1_dia.NA == TRUE, NA, SF4_L1_dia.c); summary(SF4_L1_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L1_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_L1_dia_diff.c[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[1])]
SF4_L1_dia_diff.c[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[2])]
SF4_L1_dia_diff.c[which(SF4_L1_dia_diff == sort(SF4_L1_dia_diff, TRUE)[3])]
summary (SF4_L1_dia.c)

# SF4_L2
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L2_dia_diff <- diff(dendro_SF4_raw$SF4_L2_dia_raw)
plot(SF4_Date_diff.px,SF4_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L2_dia_diff,main="SF4_L2")
boxplot(SF4_L2_dia_diff,ylim=c(-1,1),main="SF4_L2")
sort(SF4_L2_dia_diff, TRUE)[1:10]
sort(SF4_L2_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[1])]; sort(SF4_L2_dia_diff, TRUE)[1] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[2])]; sort(SF4_L2_dia_diff, TRUE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[3])]; sort(SF4_L2_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[4])]; sort(SF4_L2_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[5])]; sort(SF4_L2_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[6])]; sort(SF4_L2_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[7])]; sort(SF4_L2_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[8])]; sort(SF4_L2_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[9])]; sort(SF4_L2_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[10])]; sort(SF4_L2_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[1])]; sort(SF4_L2_dia_diff, FALSE)[1]  
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[2])]; sort(SF4_L2_dia_diff, FALSE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[3])]; sort(SF4_L2_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[4])]; sort(SF4_L2_dia_diff, FALSE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[5])]; sort(SF4_L2_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[6])]; sort(SF4_L2_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[7])]; sort(SF4_L2_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[8])]; sort(SF4_L2_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[9])]; sort(SF4_L2_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[10])]; sort(SF4_L2_dia_diff, FALSE)[10] 
SF4_L2_NA <- is.na(dendro_SF4_raw$SF4_L2_dia_raw)
SF4_L2_NAdiff <- SF4_L2_NA[-1]
SF4_L2_noNA <- na.locf(dendro_SF4_raw$SF4_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L2_noNA,type="l", col=2)
SF4_L2_dia_diff_noNA <- diff(SF4_L2_noNA)
SF4_L2_dia_diff.c <-ifelse((SF4_L2_dia_diff_noNA > 0.5 & SF4_L2_NAdiff == FALSE) | (SF4_L2_dia_diff_noNA < -0.5 & SF4_L2_NAdiff == FALSE),NA,SF4_L2_dia_diff_noNA) 
# SF4_L2_dia_diff.c[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[6])] <- NA
# SF4_L2_dia_diff.c[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_L2_dia_diff.c,type="l", col=1)
boxplot(SF4_L2_dia_diff.c,main="SF4_L2")
summary(SF4_L2_dia_diff.c)
SF4_L2_dia_diff.0 <- na.fill(SF4_L2_dia_diff.c,0);summary(SF4_L2_dia_diff.0)
SF4_L2_dia.c <- diffinv(SF4_L2_dia_diff.0)
SF4_L2_dia.NA <- is.na(dendro_SF4_raw$SF4_L2_dia_raw)
SF4_L2_dia.c <- ifelse(SF4_L2_dia.NA == TRUE, NA, SF4_L2_dia.c); summary(SF4_L2_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L2_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_L2_dia_diff.c[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[1])]
SF4_L2_dia_diff.c[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[2])]
SF4_L2_dia_diff.c[which(SF4_L2_dia_diff == sort(SF4_L2_dia_diff, TRUE)[3])]
summary (SF4_L2_dia.c)

# SF4_L3 
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L3_dia_diff <- diff(dendro_SF4_raw$SF4_L3_dia_raw)
plot(SF4_Date_diff.px,SF4_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L3_dia_diff,main="SF4_L3")
boxplot(SF4_L3_dia_diff,ylim=c(-1,1),main="SF4_L3")
sort(SF4_L3_dia_diff, TRUE)[1:10]
sort(SF4_L3_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[1])]; sort(SF4_L3_dia_diff, TRUE)[1] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[2])]; sort(SF4_L3_dia_diff, TRUE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[3])]; sort(SF4_L3_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[4])]; sort(SF4_L3_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[5])]; sort(SF4_L3_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[6])]; sort(SF4_L3_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[7])]; sort(SF4_L3_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[8])]; sort(SF4_L3_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[9])]; sort(SF4_L3_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[10])]; sort(SF4_L3_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[1])]; sort(SF4_L3_dia_diff, FALSE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[2])]; sort(SF4_L3_dia_diff, FALSE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[3])]; sort(SF4_L3_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[4])]; sort(SF4_L3_dia_diff, FALSE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[5])]; sort(SF4_L3_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[6])]; sort(SF4_L3_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[7])]; sort(SF4_L3_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[8])]; sort(SF4_L3_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[9])]; sort(SF4_L3_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[10])]; sort(SF4_L3_dia_diff, FALSE)[10] 
SF4_L3_NA <- is.na(dendro_SF4_raw$SF4_L3_dia_raw)
SF4_L3_NAdiff <- SF4_L3_NA[-1]
SF4_L3_noNA <- na.locf(dendro_SF4_raw$SF4_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L3_noNA,type="l", col=2)
SF4_L3_dia_diff_noNA <- diff(SF4_L3_noNA)
SF4_L3_dia_diff.c <-ifelse((SF4_L3_dia_diff_noNA > 1 & SF4_L3_NAdiff == FALSE) | (SF4_L3_dia_diff_noNA < -1 & SF4_L3_NAdiff == FALSE),NA,SF4_L3_dia_diff_noNA) 
# SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[6])] <- NA
# SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_L3_dia_diff.c,type="l", col=1)
boxplot(SF4_L3_dia_diff.c,main="SF4_L3")
summary(SF4_L3_dia_diff.c)
SF4_L3_dia_diff.0 <- na.fill(SF4_L3_dia_diff.c,0);summary(SF4_L3_dia_diff.0)
SF4_L3_dia.c <- diffinv(SF4_L3_dia_diff.0)
SF4_L3_dia.NA <- is.na(dendro_SF4_raw$SF4_L3_dia_raw)
SF4_L3_dia.c <- ifelse(SF4_L3_dia.NA == TRUE, NA, SF4_L3_dia.c); summary(SF4_L3_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L3_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[1])]
SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[2])]
SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, TRUE)[3])]
summary (SF4_L3_dia.c)

# SF4_L4
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L4_dia_diff <- diff(dendro_SF4_raw$SF4_L4_dia_raw)
plot(SF4_Date_diff.px,SF4_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L4_dia_diff,main="SF4_L4")
boxplot(SF4_L4_dia_diff,ylim=c(-1,1),main="SF4_L4")
sort(SF4_L4_dia_diff, TRUE)[1:10]
sort(SF4_L4_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[1])]; sort(SF4_L4_dia_diff, TRUE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[2])]; sort(SF4_L4_dia_diff, TRUE)[2] #
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[3])]; sort(SF4_L4_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[4])]; sort(SF4_L4_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[5])]; sort(SF4_L4_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[6])]; sort(SF4_L4_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[7])]; sort(SF4_L4_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[8])]; sort(SF4_L4_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[9])]; sort(SF4_L4_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[10])]; sort(SF4_L4_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[1])]; sort(SF4_L4_dia_diff, FALSE)[1] # 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[2])]; sort(SF4_L4_dia_diff, FALSE)[2]
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[3])]; sort(SF4_L4_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[4])]; sort(SF4_L4_dia_diff, FALSE)[4] #
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[5])]; sort(SF4_L4_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[6])]; sort(SF4_L4_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[7])]; sort(SF4_L4_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[8])]; sort(SF4_L4_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[9])]; sort(SF4_L4_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[10])]; sort(SF4_L4_dia_diff, FALSE)[10] 
SF4_L4_NA <- is.na(dendro_SF4_raw$SF4_L4_dia_raw)
SF4_L4_NAdiff <- SF4_L4_NA[-1]
SF4_L4_noNA <- na.locf(dendro_SF4_raw$SF4_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L4_noNA,type="l", col=2)
SF4_L4_dia_diff_noNA <- diff(SF4_L4_noNA)
SF4_L4_dia_diff.c <-ifelse((SF4_L4_dia_diff_noNA > 0.3 & SF4_L4_NAdiff == FALSE) | (SF4_L4_dia_diff_noNA < -0.31 & SF4_L4_NAdiff == FALSE),NA,SF4_L4_dia_diff_noNA) 
# SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[6])] <- NA
# SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_L4_dia_diff.c,type="l", col=1)
boxplot(SF4_L4_dia_diff.c,main="SF4_L4")
summary(SF4_L4_dia_diff.c)
SF4_L4_dia_diff.0 <- na.fill(SF4_L4_dia_diff.c,0);summary(SF4_L4_dia_diff.0)
SF4_L4_dia.c <- diffinv(SF4_L4_dia_diff.0)
SF4_L4_dia.NA <- is.na(dendro_SF4_raw$SF4_L4_dia_raw)
SF4_L4_dia.c <- ifelse(SF4_L4_dia.NA == TRUE, NA, SF4_L4_dia.c); summary(SF4_L4_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L4_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[1])]
SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[2])]
SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, TRUE)[3])]
summary (SF4_L4_dia.c)


win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4L_cleared_corr_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF4 (raw data)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=2)
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=3)
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=4)

# Plot all trees of SF4 (differences)
plot(SF4_Date_diff.px,SF4_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF4_Date_diff.px,SF4_L2_dia_diff,type="l", col=2)
lines(SF4_Date_diff.px,SF4_L3_dia_diff,type="l", col=3)
lines(SF4_Date_diff.px,SF4_L4_dia_diff,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF4 (cleaned data)
plot(dendro_SF4_raw$Date_Time.px,SF4_L1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_L2_dia.c,type="l", col=2)
lines(dendro_SF4_raw$Date_Time.px,SF4_L3_dia.c,type="l", col=3)
lines(dendro_SF4_raw$Date_Time.px,SF4_L4_dia.c,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
 dev.off()
dev.off()

# SF4_Z1 ####
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z1_dia_diff <- diff(dendro_SF4_raw$SF4_Z1_dia_raw)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z1_dia_diff,main="SF4_Z1")
boxplot(SF4_Z1_dia_diff,ylim=c(-1,1),main="SF4_Z1")
sort(SF4_Z1_dia_diff, TRUE)[1:10]
sort(SF4_Z1_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[1])]; sort(SF4_Z1_dia_diff, TRUE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[2])]; sort(SF4_Z1_dia_diff, TRUE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[3])]; sort(SF4_Z1_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[4])]; sort(SF4_Z1_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[5])]; sort(SF4_Z1_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[6])]; sort(SF4_Z1_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[7])]; sort(SF4_Z1_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[8])]; sort(SF4_Z1_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[9])]; sort(SF4_Z1_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[10])]; sort(SF4_Z1_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[1])]; sort(SF4_Z1_dia_diff, FALSE)[1] # 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[2])]; sort(SF4_Z1_dia_diff, FALSE)[2]
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[3])]; sort(SF4_Z1_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[4])]; sort(SF4_Z1_dia_diff, FALSE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[5])]; sort(SF4_Z1_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[6])]; sort(SF4_Z1_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[7])]; sort(SF4_Z1_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[8])]; sort(SF4_Z1_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[9])]; sort(SF4_Z1_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[10])]; sort(SF4_Z1_dia_diff, FALSE)[10] 
SF4_Z1_NA <- is.na(dendro_SF4_raw$SF4_Z1_dia_raw)
SF4_Z1_NAdiff <- SF4_Z1_NA[-1]
SF4_Z1_noNA <- na.locf(dendro_SF4_raw$SF4_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z1_noNA,type="l", col=2)
SF4_Z1_dia_diff_noNA <- diff(SF4_Z1_noNA)
SF4_Z1_dia_diff.c <-ifelse((SF4_Z1_dia_diff_noNA > 1 & SF4_Z1_NAdiff == FALSE) | (SF4_Z1_dia_diff_noNA < -1 & SF4_Z1_NAdiff == FALSE),NA,SF4_Z1_dia_diff_noNA) 
# SF4_Z1_dia_diff.c[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[6])] <- NA
# SF4_Z1_dia_diff.c[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_Z1_dia_diff.c,type="l", col=1)
boxplot(SF4_Z1_dia_diff.c,main="SF4_Z1")
summary(SF4_Z1_dia_diff.c)
SF4_Z1_dia_diff.0 <- na.fill(SF4_Z1_dia_diff.c,0);summary(SF4_Z1_dia_diff.0)
SF4_Z1_dia.c <- diffinv(SF4_Z1_dia_diff.0)
SF4_Z1_dia.NA <- is.na(dendro_SF4_raw$SF4_Z1_dia_raw)
SF4_Z1_dia.c <- ifelse(SF4_Z1_dia.NA == TRUE, NA, SF4_Z1_dia.c); summary(SF4_Z1_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z1_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_Z1_dia_diff.c[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[1])]
SF4_Z1_dia_diff.c[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[2])]
SF4_Z1_dia_diff.c[which(SF4_Z1_dia_diff == sort(SF4_Z1_dia_diff, TRUE)[3])]
summary (SF4_Z1_dia.c)

# SF4_Z2
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z2_dia_diff <- diff(dendro_SF4_raw$SF4_Z2_dia_raw)
plot(SF4_Date_diff.px,SF4_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z2_dia_diff,main="SF4_Z2")
boxplot(SF4_Z2_dia_diff,ylim=c(-1,1),main="SF4_Z2")
sort(SF4_Z2_dia_diff, TRUE)[1:10]
sort(SF4_Z2_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[1])]; sort(SF4_Z2_dia_diff, TRUE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[2])]; sort(SF4_Z2_dia_diff, TRUE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[3])]; sort(SF4_Z2_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[4])]; sort(SF4_Z2_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[5])]; sort(SF4_Z2_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[6])]; sort(SF4_Z2_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[7])]; sort(SF4_Z2_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[8])]; sort(SF4_Z2_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[9])]; sort(SF4_Z2_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[10])]; sort(SF4_Z2_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[1])]; sort(SF4_Z2_dia_diff, FALSE)[1] # 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[2])]; sort(SF4_Z2_dia_diff, FALSE)[2]
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[3])]; sort(SF4_Z2_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[4])]; sort(SF4_Z2_dia_diff, FALSE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[5])]; sort(SF4_Z2_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[6])]; sort(SF4_Z2_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[7])]; sort(SF4_Z2_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[8])]; sort(SF4_Z2_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[9])]; sort(SF4_Z2_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[10])]; sort(SF4_Z2_dia_diff, FALSE)[10] 
SF4_Z2_NA <- is.na(dendro_SF4_raw$SF4_Z2_dia_raw)
SF4_Z2_NAdiff <- SF4_Z2_NA[-1]
SF4_Z2_noNA <- na.locf(dendro_SF4_raw$SF4_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z2_noNA,type="l", col=2)
SF4_Z2_dia_diff_noNA <- diff(SF4_Z2_noNA)
SF4_Z2_dia_diff.c <-ifelse((SF4_Z2_dia_diff_noNA > 1 & SF4_Z2_NAdiff == FALSE) | (SF4_Z2_dia_diff_noNA < -1 & SF4_Z2_NAdiff == FALSE),NA,SF4_Z2_dia_diff_noNA) 
# SF4_Z2_dia_diff.c[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[6])] <- NA
# SF4_Z2_dia_diff.c[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_Z2_dia_diff.c,type="l", col=1)
boxplot(SF4_Z2_dia_diff.c,main="SF4_Z2")
summary(SF4_Z2_dia_diff.c)
SF4_Z2_dia_diff.0 <- na.fill(SF4_Z2_dia_diff.c,0);summary(SF4_Z2_dia_diff.0)
SF4_Z2_dia.c <- diffinv(SF4_Z2_dia_diff.0)
SF4_Z2_dia.NA <- is.na(dendro_SF4_raw$SF4_Z2_dia_raw)
SF4_Z2_dia.c <- ifelse(SF4_Z2_dia.NA == TRUE, NA, SF4_Z2_dia.c); summary(SF4_Z2_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z2_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_Z2_dia_diff.c[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[1])]
SF4_Z2_dia_diff.c[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[2])]
SF4_Z2_dia_diff.c[which(SF4_Z2_dia_diff == sort(SF4_Z2_dia_diff, TRUE)[3])]
summary (SF4_Z2_dia.c)

# SF4_Z3 
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z3_dia_diff <- diff(dendro_SF4_raw$SF4_Z3_dia_raw)
plot(SF4_Date_diff.px,SF4_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z3_dia_diff,main="SF4_Z3")
boxplot(SF4_Z3_dia_diff,ylim=c(-1,1),main="SF4_Z3")
sort(SF4_Z3_dia_diff, TRUE)[1:10]
sort(SF4_Z3_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[1])]; sort(SF4_Z3_dia_diff, TRUE)[1] # to be removed
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[2])]; sort(SF4_Z3_dia_diff, TRUE)[2] #
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[3])]; sort(SF4_Z3_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[4])]; sort(SF4_Z3_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[5])]; sort(SF4_Z3_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[6])]; sort(SF4_Z3_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[7])]; sort(SF4_Z3_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[8])]; sort(SF4_Z3_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[9])]; sort(SF4_Z3_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[10])]; sort(SF4_Z3_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[1])]; sort(SF4_Z3_dia_diff, FALSE)[1] # 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[2])]; sort(SF4_Z3_dia_diff, FALSE)[2]
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[3])]; sort(SF4_Z3_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[4])]; sort(SF4_Z3_dia_diff, FALSE)[4] #
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[5])]; sort(SF4_Z3_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[6])]; sort(SF4_Z3_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[7])]; sort(SF4_Z3_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[8])]; sort(SF4_Z3_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[9])]; sort(SF4_Z3_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[10])]; sort(SF4_Z3_dia_diff, FALSE)[10] 
SF4_Z3_NA <- is.na(dendro_SF4_raw$SF4_Z3_dia_raw)
SF4_Z3_NAdiff <- SF4_Z3_NA[-1]
SF4_Z3_noNA <- na.locf(dendro_SF4_raw$SF4_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z3_noNA,type="l", col=2)
SF4_Z3_dia_diff_noNA <- diff(SF4_Z3_noNA)
SF4_Z3_dia_diff.c <-ifelse((SF4_Z3_dia_diff_noNA > 1 & SF4_Z3_NAdiff == FALSE) | (SF4_Z3_dia_diff_noNA < -1 & SF4_Z3_NAdiff == FALSE),NA,SF4_Z3_dia_diff_noNA) 
# SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[6])] <- NA
# SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_Z3_dia_diff.c,type="l", col=1)
boxplot(SF4_Z3_dia_diff.c,main="SF4_Z3")
summary(SF4_Z3_dia_diff.c)
SF4_Z3_dia_diff.0 <- na.fill(SF4_Z3_dia_diff.c,0);summary(SF4_Z3_dia_diff.0)
SF4_Z3_dia.c <- diffinv(SF4_Z3_dia_diff.0)
SF4_Z3_dia.NA <- is.na(dendro_SF4_raw$SF4_Z3_dia_raw)
SF4_Z3_dia.c <- ifelse(SF4_Z3_dia.NA == TRUE, NA, SF4_Z3_dia.c); summary(SF4_Z3_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z3_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[1])]
SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[2])]
SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, TRUE)[3])]
summary (SF4_Z3_dia.c)

# SF4_Z4
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z4_dia_diff <- diff(dendro_SF4_raw$SF4_Z4_dia_raw)
plot(SF4_Date_diff.px,SF4_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z4_dia_diff,main="SF4_Z4")
boxplot(SF4_Z4_dia_diff,ylim=c(-1,1),main="SF4_Z4")
sort(SF4_Z4_dia_diff, TRUE)[1:10]
sort(SF4_Z4_dia_diff, FALSE)[1:10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[1])]; sort(SF4_Z4_dia_diff, TRUE)[1] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[2])]; sort(SF4_Z4_dia_diff, TRUE)[2] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[3])]; sort(SF4_Z4_dia_diff, TRUE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[4])]; sort(SF4_Z4_dia_diff, TRUE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[5])]; sort(SF4_Z4_dia_diff, TRUE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[6])]; sort(SF4_Z4_dia_diff, TRUE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[7])]; sort(SF4_Z4_dia_diff, TRUE)[7]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[8])]; sort(SF4_Z4_dia_diff, TRUE)[8]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[9])]; sort(SF4_Z4_dia_diff, TRUE)[9]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[10])]; sort(SF4_Z4_dia_diff, TRUE)[10]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[1])]; sort(SF4_Z4_dia_diff, FALSE)[1]  
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[2])]; sort(SF4_Z4_dia_diff, FALSE)[2]
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[3])]; sort(SF4_Z4_dia_diff, FALSE)[3] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[4])]; sort(SF4_Z4_dia_diff, FALSE)[4] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[5])]; sort(SF4_Z4_dia_diff, FALSE)[5] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[6])]; sort(SF4_Z4_dia_diff, FALSE)[6] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[7])]; sort(SF4_Z4_dia_diff, FALSE)[7] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[8])]; sort(SF4_Z4_dia_diff, FALSE)[8] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[9])]; sort(SF4_Z4_dia_diff, FALSE)[9] 
dendro_SF4_raw$Date_Time.px[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[10])]; sort(SF4_Z4_dia_diff, FALSE)[10] 
SF4_Z4_NA <- is.na(dendro_SF4_raw$SF4_Z4_dia_raw)
SF4_Z4_NAdiff <- SF4_Z4_NA[-1]
SF4_Z4_noNA <- na.locf(dendro_SF4_raw$SF4_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z4_noNA,type="l", col=2)
SF4_Z4_dia_diff_noNA <- diff(SF4_Z4_noNA)
SF4_Z4_dia_diff.c <-ifelse((SF4_Z4_dia_diff_noNA > 1 & SF4_Z4_NAdiff == FALSE) | (SF4_Z4_dia_diff_noNA < -1 & SF4_Z4_NAdiff == FALSE),NA,SF4_Z4_dia_diff_noNA) 
# SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[6])] <- NA
# SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[4])] <- NA
plot(SF4_Date_diff.px,SF4_Z4_dia_diff.c,type="l", col=1)
boxplot(SF4_Z4_dia_diff.c,main="SF4_Z4")
summary(SF4_Z4_dia_diff.c)
SF4_Z4_dia_diff.0 <- na.fill(SF4_Z4_dia_diff.c,0);summary(SF4_Z4_dia_diff.0)
SF4_Z4_dia.c <- diffinv(SF4_Z4_dia_diff.0)
SF4_Z4_dia.NA <- is.na(dendro_SF4_raw$SF4_Z4_dia_raw)
SF4_Z4_dia.c <- ifelse(SF4_Z4_dia.NA == TRUE, NA, SF4_Z4_dia.c); summary(SF4_Z4_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z4_dia.c,type="l", col=1)
# plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1, ylim=c(40,50))
SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[1])]
SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[2])]
SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, TRUE)[3])]
summary (SF4_Z4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4Z_cleared_corr_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF4 (raw data)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=2)
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=3)
lines(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=4)

# Plot all trees of SF4 (differences)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF4_Date_diff.px,SF4_Z2_dia_diff,type="l", col=2)
lines(SF4_Date_diff.px,SF4_Z3_dia_diff,type="l", col=3)
lines(SF4_Date_diff.px,SF4_Z4_dia_diff,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF4 (cleaned data)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_Z2_dia.c,type="l", col=2)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z3_dia.c,type="l", col=3)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z4_dia.c,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4Z_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_Z1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_Z2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_Z3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_Z4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

summary(dendro_SF5_raw)
# SF5_L1 ####
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L1_dia_diff <- diff(dendro_SF5_raw$SF5_L1_dia_raw)
plot(SF5_Date_diff.px,SF5_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L1_dia_diff,main="SF5_L1")
boxplot(SF5_L1_dia_diff,ylim=c(-1,1),main="SF5_L1")
sort(SF5_L1_dia_diff, TRUE)[1:10]
sort(SF5_L1_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[1])]; sort(SF5_L1_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[2])]; sort(SF5_L1_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[3])]; sort(SF5_L1_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[4])]; sort(SF5_L1_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[5])]; sort(SF5_L1_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[6])]; sort(SF5_L1_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[7])]; sort(SF5_L1_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[8])]; sort(SF5_L1_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[9])]; sort(SF5_L1_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[10])]; sort(SF5_L1_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[1])]; sort(SF5_L1_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[2])]; sort(SF5_L1_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[3])]; sort(SF5_L1_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[4])]; sort(SF5_L1_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[5])]; sort(SF5_L1_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[6])]; sort(SF5_L1_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[7])]; sort(SF5_L1_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[8])]; sort(SF5_L1_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[9])]; sort(SF5_L1_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[10])]; sort(SF5_L1_dia_diff, FALSE)[10] 
SF5_L1_NA <- is.na(dendro_SF5_raw$SF5_L1_dia_raw)
SF5_L1_NAdiff <- SF5_L1_NA[-1]
SF5_L1_noNA <- na.locf(dendro_SF5_raw$SF5_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L1_noNA,type="l", col=2)
SF5_L1_dia_diff_noNA <- diff(SF5_L1_noNA)
SF5_L1_dia_diff.c <-ifelse((SF5_L1_dia_diff_noNA > 1 & SF5_L1_NAdiff == FALSE) | (SF5_L1_dia_diff_noNA < -1 & SF5_L1_NAdiff == FALSE),NA,SF5_L1_dia_diff_noNA) 
# SF5_L1_dia_diff.c[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[6])] <- NA
# SF5_L1_dia_diff.c[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_L1_dia_diff.c,type="l", col=1)
boxplot(SF5_L1_dia_diff.c,main="SF5_L1")
summary(SF5_L1_dia_diff.c)
SF5_L1_dia_diff.0 <- na.fill(SF5_L1_dia_diff.c,0);summary(SF5_L1_dia_diff.0)
SF5_L1_dia.c <- diffinv(SF5_L1_dia_diff.0)
SF5_L1_dia.NA <- is.na(dendro_SF5_raw$SF5_L1_dia_raw)
SF5_L1_dia.c <- ifelse(SF5_L1_dia.NA == TRUE, NA, SF5_L1_dia.c); summary(SF5_L1_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L1_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_L1_dia_diff.c[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[1])]
SF5_L1_dia_diff.c[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[2])]
SF5_L1_dia_diff.c[which(SF5_L1_dia_diff == sort(SF5_L1_dia_diff, TRUE)[3])]
summary (SF5_L1_dia.c)

# SF5_L2
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L2_dia_diff <- diff(dendro_SF5_raw$SF5_L2_dia_raw)
plot(SF5_Date_diff.px,SF5_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L2_dia_diff,main="SF5_L2")
boxplot(SF5_L2_dia_diff,ylim=c(-1,1),main="SF5_L2")
sort(SF5_L2_dia_diff, TRUE)[1:10]
sort(SF5_L2_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[1])]; sort(SF5_L2_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[2])]; sort(SF5_L2_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[3])]; sort(SF5_L2_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[4])]; sort(SF5_L2_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[5])]; sort(SF5_L2_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[6])]; sort(SF5_L2_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[7])]; sort(SF5_L2_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[8])]; sort(SF5_L2_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[9])]; sort(SF5_L2_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[10])]; sort(SF5_L2_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[1])]; sort(SF5_L2_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[2])]; sort(SF5_L2_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[3])]; sort(SF5_L2_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[4])]; sort(SF5_L2_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[5])]; sort(SF5_L2_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[6])]; sort(SF5_L2_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[7])]; sort(SF5_L2_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[8])]; sort(SF5_L2_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[9])]; sort(SF5_L2_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[10])]; sort(SF5_L2_dia_diff, FALSE)[10] 
SF5_L2_NA <- is.na(dendro_SF5_raw$SF5_L2_dia_raw)
SF5_L2_NAdiff <- SF5_L2_NA[-1]
SF5_L2_noNA <- na.locf(dendro_SF5_raw$SF5_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L2_noNA,type="l", col=2)
SF5_L2_dia_diff_noNA <- diff(SF5_L2_noNA)
SF5_L2_dia_diff.c <-ifelse((SF5_L2_dia_diff_noNA > 1.5 & SF5_L2_NAdiff == FALSE) | (SF5_L2_dia_diff_noNA < -1.5 & SF5_L2_NAdiff == FALSE),NA,SF5_L2_dia_diff_noNA) 
# SF5_L2_dia_diff.c[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[6])] <- NA
# SF5_L2_dia_diff.c[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_L2_dia_diff.c,type="l", col=1)
boxplot(SF5_L2_dia_diff.c,main="SF5_L2")
summary(SF5_L2_dia_diff.c)
SF5_L2_dia_diff.0 <- na.fill(SF5_L2_dia_diff.c,0);summary(SF5_L2_dia_diff.0)
SF5_L2_dia.c <- diffinv(SF5_L2_dia_diff.0)
SF5_L2_dia.NA <- is.na(dendro_SF5_raw$SF5_L2_dia_raw)
SF5_L2_dia.c <- ifelse(SF5_L2_dia.NA == TRUE, NA, SF5_L2_dia.c); summary(SF5_L2_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L2_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_L2_dia_diff.c[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[1])]
SF5_L2_dia_diff.c[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[2])]
SF5_L2_dia_diff.c[which(SF5_L2_dia_diff == sort(SF5_L2_dia_diff, TRUE)[3])]
summary (SF5_L2_dia.c)

# SF5_L3 
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L3_dia_diff <- diff(dendro_SF5_raw$SF5_L3_dia_raw)
plot(SF5_Date_diff.px,SF5_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L3_dia_diff,main="SF5_L3")
boxplot(SF5_L3_dia_diff,ylim=c(-1,1),main="SF5_L3")
sort(SF5_L3_dia_diff, TRUE)[1:10]
sort(SF5_L3_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[1])]; sort(SF5_L3_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[2])]; sort(SF5_L3_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[3])]; sort(SF5_L3_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[4])]; sort(SF5_L3_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[5])]; sort(SF5_L3_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[6])]; sort(SF5_L3_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[7])]; sort(SF5_L3_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[8])]; sort(SF5_L3_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[9])]; sort(SF5_L3_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[10])]; sort(SF5_L3_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[1])]; sort(SF5_L3_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[2])]; sort(SF5_L3_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[3])]; sort(SF5_L3_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[4])]; sort(SF5_L3_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[5])]; sort(SF5_L3_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[6])]; sort(SF5_L3_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[7])]; sort(SF5_L3_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[8])]; sort(SF5_L3_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[9])]; sort(SF5_L3_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[10])]; sort(SF5_L3_dia_diff, FALSE)[10] 
SF5_L3_NA <- is.na(dendro_SF5_raw$SF5_L3_dia_raw)
SF5_L3_NAdiff <- SF5_L3_NA[-1]
SF5_L3_noNA <- na.locf(dendro_SF5_raw$SF5_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L3_noNA,type="l", col=2)
SF5_L3_dia_diff_noNA <- diff(SF5_L3_noNA)
SF5_L3_dia_diff.c <-ifelse((SF5_L3_dia_diff_noNA > 1 & SF5_L3_NAdiff == FALSE) | (SF5_L3_dia_diff_noNA < -1 & SF5_L3_NAdiff == FALSE),NA,SF5_L3_dia_diff_noNA) 
# SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[6])] <- NA
# SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_L3_dia_diff.c,type="l", col=1)
boxplot(SF5_L3_dia_diff.c,main="SF5_L3")
summary(SF5_L3_dia_diff.c);summary(SF5_L3_dia_diff)
SF5_L3_dia_diff.0 <- na.fill(SF5_L3_dia_diff.c,0);summary(SF5_L3_dia_diff.0)
SF5_L3_dia.c <- diffinv(SF5_L3_dia_diff.0)
SF5_L3_dia.NA <- is.na(dendro_SF5_raw$SF5_L3_dia_raw)
SF5_L3_dia.c <- ifelse(SF5_L3_dia.NA == TRUE, NA, SF5_L3_dia.c); summary(SF5_L3_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L3_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[1])]
SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[2])]
SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, TRUE)[3])]
summary (SF5_L3_dia.c)
summary (dendro_SF5_raw$SF5_L3_dia_raw)

# SF5_L4
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L4_dia_diff <- diff(dendro_SF5_raw$SF5_L4_dia_raw)
plot(SF5_Date_diff.px,SF5_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L4_dia_diff,main="SF5_L4")
boxplot(SF5_L4_dia_diff,ylim=c(-1,1),main="SF5_L4")
sort(SF5_L4_dia_diff, TRUE)[1:10]
sort(SF5_L4_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[1])]; sort(SF5_L4_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[2])]; sort(SF5_L4_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[3])]; sort(SF5_L4_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[4])]; sort(SF5_L4_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[5])]; sort(SF5_L4_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[6])]; sort(SF5_L4_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[7])]; sort(SF5_L4_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[8])]; sort(SF5_L4_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[9])]; sort(SF5_L4_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[10])]; sort(SF5_L4_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[1])]; sort(SF5_L4_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[2])]; sort(SF5_L4_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[3])]; sort(SF5_L4_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[4])]; sort(SF5_L4_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[5])]; sort(SF5_L4_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[6])]; sort(SF5_L4_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[7])]; sort(SF5_L4_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[8])]; sort(SF5_L4_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[9])]; sort(SF5_L4_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[10])]; sort(SF5_L4_dia_diff, FALSE)[10] 
SF5_L4_NA <- is.na(dendro_SF5_raw$SF5_L4_dia_raw)
SF5_L4_NAdiff <- SF5_L4_NA[-1]
SF5_L4_noNA <- na.locf(dendro_SF5_raw$SF5_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L4_noNA,type="l", col=2)
SF5_L4_dia_diff_noNA <- diff(SF5_L4_noNA)
SF5_L4_dia_diff.c <-ifelse((SF5_L4_dia_diff_noNA > 1 & SF5_L4_NAdiff == FALSE) | (SF5_L4_dia_diff_noNA < -1 & SF5_L4_NAdiff == FALSE),NA,SF5_L4_dia_diff_noNA) 
# SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[6])] <- NA
# SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_L4_dia_diff.c,type="l", col=1)
boxplot(SF5_L4_dia_diff.c,main="SF5_L4")
summary(SF5_L4_dia_diff.c)
SF5_L4_dia_diff.0 <- na.fill(SF5_L4_dia_diff.c,0);summary(SF5_L4_dia_diff.0)
SF5_L4_dia.c <- diffinv(SF5_L4_dia_diff.0)
SF5_L4_dia.NA <- is.na(dendro_SF5_raw$SF5_L4_dia_raw)
SF5_L4_dia.c <- ifelse(SF5_L4_dia.NA == TRUE, NA, SF5_L4_dia.c); summary(SF5_L4_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L4_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[1])]
SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[2])]
SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, TRUE)[3])]
summary (SF5_L4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5L_cleared_corr_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF5 (raw data)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1,ylim=c(0,65))
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=2)
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=3)
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=4)

# Plot all trees of SF5 (differences)
plot(SF5_Date_diff.px,SF5_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF5_Date_diff.px,SF5_L2_dia_diff,type="l", col=2)
lines(SF5_Date_diff.px,SF5_L3_dia_diff,type="l", col=3)
lines(SF5_Date_diff.px,SF5_L4_dia_diff,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF5 (cleaned data)
plot(dendro_SF5_raw$Date_Time.px,SF5_L1_dia.c,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF5_raw$Date_Time.px,SF5_L2_dia.c,type="l", col=2)
lines(dendro_SF5_raw$Date_Time.px,SF5_L3_dia.c,type="l", col=3)
lines(dendro_SF5_raw$Date_Time.px,SF5_L4_dia.c,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
 dev.off()

 win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF5_raw$Date_Time.px,SF5_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF5_raw$Date_Time.px,SF5_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF5_raw$Date_Time.px,SF5_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF5_raw$Date_Time.px,SF5_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
 dev.off()

par(mfcol=c(1,1))
# SF5_Z1 ####
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z1_dia_diff <- diff(dendro_SF5_raw$SF5_Z1_dia_raw)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z1_dia_diff,main="SF5_Z1")
boxplot(SF5_Z1_dia_diff,ylim=c(-1,1),main="SF5_Z1")
sort(SF5_Z1_dia_diff, TRUE)[1:10]
sort(SF5_Z1_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[1])]; sort(SF5_Z1_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[2])]; sort(SF5_Z1_dia_diff, TRUE)[2] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[3])]; sort(SF5_Z1_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[4])]; sort(SF5_Z1_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[5])]; sort(SF5_Z1_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[6])]; sort(SF5_Z1_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[7])]; sort(SF5_Z1_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[8])]; sort(SF5_Z1_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[9])]; sort(SF5_Z1_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[10])]; sort(SF5_Z1_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[1])]; sort(SF5_Z1_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[2])]; sort(SF5_Z1_dia_diff, FALSE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[3])]; sort(SF5_Z1_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[4])]; sort(SF5_Z1_dia_diff, FALSE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[5])]; sort(SF5_Z1_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[6])]; sort(SF5_Z1_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[7])]; sort(SF5_Z1_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[8])]; sort(SF5_Z1_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[9])]; sort(SF5_Z1_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[10])]; sort(SF5_Z1_dia_diff, FALSE)[10] 
SF5_Z1_NA <- is.na(dendro_SF5_raw$SF5_Z1_dia_raw)
SF5_Z1_NAdiff <- SF5_Z1_NA[-1]
SF5_Z1_noNA <- na.locf(dendro_SF5_raw$SF5_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z1_noNA,type="l", col=2)
SF5_Z1_dia_diff_noNA <- diff(SF5_Z1_noNA)
SF5_Z1_dia_diff.c <-ifelse((SF5_Z1_dia_diff_noNA > 0.25 & SF5_Z1_NAdiff == FALSE) | (SF5_Z1_dia_diff_noNA < -0.1 & SF5_Z1_NAdiff == FALSE),NA,SF5_Z1_dia_diff_noNA) 
# SF5_Z1_dia_diff.c[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[6])] <- NA
# SF5_Z1_dia_diff.c[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_Z1_dia_diff.c,type="l", col=1)
boxplot(SF5_Z1_dia_diff.c,main="SF5_Z1")
summary(SF5_Z1_dia_diff.c)
SF5_Z1_dia_diff.0 <- na.fill(SF5_Z1_dia_diff.c,0);summary(SF5_Z1_dia_diff.0)
SF5_Z1_dia.c <- diffinv(SF5_Z1_dia_diff.0)
SF5_Z1_dia.NA <- is.na(dendro_SF5_raw$SF5_Z1_dia_raw)
SF5_Z1_dia.c <- ifelse(SF5_Z1_dia.NA == TRUE, NA, SF5_Z1_dia.c); summary(SF5_Z1_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z1_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_Z1_dia_diff.c[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[1])]
SF5_Z1_dia_diff.c[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[2])]
SF5_Z1_dia_diff.c[which(SF5_Z1_dia_diff == sort(SF5_Z1_dia_diff, TRUE)[3])]
summary (SF5_Z1_dia.c)

# SF5_Z2
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z2_dia_diff <- diff(dendro_SF5_raw$SF5_Z2_dia_raw)
plot(SF5_Date_diff.px,SF5_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z2_dia_diff,main="SF5_Z2")
boxplot(SF5_Z2_dia_diff,ylim=c(-1,1),main="SF5_Z2")
sort(SF5_Z2_dia_diff, TRUE)[1:10]
sort(SF5_Z2_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[1])]; sort(SF5_Z2_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[2])]; sort(SF5_Z2_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[3])]; sort(SF5_Z2_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[4])]; sort(SF5_Z2_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[5])]; sort(SF5_Z2_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[6])]; sort(SF5_Z2_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[7])]; sort(SF5_Z2_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[8])]; sort(SF5_Z2_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[9])]; sort(SF5_Z2_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[10])]; sort(SF5_Z2_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[1])]; sort(SF5_Z2_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[2])]; sort(SF5_Z2_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[3])]; sort(SF5_Z2_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[4])]; sort(SF5_Z2_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[5])]; sort(SF5_Z2_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[6])]; sort(SF5_Z2_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[7])]; sort(SF5_Z2_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[8])]; sort(SF5_Z2_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[9])]; sort(SF5_Z2_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[10])]; sort(SF5_Z2_dia_diff, FALSE)[10] 
SF5_Z2_NA <- is.na(dendro_SF5_raw$SF5_Z2_dia_raw)
SF5_Z2_NAdiff <- SF5_Z2_NA[-1]
SF5_Z2_noNA <- na.locf(dendro_SF5_raw$SF5_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z2_noNA,type="l", col=2)
SF5_Z2_dia_diff_noNA <- diff(SF5_Z2_noNA)
SF5_Z2_dia_diff.c <-ifelse((SF5_Z2_dia_diff_noNA > 0.3 & SF5_Z2_NAdiff == FALSE) | (SF5_Z2_dia_diff_noNA < -0.3 & SF5_Z2_NAdiff == FALSE),NA,SF5_Z2_dia_diff_noNA) 
# SF5_Z2_dia_diff.c[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[6])] <- NA
# SF5_Z2_dia_diff.c[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_Z2_dia_diff.c,type="l", col=1)
boxplot(SF5_Z2_dia_diff.c,main="SF5_Z2")
summary(SF5_Z2_dia_diff.c)
SF5_Z2_dia_diff.0 <- na.fill(SF5_Z2_dia_diff.c,0);summary(SF5_Z2_dia_diff.0)
SF5_Z2_dia.c <- diffinv(SF5_Z2_dia_diff.0)
SF5_Z2_dia.NA <- is.na(dendro_SF5_raw$SF5_Z2_dia_raw)
SF5_Z2_dia.c <- ifelse(SF5_Z2_dia.NA == TRUE, NA, SF5_Z2_dia.c); summary(SF5_Z2_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z2_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_Z2_dia_diff.c[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[1])]
SF5_Z2_dia_diff.c[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[2])]
SF5_Z2_dia_diff.c[which(SF5_Z2_dia_diff == sort(SF5_Z2_dia_diff, TRUE)[3])]
summary (SF5_Z2_dia.c)

# SF5_Z3 
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z3_dia_diff <- diff(dendro_SF5_raw$SF5_Z3_dia_raw)
plot(SF5_Date_diff.px,SF5_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z3_dia_diff,main="SF5_Z3")
boxplot(SF5_Z3_dia_diff,ylim=c(-1,1),main="SF5_Z3")
sort(SF5_Z3_dia_diff, TRUE)[1:10]
sort(SF5_Z3_dia_diff, FALSE)[1:10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[1])]; sort(SF5_Z3_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[2])]; sort(SF5_Z3_dia_diff, TRUE)[2] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[3])]; sort(SF5_Z3_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[4])]; sort(SF5_Z3_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[5])]; sort(SF5_Z3_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[6])]; sort(SF5_Z3_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[7])]; sort(SF5_Z3_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[8])]; sort(SF5_Z3_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[9])]; sort(SF5_Z3_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[10])]; sort(SF5_Z3_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[1])]; sort(SF5_Z3_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[2])]; sort(SF5_Z3_dia_diff, FALSE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[3])]; sort(SF5_Z3_dia_diff, FALSE)[3] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[4])]; sort(SF5_Z3_dia_diff, FALSE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[5])]; sort(SF5_Z3_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[6])]; sort(SF5_Z3_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[7])]; sort(SF5_Z3_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[8])]; sort(SF5_Z3_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[9])]; sort(SF5_Z3_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[10])]; sort(SF5_Z3_dia_diff, FALSE)[10] 
SF5_Z3_NA <- is.na(dendro_SF5_raw$SF5_Z3_dia_raw)
SF5_Z3_NAdiff <- SF5_Z3_NA[-1]
SF5_Z3_noNA <- na.locf(dendro_SF5_raw$SF5_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z3_noNA,type="l", col=2)
SF5_Z3_dia_diff_noNA <- diff(SF5_Z3_noNA)
SF5_Z3_dia_diff.c <-ifelse((SF5_Z3_dia_diff_noNA > 1 & SF5_Z3_NAdiff == FALSE) | (SF5_Z3_dia_diff_noNA < -1 & SF5_Z3_NAdiff == FALSE),NA,SF5_Z3_dia_diff_noNA) 
# SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[6])] <- NA
# SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_Z3_dia_diff.c,type="l", col=1)
boxplot(SF5_Z3_dia_diff.c,main="SF5_Z3")
summary(SF5_Z3_dia_diff.c)
SF5_Z3_dia_diff.0 <- na.fill(SF5_Z3_dia_diff.c,0);summary(SF5_Z3_dia_diff.0)
SF5_Z3_dia.c <- diffinv(SF5_Z3_dia_diff.0)
SF5_Z3_dia.NA <- is.na(dendro_SF5_raw$SF5_Z3_dia_raw)
SF5_Z3_dia.c <- ifelse(SF5_Z3_dia.NA == TRUE, NA, SF5_Z3_dia.c); summary(SF5_Z3_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z3_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[1])]
SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[2])]
SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, TRUE)[3])]
summary (SF5_Z3_dia.c)

# SF5_Z4
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z4_dia_diff <- diff(dendro_SF5_raw$SF5_Z4_dia_raw)
plot(SF5_Date_diff.px,SF5_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z4_dia_diff,main="SF5_Z4")
boxplot(SF5_Z4_dia_diff,ylim=c(-1,1),main="SF5_Z4")
sort(SF5_Z4_dia_diff, TRUE)[1:10]
sort(SF5_Z4_dia_diff, FALSE)[1:10]
summary(SF5_Z4_dia_diff)
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[1])]; sort(SF5_Z4_dia_diff, TRUE)[1] # to be removed
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[2])]; sort(SF5_Z4_dia_diff, TRUE)[2] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[3])]; sort(SF5_Z4_dia_diff, TRUE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[4])]; sort(SF5_Z4_dia_diff, TRUE)[4] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[5])]; sort(SF5_Z4_dia_diff, TRUE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[6])]; sort(SF5_Z4_dia_diff, TRUE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[7])]; sort(SF5_Z4_dia_diff, TRUE)[7]
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[8])]; sort(SF5_Z4_dia_diff, TRUE)[8]
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[9])]; sort(SF5_Z4_dia_diff, TRUE)[9]
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[10])]; sort(SF5_Z4_dia_diff, TRUE)[10]
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[1])]; sort(SF5_Z4_dia_diff, FALSE)[1] # 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[2])]; sort(SF5_Z4_dia_diff, FALSE)[2]
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[3])]; sort(SF5_Z4_dia_diff, FALSE)[3] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[4])]; sort(SF5_Z4_dia_diff, FALSE)[4] #
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[5])]; sort(SF5_Z4_dia_diff, FALSE)[5] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[6])]; sort(SF5_Z4_dia_diff, FALSE)[6] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[7])]; sort(SF5_Z4_dia_diff, FALSE)[7] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[8])]; sort(SF5_Z4_dia_diff, FALSE)[8] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[9])]; sort(SF5_Z4_dia_diff, FALSE)[9] 
dendro_SF5_raw$Date_Time.px[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[10])]; sort(SF5_Z4_dia_diff, FALSE)[10] 
SF5_Z4_NA <- is.na(dendro_SF5_raw$SF5_Z4_dia_raw)
SF5_Z4_NAdiff <- SF5_Z4_NA[-1]
SF5_Z4_noNA <- na.locf(dendro_SF5_raw$SF5_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z4_noNA,type="l", col=2)
SF5_Z4_dia_diff_noNA <- diff(SF5_Z4_noNA)
SF5_Z4_dia_diff.c <-ifelse((SF5_Z4_dia_diff_noNA > 0.5 & SF5_Z4_NAdiff == FALSE) | (SF5_Z4_dia_diff_noNA < -0.18 & SF5_Z4_NAdiff == FALSE),NA,SF5_Z4_dia_diff_noNA) 
summary(SF5_Z4_dia_diff.c)
# SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[6])] <- NA
# SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[4])] <- NA
plot(SF5_Date_diff.px,SF5_Z4_dia_diff.c,type="l", col=1)
boxplot(SF5_Z4_dia_diff.c,main="SF5_Z4")
summary(SF5_Z4_dia_diff.c)
SF5_Z4_dia_diff.0 <- na.fill(SF5_Z4_dia_diff.c,0);summary(SF5_Z4_dia_diff.0)
SF5_Z4_dia.c <- diffinv(SF5_Z4_dia_diff.0)
SF5_Z4_dia.NA <- is.na(dendro_SF5_raw$SF5_Z4_dia_raw)
SF5_Z4_dia.c <- ifelse(SF5_Z4_dia.NA == TRUE, NA, SF5_Z4_dia.c); summary(SF5_Z4_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z4_dia.c,type="l", col=1)
# plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1, ylim=c(40,50))
SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[1])]
SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[2])]
SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, TRUE)[3])]
summary (SF5_Z4_dia.c)
summary(SF5_Z4_dia_diff)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5Z_cleared_corr_05_2015.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF5 (raw data)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=2)
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=3)
lines(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=4)

# Plot all trees of SF5 (differences)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF5_Date_diff.px,SF5_Z2_dia_diff,type="l", col=2)
lines(SF5_Date_diff.px,SF5_Z3_dia_diff,type="l", col=3)
lines(SF5_Date_diff.px,SF5_Z4_dia_diff,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF5 (cleaned data)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw$Date_Time.px,SF5_Z2_dia.c,type="l", col=2)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z3_dia.c,type="l", col=3)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z4_dia.c,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
 dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw$Date_Time.px,SF5_Z1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw$Date_Time.px,SF5_Z2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw$Date_Time.px,SF5_Z3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw$Date_Time.px,SF5_Z4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
 dev.off()

# combine and export cleaned data (differences + absolut values) ####
# absolut values
exp_SF1_dendro_abs <- data.frame(dendro_SF1_raw$Date_Time.px,SF1_L2_dia.c, SF1_L4_dia.c, SF1_L5_dia.c, SF1_L6_dia.c); colnames(exp_SF1_dendro_abs)[1] <-"Date_Time"
exp_SF2_dendro_abs <- data.frame(dendro_SF2_raw$Date_Time.px,SF2_L1_dia.c, SF2_L4_dia.c, SF2_L5_dia.c, SF2_L7_dia.c); colnames(exp_SF2_dendro_abs)[1] <-"Date_Time"
exp_SF3_dendro_abs <- data.frame(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c, SF3_L4_dia.c, SF3_L5_dia.c, SF3_L6_dia.c); colnames(exp_SF3_dendro_abs)[1] <-"Date_Time"
exp_SF4_dendro_abs <- data.frame(dendro_SF4_raw$Date_Time.px,SF4_L1_dia.c, SF4_L2_dia.c, SF4_L3_dia.c, SF4_L4_dia.c
                                 ,SF4_Z1_dia.c, SF4_Z2_dia.c, SF4_Z3_dia.c, SF4_Z4_dia.c); colnames(exp_SF4_dendro_abs)[1] <-"Date_Time"
exp_SF5_dendro_abs <- data.frame(dendro_SF5_raw$Date_Time.px,SF5_L1_dia.c, SF5_L2_dia.c, SF5_L3_dia.c, SF5_L4_dia.c
                                 ,SF5_Z1_dia.c, SF5_Z2_dia.c, SF5_Z3_dia.c, SF5_Z4_dia.c); colnames(exp_SF5_dendro_abs)[1] <-"Date_Time"

exp_dendro_abs <- merge(exp_SF1_dendro_abs,exp_SF2_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF3_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF4_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF5_dendro_abs,by="Date_Time",all=TRUE)

# differences
exp_SF1_dendro_diff <- data.frame(SF1_Date_diff.px,SF1_L2_dia_diff.c, SF1_L4_dia_diff.c, SF1_L5_dia_diff.c, SF1_L6_dia_diff.c); colnames(exp_SF1_dendro_diff)[1] <-"Date_Time"
exp_SF2_dendro_diff <- data.frame(SF2_Date_diff.px,SF2_L1_dia_diff.c, SF2_L4_dia_diff.c, SF2_L5_dia_diff.c, SF2_L7_dia_diff.c); colnames(exp_SF2_dendro_diff)[1] <-"Date_Time"
exp_SF3_dendro_diff <- data.frame(SF3_Date_diff.px,SF3_L3_dia_diff.c, SF3_L4_dia_diff.c, SF3_L5_dia_diff.c, SF3_L6_dia_diff.c); colnames(exp_SF3_dendro_diff)[1] <-"Date_Time"

# add NAs due to power shortages into corrected dendro-diff data for sites SF4+SF5
SF4_L1_dia_diff.c2 <- ifelse(SF4_L1_NAdiff == TRUE, NA, SF4_L1_dia_diff.c)
summary(SF4_L1_dia_diff.c); summary(SF4_L1_NAdiff); summary(SF4_L1_dia_diff.c2)
SF4_L2_dia_diff.c2 <- ifelse(SF4_L2_NAdiff == TRUE, NA, SF4_L2_dia_diff.c)
summary(SF4_L2_dia_diff.c); summary(SF4_L2_NAdiff); summary(SF4_L2_dia_diff.c2)
SF4_L3_dia_diff.c2 <- ifelse(SF4_L3_NAdiff == TRUE, NA, SF4_L3_dia_diff.c)
summary(SF4_L3_dia_diff.c); summary(SF4_L3_NAdiff); summary(SF4_L3_dia_diff.c2)
SF4_L4_dia_diff.c2 <- ifelse(SF4_L4_NAdiff == TRUE, NA, SF4_L4_dia_diff.c)
summary(SF4_L4_dia_diff.c); summary(SF4_L4_NAdiff); summary(SF4_L4_dia_diff.c2)

SF4_Z1_dia_diff.c2 <- ifelse(SF4_Z1_NAdiff == TRUE, NA, SF4_Z1_dia_diff.c)
summary(SF4_Z1_dia_diff.c); summary(SF4_Z1_NAdiff); summary(SF4_Z1_dia_diff.c2)
SF4_Z2_dia_diff.c2 <- ifelse(SF4_Z2_NAdiff == TRUE, NA, SF4_Z2_dia_diff.c)
summary(SF4_Z2_dia_diff.c); summary(SF4_Z2_NAdiff); summary(SF4_Z2_dia_diff.c2)
SF4_Z3_dia_diff.c2 <- ifelse(SF4_Z3_NAdiff == TRUE, NA, SF4_Z3_dia_diff.c)
summary(SF4_Z3_dia_diff.c); summary(SF4_Z3_NAdiff); summary(SF4_Z3_dia_diff.c2)
SF4_Z4_dia_diff.c2 <- ifelse(SF4_Z4_NAdiff == TRUE, NA, SF4_Z4_dia_diff.c)
summary(SF4_Z4_dia_diff.c); summary(SF4_Z4_NAdiff); summary(SF4_Z4_dia_diff.c2)

SF5_L1_dia_diff.c2 <- ifelse(SF5_L1_NAdiff == TRUE, NA, SF5_L1_dia_diff.c)
summary(SF5_L1_dia_diff.c); summary(SF5_L1_NAdiff); summary(SF5_L1_dia_diff.c2)
SF5_L2_dia_diff.c2 <- ifelse(SF5_L2_NAdiff == TRUE, NA, SF5_L2_dia_diff.c)
summary(SF5_L2_dia_diff.c); summary(SF5_L2_NAdiff); summary(SF5_L2_dia_diff.c2)
SF5_L3_dia_diff.c2 <- ifelse(SF5_L3_NAdiff == TRUE, NA, SF5_L3_dia_diff.c)
summary(SF5_L3_dia_diff.c); summary(SF5_L3_NAdiff); summary(SF5_L3_dia_diff.c2)
SF5_L4_dia_diff.c2 <- ifelse(SF5_L4_NAdiff == TRUE, NA, SF5_L4_dia_diff.c)
summary(SF5_L4_dia_diff.c); summary(SF5_L4_NAdiff); summary(SF5_L4_dia_diff.c2)

SF5_Z1_dia_diff.c2 <- ifelse(SF5_Z1_NAdiff == TRUE, NA, SF5_Z1_dia_diff.c)
summary(SF5_Z1_dia_diff.c); summary(SF5_Z1_NAdiff); summary(SF5_Z1_dia_diff.c2)
SF5_Z2_dia_diff.c2 <- ifelse(SF5_Z2_NAdiff == TRUE, NA, SF5_Z2_dia_diff.c)
summary(SF5_Z2_dia_diff.c); summary(SF5_Z2_NAdiff); summary(SF5_Z2_dia_diff.c2)
SF5_Z3_dia_diff.c2 <- ifelse(SF5_Z3_NAdiff == TRUE, NA, SF5_Z3_dia_diff.c)
summary(SF5_Z3_dia_diff.c); summary(SF5_Z3_NAdiff); summary(SF5_Z3_dia_diff.c2)
SF5_Z4_dia_diff.c2 <- ifelse(SF5_Z4_NAdiff == TRUE, NA, SF5_Z4_dia_diff.c)
summary(SF5_Z4_dia_diff.c); summary(SF5_Z4_NAdiff); summary(SF5_Z4_dia_diff.c2)

exp_SF4_dendro_diff <- data.frame(SF4_Date_diff.px,SF4_L1_dia_diff.c2, SF4_L2_dia_diff.c2, SF4_L3_dia_diff.c2, SF4_L4_dia_diff.c2
                                 ,SF4_Z1_dia_diff.c2, SF4_Z2_dia_diff.c2, SF4_Z3_dia_diff.c2, SF4_Z4_dia_diff.c2); colnames(exp_SF4_dendro_diff)[1] <-"Date_Time"
exp_SF5_dendro_diff <- data.frame(SF5_Date_diff.px,SF5_L1_dia_diff.c2, SF5_L2_dia_diff.c2, SF5_L3_dia_diff.c2, SF5_L4_dia_diff.c2
                                 ,SF5_Z1_dia_diff.c2, SF5_Z2_dia_diff.c2, SF5_Z3_dia_diff.c2, SF5_Z4_dia_diff.c2); colnames(exp_SF5_dendro_diff)[1] <-"Date_Time"

exp_dendro_diff <- merge(exp_SF1_dendro_diff,exp_SF2_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF3_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF4_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF5_dendro_diff,by="Date_Time",all=TRUE)

exp_dendro_temp <- merge(dendro_SF1_raw[,c(3,5,7,9,10)],dendro_SF2_raw[,c(3,5,7,9,10)],by="Date_Time.px",all=TRUE)
exp_dendro_temp <- merge(exp_dendro_temp,dendro_SF3_raw[,c(3,5,7,9,10)],by="Date_Time.px",all=TRUE)

summary(exp_dendro_abs)
summary(exp_dendro_diff)

outfile.dendro_abs <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_new2015_05_abs.csv")  
write.csv(exp_dendro_abs,file=outfile.dendro_abs)

outfile.dendro_diff <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_new2015_05_diff.csv")  
write.csv(exp_dendro_diff,file=outfile.dendro_diff)

outfile.dendro_temp <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_temp.csv")  
write.csv(exp_dendro_temp,file=outfile.dendro_temp)

# continue to Dendrometer_data_load_merge_and-figures.R   
# => check NA's in Temperature-values SF1-SF3 (nur Werte ab Umstellung auf 10min-Messintervall)
# => correct starting values after winter SF4+SF5
                         
                         
                    