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
SF1_L2_NA <- is.na(dendro_SF1_raw$SF1_L2_dia_raw)
SF1_L2_NAdiff <- SF1_L2_NA[-1]
SF1_L2_noNA <- na.locf(dendro_SF1_raw$SF1_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L2_noNA,type="l", col=2)
SF1_L2_dia_diff_noNA <- diff(SF1_L2_noNA)
SF1_L2_dia_diff.c <-ifelse((SF1_L2_dia_diff_noNA > 1 & SF1_L2_NAdiff == FALSE) | (SF1_L2_dia_diff_noNA < -1 & SF1_L2_NAdiff == FALSE),NA,SF1_L2_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L2_dia_diff.c,type="l", col=1)
summary(SF1_L2_dia_diff.c)
SF1_L2_dia_diff.0 <- na.fill(SF1_L2_dia_diff.c,0);summary(SF1_L2_dia_diff.0)
SF1_L2_dia.c <- diffinv(SF1_L2_dia_diff.0)
SF1_L2_dia.NA <- is.na(dendro_SF1_raw$SF1_L2_dia_raw)
SF1_L2_dia.c <- ifelse(SF1_L2_dia.NA == TRUE, NA, SF1_L2_dia.c); summary(SF1_L2_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L2_dia.c,type="l", col=1)
# plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L2_dia_raw,type="l", col=1, ylim=c(40,50))
summary (SF1_L2_dia.c)

# SF1_L4
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L4_dia_diff <- diff(dendro_SF1_raw$SF1_L4_dia_raw)
plot(SF1_Date_diff.px,SF1_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF1_L4_NA <- is.na(dendro_SF1_raw$SF1_L4_dia_raw)
SF1_L4_NAdiff <- SF1_L4_NA[-length(SF1_L4_NA)]
SF1_L4_noNA <- na.locf(dendro_SF1_raw$SF1_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L4_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L4_noNA,type="l", col=2)
SF1_L4_dia_diff_noNA <- diff(SF1_L4_noNA)
# SF1_L4_dia_diff.c <-ifelse(SF1_L4_dia_diff_noNA > 1 | SF1_L4_dia_diff_noNA < -1 & SF1_L4_NAdiff == FALSE,NA,SF1_L4_dia_diff_noNA) 
SF1_L4_dia_diff.c <-ifelse((SF1_L4_dia_diff_noNA > 1 & SF1_L4_NAdiff == FALSE) | (SF1_L4_dia_diff_noNA < -1 & SF1_L4_NAdiff == FALSE),NA,SF1_L4_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L4_dia_diff.c,type="l", col=1)
summary(SF1_L4_dia_diff.c)
SF1_L4_dia_diff.0 <- na.fill(SF1_L4_dia_diff.c,0);summary(SF1_L4_dia_diff.0)
SF1_L4_dia.c <- diffinv(SF1_L4_dia_diff.0)
SF1_L4_dia.NA <- is.na(dendro_SF1_raw$SF1_L4_dia_raw)
SF1_L4_dia.c <- ifelse(SF1_L4_dia.NA == TRUE, NA, SF1_L4_dia.c); summary(SF1_L4_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L4_dia.c,type="l", col=1)
summary (SF1_L4_dia.c)

# SF1_L5
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L5_dia_diff <- diff(dendro_SF1_raw$SF1_L5_dia_raw)
plot(SF1_Date_diff.px,SF1_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF1_L5_NA <- is.na(dendro_SF1_raw$SF1_L5_dia_raw)
SF1_L5_NAdiff <- SF1_L5_NA[-length(SF1_L5_NA)]
SF1_L5_noNA <- na.locf(dendro_SF1_raw$SF1_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L5_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L5_noNA,type="l", col=2)
SF1_L5_dia_diff_noNA <- diff(SF1_L5_noNA)
# SF1_L5_dia_diff.c <-ifelse(SF1_L5_dia_diff_noNA > 1 | SF1_L5_dia_diff_noNA < -1 & SF1_L5_NAdiff == FALSE,NA,SF1_L5_dia_diff_noNA) 
SF1_L5_dia_diff.c <-ifelse((SF1_L5_dia_diff_noNA > 1 & SF1_L5_NAdiff == FALSE) | (SF1_L5_dia_diff_noNA < -1 & SF1_L5_NAdiff == FALSE),NA,SF1_L5_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L5_dia_diff.c,type="l", col=1)
summary(SF1_L5_dia_diff.c)
SF1_L5_dia_diff.0 <- na.fill(SF1_L5_dia_diff.c,0);summary(SF1_L5_dia_diff.0)
SF1_L5_dia.c <- diffinv(SF1_L5_dia_diff.0)
SF1_L5_dia.NA <- is.na(dendro_SF1_raw$SF1_L5_dia_raw)
SF1_L5_dia.c <- ifelse(SF1_L5_dia.NA == TRUE, NA, SF1_L5_dia.c); summary(SF1_L5_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L5_dia.c,type="l", col=1)
summary (SF1_L5_dia.c)

# SF1_L6
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L6_dia_diff <- diff(dendro_SF1_raw$SF1_L6_dia_raw)
plot(SF1_Date_diff.px,SF1_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
SF1_L6_NA <- is.na(dendro_SF1_raw$SF1_L6_dia_raw)
SF1_L6_NAdiff <- SF1_L6_NA[-length(SF1_L6_NA)]
SF1_L6_noNA <- na.locf(dendro_SF1_raw$SF1_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw$Date_Time.px,dendro_SF1_raw$SF1_L6_dia_raw,type="l", col=1)
lines(dendro_SF1_raw$Date_Time.px,SF1_L6_noNA,type="l", col=2)
SF1_L6_dia_diff_noNA <- diff(SF1_L6_noNA)
# SF1_L6_dia_diff.c <-ifelse(SF1_L6_dia_diff_noNA > 1 | SF1_L6_dia_diff_noNA < -1 & SF1_L6_NAdiff == FALSE,NA,SF1_L6_dia_diff_noNA) 
SF1_L6_dia_diff.c <-ifelse((SF1_L6_dia_diff_noNA > 1 & SF1_L6_NAdiff == FALSE) | (SF1_L6_dia_diff_noNA < -1 & SF1_L6_NAdiff == FALSE),NA,SF1_L6_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L6_dia_diff.c,type="l", col=1)
summary(SF1_L6_dia_diff.c)
SF1_L6_dia_diff.0 <- na.fill(SF1_L6_dia_diff.c,0);summary(SF1_L6_dia_diff.0)
SF1_L6_dia.c <- diffinv(SF1_L6_dia_diff.0)
SF1_L6_dia.NA <- is.na(dendro_SF1_raw$SF1_L6_dia_raw)
SF1_L6_dia.c <- ifelse(SF1_L6_dia.NA == TRUE, NA, SF1_L6_dia.c); summary(SF1_L6_dia.c)
plot(dendro_SF1_raw$Date_Time.px,SF1_L6_dia.c,type="l", col=1)
summary (SF1_L6_dia.c)

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF1_cleared_5mv_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF1_comp_raw-cleared_5mv_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

summary(dendro_SF2_raw)
# SF2_L1 ####
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L1_dia_diff <- diff(dendro_SF2_raw$SF2_L1_dia_raw)
plot(SF2_Date_diff.px,SF2_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF2_L1_NA <- is.na(dendro_SF2_raw$SF2_L1_dia_raw)
SF2_L1_NAdiff <- SF2_L1_NA[-length(SF2_L1_NA)]
SF2_L1_noNA <- na.locf(dendro_SF2_raw$SF2_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L1_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L1_noNA,type="l", col=2)
SF2_L1_dia_diff_noNA <- diff(SF2_L1_noNA)
# SF2_L1_dia_diff.c <-ifelse(SF2_L1_dia_diff_noNA > 1 | SF2_L1_dia_diff_noNA < -1 & SF2_L1_NAdiff == FALSE,NA,SF2_L1_dia_diff_noNA) 
SF2_L1_dia_diff.c <-ifelse((SF2_L1_dia_diff_noNA > 1 & SF2_L1_NAdiff == FALSE) | (SF2_L1_dia_diff_noNA < -1 & SF2_L1_NAdiff == FALSE),NA,SF2_L1_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L1_dia_diff.c,type="l", col=1)
summary(SF2_L1_dia_diff.c)
SF2_L1_dia_diff.0 <- na.fill(SF2_L1_dia_diff.c,0);summary(SF2_L1_dia_diff.0)
SF2_L1_dia.c <- diffinv(SF2_L1_dia_diff.0)
SF2_L1_dia.NA <- is.na(dendro_SF2_raw$SF2_L1_dia_raw)
SF2_L1_dia.c <- ifelse(SF2_L1_dia.NA == TRUE, NA, SF2_L1_dia.c); summary(SF2_L1_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L1_dia.c,type="l", col=1)

# SF2_L4
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L4_dia_diff <- diff(dendro_SF2_raw$SF2_L4_dia_raw)
plot(SF2_Date_diff.px,SF2_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF2_L4_NA <- is.na(dendro_SF2_raw$SF2_L4_dia_raw)
SF2_L4_NAdiff <- SF2_L4_NA[-length(SF2_L4_NA)]
SF2_L4_noNA <- na.locf(dendro_SF2_raw$SF2_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L4_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L4_noNA,type="l", col=2)
SF2_L4_dia_diff_noNA <- diff(SF2_L4_noNA)
# SF2_L4_dia_diff.c <-ifelse(SF2_L4_dia_diff_noNA > 1 | SF2_L4_dia_diff_noNA < -1 & SF2_L4_NAdiff == FALSE,NA,SF2_L4_dia_diff_noNA) 
SF2_L4_dia_diff.c <-ifelse((SF2_L4_dia_diff_noNA > 1 & SF2_L4_NAdiff == FALSE) | (SF2_L4_dia_diff_noNA < -1 & SF2_L4_NAdiff == FALSE),NA,SF2_L4_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L4_dia_diff.c,type="l", col=1)
summary(SF2_L4_dia_diff.c)
SF2_L4_dia_diff.0 <- na.fill(SF2_L4_dia_diff.c,0);summary(SF2_L4_dia_diff.0)
SF2_L4_dia.c <- diffinv(SF2_L4_dia_diff.0)
SF2_L4_dia.NA <- is.na(dendro_SF2_raw$SF2_L4_dia_raw)
SF2_L4_dia.c <- ifelse(SF2_L4_dia.NA == TRUE, NA, SF2_L4_dia.c); summary(SF2_L4_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L4_dia.c,type="l", col=1)

# SF2_L5
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L5_dia_diff <- diff(dendro_SF2_raw$SF2_L5_dia_raw)
plot(SF2_Date_diff.px,SF2_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF2_L5_NA <- is.na(dendro_SF2_raw$SF2_L5_dia_raw)
SF2_L5_NAdiff <- SF2_L5_NA[-length(SF2_L5_NA)]
SF2_L5_noNA <- na.locf(dendro_SF2_raw$SF2_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L5_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L5_noNA,type="l", col=2)
SF2_L5_dia_diff_noNA <- diff(SF2_L5_noNA)
# SF2_L5_dia_diff.c <-ifelse(SF2_L5_dia_diff_noNA > 1 | SF2_L5_dia_diff_noNA < -1 & SF2_L5_NAdiff == FALSE,NA,SF2_L5_dia_diff_noNA) 
SF2_L5_dia_diff.c <-ifelse((SF2_L5_dia_diff_noNA > 1 & SF2_L5_NAdiff == FALSE) | (SF2_L5_dia_diff_noNA < -1 & SF2_L5_NAdiff == FALSE),NA,SF2_L5_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L5_dia_diff.c,type="l", col=1)
summary(SF2_L5_dia_diff.c)
SF2_L5_dia_diff.0 <- na.fill(SF2_L5_dia_diff.c,0);summary(SF2_L5_dia_diff.0)
SF2_L5_dia.c <- diffinv(SF2_L5_dia_diff.0)
SF2_L5_dia.NA <- is.na(dendro_SF2_raw$SF2_L5_dia_raw)
SF2_L5_dia.c <- ifelse(SF2_L5_dia.NA == TRUE, NA, SF2_L5_dia.c); summary(SF2_L5_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L5_dia.c,type="l", col=1)

# SF2_L7
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L7_dia_diff <- diff(dendro_SF2_raw$SF2_L7_dia_raw)
plot(SF2_Date_diff.px,SF2_L7_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF2_L7_NA <- is.na(dendro_SF2_raw$SF2_L7_dia_raw)
SF2_L7_NAdiff <- SF2_L7_NA[-length(SF2_L7_NA)]
SF2_L7_noNA <- na.locf(dendro_SF2_raw$SF2_L7_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1)
lines(dendro_SF2_raw$Date_Time.px,SF2_L7_noNA,type="l", col=2)
SF2_L7_dia_diff_noNA <- diff(SF2_L7_noNA)
# SF2_L7_dia_diff.c <-ifelse(SF2_L7_dia_diff_noNA > 1 | SF2_L7_dia_diff_noNA < -1 & SF2_L7_NAdiff == FALSE,NA,SF2_L7_dia_diff_noNA) 
SF2_L7_dia_diff.c <-ifelse((SF2_L7_dia_diff_noNA > 1 & SF2_L7_NAdiff == FALSE) | (SF2_L7_dia_diff_noNA < -1 & SF2_L7_NAdiff == FALSE),NA,SF2_L7_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L7_dia_diff.c,type="l", col=1)
summary(SF2_L7_dia_diff.c)
SF2_L7_dia_diff.0 <- na.fill(SF2_L7_dia_diff.c,0);summary(SF2_L7_dia_diff.0)
SF2_L7_dia.c <- diffinv(SF2_L7_dia_diff.0)
SF2_L7_dia.NA <- is.na(dendro_SF2_raw$SF2_L7_dia_raw)
SF2_L7_dia.c <- ifelse(SF2_L7_dia.NA == TRUE, NA, SF2_L7_dia.c); summary(SF2_L7_dia.c)
plot(dendro_SF2_raw$Date_Time.px,SF2_L7_dia.c,type="l", col=1)
# plot(dendro_SF2_raw$Date_Time.px,dendro_SF2_raw$SF2_L7_dia_raw,type="l", col=1, ylim=c(40,50))

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF2_cleared_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF2_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

summary(dendro_SF3_raw)
# SF3_L3 ####
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L3_dia_diff <- diff(dendro_SF3_raw$SF3_L3_dia_raw)
plot(SF3_Date_diff.px,SF3_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF3_L3_NA <- is.na(dendro_SF3_raw$SF3_L3_dia_raw)
SF3_L3_NAdiff <- SF3_L3_NA[-length(SF3_L3_NA)]
SF3_L3_noNA <- na.locf(dendro_SF3_raw$SF3_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L3_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L3_noNA,type="l", col=2)
SF3_L3_dia_diff_noNA <- diff(SF3_L3_noNA)
# SF3_L3_dia_diff.c <-ifelse(SF3_L3_dia_diff_noNA > 1 | SF3_L3_dia_diff_noNA < -1 & SF3_L3_NAdiff == FALSE,NA,SF3_L3_dia_diff_noNA) 
SF3_L3_dia_diff.c <-ifelse((SF3_L3_dia_diff_noNA > 1 & SF3_L3_NAdiff == FALSE) | (SF3_L3_dia_diff_noNA < -1 & SF3_L3_NAdiff == FALSE),NA,SF3_L3_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
summary(SF3_L3_dia_diff.c)
SF3_L3_dia_diff.0 <- na.fill(SF3_L3_dia_diff.c,0);summary(SF3_L3_dia_diff.0)
SF3_L3_dia.c <- diffinv(SF3_L3_dia_diff.0)
SF3_L3_dia.NA <- is.na(dendro_SF3_raw$SF3_L3_dia_raw)
SF3_L3_dia.c <- ifelse(SF3_L3_dia.NA == TRUE, NA, SF3_L3_dia.c); summary(SF3_L3_dia.c)
july27.2013 <- as.POSIXct("2013-07-27 00:00", format="%Y-%m-%d %H:%M")
aug07.2013 <- as.POSIXct("2013-08-07 12:00", format="%Y-%m-%d %H:%M")
SF3_L3_dia.c[c(match(july27.2013,dendro_SF3_raw$Date_Time.px):  
               match(aug07.2013,dendro_SF3_raw$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(dendro_SF3_raw$Date_Time.px,SF3_L3_dia.c,type="l", col=1)
SF3_L3_dia_diff.c[c(match(july27.2013,dendro_SF3_raw$Date_Time.px):
                 match(aug07.2013,dendro_SF3_raw$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
summary(SF3_L3_dia.c); summary(SF3_L3_dia_diff.c)

# SF3_L4
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L4_dia_diff <- diff(dendro_SF3_raw$SF3_L4_dia_raw)
plot(SF3_Date_diff.px,SF3_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF3_L4_NA <- is.na(dendro_SF3_raw$SF3_L4_dia_raw)
SF3_L4_NAdiff <- SF3_L4_NA[-length(SF3_L4_NA)]
SF3_L4_noNA <- na.locf(dendro_SF3_raw$SF3_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L4_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L4_noNA,type="l", col=2)
SF3_L4_dia_diff_noNA <- diff(SF3_L4_noNA)
# SF3_L4_dia_diff.c <-ifelse(SF3_L4_dia_diff_noNA > 1 | SF3_L4_dia_diff_noNA < -1 & SF3_L4_NAdiff == FALSE,NA,SF3_L4_dia_diff_noNA) 
SF3_L4_dia_diff.c <-ifelse((SF3_L4_dia_diff_noNA > 1 & SF3_L4_NAdiff == FALSE) | (SF3_L4_dia_diff_noNA < -1 & SF3_L4_NAdiff == FALSE),NA,SF3_L4_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L4_dia_diff.c,type="l", col=1)
summary(SF3_L4_dia_diff.c)
SF3_L4_dia_diff.0 <- na.fill(SF3_L4_dia_diff.c,0);summary(SF3_L4_dia_diff.0)
SF3_L4_dia.c <- diffinv(SF3_L4_dia_diff.0)
SF3_L4_dia.NA <- is.na(dendro_SF3_raw$SF3_L4_dia_raw)
SF3_L4_dia.c <- ifelse(SF3_L4_dia.NA == TRUE, NA, SF3_L4_dia.c); summary(SF3_L4_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L4_dia.c,type="l", col=1)

# SF3_L5
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L5_dia_diff <- diff(dendro_SF3_raw$SF3_L5_dia_raw)
plot(SF3_Date_diff.px,SF3_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF3_L5_NA <- is.na(dendro_SF3_raw$SF3_L5_dia_raw)
SF3_L5_NAdiff <- SF3_L5_NA[-length(SF3_L5_NA)]
SF3_L5_noNA <- na.locf(dendro_SF3_raw$SF3_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L5_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L5_noNA,type="l", col=2)
SF3_L5_dia_diff_noNA <- diff(SF3_L5_noNA)
# SF3_L5_dia_diff.c <-ifelse(SF3_L5_dia_diff_noNA > 1 | SF3_L5_dia_diff_noNA < -1 & SF3_L5_NAdiff == FALSE,NA,SF3_L5_dia_diff_noNA) 
SF3_L5_dia_diff.c <-ifelse((SF3_L5_dia_diff_noNA > 1 & SF3_L5_NAdiff == FALSE) | (SF3_L5_dia_diff_noNA < -1 & SF3_L5_NAdiff == FALSE),NA,SF3_L5_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L5_dia_diff.c,type="l", col=1)
summary(SF3_L5_dia_diff.c)
SF3_L5_dia_diff.0 <- na.fill(SF3_L5_dia_diff.c,0);summary(SF3_L5_dia_diff.0)
SF3_L5_dia.c <- diffinv(SF3_L5_dia_diff.0)
SF3_L5_dia.NA <- is.na(dendro_SF3_raw$SF3_L5_dia_raw)
SF3_L5_dia.c <- ifelse(SF3_L5_dia.NA == TRUE, NA, SF3_L5_dia.c); summary(SF3_L5_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L5_dia.c,type="l", col=1)

# SF3_L6
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L6_dia_diff <- diff(dendro_SF3_raw$SF3_L6_dia_raw)
plot(SF3_Date_diff.px,SF3_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF3_L6_NA <- is.na(dendro_SF3_raw$SF3_L6_dia_raw)
SF3_L6_NAdiff <- SF3_L6_NA[-length(SF3_L6_NA)]
SF3_L6_noNA <- na.locf(dendro_SF3_raw$SF3_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw$Date_Time.px,dendro_SF3_raw$SF3_L6_dia_raw,type="l", col=1)
lines(dendro_SF3_raw$Date_Time.px,SF3_L6_noNA,type="l", col=2)
SF3_L6_dia_diff_noNA <- diff(SF3_L6_noNA)
# SF3_L6_dia_diff.c <-ifelse(SF3_L6_dia_diff_noNA > 1 | SF3_L6_dia_diff_noNA < -1 & SF3_L6_NAdiff == FALSE,NA,SF3_L6_dia_diff_noNA) 
SF3_L6_dia_diff.c <-ifelse((SF3_L6_dia_diff_noNA > 1 & SF3_L6_NAdiff == FALSE) | (SF3_L6_dia_diff_noNA < -0.8 & SF3_L6_NAdiff == FALSE),NA,SF3_L6_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L6_dia_diff.c,type="l", col=1)
summary(SF3_L6_dia_diff.c)
SF3_L6_dia_diff.0 <- na.fill(SF3_L6_dia_diff.c,0);summary(SF3_L6_dia_diff.0)
SF3_L6_dia.c <- diffinv(SF3_L6_dia_diff.0)
SF3_L6_dia.NA <- is.na(dendro_SF3_raw$SF3_L6_dia_raw)
SF3_L6_dia.c <- ifelse(SF3_L6_dia.NA == TRUE, NA, SF3_L6_dia.c); summary(SF3_L6_dia.c)
plot(dendro_SF3_raw$Date_Time.px,SF3_L6_dia.c,type="l", col=1)

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF3_cleared_5mv_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF3_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

summary(dendro_SF4_raw)
# SF4_L1 ####
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L1_dia_diff <- diff(dendro_SF4_raw$SF4_L1_dia_raw)
plot(SF4_Date_diff.px,SF4_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF4_L1_NA <- is.na(dendro_SF4_raw$SF4_L1_dia_raw)
SF4_L1_NAdiff <- SF4_L1_NA[-length(SF4_L1_NA)]
SF4_L1_noNA <- na.locf(dendro_SF4_raw$SF4_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L1_noNA,type="l", col=2)
SF4_L1_dia_diff_noNA <- diff(SF4_L1_noNA)
# SF4_L1_dia_diff.c <-ifelse(SF4_L1_dia_diff_noNA > 1 | SF4_L1_dia_diff_noNA < -1 & SF4_L1_NAdiff == FALSE,NA,SF4_L1_dia_diff_noNA) 
SF4_L1_dia_diff.c <-ifelse((SF4_L1_dia_diff_noNA > 1 & SF4_L1_NAdiff == FALSE) | (SF4_L1_dia_diff_noNA < -1 & SF4_L1_NAdiff == FALSE),NA,SF4_L1_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L1_dia_diff.c,type="l", col=1)
summary(SF4_L1_dia_diff.c)
SF4_L1_dia_diff.0 <- na.fill(SF4_L1_dia_diff.c,0);summary(SF4_L1_dia_diff.0)
SF4_L1_dia.c <- diffinv(SF4_L1_dia_diff.0)
SF4_L1_dia.NA <- is.na(dendro_SF4_raw$SF4_L1_dia_raw)
SF4_L1_dia.c <- ifelse(SF4_L1_dia.NA == TRUE, NA, SF4_L1_dia.c); summary(SF4_L1_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L1_dia.c,type="l", col=1)
summary(SF4_L1_dia.c)
SF4_L1_dia_diff.c2 <- ifelse(SF4_L1_NAdiff == TRUE, NA, SF4_L1_dia_diff.c); summary(SF4_L1_dia_diff.c2)

# SF4_L2
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L2_dia_diff <- diff(dendro_SF4_raw$SF4_L2_dia_raw)
plot(SF4_Date_diff.px,SF4_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF4_L2_NA <- is.na(dendro_SF4_raw$SF4_L2_dia_raw)
SF4_L2_NAdiff <- SF4_L2_NA[-length(SF4_L2_NA)]
SF4_L2_noNA <- na.locf(dendro_SF4_raw$SF4_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L2_noNA,type="l", col=2)
SF4_L2_dia_diff_noNA <- diff(SF4_L2_noNA)
# SF4_L2_dia_diff.c <-ifelse(SF4_L2_dia_diff_noNA > 1 | SF4_L2_dia_diff_noNA < -1 & SF4_L2_NAdiff == FALSE,NA,SF4_L2_dia_diff_noNA) 
SF4_L2_dia_diff.c <-ifelse((SF4_L2_dia_diff_noNA > 1 & SF4_L2_NAdiff == FALSE) | (SF4_L2_dia_diff_noNA < -1 & SF4_L2_NAdiff == FALSE),NA,SF4_L2_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L2_dia_diff.c,type="l", col=1)
summary(SF4_L2_dia_diff.c)
SF4_L2_dia_diff.0 <- na.fill(SF4_L2_dia_diff.c,0);summary(SF4_L2_dia_diff.0)
SF4_L2_dia.c <- diffinv(SF4_L2_dia_diff.0)
SF4_L2_dia.NA <- is.na(dendro_SF4_raw$SF4_L2_dia_raw)
SF4_L2_dia.c <- ifelse(SF4_L2_dia.NA == TRUE, NA, SF4_L2_dia.c); summary(SF4_L2_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L2_dia.c,type="l", col=1)
SF4_L2_dia_diff.c2 <- ifelse(SF4_L2_NAdiff == TRUE, NA, SF4_L2_dia_diff.c); summary(SF4_L2_dia_diff.c2)

# SF4_L3 
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L3_dia_diff <- diff(dendro_SF4_raw$SF4_L3_dia_raw)
plot(SF4_Date_diff.px,SF4_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_L3_NA <- is.na(dendro_SF4_raw$SF4_L3_dia_raw)
SF4_L3_NAdiff <- SF4_L3_NA[-length(SF4_L3_NA)]
SF4_L3_noNA <- na.locf(dendro_SF4_raw$SF4_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L3_noNA,type="l", col=2)
SF4_L3_dia_diff_noNA <- diff(SF4_L3_noNA)
# SF4_L3_dia_diff.c <-ifelse(SF4_L3_dia_diff_noNA > 1 | SF4_L3_dia_diff_noNA < -1 & SF4_L3_NAdiff == FALSE,NA,SF4_L3_dia_diff_noNA) 
SF4_L3_dia_diff.c <-ifelse((SF4_L3_dia_diff_noNA > 1 & SF4_L3_NAdiff == FALSE) | (SF4_L3_dia_diff_noNA < -1 & SF4_L3_NAdiff == FALSE),NA,SF4_L3_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L3_dia_diff.c,type="l", col=1)
summary(SF4_L3_dia_diff.c)
SF4_L3_dia_diff.0 <- na.fill(SF4_L3_dia_diff.c,0);summary(SF4_L3_dia_diff.0)
SF4_L3_dia.c <- diffinv(SF4_L3_dia_diff.0)
SF4_L3_dia.NA <- is.na(dendro_SF4_raw$SF4_L3_dia_raw)
SF4_L3_dia.c <- ifelse(SF4_L3_dia.NA == TRUE, NA, SF4_L3_dia.c); summary(SF4_L3_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L3_dia.c,type="l", col=1)
SF4_L3_dia_diff.c2 <- ifelse(SF4_L3_NAdiff == TRUE, NA, SF4_L3_dia_diff.c); summary(SF4_L3_dia_diff.c2)

# SF4_L4
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L4_dia_diff <- diff(dendro_SF4_raw$SF4_L4_dia_raw)
plot(SF4_Date_diff.px,SF4_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_L4_NA <- is.na(dendro_SF4_raw$SF4_L4_dia_raw)
SF4_L4_NAdiff <- SF4_L4_NA[-length(SF4_L4_NA)]
SF4_L4_noNA <- na.locf(dendro_SF4_raw$SF4_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_L4_noNA,type="l", col=2)
SF4_L4_dia_diff_noNA <- diff(SF4_L4_noNA)
# SF4_L4_dia_diff.c <-ifelse(SF4_L4_dia_diff_noNA > 1 | SF4_L4_dia_diff_noNA < -1 & SF4_L4_NAdiff == FALSE,NA,SF4_L4_dia_diff_noNA) 
SF4_L4_dia_diff.c <-ifelse((SF4_L4_dia_diff_noNA > 1 & SF4_L4_NAdiff == FALSE) | (SF4_L4_dia_diff_noNA < -1 & SF4_L4_NAdiff == FALSE),NA,SF4_L4_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L4_dia_diff.c,type="l", col=1)
summary(SF4_L4_dia_diff.c)
SF4_L4_dia_diff.0 <- na.fill(SF4_L4_dia_diff.c,0);summary(SF4_L4_dia_diff.0)
SF4_L4_dia.c <- diffinv(SF4_L4_dia_diff.0)
SF4_L4_dia.NA <- is.na(dendro_SF4_raw$SF4_L4_dia_raw)
SF4_L4_dia.c <- ifelse(SF4_L4_dia.NA == TRUE, NA, SF4_L4_dia.c); summary(SF4_L4_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_L4_dia.c,type="l", col=1)
SF4_L4_dia_diff.c2 <- ifelse(SF4_L4_NAdiff == TRUE, NA, SF4_L4_dia_diff.c); summary(SF4_L4_dia_diff.c2)

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4L_cleared_5mv_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw$Date_Time.px,SF4_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
# dev.off()

# SF4_Z1 ####
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z1_dia_diff <- diff(dendro_SF4_raw$SF4_Z1_dia_raw)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF4_Z1_NA <- is.na(dendro_SF4_raw$SF4_Z1_dia_raw)
SF4_Z1_NAdiff <- SF4_Z1_NA[-length(SF4_Z1_NA)]
SF4_Z1_noNA <- na.locf(dendro_SF4_raw$SF4_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z1_noNA,type="l", col=2)
SF4_Z1_dia_diff_noNA <- diff(SF4_Z1_noNA)
# SF4_Z1_dia_diff.c <-ifelse(SF4_Z1_dia_diff_noNA > 1 | SF4_Z1_dia_diff_noNA < -1 & SF4_Z1_NAdiff == FALSE,NA,SF4_Z1_dia_diff_noNA) 
SF4_Z1_dia_diff.c <-ifelse((SF4_Z1_dia_diff_noNA > 1 & SF4_Z1_NAdiff == FALSE) | (SF4_Z1_dia_diff_noNA < -1 & SF4_Z1_NAdiff == FALSE),NA,SF4_Z1_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_Z1_dia_diff.c,type="l", col=1)
summary(SF4_Z1_dia_diff.c)
SF4_Z1_dia_diff.0 <- na.fill(SF4_Z1_dia_diff.c,0);summary(SF4_Z1_dia_diff.0)
SF4_Z1_dia.c <- diffinv(SF4_Z1_dia_diff.0)
SF4_Z1_dia.NA <- is.na(dendro_SF4_raw$SF4_Z1_dia_raw)
SF4_Z1_dia.c <- ifelse(SF4_Z1_dia.NA == TRUE, NA, SF4_Z1_dia.c); summary(SF4_Z1_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z1_dia.c,type="l", col=1)
SF4_Z1_dia_diff.c2 <- ifelse(SF4_Z1_NAdiff == TRUE, NA, SF4_Z1_dia_diff.c); summary(SF4_Z1_dia_diff.c2)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff.c2,type="l", col=1)

# SF4_Z2
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z2_dia_diff <- diff(dendro_SF4_raw$SF4_Z2_dia_raw)
plot(SF4_Date_diff.px,SF4_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF4_Z2_NA <- is.na(dendro_SF4_raw$SF4_Z2_dia_raw)
SF4_Z2_NAdiff <- SF4_Z2_NA[-length(SF4_Z2_NA)]
SF4_Z2_noNA <- na.locf(dendro_SF4_raw$SF4_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z2_noNA,type="l", col=2)
SF4_Z2_dia_diff_noNA <- diff(SF4_Z2_noNA)
min(SF4_Z2_dia_diff_noNA,na.rm=TRUE)
SF4_Z2_NAdiff[match(min(SF4_Z2_dia_diff_noNA,na.rm=TRUE),SF4_Z2_dia_diff_noNA)]
# SF4_Z2_dia_diff.c <-ifelse(SF4_Z2_dia_diff_noNA > 1 | SF4_Z2_dia_diff_noNA < -1 & SF4_Z2_NAdiff == FALSE,NA,SF4_Z2_dia_diff_noNA) 
SF4_Z2_dia_diff.c <-ifelse((SF4_Z2_dia_diff_noNA > 1 & SF4_Z2_NAdiff == FALSE) | (SF4_Z2_dia_diff_noNA < -1 & SF4_Z2_NAdiff == FALSE),NA,SF4_Z2_dia_diff_noNA) 
min(SF4_Z2_dia_diff.c ,na.rm=TRUE)
SF4_Z2_dia_diff.c[match(min(SF4_Z2_dia_diff.c,na.rm=TRUE),SF4_Z2_dia_diff.c)] <- NA            # new installation in Spring 2014
plot(SF4_Date_diff.px,SF4_Z2_dia_diff.c,type="l", col=1)
summary(SF4_Z2_dia_diff.c)
SF4_Z2_dia_diff.0 <- na.fill(SF4_Z2_dia_diff.c,0);summary(SF4_Z2_dia_diff.0)
SF4_Z2_dia.c <- diffinv(SF4_Z2_dia_diff.0)
SF4_Z2_dia.NA <- is.na(dendro_SF4_raw$SF4_Z2_dia_raw)
SF4_Z2_dia.c <- ifelse(SF4_Z2_dia.NA == TRUE, NA, SF4_Z2_dia.c); summary(SF4_Z2_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z2_dia.c,type="l", col=1)
SF4_Z2_dia_diff.c2 <- ifelse(SF4_Z2_NAdiff == TRUE, NA, SF4_Z2_dia_diff.c); summary(SF4_Z2_dia_diff.c2)
plot(SF4_Date_diff.px,SF4_Z2_dia_diff.c2,type="l", col=1)

# SF4_Z3 
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z3_dia_diff <- diff(dendro_SF4_raw$SF4_Z3_dia_raw)
plot(SF4_Date_diff.px,SF4_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_Z3_NA <- is.na(dendro_SF4_raw$SF4_Z3_dia_raw)
SF4_Z3_NAdiff <- SF4_Z3_NA[-length(SF4_Z3_NA)]
SF4_Z3_noNA <- na.locf(dendro_SF4_raw$SF4_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z3_noNA,type="l", col=2)
SF4_Z3_dia_diff_noNA <- diff(SF4_Z3_noNA)
# SF4_Z3_dia_diff.c <-ifelse(SF4_Z3_dia_diff_noNA > 1 | SF4_Z3_dia_diff_noNA < -1 & SF4_Z3_NAdiff == FALSE,NA,SF4_Z3_dia_diff_noNA) 
SF4_Z3_dia_diff.c <-ifelse((SF4_Z3_dia_diff_noNA > 1 & SF4_Z3_NAdiff == FALSE) | (SF4_Z3_dia_diff_noNA < -1 & SF4_Z3_NAdiff == FALSE),NA,SF4_Z3_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_Z3_dia_diff.c,type="l", col=1)
summary(SF4_Z3_dia_diff.c)
SF4_Z3_dia_diff.0 <- na.fill(SF4_Z3_dia_diff.c,0);summary(SF4_Z3_dia_diff.0)
SF4_Z3_dia.c <- diffinv(SF4_Z3_dia_diff.0)
SF4_Z3_dia.NA <- is.na(dendro_SF4_raw$SF4_Z3_dia_raw)
SF4_Z3_dia.c <- ifelse(SF4_Z3_dia.NA == TRUE, NA, SF4_Z3_dia.c); summary(SF4_Z3_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z3_dia.c,type="l", col=1)
SF4_Z3_dia_diff.c2 <- ifelse(SF4_Z3_NAdiff == TRUE, NA, SF4_Z3_dia_diff.c); summary(SF4_Z3_dia_diff.c2)
plot(SF4_Date_diff.px,SF4_Z3_dia_diff.c2,type="l", col=1)

# SF4_Z4
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z4_dia_diff <- diff(dendro_SF4_raw$SF4_Z4_dia_raw)
plot(SF4_Date_diff.px,SF4_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_Z4_NA <- is.na(dendro_SF4_raw$SF4_Z4_dia_raw)
SF4_Z4_NAdiff <- SF4_Z4_NA[-length(SF4_Z4_NA)]
SF4_Z4_noNA <- na.locf(dendro_SF4_raw$SF4_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw$Date_Time.px,dendro_SF4_raw$SF4_Z4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw$Date_Time.px,SF4_Z4_noNA,type="l", col=2)
SF4_Z4_dia_diff_noNA <- diff(SF4_Z4_noNA)
# SF4_Z4_dia_diff.c <-ifelse(SF4_Z4_dia_diff_noNA > 1 | SF4_Z4_dia_diff_noNA < -1 & SF4_Z4_NAdiff == FALSE,NA,SF4_Z4_dia_diff_noNA) 
SF4_Z4_dia_diff.c <-ifelse((SF4_Z4_dia_diff_noNA > 1 & SF4_Z4_NAdiff == FALSE) | (SF4_Z4_dia_diff_noNA < -1 & SF4_Z4_NAdiff == FALSE),NA,SF4_Z4_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_Z4_dia_diff.c,type="l", col=1)
summary(SF4_Z4_dia_diff.c)
SF4_Z4_dia_diff.0 <- na.fill(SF4_Z4_dia_diff.c,0);summary(SF4_Z4_dia_diff.0)
SF4_Z4_dia.c <- diffinv(SF4_Z4_dia_diff.0)
SF4_Z4_dia.NA <- is.na(dendro_SF4_raw$SF4_Z4_dia_raw)
SF4_Z4_dia.c <- ifelse(SF4_Z4_dia.NA == TRUE, NA, SF4_Z4_dia.c); summary(SF4_Z4_dia.c)
plot(dendro_SF4_raw$Date_Time.px,SF4_Z4_dia.c,type="l", col=1)
SF4_Z4_dia_diff.c2 <- ifelse(SF4_Z4_NAdiff == TRUE, NA, SF4_Z4_dia_diff.c); summary(SF4_Z4_dia_diff.c2)
plot(SF4_Date_diff.px,SF4_Z4_dia_diff.c2,type="l", col=1)

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4Z_cleared_5mv_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF4Z_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

summary(dendro_SF5_raw)
# SF5_L1 ####
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L1_dia_diff <- diff(dendro_SF5_raw$SF5_L1_dia_raw)
plot(SF5_Date_diff.px,SF5_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF5_L1_NA <- is.na(dendro_SF5_raw$SF5_L1_dia_raw)
SF5_L1_NAdiff <- SF5_L1_NA[-length(SF5_L1_NA)]
SF5_L1_noNA <- na.locf(dendro_SF5_raw$SF5_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L1_noNA,type="l", col=2)
SF5_L1_dia_diff_noNA <- diff(SF5_L1_noNA)
# SF5_L1_dia_diff.c <-ifelse(SF5_L1_dia_diff_noNA > 1 | SF5_L1_dia_diff_noNA < -1 & SF5_L1_NAdiff == FALSE,NA,SF5_L1_dia_diff_noNA) 
SF5_L1_dia_diff.c <-ifelse((SF5_L1_dia_diff_noNA > 1 & SF5_L1_NAdiff == FALSE) | (SF5_L1_dia_diff_noNA < -1 & SF5_L1_NAdiff == FALSE),NA,SF5_L1_dia_diff_noNA) 
SF5_L1_dia_diff.c[match(min(SF5_L1_dia_diff.c,na.rm=TRUE),SF5_L1_dia_diff.c)] <- NA            # new installation in Spring 2014
plot(SF5_Date_diff.px,SF5_L1_dia_diff.c,type="l", col=1)
summary(SF5_L1_dia_diff.c)
SF5_L1_dia_diff.0 <- na.fill(SF5_L1_dia_diff.c,0);summary(SF5_L1_dia_diff.0)
SF5_L1_dia.c <- diffinv(SF5_L1_dia_diff.0)
SF5_L1_dia.NA <- is.na(dendro_SF5_raw$SF5_L1_dia_raw)
SF5_L1_dia.c <- ifelse(SF5_L1_dia.NA == TRUE, NA, SF5_L1_dia.c); summary(SF5_L1_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L1_dia.c,type="l", col=1)
SF5_L1_dia_diff.c2 <- ifelse(SF5_L1_NAdiff == TRUE, NA, SF5_L1_dia_diff.c); summary(SF5_L1_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_L1_dia_diff.c2,type="l", col=1)

# SF5_L2
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L2_dia_diff <- diff(dendro_SF5_raw$SF5_L2_dia_raw)
plot(SF5_Date_diff.px,SF5_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_L2_NA <- is.na(dendro_SF5_raw$SF5_L2_dia_raw)
SF5_L2_NAdiff <- SF5_L2_NA[-length(SF5_L2_NA)]
SF5_L2_noNA <- na.locf(dendro_SF5_raw$SF5_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L2_noNA,type="l", col=2)
SF5_L2_dia_diff_noNA <- diff(SF5_L2_noNA)
SF5_L2_dia_diff.c <-ifelse((SF5_L2_dia_diff_noNA > 1 & SF5_L2_NAdiff == FALSE) | (SF5_L2_dia_diff_noNA < -1 & SF5_L2_NAdiff == FALSE),NA,SF5_L2_dia_diff_noNA) 
SF5_L2_dia_diff.c[c(9672,12189)] <- NA
plot(SF5_Date_diff.px,SF5_L2_dia_diff.c,type="l", col=1)
summary(SF5_L2_dia_diff.c)
SF5_L2_dia_diff.0 <- na.fill(SF5_L2_dia_diff.c,0);summary(SF5_L2_dia_diff.0)
SF5_L2_dia.c <- diffinv(SF5_L2_dia_diff.0)
SF5_L2_dia.NA <- is.na(dendro_SF5_raw$SF5_L2_dia_raw)
SF5_L2_dia.c <- ifelse(SF5_L2_dia.NA == TRUE, NA, SF5_L2_dia.c); summary(SF5_L2_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L2_dia.c,type="l", col=1)
SF5_L2_dia_diff.c2 <- ifelse(SF5_L2_NAdiff == TRUE, NA, SF5_L2_dia_diff.c); summary(SF5_L2_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_L2_dia_diff.c2,type="l", col=1)

# SF5_L3 
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L3_dia_diff <- diff(dendro_SF5_raw$SF5_L3_dia_raw)
plot(SF5_Date_diff.px,SF5_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_L3_NA <- is.na(dendro_SF5_raw$SF5_L3_dia_raw)
SF5_L3_NAdiff <- SF5_L3_NA[-length(SF5_L3_NA)]
SF5_L3_noNA <- na.locf(dendro_SF5_raw$SF5_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L3_noNA,type="l", col=2)
SF5_L3_dia_diff_noNA <- diff(SF5_L3_noNA)
# SF5_L3_dia_diff.c <-ifelse(SF5_L3_dia_diff_noNA > 1 | SF5_L3_dia_diff_noNA < -1 & SF5_L3_NAdiff == FALSE,NA,SF5_L3_dia_diff_noNA) 
SF5_L3_dia_diff.c <-ifelse((SF5_L3_dia_diff_noNA > 1 & SF5_L3_NAdiff == FALSE) | (SF5_L3_dia_diff_noNA < -1 & SF5_L3_NAdiff == FALSE),NA,SF5_L3_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_L3_dia_diff.c,type="l", col=1)
summary(SF5_L3_dia_diff.c)
SF5_L3_dia_diff.0 <- na.fill(SF5_L3_dia_diff.c,0);summary(SF5_L3_dia_diff.0)
SF5_L3_dia.c <- diffinv(SF5_L3_dia_diff.0)
SF5_L3_dia.NA <- is.na(dendro_SF5_raw$SF5_L3_dia_raw)
SF5_L3_dia.c <- ifelse(SF5_L3_dia.NA == TRUE, NA, SF5_L3_dia.c); summary(SF5_L3_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L3_dia.c,type="l", col=1)
SF5_L3_dia_diff.c2 <- ifelse(SF5_L3_NAdiff == TRUE, NA, SF5_L3_dia_diff.c); summary(SF5_L3_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_L3_dia_diff.c2,type="l", col=1)

# SF5_L4
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L4_dia_diff <- diff(dendro_SF5_raw$SF5_L4_dia_raw)
plot(SF5_Date_diff.px,SF5_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_L4_NA <- is.na(dendro_SF5_raw$SF5_L4_dia_raw)
SF5_L4_NAdiff <- SF5_L4_NA[-length(SF5_L4_NA)]
SF5_L4_noNA <- na.locf(dendro_SF5_raw$SF5_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_L4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_L4_noNA,type="l", col=2)
SF5_L4_dia_diff_noNA <- diff(SF5_L4_noNA)
# SF5_L4_dia_diff.c <-ifelse(SF5_L4_dia_diff_noNA > 1 | SF5_L4_dia_diff_noNA < -1 & SF5_L4_NAdiff == FALSE,NA,SF5_L4_dia_diff_noNA) 
SF5_L4_dia_diff.c <-ifelse((SF5_L4_dia_diff_noNA > 1 & SF5_L4_NAdiff == FALSE) | (SF5_L4_dia_diff_noNA < -1 & SF5_L4_NAdiff == FALSE),NA,SF5_L4_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_L4_dia_diff.c,type="l", col=1)
summary(SF5_L4_dia_diff.c)
SF5_L4_dia_diff.0 <- na.fill(SF5_L4_dia_diff.c,0);summary(SF5_L4_dia_diff.0)
SF5_L4_dia.c <- diffinv(SF5_L4_dia_diff.0)
SF5_L4_dia.NA <- is.na(dendro_SF5_raw$SF5_L4_dia_raw)
SF5_L4_dia.c <- ifelse(SF5_L4_dia.NA == TRUE, NA, SF5_L4_dia.c); summary(SF5_L4_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_L4_dia.c,type="l", col=1)
SF5_L4_dia_diff.c2 <- ifelse(SF5_L4_NAdiff == TRUE, NA, SF5_L4_dia_diff.c); summary(SF5_L4_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_L4_dia_diff.c2,type="l", col=1)

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5L_cleared_5mv_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

# SF5_Z1 ####
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z1_dia_diff <- diff(dendro_SF5_raw$SF5_Z1_dia_raw)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF5_Z1_NA <- is.na(dendro_SF5_raw$SF5_Z1_dia_raw)
SF5_Z1_NAdiff <- SF5_Z1_NA[-length(SF5_Z1_NA)]
SF5_Z1_noNA <- na.locf(dendro_SF5_raw$SF5_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z1_noNA,type="l", col=2)
SF5_Z1_dia_diff_noNA <- diff(SF5_Z1_noNA)
# SF5_Z1_dia_diff.c <-ifelse(SF5_Z1_dia_diff_noNA > 1 | SF5_Z1_dia_diff_noNA < -1 & SF5_Z1_NAdiff == FALSE,NA,SF5_Z1_dia_diff_noNA) 
SF5_Z1_dia_diff.c <-ifelse((SF5_Z1_dia_diff_noNA > 1 & SF5_Z1_NAdiff == FALSE) | (SF5_Z1_dia_diff_noNA < -1 & SF5_Z1_NAdiff == FALSE),NA,SF5_Z1_dia_diff_noNA) 
SF5_Z1_dia_diff.c[match(min(SF5_Z1_dia_diff.c,na.rm=TRUE),SF5_Z1_dia_diff.c)] <- NA  
plot(SF5_Date_diff.px,SF5_Z1_dia_diff.c,type="l", col=1)
summary(SF5_Z1_dia_diff.c)
SF5_Z1_dia_diff.0 <- na.fill(SF5_Z1_dia_diff.c,0);summary(SF5_Z1_dia_diff.0)
SF5_Z1_dia.c <- diffinv(SF5_Z1_dia_diff.0)
SF5_Z1_dia.NA <- is.na(dendro_SF5_raw$SF5_Z1_dia_raw)
SF5_Z1_dia.c <- ifelse(SF5_Z1_dia.NA == TRUE, NA, SF5_Z1_dia.c); summary(SF5_Z1_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z1_dia.c,type="l", col=1)
SF5_Z1_dia_diff.c2 <- ifelse(SF5_Z1_NAdiff == TRUE, NA, SF5_Z1_dia_diff.c); summary(SF5_Z1_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff.c2,type="l", col=1)

# SF5_Z2
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z2_dia_diff <- diff(dendro_SF5_raw$SF5_Z2_dia_raw)
plot(SF5_Date_diff.px,SF5_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_Z2_NA <- is.na(dendro_SF5_raw$SF5_Z2_dia_raw)
SF5_Z2_NAdiff <- SF5_Z2_NA[-length(SF5_Z2_NA)]
SF5_Z2_noNA <- na.locf(dendro_SF5_raw$SF5_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z2_noNA,type="l", col=2)
SF5_Z2_dia_diff_noNA <- diff(SF5_Z2_noNA)
# SF5_Z2_dia_diff.c <-ifelse(SF5_Z2_dia_diff_noNA > 1 | SF5_Z2_dia_diff_noNA < -1 & SF5_Z2_NAdiff == FALSE,NA,SF5_Z2_dia_diff_noNA) 
SF5_Z2_dia_diff.c <-ifelse((SF5_Z2_dia_diff_noNA > 1 & SF5_Z2_NAdiff == FALSE) | (SF5_Z2_dia_diff_noNA < -1 & SF5_Z2_NAdiff == FALSE),NA,SF5_Z2_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_Z2_dia_diff.c,type="l", col=1)
summary(SF5_Z2_dia_diff.c)
SF5_Z2_dia_diff.0 <- na.fill(SF5_Z2_dia_diff.c,0);summary(SF5_Z2_dia_diff.0)
SF5_Z2_dia.c <- diffinv(SF5_Z2_dia_diff.0)
SF5_Z2_dia.NA <- is.na(dendro_SF5_raw$SF5_Z2_dia_raw)
SF5_Z2_dia.c <- ifelse(SF5_Z2_dia.NA == TRUE, NA, SF5_Z2_dia.c); summary(SF5_Z2_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z2_dia.c,type="l", col=1)
SF5_Z2_dia_diff.c2 <- ifelse(SF5_Z2_NAdiff == TRUE, NA, SF5_Z2_dia_diff.c); summary(SF5_Z2_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_Z2_dia_diff.c2,type="l", col=1)

# SF5_Z3 
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z3_dia_diff <- diff(dendro_SF5_raw$SF5_Z3_dia_raw)
plot(SF5_Date_diff.px,SF5_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_Z3_NA <- is.na(dendro_SF5_raw$SF5_Z3_dia_raw)
SF5_Z3_NAdiff <- SF5_Z3_NA[-length(SF5_Z3_NA)]
SF5_Z3_noNA <- na.locf(dendro_SF5_raw$SF5_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z3_noNA,type="l", col=2)
SF5_Z3_dia_diff_noNA <- diff(SF5_Z3_noNA)
# SF5_Z3_dia_diff.c <-ifelse(SF5_Z3_dia_diff_noNA > 1 | SF5_Z3_dia_diff_noNA < -1 & SF5_Z3_NAdiff == FALSE,NA,SF5_Z3_dia_diff_noNA) 
SF5_Z3_dia_diff.c <-ifelse((SF5_Z3_dia_diff_noNA > 1 & SF5_Z3_NAdiff == FALSE) | (SF5_Z3_dia_diff_noNA < -1 & SF5_Z3_NAdiff == FALSE),NA,SF5_Z3_dia_diff_noNA) 
SF5_Z3_dia_diff.c[match(min(SF5_Z3_dia_diff.c,na.rm=TRUE),SF5_Z3_dia_diff.c)] <- NA  
plot(SF5_Date_diff.px,SF5_Z3_dia_diff.c,type="l", col=1)
summary(SF5_Z3_dia_diff.c)
SF5_Z3_dia_diff.0 <- na.fill(SF5_Z3_dia_diff.c,0);summary(SF5_Z3_dia_diff.0)
SF5_Z3_dia.c <- diffinv(SF5_Z3_dia_diff.0)
SF5_Z3_dia.NA <- is.na(dendro_SF5_raw$SF5_Z3_dia_raw)
SF5_Z3_dia.c <- ifelse(SF5_Z3_dia.NA == TRUE, NA, SF5_Z3_dia.c); summary(SF5_Z3_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z3_dia.c,type="l", col=1)
SF5_Z3_dia_diff.c2 <- ifelse(SF5_Z3_NAdiff == TRUE, NA, SF5_Z3_dia_diff.c); summary(SF5_Z3_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_Z3_dia_diff.c2,type="l", col=1)

# SF5_Z4
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z4_dia_diff <- diff(dendro_SF5_raw$SF5_Z4_dia_raw)
plot(SF5_Date_diff.px,SF5_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)

SF5_Z4_NA <- is.na(dendro_SF5_raw$SF5_Z4_dia_raw)
SF5_Z4_NAdiff <- SF5_Z4_NA[-length(SF5_Z4_NA)]
SF5_Z4_noNA <- na.locf(dendro_SF5_raw$SF5_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw$Date_Time.px,dendro_SF5_raw$SF5_Z4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw$Date_Time.px,SF5_Z4_noNA,type="l", col=2)
SF5_Z4_dia_diff_noNA <- diff(SF5_Z4_noNA)
# SF5_Z4_dia_diff.c <-ifelse(SF5_Z4_dia_diff_noNA > 1 | SF5_Z4_dia_diff_noNA < -1 & SF5_Z4_NAdiff == FALSE,NA,SF5_Z4_dia_diff_noNA) 
SF5_Z4_dia_diff.c <-ifelse((SF5_Z4_dia_diff_noNA > 1 & SF5_Z4_NAdiff == FALSE) | (SF5_Z4_dia_diff_noNA < -1 & SF5_Z4_NAdiff == FALSE),NA,SF5_Z4_dia_diff_noNA) 
SF5_Z4_dia_diff.c[match(min(SF5_Z4_dia_diff.c,na.rm=TRUE),SF5_Z4_dia_diff.c)] <- NA  
plot(SF5_Date_diff.px,SF5_Z4_dia_diff.c,type="l", col=1)
summary(SF5_Z4_dia_diff.c)
SF5_Z4_dia_diff.0 <- na.fill(SF5_Z4_dia_diff.c,0);summary(SF5_Z4_dia_diff.0)
SF5_Z4_dia.c <- diffinv(SF5_Z4_dia_diff.0)
SF5_Z4_dia.NA <- is.na(dendro_SF5_raw$SF5_Z4_dia_raw)
SF5_Z4_dia.c <- ifelse(SF5_Z4_dia.NA == TRUE, NA, SF5_Z4_dia.c); summary(SF5_Z4_dia.c)
plot(dendro_SF5_raw$Date_Time.px,SF5_Z4_dia.c,type="l", col=1)
SF5_Z4_dia_diff.c2 <- ifelse(SF5_Z4_NAdiff == TRUE, NA, SF5_Z4_dia_diff.c); summary(SF5_Z4_dia_diff.c2)
plot(SF5_Date_diff.px,SF5_Z4_dia_diff.c2,type="l", col=1)

# # win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5Z_cleared_5mv_Trs_10new.emf", height=8, width=12, pointsize=14)
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
# dev.off()

# # win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2014_SF5_comp_raw-cleared_Trs_10new.emf", height=12, width=12, pointsize=14)
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
# dev.off()

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

outfile.dendro_abs <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_cf10new_abs.csv")  
write.csv(exp_dendro_abs,file=outfile.dendro_abs)

outfile.dendro_diff <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_cf10new_diff.csv")  
write.csv(exp_dendro_diff,file=outfile.dendro_diff)

outfile.dendro_temp <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_temp.csv")  
write.csv(exp_dendro_temp,file=outfile.dendro_temp)

# continue to Dendrometer_data_load_merge_and-figures.R   
# => check NA's in Temperature-values SF1-SF3 (nur Werte ab Umstellung auf 10min-Messintervall)
# => correct starting values after winter SF4+SF5
                         
                         
                    