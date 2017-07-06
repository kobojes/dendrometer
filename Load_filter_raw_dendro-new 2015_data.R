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

# load 2015 data and merge datasets
setwd("H:/Data/Matsch_Catchment/SF/dendro files for R/2015_data")
dendro_SF1_raw2015_1 <- read.csv("SF1_dendro-DRL26_hourly_until 2015_06_26.csv",header = T,sep=",")
dendro_SF1_raw2015_2 <- read.csv("SF1_dendro-DRL26_hourly_2015_10_28.csv",header = T,sep=",")
dendro_SF2_raw2015_1 <- read.csv("SF2_dendro-DRL26_hourly_until 2015_06_26.csv",header = T,sep=",")
dendro_SF2_raw2015_2 <- read.csv("SF2_dendro-DR26_hourly_2015_11_03.csv",header = T,sep=",")
dendro_SF3_raw2015 <- read.csv("SF3_dendro_hourly_2015_11_03.csv",header = T,sep=",")
dendro_SF4_raw2015 <- read.csv("SF4_dendro_hourly_2015_10_28.csv",header = T,sep=",")
dendro_SF5_raw2015 <- read.csv("SF5_dendro_hourly_2015_10_28.csv",header = T,sep=",")
dendro_WgN_raw2015 <- read.csv("WG-N_dendro_hourly_2015_10_28.csv",header = T,sep=",")
dendro_WgS_raw2015 <- read.csv("WG-S_dendro_hourly_2015_11_03.csv",header = T,sep=",")

colnames(dendro_SF1_raw2015_1) <- c("Date_Time","SF1_L2_dia_raw","SF1_L2_temp","SF1_L4_dia_raw","SF1_L4_temp",
                              "SF1_L5_dia_raw","SF1_L5_temp","SF1_L6_dia_raw","SF1_L6_temp")
colnames(dendro_SF1_raw2015_2) <- c("Date_Time","SF1_L2_dia_raw","SF1_L4_dia_raw","SF1_L5_dia_raw","SF1_L6_dia_raw")
colnames(dendro_SF2_raw2015_1) <- c("Date_Time","SF2_L1_dia_raw","SF2_L1_temp","SF2_L4_dia_raw","SF2_L4_temp",
                              "SF2_L5_dia_raw","SF2_L5_temp","SF2_L7_dia_raw","SF2_L7_temp")
colnames(dendro_SF2_raw2015_1) <- c("Date_Time","SF2_L1_dia_raw","SF2_L1_temp","SF2_L4_dia_raw","SF2_L4_temp",
                                    "SF2_L5_dia_raw","SF2_L5_temp","SF2_L7_dia_raw","SF2_L7_temp")
colnames(dendro_SF2_raw2015_2) <- c("Date_Time","SF2_L1_dia_raw","SF2_L4_dia_raw","SF2_L5_dia_raw","SF2_L7_dia_raw")
colnames(dendro_SF3_raw2015) <- c("Date_Time","SF3_L3_dia_raw","SF3_L3_temp","SF3_L4_dia_raw","SF3_L4_temp",
                              "SF3_L5_dia_raw","SF3_L5_temp","SF3_L6_dia_raw","SF3_L6_temp")
colnames(dendro_SF4_raw2015) <- c("Date_Time","SF4_L1_dia_raw","SF4_Z1_dia_raw","SF4_L2_dia_raw","SF4_Z2_dia_raw",
                              "SF4_L3_dia_raw","SF4_Z3_dia_raw","SF4_L4_dia_raw","SF4_Z4_dia_raw")
names(dendro_SF5_raw2015)
colnames(dendro_SF5_raw2015) <- c("Date_Time","SF5_L1_dia_raw","SF5_L1_temp","SF5_L2_dia_raw","SF5_L2_temp","SF5_L3_dia_raw","SF5_L3_temp",
                                  "SF5_L4_dia_raw","SF5_L4_temp","SF5_Z1_dia_raw","SF5_Z1_temp","SF5_Z2_dia_raw","SF5_Z2_temp",
                                  "SF5_Z3_dia_raw","SF5_Z31_temp","SF5_Z4_dia_raw","SF5_Z4_temp")
colnames(dendro_WgN_raw2015) <- c("Date_Time","WgN_Z1_dia_raw","WgN_Z1_temp","WgN_Z2_dia_raw","WgN_Z2_temp",
                                  "WgN_Z3_dia_raw","WgN_Z31_temp","WgN_Z4_dia_raw","WgN_Z4_temp")
colnames(dendro_WgS_raw2015) <- c("Date_Time","WgS_L1_dia_raw","WgS_L1_temp","WgS_L2_dia_raw","WgS_L2_temp",
                                  "WgS_L3_dia_raw","WgS_L31_temp","WgS_L4_dia_raw","WgS_L4_temp")
dendro_SF1_raw2015_1$Date_Time.px <- as.POSIXct(dendro_SF1_raw2015_1$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF1_raw2015_2$Date_Time.px <- as.POSIXct(dendro_SF1_raw2015_2$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF2_raw2015_1$Date_Time.px <- as.POSIXct(dendro_SF2_raw2015_1$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF2_raw2015_2$Date_Time.px <- as.POSIXct(dendro_SF2_raw2015_2$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF3_raw2015$Date_Time.px <- as.POSIXct(dendro_SF3_raw2015$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF4_raw2015$Date_Time.px <- as.POSIXct(dendro_SF4_raw2015$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_SF5_raw2015$Date_Time.px <- as.POSIXct(dendro_SF5_raw2015$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_WgN_raw2015$Date_Time.px <- as.POSIXct(dendro_WgN_raw2015$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 
dendro_WgS_raw2015$Date_Time.px <- as.POSIXct(dendro_WgS_raw2015$Date_Time,format="%Y-%m-%d %H:%M", tz="GMT") 

# load field days ####
field_days <- read.csv("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_field_days.csv",header = T,sep=",")
colnames(field_days) <- "Date"
field_days$Date <- as.POSIXct(field_days$Date,format="%Y-%m-%d", tz="GMT") 
jun26.2015 <- as.POSIXct("2015-06-26", format="%Y-%m-%d") 
mar23.2015 <- as.POSIXct("2015-03-23", format="%Y-%m-%d") 

summary(dendro_SF5_raw)
# plot raw value, calculate and plot differences SF1 ####
# Treshold for cleaning dendro data: specific to each dendro (check if max/min values were during field days) => check again with literature!!!
names(dendro_SF1_raw);names(dendro_SF1_raw2015_1);names(dendro_SF1_raw2015_2)
tail(dendro_SF1_raw[1],2); head(dendro_SF1_raw2015_1[1],2); tail(dendro_SF1_raw2015_1[1],2); head(dendro_SF1_raw2015_2[1],2); tail(dendro_SF1_raw2015_2[1],2)

dendro_SF1_raw.all <- rbind(dendro_SF1_raw[,c(10,2,4,6,8)],
                            dendro_SF1_raw2015_1[c((match(tail(dendro_SF1_raw$Date_Time.px,1),dendro_SF1_raw2015_1$Date_Time.px)+1):(dim(dendro_SF1_raw2015_1)[1])),c(10,2,4,6,8)],
                            dendro_SF1_raw2015_2[c(match(tail(dendro_SF1_raw2015_1$Date_Time.px,1),dendro_SF1_raw2015_2$Date_Time.px)+1:dim(dendro_SF1_raw2015_2)[1]),c(6,2,3,4,5)])
summary(dendro_SF1_raw.all)

# SF1_L2
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L2_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw.all$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L2_dia_diff <- diff(dendro_SF1_raw.all$SF1_L2_dia_raw)
plot(SF1_Date_diff.px,SF1_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF1_year_diff.px <- as.POSIXct(cut(SF1_Date_diff.px, "year"))
boxplot(SF1_L2_dia_diff,main="SF1_L2"); boxplot(SF1_L2_dia_diff~SF1_year_diff.px,main="SF1_L2"); 
boxplot(SF1_L2_dia_diff,ylim=c(-1,1),main="SF1_L2"); boxplot(SF1_L2_dia_diff~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L2")
# check if 10 highest max and min values occured during field day
SF1_L2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF1_L2_maxvalue.i <- sort(SF1_L2_dia_diff, TRUE)[i]
  SF1_L2_maxdate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L2_dia_diff == SF1_L2_maxvalue.i)]
  SF1_L2_maxdate.i.1 <- as.POSIXct(cut(SF1_L2_maxdate.i, "day"))
  SF1_L2_minvalue.i <- sort(SF1_L2_dia_diff, FALSE)[i]
  SF1_L2_mindate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L2_dia_diff == SF1_L2_minvalue.i)]
  SF1_L2_mindate.i.1 <- as.POSIXct(cut(SF1_L2_mindate.i, "day"))
  SF1_L2_check[i,] <- c(SF1_L2_maxvalue.i,as.character(SF1_L2_maxdate.i),match(SF1_L2_maxdate.i.1,field_days$Date),
                                 SF1_L2_minvalue.i,as.character(SF1_L2_mindate.i),match(SF1_L2_mindate.i.1,field_days$Date)) 
} 
colnames(SF1_L2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF1_L2_check)

SF1_L2_NA <- is.na(dendro_SF1_raw.all$SF1_L2_dia_raw)
SF1_L2_NAdiff <- SF1_L2_NA[-1]
SF1_L2_noNA <- na.locf(dendro_SF1_raw.all$SF1_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L2_dia_raw,type="l", col=1)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L2_noNA,type="l", col=2)
SF1_L2_dia_diff_noNA <- diff(SF1_L2_noNA)
SF1_L2_dia_diff.c <-ifelse((SF1_L2_dia_diff_noNA > 0.21 & SF1_L2_NAdiff == FALSE) | (SF1_L2_dia_diff_noNA < -0.2 & SF1_L2_NAdiff == FALSE),NA,SF1_L2_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L2_dia_diff.c,type="l", col=1)
boxplot(SF1_L2_dia_diff.c,main="SF1_L2"); boxplot(SF1_L2_dia_diff.c~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L2")
summary(SF1_L2_dia_diff.c)
SF1_L2_dia_diff.0 <- na.fill(SF1_L2_dia_diff.c,0);summary(SF1_L2_dia_diff.0)
SF1_L2_dia.c <- diffinv(SF1_L2_dia_diff.0)
SF1_L2_dia.NA <- is.na(dendro_SF1_raw.all$SF1_L2_dia_raw)
SF1_L2_dia.c <- ifelse(SF1_L2_dia.NA == TRUE, NA, SF1_L2_dia.c); summary(SF1_L2_dia.c)
plot(dendro_SF1_raw.all$Date_Time.px,SF1_L2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF1_L2_dia.c)

# SF1_L4
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L4_dia_raw,type="l", col=1)
SF1_L4_dia_diff <- diff(dendro_SF1_raw.all$SF1_L4_dia_raw)
plot(SF1_Date_diff.px,SF1_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L4_dia_diff,main="SF1_L4"); boxplot(SF1_L4_dia_diff~SF1_year_diff.px,main="SF1_L4")
boxplot(SF1_L4_dia_diff,ylim=c(-1,1),main="SF1_L4"); boxplot(SF1_L4_dia_diff~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L4")
# check if 10 highest max and min values occured during field day
SF1_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF1_L4_maxvalue.i <- sort(SF1_L4_dia_diff, TRUE)[i]
  SF1_L4_maxdate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L4_dia_diff == SF1_L4_maxvalue.i)]
  SF1_L4_maxdate.i.1 <- as.POSIXct(cut(SF1_L4_maxdate.i, "day"))
  SF1_L4_minvalue.i <- sort(SF1_L4_dia_diff, FALSE)[i]
  SF1_L4_mindate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L4_dia_diff == SF1_L4_minvalue.i)]
  SF1_L4_mindate.i.1 <- as.POSIXct(cut(SF1_L4_mindate.i, "day"))
  SF1_L4_check[i,] <- c(SF1_L4_maxvalue.i,as.character(SF1_L4_maxdate.i),match(SF1_L4_maxdate.i.1,field_days$Date),
                        SF1_L4_minvalue.i,as.character(SF1_L4_mindate.i),match(SF1_L4_mindate.i.1,field_days$Date)) 
} 
colnames(SF1_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF1_L4_check)
SF1_L4_NA <- is.na(dendro_SF1_raw.all$SF1_L4_dia_raw)
SF1_L4_NAdiff <- SF1_L4_NA[-1]
SF1_L4_noNA <- na.locf(dendro_SF1_raw.all$SF1_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L4_dia_raw,type="l", col=1)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L4_noNA,type="l", col=2)
SF1_L4_dia_diff_noNA <- diff(SF1_L4_noNA)
SF1_L4_dia_diff.c <-ifelse((SF1_L4_dia_diff_noNA > 0.5 & SF1_L4_NAdiff == FALSE) | (SF1_L4_dia_diff_noNA < -0.5 & SF1_L4_NAdiff == FALSE),NA,SF1_L4_dia_diff_noNA) 
plot(SF1_Date_diff.px,SF1_L4_dia_diff.c,type="l", col=1)
boxplot(SF1_L4_dia_diff.c,main="SF1_L4"); boxplot(SF1_L4_dia_diff.c~SF1_year_diff.px,main="SF1_L4")
summary(SF1_L4_dia_diff.c)
SF1_L4_dia_diff.0 <- na.fill(SF1_L4_dia_diff.c,0);summary(SF1_L4_dia_diff.0)
SF1_L4_dia.c <- diffinv(SF1_L4_dia_diff.0)
SF1_L4_dia.NA <- is.na(dendro_SF1_raw.all$SF1_L4_dia_raw)
SF1_L4_dia.c <- ifelse(SF1_L4_dia.NA == TRUE, NA, SF1_L4_dia.c)
plot(dendro_SF1_raw.all$Date_Time.px,SF1_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF1_L4_dia.c)

# SF1_L5
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L5_dia_raw,type="l", col=1)
SF1_L5_dia_diff <- diff(dendro_SF1_raw.all$SF1_L5_dia_raw)
plot(SF1_Date_diff.px,SF1_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L5_dia_diff,main="SF1_L5"); boxplot(SF1_L5_dia_diff~SF1_year_diff.px,main="SF1_L5"); 
boxplot(SF1_L5_dia_diff,ylim=c(-1,1),main="SF1_L5"); boxplot(SF1_L5_dia_diff~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L5")
# check if 10 highest max and min values occured during field day
SF1_L5_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF1_L5_maxvalue.i <- sort(SF1_L5_dia_diff, TRUE)[i]
  SF1_L5_maxdate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L5_dia_diff == SF1_L5_maxvalue.i)]
  SF1_L5_maxdate.i.1 <- as.POSIXct(cut(SF1_L5_maxdate.i, "day"))
  SF1_L5_minvalue.i <- sort(SF1_L5_dia_diff, FALSE)[i]
  SF1_L5_mindate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L5_dia_diff == SF1_L5_minvalue.i)]
  SF1_L5_mindate.i.1 <- as.POSIXct(cut(SF1_L5_mindate.i, "day"))
 SF1_L5_check[i,] <- c(SF1_L5_maxvalue.i,as.character(SF1_L5_maxdate.i),match(SF1_L5_maxdate.i.1,field_days$Date),
                        SF1_L5_minvalue.i,as.character(SF1_L5_mindate.i),match(SF1_L5_mindate.i.1,field_days$Date)) 
} 
colnames(SF1_L5_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF1_L5_check)
SF1_L5_NA <- is.na(dendro_SF1_raw.all$SF1_L5_dia_raw)
SF1_L5_NAdiff <- SF1_L5_NA[-1]
SF1_L5_noNA <- na.locf(dendro_SF1_raw.all$SF1_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L5_dia_raw,type="l", col=1)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L5_noNA,type="l", col=2)
SF1_L5_dia_diff_noNA <- diff(SF1_L5_noNA)
SF1_L5_dia_diff.c <-ifelse((SF1_L5_dia_diff_noNA > 0.2 & SF1_L5_NAdiff == FALSE) | (SF1_L5_dia_diff_noNA < -0.2 & SF1_L5_NAdiff == FALSE),NA,SF1_L5_dia_diff_noNA) 
SF1_L5_dia_diff.c[which(SF1_L5_dia_diff == sort(SF1_L5_dia_diff, FALSE)[5])] <- NA
plot(SF1_Date_diff.px,SF1_L5_dia_diff.c,type="l", col=1)
boxplot(SF1_L5_dia_diff.c,main="SF1_L5"); boxplot(SF1_L5_dia_diff.c~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L5")
summary(SF1_L5_dia_diff.c)
SF1_L5_dia_diff.0 <- na.fill(SF1_L5_dia_diff.c,0);summary(SF1_L5_dia_diff.0)
SF1_L5_dia.c <- diffinv(SF1_L5_dia_diff.0)
SF1_L5_dia.NA <- is.na(dendro_SF1_raw.all$SF1_L5_dia_raw)
SF1_L5_dia.c <- ifelse(SF1_L5_dia.NA == TRUE, NA, SF1_L5_dia.c); summary(SF1_L5_dia.c)
plot(dendro_SF1_raw.all$Date_Time.px,SF1_L5_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF1_L5_dia.c)

# SF1_L6
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L6_dia_raw,type="l", col=1)
SF1_Date_diff <- dendro_SF1_raw.all$Date_Time[-1]
SF1_Date_diff.px <- as.POSIXct(SF1_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF1_L6_dia_diff <- diff(dendro_SF1_raw.all$SF1_L6_dia_raw)
plot(SF1_Date_diff.px,SF1_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF1_L6_dia_diff,main="SF1_L6"); boxplot(SF1_L6_dia_diff~SF1_year_diff.px,main="SF1_L6"); 
boxplot(SF1_L6_dia_diff,ylim=c(-1,1),main="SF1_L6"); boxplot(SF1_L6_dia_diff~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L6")
# check if 10 highest max and min values occured during field day
SF1_L6_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF1_L6_maxvalue.i <- sort(SF1_L6_dia_diff, TRUE)[i]
  SF1_L6_maxdate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L6_dia_diff == SF1_L6_maxvalue.i)]
  SF1_L6_maxdate.i.1 <- as.POSIXct(cut(SF1_L6_maxdate.i, "day"))
  SF1_L6_minvalue.i <- sort(SF1_L6_dia_diff, FALSE)[i]
  SF1_L6_mindate.i <- dendro_SF1_raw.all$Date_Time.px[which(SF1_L6_dia_diff == SF1_L6_minvalue.i)]
  SF1_L6_mindate.i.1 <- as.POSIXct(cut(SF1_L6_mindate.i, "day"))
  SF1_L6_check[i,] <- c(SF1_L6_maxvalue.i,as.character(SF1_L6_maxdate.i),match(SF1_L6_maxdate.i.1,field_days$Date),
                        SF1_L6_minvalue.i,as.character(SF1_L6_mindate.i),match(SF1_L6_mindate.i.1,field_days$Date)) 
} 
colnames(SF1_L6_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF1_L6_check)

SF1_L6_NA <- is.na(dendro_SF1_raw.all$SF1_L6_dia_raw)
SF1_L6_NAdiff <- SF1_L6_NA[-1]
SF1_L6_noNA <- na.locf(dendro_SF1_raw.all$SF1_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L6_dia_raw,type="l", col=1)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L6_noNA,type="l", col=2)
SF1_L6_dia_diff_noNA <- diff(SF1_L6_noNA)
SF1_L6_dia_diff.c <-ifelse((SF1_L6_dia_diff_noNA > 0.6 & SF1_L6_NAdiff == FALSE) | (SF1_L6_dia_diff_noNA < -0.3 & SF1_L6_NAdiff == FALSE),NA,SF1_L6_dia_diff_noNA) 
SF1_L6_dia_diff.c[which(SF1_L6_dia_diff == sort(SF1_L6_dia_diff, FALSE)[5])] <- NA
plot(SF1_Date_diff.px,SF1_L6_dia_diff.c,type="l", col=1)
boxplot(SF1_L6_dia_diff.c,main="SF1_L6"); boxplot(SF1_L6_dia_diff.c~SF1_year_diff.px,ylim=c(-1,1),main="SF1_L6")
summary(SF1_L6_dia_diff.c)
SF1_L6_dia_diff.0 <- na.fill(SF1_L6_dia_diff.c,0);summary(SF1_L6_dia_diff.0)
SF1_L6_dia.c <- diffinv(SF1_L6_dia_diff.0)
SF1_L6_dia.NA <- is.na(dendro_SF1_raw.all$SF1_L6_dia_raw)
SF1_L6_dia.c <- ifelse(SF1_L6_dia.NA == TRUE, NA, SF1_L6_dia.c); summary(SF1_L6_dia.c)
plot(dendro_SF1_raw.all$Date_Time.px,SF1_L6_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF1_L6_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF1_corr.emf", height=12, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF1 (raw data)
plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L2_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L4_dia_raw,type="l", col=2)
lines(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L5_dia_raw,type="l", col=3)
lines(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L6_dia_raw,type="l", col=4)

# Plot all trees of SF1 (differences)
plot(SF1_Date_diff.px,SF1_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF1_Date_diff.px,SF1_L4_dia_diff,type="l", col=2)
lines(SF1_Date_diff.px,SF1_L5_dia_diff,type="l", col=3)
lines(SF1_Date_diff.px,SF1_L6_dia_diff,type="l", col=4)
legend("topleft",c("L2","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF1 (cleaned data)
plot(dendro_SF1_raw.all$Date_Time.px,SF1_L2_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L4_dia.c,type="l", col=2)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L5_dia.c,type="l", col=3)
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L6_dia.c,type="l", col=4)
legend("topleft",c("L2","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF1_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L5_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF1_raw.all$Date_Time.px,dendro_SF1_raw.all$SF1_L6_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF1_raw.all$Date_Time.px,SF1_L6_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

summary(dendro_SF2_raw)
par(mfcol=c(1,1))

# SF2 - merge data ####
names(dendro_SF2_raw);names(dendro_SF2_raw2015_1);names(dendro_SF2_raw2015_2)
tail(dendro_SF2_raw[1],2); head(dendro_SF2_raw2015_1[1],2); tail(dendro_SF2_raw2015_1[1],2); head(dendro_SF2_raw2015_2[1],2); tail(dendro_SF2_raw2015_2[1],2)

dendro_SF2_raw.all <- rbind(dendro_SF2_raw[,c(10,2,4,6,8)],
                            dendro_SF2_raw2015_1[c((match(tail(dendro_SF2_raw$Date_Time.px,1),dendro_SF2_raw2015_1$Date_Time.px)+1):(dim(dendro_SF2_raw2015_1)[1])),c(10,2,4,6,8)],
                            dendro_SF2_raw2015_2[,c(6,2,3,4,5)])
summary(dendro_SF2_raw2015_1)
# SF2_L1 ####
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L1_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw.all$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L1_dia_diff <- diff(dendro_SF2_raw.all$SF2_L1_dia_raw)
plot(SF2_Date_diff.px,SF2_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF2_year_diff.px <- as.POSIXct(cut(SF2_Date_diff.px, "year"))
boxplot(SF2_L1_dia_diff,main="SF2_L1"); boxplot(SF2_L1_dia_diff~SF2_year_diff.px,main="SF2_L1"); 
boxplot(SF2_L1_dia_diff,ylim=c(-1,1),main="SF2_L1"); boxplot(SF2_L1_dia_diff~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L1")
# check if 10 highest max and min values occured during field day
SF2_L1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF2_L1_maxvalue.i <- sort(SF2_L1_dia_diff, TRUE)[i]
  SF2_L1_maxdate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L1_dia_diff == SF2_L1_maxvalue.i)]
  SF2_L1_maxdate.i.1 <- as.POSIXct(cut(SF2_L1_maxdate.i, "day"))
  SF2_L1_minvalue.i <- sort(SF2_L1_dia_diff, FALSE)[i]
  SF2_L1_mindate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L1_dia_diff == SF2_L1_minvalue.i)]
  SF2_L1_mindate.i.1 <- as.POSIXct(cut(SF2_L1_mindate.i, "day"))
  SF2_L1_check[i,] <- c(SF2_L1_maxvalue.i,as.character(SF2_L1_maxdate.i),match(SF2_L1_maxdate.i.1,field_days$Date),
                        SF2_L1_minvalue.i,as.character(SF2_L1_mindate.i),match(SF2_L1_mindate.i.1,field_days$Date)) 
} 
colnames(SF2_L1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF2_L1_check)

SF2_L1_NA <- is.na(dendro_SF2_raw.all$SF2_L1_dia_raw)
SF2_L1_NAdiff <- SF2_L1_NA[-1]
SF2_L1_noNA <- na.locf(dendro_SF2_raw.all$SF2_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L1_dia_raw,type="l", col=1)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L1_noNA,type="l", col=2)
SF2_L1_dia_diff_noNA <- diff(SF2_L1_noNA)
SF2_L1_dia_diff.c <-ifelse((SF2_L1_dia_diff_noNA > 0.5 & SF2_L1_NAdiff == FALSE) | (SF2_L1_dia_diff_noNA < -0.5 & SF2_L1_NAdiff == FALSE),NA,SF2_L1_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L1_dia_diff.c,type="l", col=1)
boxplot(SF2_L1_dia_diff.c,main="SF2_L1"); boxplot(SF2_L1_dia_diff.c~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L1")
summary(SF2_L1_dia_diff.c)
SF2_L1_dia_diff.0 <- na.fill(SF2_L1_dia_diff.c,0);summary(SF2_L1_dia_diff.0)
SF2_L1_dia.c <- diffinv(SF2_L1_dia_diff.0)
SF2_L1_dia.NA <- is.na(dendro_SF2_raw.all$SF2_L1_dia_raw)
SF2_L1_dia.c <- ifelse(SF2_L1_dia.NA == TRUE, NA, SF2_L1_dia.c); summary(SF2_L1_dia.c)
plot(dendro_SF2_raw.all$Date_Time.px,SF2_L1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF2_L1_dia.c)

# SF2_L4 ####
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L4_dia_raw,type="l", col=1)
SF2_L4_dia_diff <- diff(dendro_SF2_raw.all$SF2_L4_dia_raw)
plot(SF2_Date_diff.px,SF2_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L4_dia_diff,main="SF2_L4"); boxplot(SF2_L4_dia_diff~SF2_year_diff.px,main="SF2_L4")
boxplot(SF2_L4_dia_diff,ylim=c(-1,1),main="SF2_L4"); boxplot(SF2_L4_dia_diff~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L4")
# check if 10 highest max and min values occured during field day
SF2_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF2_L4_maxvalue.i <- sort(SF2_L4_dia_diff, TRUE)[i]
  SF2_L4_maxdate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L4_dia_diff == SF2_L4_maxvalue.i)]
  SF2_L4_maxdate.i.1 <- as.POSIXct(cut(SF2_L4_maxdate.i, "day"))
  SF2_L4_minvalue.i <- sort(SF2_L4_dia_diff, FALSE)[i]
  SF2_L4_mindate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L4_dia_diff == SF2_L4_minvalue.i)]
  SF2_L4_mindate.i.1 <- as.POSIXct(cut(SF2_L4_mindate.i, "day"))
  SF2_L4_check[i,] <- c(SF2_L4_maxvalue.i,as.character(SF2_L4_maxdate.i),match(SF2_L4_maxdate.i.1,field_days$Date),
                        SF2_L4_minvalue.i,as.character(SF2_L4_mindate.i),match(SF2_L4_mindate.i.1,field_days$Date)) 
} 
colnames(SF2_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF2_L4_check)
SF2_L4_NA <- is.na(dendro_SF2_raw.all$SF2_L4_dia_raw)
SF2_L4_NAdiff <- SF2_L4_NA[-1]
SF2_L4_noNA <- na.locf(dendro_SF2_raw.all$SF2_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L4_dia_raw,type="l", col=1)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L4_noNA,type="l", col=2)
SF2_L4_dia_diff_noNA <- diff(SF2_L4_noNA)
SF2_L4_dia_diff.c <-ifelse((SF2_L4_dia_diff_noNA > 0.5 & SF2_L4_NAdiff == FALSE) | (SF2_L4_dia_diff_noNA < -0.6 & SF2_L4_NAdiff == FALSE),NA,SF2_L4_dia_diff_noNA) 
plot(SF2_Date_diff.px,SF2_L4_dia_diff.c,type="l", col=1)
boxplot(SF2_L4_dia_diff.c,main="SF2_L4"); boxplot(SF2_L4_dia_diff.c~SF2_year_diff.px,main="SF2_L4")
summary(SF2_L4_dia_diff.c)
SF2_L4_dia_diff.0 <- na.fill(SF2_L4_dia_diff.c,0);summary(SF2_L4_dia_diff.0)
SF2_L4_dia.c <- diffinv(SF2_L4_dia_diff.0)
SF2_L4_dia.NA <- is.na(dendro_SF2_raw.all$SF2_L4_dia_raw)
SF2_L4_dia.c <- ifelse(SF2_L4_dia.NA == TRUE, NA, SF2_L4_dia.c)
plot(dendro_SF2_raw.all$Date_Time.px,SF2_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF2_L4_dia.c)

# SF2_L5 #### 
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L5_dia_raw,type="l", col=1)
SF2_L5_dia_diff <- diff(dendro_SF2_raw.all$SF2_L5_dia_raw)
plot(SF2_Date_diff.px,SF2_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L5_dia_diff,main="SF2_L5"); boxplot(SF2_L5_dia_diff~SF2_year_diff.px,main="SF2_L5"); 
boxplot(SF2_L5_dia_diff,ylim=c(-1,1),main="SF2_L5"); boxplot(SF2_L5_dia_diff~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L5")
# check if 10 highest max and min values occured during field day
SF2_L5_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF2_L5_maxvalue.i <- sort(SF2_L5_dia_diff, TRUE)[i]
  SF2_L5_maxdate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L5_dia_diff == SF2_L5_maxvalue.i)]
  SF2_L5_maxdate.i.1 <- as.POSIXct(cut(SF2_L5_maxdate.i, "day"))
  SF2_L5_minvalue.i <- sort(SF2_L5_dia_diff, FALSE)[i]
  SF2_L5_mindate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L5_dia_diff == SF2_L5_minvalue.i)]
  SF2_L5_mindate.i.1 <- as.POSIXct(cut(SF2_L5_mindate.i, "day"))
  SF2_L5_check[i,] <- c(SF2_L5_maxvalue.i,as.character(SF2_L5_maxdate.i),match(SF2_L5_maxdate.i.1,field_days$Date),
                        SF2_L5_minvalue.i,as.character(SF2_L5_mindate.i),match(SF2_L5_mindate.i.1,field_days$Date)) 
} 
colnames(SF2_L5_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF2_L5_check)
SF2_L5_NA <- is.na(dendro_SF2_raw.all$SF2_L5_dia_raw)
SF2_L5_NAdiff <- SF2_L5_NA[-1]
SF2_L5_noNA <- na.locf(dendro_SF2_raw.all$SF2_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L5_dia_raw,type="l", col=1)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L5_noNA,type="l", col=2)
SF2_L5_dia_diff_noNA <- diff(SF2_L5_noNA)
SF2_L5_dia_diff.c <-ifelse((SF2_L5_dia_diff_noNA > 0.5 & SF2_L5_NAdiff == FALSE) | (SF2_L5_dia_diff_noNA < -0.5 & SF2_L5_NAdiff == FALSE),NA,SF2_L5_dia_diff_noNA) 
SF2_L5_dia_diff.c[which(SF2_L5_dia_diff == sort(SF2_L5_dia_diff, FALSE)[5])] <- NA
plot(SF2_Date_diff.px,SF2_L5_dia_diff.c,type="l", col=1)
boxplot(SF2_L5_dia_diff.c,main="SF2_L5"); boxplot(SF2_L5_dia_diff.c~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L5")
summary(SF2_L5_dia_diff.c)
SF2_L5_dia_diff.0 <- na.fill(SF2_L5_dia_diff.c,0);summary(SF2_L5_dia_diff.0)
SF2_L5_dia.c <- diffinv(SF2_L5_dia_diff.0)
SF2_L5_dia.NA <- is.na(dendro_SF2_raw.all$SF2_L5_dia_raw)
SF2_L5_dia.c <- ifelse(SF2_L5_dia.NA == TRUE, NA, SF2_L5_dia.c); summary(SF2_L5_dia.c)
plot(dendro_SF2_raw.all$Date_Time.px,SF2_L5_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF2_L5_dia.c)

# SF2_L7 ####
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L7_dia_raw,type="l", col=1)
SF2_Date_diff <- dendro_SF2_raw.all$Date_Time[-1]
SF2_Date_diff.px <- as.POSIXct(SF2_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF2_L7_dia_diff <- diff(dendro_SF2_raw.all$SF2_L7_dia_raw)
plot(SF2_Date_diff.px,SF2_L7_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF2_L7_dia_diff,main="SF2_L7"); boxplot(SF2_L7_dia_diff~SF2_year_diff.px,main="SF2_L7"); 
boxplot(SF2_L7_dia_diff,ylim=c(-1,1),main="SF2_L7"); boxplot(SF2_L7_dia_diff~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L7")
# check if 10 highest max and min values occured during field day
SF2_L7_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF2_L7_maxvalue.i <- sort(SF2_L7_dia_diff, TRUE)[i]
  SF2_L7_maxdate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L7_dia_diff == SF2_L7_maxvalue.i)]
  SF2_L7_maxdate.i.1 <- as.POSIXct(cut(SF2_L7_maxdate.i, "day"))
  SF2_L7_minvalue.i <- sort(SF2_L7_dia_diff, FALSE)[i]
  SF2_L7_mindate.i <- dendro_SF2_raw.all$Date_Time.px[which(SF2_L7_dia_diff == SF2_L7_minvalue.i)]
  SF2_L7_mindate.i.1 <- as.POSIXct(cut(SF2_L7_mindate.i, "day"))
  SF2_L7_check[i,] <- c(SF2_L7_maxvalue.i,as.character(SF2_L7_maxdate.i),match(SF2_L7_maxdate.i.1,field_days$Date),
                        SF2_L7_minvalue.i,as.character(SF2_L7_mindate.i),match(SF2_L7_mindate.i.1,field_days$Date)) 
} 
colnames(SF2_L7_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF2_L7_check)

SF2_L7_NA <- is.na(dendro_SF2_raw.all$SF2_L7_dia_raw)
SF2_L7_NAdiff <- SF2_L7_NA[-1]
SF2_L7_noNA <- na.locf(dendro_SF2_raw.all$SF2_L7_dia_raw, na.rm = FALSE)
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L7_dia_raw,type="l", col=1)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L7_noNA,type="l", col=2)
SF2_L7_dia_diff_noNA <- diff(SF2_L7_noNA)
SF2_L7_dia_diff.c <-ifelse((SF2_L7_dia_diff_noNA > 0.5 & SF2_L7_NAdiff == FALSE) | (SF2_L7_dia_diff_noNA < -0.5 & SF2_L7_NAdiff == FALSE),NA,SF2_L7_dia_diff_noNA) 
SF2_L7_dia_diff.c[which(SF2_L7_dia_diff == sort(SF2_L7_dia_diff, FALSE)[5])] <- NA
plot(SF2_Date_diff.px,SF2_L7_dia_diff.c,type="l", col=1)
boxplot(SF2_L7_dia_diff.c,main="SF2_L7"); boxplot(SF2_L7_dia_diff.c~SF2_year_diff.px,ylim=c(-1,1),main="SF2_L7")
summary(SF2_L7_dia_diff.c)
SF2_L7_dia_diff.0 <- na.fill(SF2_L7_dia_diff.c,0);summary(SF2_L7_dia_diff.0)
SF2_L7_dia.c <- diffinv(SF2_L7_dia_diff.0)
SF2_L7_dia.NA <- is.na(dendro_SF2_raw.all$SF2_L7_dia_raw)
SF2_L7_dia.c <- ifelse(SF2_L7_dia.NA == TRUE, NA, SF2_L7_dia.c); summary(SF2_L7_dia.c)
plot(dendro_SF2_raw.all$Date_Time.px,SF2_L7_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF2_L7_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF2_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF2 (raw data)
plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L4_dia_raw,type="l", col=2)
lines(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L5_dia_raw,type="l", col=3)
lines(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L7_dia_raw,type="l", col=4)

# Plot all trees of SF2 (differences)
plot(SF2_Date_diff.px,SF2_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF2_Date_diff.px,SF2_L4_dia_diff,type="l", col=2)
lines(SF2_Date_diff.px,SF2_L5_dia_diff,type="l", col=3)
lines(SF2_Date_diff.px,SF2_L7_dia_diff,type="l", col=4)
legend("topleft",c("L1","L4","L5","L7"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF2 (cleaned data)
plot(dendro_SF2_raw.all$Date_Time.px,SF2_L1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L4_dia.c,type="l", col=2)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L5_dia.c,type="l", col=3)
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L7_dia.c,type="l", col=4)
legend("topleft",c("L1","L4","L5","L7"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF2_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L5_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF2_raw.all$Date_Time.px,dendro_SF2_raw.all$SF2_L7_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF2_raw.all$Date_Time.px,SF2_L7_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# SF3 - merge data ####
names(dendro_SF3_raw);names(dendro_SF3_raw2015)
tail(dendro_SF3_raw[1],2); head(dendro_SF3_raw2015[1],2); tail(dendro_SF3_raw2015[1],2)

dendro_SF3_raw.all <- rbind(dendro_SF3_raw[,c(10,2,4,6,8)],
                            dendro_SF3_raw2015[c((match(tail(dendro_SF3_raw$Date_Time.px,1),dendro_SF3_raw2015$Date_Time.px)+1):(dim(dendro_SF3_raw2015)[1])),c(10,2,4,6,8)])
summary(dendro_SF3_raw.all)

# SF3_L3
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L3_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw.all$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L3_dia_diff <- diff(dendro_SF3_raw.all$SF3_L3_dia_raw)
plot(SF3_Date_diff.px,SF3_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF3_year_diff.px <- as.POSIXct(cut(SF3_Date_diff.px, "year"))
boxplot(SF3_L3_dia_diff,main="SF3_L3"); boxplot(SF3_L3_dia_diff~SF3_year_diff.px,main="SF3_L3"); 
boxplot(SF3_L3_dia_diff,ylim=c(-1,1),main="SF3_L3"); boxplot(SF3_L3_dia_diff~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L3")
# check if 10 highest max and min values occured during field day
SF3_L3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF3_L3_maxvalue.i <- sort(SF3_L3_dia_diff, TRUE)[i]
  SF3_L3_maxdate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L3_dia_diff == SF3_L3_maxvalue.i)]
  SF3_L3_maxdate.i.1 <- as.POSIXct(cut(SF3_L3_maxdate.i, "day"))
  SF3_L3_minvalue.i <- sort(SF3_L3_dia_diff, FALSE)[i]
  SF3_L3_mindate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L3_dia_diff == SF3_L3_minvalue.i)]
  SF3_L3_mindate.i.1 <- as.POSIXct(cut(SF3_L3_mindate.i, "day"))
  SF3_L3_check[i,] <- c(SF3_L3_maxvalue.i,as.character(SF3_L3_maxdate.i),match(SF3_L3_maxdate.i.1,field_days$Date),
                        SF3_L3_minvalue.i,as.character(SF3_L3_mindate.i),match(SF3_L3_mindate.i.1,field_days$Date)) 
} 
colnames(SF3_L3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF3_L3_check)

SF3_L3_NA <- is.na(dendro_SF3_raw.all$SF3_L3_dia_raw)
SF3_L3_NAdiff <- SF3_L3_NA[-1]
SF3_L3_noNA <- na.locf(dendro_SF3_raw.all$SF3_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L3_dia_raw,type="l", col=1)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L3_noNA,type="l", col=2)
SF3_L3_dia_diff_noNA <- diff(SF3_L3_noNA)
SF3_L3_dia_diff.c <-ifelse((SF3_L3_dia_diff_noNA > 0.8 & SF3_L3_NAdiff == FALSE) | (SF3_L3_dia_diff_noNA < -0.5 & SF3_L3_NAdiff == FALSE),NA,SF3_L3_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
boxplot(SF3_L3_dia_diff.c,main="SF3_L3"); boxplot(SF3_L3_dia_diff.c~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L3")
summary(SF3_L3_dia_diff.c)
SF3_L3_dia_diff.0 <- na.fill(SF3_L3_dia_diff.c,0);summary(SF3_L3_dia_diff.0)
SF3_L3_dia.c <- diffinv(SF3_L3_dia_diff.0)
SF3_L3_dia.NA <- is.na(dendro_SF3_raw.all$SF3_L3_dia_raw)
SF3_L3_dia.c <- ifelse(SF3_L3_dia.NA == TRUE, NA, SF3_L3_dia.c); summary(SF3_L3_dia.c)
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
# delete 2013 stretch with dendrometer reaching its max extend 
july27.2013 <- as.POSIXct("2013-07-27 00:00", format="%Y-%m-%d %H:%M")
aug07.2013 <- as.POSIXct("2013-08-07 12:00", format="%Y-%m-%d %H:%M")
SF3_L3_dia.c[c(match(july27.2013,dendro_SF3_raw.all$Date_Time.px):  
                 match(aug07.2013,dendro_SF3_raw.all$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L3_dia.c,type="l", col=1)
SF3_L3_dia_diff.c[c(match(july27.2013,dendro_SF3_raw.all$Date_Time.px):
                      match(aug07.2013,dendro_SF3_raw.all$Date_Time.px))] <- NA    # due to sensor malfunction: sensor reached upper limit
plot(SF3_Date_diff.px,SF3_L3_dia_diff.c,type="l", col=1)
summary(SF3_L3_dia.c); summary(SF3_L3_dia_diff.c)
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[1])]
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[2])]
SF3_L3_dia_diff.c[which(SF3_L3_dia_diff == sort(SF3_L3_dia_diff, TRUE)[3])]
summary (SF3_L3_dia.c)

# SF3_L4
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L4_dia_raw,type="l", col=1)
SF3_L4_dia_diff <- diff(dendro_SF3_raw.all$SF3_L4_dia_raw)
plot(SF3_Date_diff.px,SF3_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L4_dia_diff,main="SF3_L4"); boxplot(SF3_L4_dia_diff~SF3_year_diff.px,main="SF3_L4")
boxplot(SF3_L4_dia_diff,ylim=c(-1,1),main="SF3_L4"); boxplot(SF3_L4_dia_diff~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L4")
# check if 10 highest max and min values occured during field day
SF3_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF3_L4_maxvalue.i <- sort(SF3_L4_dia_diff, TRUE)[i]
  SF3_L4_maxdate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L4_dia_diff == SF3_L4_maxvalue.i)]
  SF3_L4_maxdate.i.1 <- as.POSIXct(cut(SF3_L4_maxdate.i, "day"))
  SF3_L4_minvalue.i <- sort(SF3_L4_dia_diff, FALSE)[i]
  SF3_L4_mindate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L4_dia_diff == SF3_L4_minvalue.i)]
  SF3_L4_mindate.i.1 <- as.POSIXct(cut(SF3_L4_mindate.i, "day"))
  SF3_L4_check[i,] <- c(SF3_L4_maxvalue.i,as.character(SF3_L4_maxdate.i),match(SF3_L4_maxdate.i.1,field_days$Date),
                        SF3_L4_minvalue.i,as.character(SF3_L4_mindate.i),match(SF3_L4_mindate.i.1,field_days$Date)) 
} 
colnames(SF3_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF3_L4_check)
SF3_L4_NA <- is.na(dendro_SF3_raw.all$SF3_L4_dia_raw)
SF3_L4_NAdiff <- SF3_L4_NA[-1]
SF3_L4_noNA <- na.locf(dendro_SF3_raw.all$SF3_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L4_dia_raw,type="l", col=1)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L4_noNA,type="l", col=2)
SF3_L4_dia_diff_noNA <- diff(SF3_L4_noNA)
SF3_L4_dia_diff.c <-ifelse((SF3_L4_dia_diff_noNA > 1 & SF3_L4_NAdiff == FALSE) | (SF3_L4_dia_diff_noNA < -1 & SF3_L4_NAdiff == FALSE),NA,SF3_L4_dia_diff_noNA) 
plot(SF3_Date_diff.px,SF3_L4_dia_diff.c,type="l", col=1)
boxplot(SF3_L4_dia_diff.c,main="SF3_L4"); boxplot(SF3_L4_dia_diff.c~SF3_year_diff.px,main="SF3_L4")
summary(SF3_L4_dia_diff.c)
SF3_L4_dia_diff.0 <- na.fill(SF3_L4_dia_diff.c,0);summary(SF3_L4_dia_diff.0)
SF3_L4_dia.c <- diffinv(SF3_L4_dia_diff.0)
SF3_L4_dia.NA <- is.na(dendro_SF3_raw.all$SF3_L4_dia_raw)
SF3_L4_dia.c <- ifelse(SF3_L4_dia.NA == TRUE, NA, SF3_L4_dia.c)
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF3_L4_dia.c)

# SF3_L5
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L5_dia_raw,type="l", col=1)
SF3_L5_dia_diff <- diff(dendro_SF3_raw.all$SF3_L5_dia_raw)
plot(SF3_Date_diff.px,SF3_L5_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L5_dia_diff,main="SF3_L5"); boxplot(SF3_L5_dia_diff~SF3_year_diff.px,main="SF3_L5"); 
boxplot(SF3_L5_dia_diff,ylim=c(-1,1),main="SF3_L5"); boxplot(SF3_L5_dia_diff~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L5")
# check if 10 highest max and min values occured during field day
SF3_L5_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF3_L5_maxvalue.i <- sort(SF3_L5_dia_diff, TRUE)[i]
  SF3_L5_maxdate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L5_dia_diff == SF3_L5_maxvalue.i)]
  SF3_L5_maxdate.i.1 <- as.POSIXct(cut(SF3_L5_maxdate.i, "day"))
  SF3_L5_minvalue.i <- sort(SF3_L5_dia_diff, FALSE)[i]
  SF3_L5_mindate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L5_dia_diff == SF3_L5_minvalue.i)]
  SF3_L5_mindate.i.1 <- as.POSIXct(cut(SF3_L5_mindate.i, "day"))
  SF3_L5_check[i,] <- c(SF3_L5_maxvalue.i,as.character(SF3_L5_maxdate.i),match(SF3_L5_maxdate.i.1,field_days$Date),
                        SF3_L5_minvalue.i,as.character(SF3_L5_mindate.i),match(SF3_L5_mindate.i.1,field_days$Date)) 
} 
colnames(SF3_L5_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF3_L5_check)
SF3_L5_NA <- is.na(dendro_SF3_raw.all$SF3_L5_dia_raw)
SF3_L5_NAdiff <- SF3_L5_NA[-1]
SF3_L5_noNA <- na.locf(dendro_SF3_raw.all$SF3_L5_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L5_dia_raw,type="l", col=1)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L5_noNA,type="l", col=2)
SF3_L5_dia_diff_noNA <- diff(SF3_L5_noNA)
SF3_L5_dia_diff.c <-ifelse((SF3_L5_dia_diff_noNA > 1 & SF3_L5_NAdiff == FALSE) | (SF3_L5_dia_diff_noNA < -1 & SF3_L5_NAdiff == FALSE),NA,SF3_L5_dia_diff_noNA) 
SF3_L5_dia_diff.c[which(SF3_L5_dia_diff == sort(SF3_L5_dia_diff, FALSE)[5])] <- NA
plot(SF3_Date_diff.px,SF3_L5_dia_diff.c,type="l", col=1)
boxplot(SF3_L5_dia_diff.c,main="SF3_L5"); boxplot(SF3_L5_dia_diff.c~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L5")
summary(SF3_L5_dia_diff.c)
SF3_L5_dia_diff.0 <- na.fill(SF3_L5_dia_diff.c,0);summary(SF3_L5_dia_diff.0)
SF3_L5_dia.c <- diffinv(SF3_L5_dia_diff.0)
SF3_L5_dia.NA <- is.na(dendro_SF3_raw.all$SF3_L5_dia_raw)
SF3_L5_dia.c <- ifelse(SF3_L5_dia.NA == TRUE, NA, SF3_L5_dia.c); summary(SF3_L5_dia.c)
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L5_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF3_L5_dia.c)

# SF3_L6
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L6_dia_raw,type="l", col=1)
SF3_Date_diff <- dendro_SF3_raw.all$Date_Time[-1]
SF3_Date_diff.px <- as.POSIXct(SF3_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF3_L6_dia_diff <- diff(dendro_SF3_raw.all$SF3_L6_dia_raw)
plot(SF3_Date_diff.px,SF3_L6_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF3_L6_dia_diff,main="SF3_L6"); boxplot(SF3_L6_dia_diff~SF3_year_diff.px,main="SF3_L6"); 
boxplot(SF3_L6_dia_diff,ylim=c(-1,1),main="SF3_L6"); boxplot(SF3_L6_dia_diff~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L6")
# check if 10 highest max and min values occured during field day
SF3_L6_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF3_L6_maxvalue.i <- sort(SF3_L6_dia_diff, TRUE)[i]
  SF3_L6_maxdate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L6_dia_diff == SF3_L6_maxvalue.i)]
  SF3_L6_maxdate.i.1 <- as.POSIXct(cut(SF3_L6_maxdate.i, "day"))
  SF3_L6_minvalue.i <- sort(SF3_L6_dia_diff, FALSE)[i]
  SF3_L6_mindate.i <- dendro_SF3_raw.all$Date_Time.px[which(SF3_L6_dia_diff == SF3_L6_minvalue.i)]
  SF3_L6_mindate.i.1 <- as.POSIXct(cut(SF3_L6_mindate.i, "day"))
  SF3_L6_check[i,] <- c(SF3_L6_maxvalue.i,as.character(SF3_L6_maxdate.i),match(SF3_L6_maxdate.i.1,field_days$Date),
                        SF3_L6_minvalue.i,as.character(SF3_L6_mindate.i),match(SF3_L6_mindate.i.1,field_days$Date)) 
} 
colnames(SF3_L6_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF3_L6_check)

SF3_L6_NA <- is.na(dendro_SF3_raw.all$SF3_L6_dia_raw)
SF3_L6_NAdiff <- SF3_L6_NA[-1]
SF3_L6_noNA <- na.locf(dendro_SF3_raw.all$SF3_L6_dia_raw, na.rm = FALSE)
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L6_dia_raw,type="l", col=1)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L6_noNA,type="l", col=2)
SF3_L6_dia_diff_noNA <- diff(SF3_L6_noNA)
SF3_L6_dia_diff.c <-ifelse((SF3_L6_dia_diff_noNA > 1 & SF3_L6_NAdiff == FALSE) | (SF3_L6_dia_diff_noNA < -0.5 & SF3_L6_NAdiff == FALSE),NA,SF3_L6_dia_diff_noNA) 
SF3_L6_dia_diff.c[which(SF3_L6_dia_diff == sort(SF3_L6_dia_diff, FALSE)[5])] <- NA
plot(SF3_Date_diff.px,SF3_L6_dia_diff.c,type="l", col=1)
boxplot(SF3_L6_dia_diff.c,main="SF3_L6"); boxplot(SF3_L6_dia_diff.c~SF3_year_diff.px,ylim=c(-1,1),main="SF3_L6")
summary(SF3_L6_dia_diff.c)
SF3_L6_dia_diff.0 <- na.fill(SF3_L6_dia_diff.c,0);summary(SF3_L6_dia_diff.0)
SF3_L6_dia.c <- diffinv(SF3_L6_dia_diff.0)
SF3_L6_dia.NA <- is.na(dendro_SF3_raw.all$SF3_L6_dia_raw)
SF3_L6_dia.c <- ifelse(SF3_L6_dia.NA == TRUE, NA, SF3_L6_dia.c); summary(SF3_L6_dia.c)
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L6_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF3_L6_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF3_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

# Plot all trees of SF3 (raw data)
plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L3_dia_raw,type="l", col=1,ylim=c(0,65))
lines(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L4_dia_raw,type="l", col=2)
lines(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L5_dia_raw,type="l", col=3)
lines(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L6_dia_raw,type="l", col=4)

# Plot all trees of SF3 (differences)
plot(SF3_Date_diff.px,SF3_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF3_Date_diff.px,SF3_L4_dia_diff,type="l", col=2)
lines(SF3_Date_diff.px,SF3_L5_dia_diff,type="l", col=3)
lines(SF3_Date_diff.px,SF3_L6_dia_diff,type="l", col=4)
legend("topleft",c("L3","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF3 (cleaned data)
plot(dendro_SF3_raw.all$Date_Time.px,SF3_L3_dia.c,type="l", col=1,ylim=c(-15,80))
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L4_dia.c,type="l", col=2)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L5_dia.c,type="l", col=3)
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L6_dia.c,type="l", col=4)
legend("topleft",c("L3","L4","L5","L6"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF3_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)

plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L3_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L4_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L5_dia_raw,type="l", col=1,ylim=c(-15,65))
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L5_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF3_raw.all$Date_Time.px,dendro_SF3_raw.all$SF3_L6_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF3_raw.all$Date_Time.px,SF3_L6_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# SF4 - merge data ####
names(dendro_SF4_raw);names(dendro_SF4_raw2015)
tail(dendro_SF4_raw[1],2); head(dendro_SF4_raw2015[1],2); tail(dendro_SF4_raw2015[1],2)

dendro_SF4_raw.all <- rbind(dendro_SF4_raw,dendro_SF4_raw2015)
summary(dendro_SF4_raw.all)
summary(dendro_SF4_raw)
summary(dendro_SF4_raw2015)

# SF4_L1
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw.all$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L1_dia_diff <- diff(dendro_SF4_raw.all$SF4_L1_dia_raw)
plot(SF4_Date_diff.px,SF4_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_year_diff.px <- as.POSIXct(cut(SF4_Date_diff.px, "year"))
boxplot(SF4_L1_dia_diff,main="SF4_L1"); boxplot(SF4_L1_dia_diff~SF4_year_diff.px,main="SF4_L1"); 
boxplot(SF4_L1_dia_diff,ylim=c(-1,1),main="SF4_L1"); boxplot(SF4_L1_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L1")
# check if 10 highest max and min values occured during field day
SF4_L1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_L1_maxvalue.i <- sort(SF4_L1_dia_diff, TRUE)[i]
  SF4_L1_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L1_dia_diff == SF4_L1_maxvalue.i)]
  SF4_L1_maxdate.i.1 <- as.POSIXct(cut(SF4_L1_maxdate.i, "day"))
  SF4_L1_minvalue.i <- sort(SF4_L1_dia_diff, FALSE)[i]
  SF4_L1_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L1_dia_diff == SF4_L1_minvalue.i)]
  SF4_L1_mindate.i.1 <- as.POSIXct(cut(SF4_L1_mindate.i, "day"))
  SF4_L1_check[i,] <- c(SF4_L1_maxvalue.i,as.character(SF4_L1_maxdate.i),match(SF4_L1_maxdate.i.1,field_days$Date),
                        SF4_L1_minvalue.i,as.character(SF4_L1_mindate.i),match(SF4_L1_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_L1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_L1_check)

SF4_L1_NA <- is.na(dendro_SF4_raw.all$SF4_L1_dia_raw)
SF4_L1_NAdiff <- SF4_L1_NA[-1]
SF4_L1_noNA <- na.locf(dendro_SF4_raw.all$SF4_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L1_noNA,type="l", col=2)
SF4_L1_dia_diff_noNA <- diff(SF4_L1_noNA)
SF4_L1_dia_diff.c <-ifelse((SF4_L1_dia_diff_noNA > 0.5 & SF4_L1_NAdiff == FALSE) | (SF4_L1_dia_diff_noNA < -0.5 & SF4_L1_NAdiff == FALSE),NA,SF4_L1_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L1_dia_diff.c,type="l", col=1)
boxplot(SF4_L1_dia_diff.c,main="SF4_L1"); boxplot(SF4_L1_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L1")
summary(SF4_L1_dia_diff.c)
SF4_L1_dia_diff.0 <- na.fill(SF4_L1_dia_diff.c,0);summary(SF4_L1_dia_diff.0)
SF4_L1_dia.c <- diffinv(SF4_L1_dia_diff.0)
SF4_L1_dia.NA <- is.na(dendro_SF4_raw.all$SF4_L1_dia_raw)
SF4_L1_dia.c <- ifelse(SF4_L1_dia.NA == TRUE, NA, SF4_L1_dia.c); summary(SF4_L1_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_L1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_L1_dia.c)

# SF4_L2
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L2_dia_raw,type="l", col=1)
SF4_L2_dia_diff <- diff(dendro_SF4_raw.all$SF4_L2_dia_raw)
plot(SF4_Date_diff.px,SF4_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L2_dia_diff,main="SF4_L2"); boxplot(SF4_L2_dia_diff~SF4_year_diff.px,main="SF4_L2")
boxplot(SF4_L2_dia_diff,ylim=c(-1,1),main="SF4_L2"); boxplot(SF4_L2_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L2")
# check if 10 highest max and min values occured during field day
SF4_L2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_L2_maxvalue.i <- sort(SF4_L2_dia_diff, TRUE)[i]
  SF4_L2_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L2_dia_diff == SF4_L2_maxvalue.i)]
  SF4_L2_maxdate.i.1 <- as.POSIXct(cut(SF4_L2_maxdate.i, "day"))
  SF4_L2_minvalue.i <- sort(SF4_L2_dia_diff, FALSE)[i]
  SF4_L2_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L2_dia_diff == SF4_L2_minvalue.i)]
  SF4_L2_mindate.i.1 <- as.POSIXct(cut(SF4_L2_mindate.i, "day"))
  SF4_L2_check[i,] <- c(SF4_L2_maxvalue.i,as.character(SF4_L2_maxdate.i),match(SF4_L2_maxdate.i.1,field_days$Date),
                        SF4_L2_minvalue.i,as.character(SF4_L2_mindate.i),match(SF4_L2_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_L2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_L2_check)
SF4_L2_NA <- is.na(dendro_SF4_raw.all$SF4_L2_dia_raw)
SF4_L2_NAdiff <- SF4_L2_NA[-1]
SF4_L2_noNA <- na.locf(dendro_SF4_raw.all$SF4_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L2_noNA,type="l", col=2)
SF4_L2_dia_diff_noNA <- diff(SF4_L2_noNA)
SF4_L2_dia_diff.c <-ifelse((SF4_L2_dia_diff_noNA > 1 & SF4_L2_NAdiff == FALSE) | (SF4_L2_dia_diff_noNA < -1 & SF4_L2_NAdiff == FALSE),NA,SF4_L2_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_L2_dia_diff.c,type="l", col=1)
boxplot(SF4_L2_dia_diff.c,main="SF4_L2"); boxplot(SF4_L2_dia_diff.c~SF4_year_diff.px,main="SF4_L2")
summary(SF4_L2_dia_diff.c)
SF4_L2_dia_diff.0 <- na.fill(SF4_L2_dia_diff.c,0);summary(SF4_L2_dia_diff.0)
SF4_L2_dia.c <- diffinv(SF4_L2_dia_diff.0)
SF4_L2_dia.NA <- is.na(dendro_SF4_raw.all$SF4_L2_dia_raw)
SF4_L2_dia.c <- ifelse(SF4_L2_dia.NA == TRUE, NA, SF4_L2_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_L2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_L2_dia.c)

# SF4_L3
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L3_dia_raw,type="l", col=1)
SF4_L3_dia_diff <- diff(dendro_SF4_raw.all$SF4_L3_dia_raw)
plot(SF4_Date_diff.px,SF4_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L3_dia_diff,main="SF4_L3"); boxplot(SF4_L3_dia_diff~SF4_year_diff.px,main="SF4_L3"); 
boxplot(SF4_L3_dia_diff,ylim=c(-1,1),main="SF4_L3"); boxplot(SF4_L3_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L3")
# check if 10 highest max and min values occured during field day
SF4_L3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_L3_maxvalue.i <- sort(SF4_L3_dia_diff, TRUE)[i]
  SF4_L3_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L3_dia_diff == SF4_L3_maxvalue.i)]
  SF4_L3_maxdate.i.1 <- as.POSIXct(cut(SF4_L3_maxdate.i, "day"))
  SF4_L3_minvalue.i <- sort(SF4_L3_dia_diff, FALSE)[i]
  SF4_L3_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L3_dia_diff == SF4_L3_minvalue.i)]
  SF4_L3_mindate.i.1 <- as.POSIXct(cut(SF4_L3_mindate.i, "day"))
  SF4_L3_check[i,] <- c(SF4_L3_maxvalue.i,as.character(SF4_L3_maxdate.i),match(SF4_L3_maxdate.i.1,field_days$Date),
                        SF4_L3_minvalue.i,as.character(SF4_L3_mindate.i),match(SF4_L3_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_L3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_L3_check)
SF4_L3_NA <- is.na(dendro_SF4_raw.all$SF4_L3_dia_raw)
SF4_L3_NAdiff <- SF4_L3_NA[-1]
SF4_L3_noNA <- na.locf(dendro_SF4_raw.all$SF4_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L3_noNA,type="l", col=2)
SF4_L3_dia_diff_noNA <- diff(SF4_L3_noNA)
SF4_L3_dia_diff.c <-ifelse((SF4_L3_dia_diff_noNA > 1 & SF4_L3_NAdiff == FALSE) | (SF4_L3_dia_diff_noNA < -1 & SF4_L3_NAdiff == FALSE),NA,SF4_L3_dia_diff_noNA) 
SF4_L3_dia_diff.c[which(SF4_L3_dia_diff == sort(SF4_L3_dia_diff, FALSE)[5])] <- NA
plot(SF4_Date_diff.px,SF4_L3_dia_diff.c,type="l", col=1)
boxplot(SF4_L3_dia_diff.c,main="SF4_L3"); boxplot(SF4_L3_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L3")
summary(SF4_L3_dia_diff.c)
SF4_L3_dia_diff.0 <- na.fill(SF4_L3_dia_diff.c,0);summary(SF4_L3_dia_diff.0)
SF4_L3_dia.c <- diffinv(SF4_L3_dia_diff.0)
SF4_L3_dia.NA <- is.na(dendro_SF4_raw.all$SF4_L3_dia_raw)
SF4_L3_dia.c <- ifelse(SF4_L3_dia.NA == TRUE, NA, SF4_L3_dia.c); summary(SF4_L3_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_L3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_L3_dia.c)

# SF4_L4
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw.all$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_L4_dia_diff <- diff(dendro_SF4_raw.all$SF4_L4_dia_raw)
plot(SF4_Date_diff.px,SF4_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_L4_dia_diff,main="SF4_L4"); boxplot(SF4_L4_dia_diff~SF4_year_diff.px,main="SF4_L4"); 
boxplot(SF4_L4_dia_diff,ylim=c(-1,1),main="SF4_L4"); boxplot(SF4_L4_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L4")
# check if 10 highest max and min values occured during field day
SF4_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_L4_maxvalue.i <- sort(SF4_L4_dia_diff, TRUE)[i]
  SF4_L4_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L4_dia_diff == SF4_L4_maxvalue.i)]
  SF4_L4_maxdate.i.1 <- as.POSIXct(cut(SF4_L4_maxdate.i, "day"))
  SF4_L4_minvalue.i <- sort(SF4_L4_dia_diff, FALSE)[i]
  SF4_L4_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_L4_dia_diff == SF4_L4_minvalue.i)]
  SF4_L4_mindate.i.1 <- as.POSIXct(cut(SF4_L4_mindate.i, "day"))
  SF4_L4_check[i,] <- c(SF4_L4_maxvalue.i,as.character(SF4_L4_maxdate.i),match(SF4_L4_maxdate.i.1,field_days$Date),
                        SF4_L4_minvalue.i,as.character(SF4_L4_mindate.i),match(SF4_L4_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_L4_check)

SF4_L4_NA <- is.na(dendro_SF4_raw.all$SF4_L4_dia_raw)
SF4_L4_NAdiff <- SF4_L4_NA[-1]
SF4_L4_noNA <- na.locf(dendro_SF4_raw.all$SF4_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L4_noNA,type="l", col=2)
SF4_L4_dia_diff_noNA <- diff(SF4_L4_noNA)
SF4_L4_dia_diff.c <-ifelse((SF4_L4_dia_diff_noNA > 1 & SF4_L4_NAdiff == FALSE) | (SF4_L4_dia_diff_noNA < -1 & SF4_L4_NAdiff == FALSE),NA,SF4_L4_dia_diff_noNA) 
SF4_L4_dia_diff.c[which(SF4_L4_dia_diff == sort(SF4_L4_dia_diff, FALSE)[5])] <- NA
plot(SF4_Date_diff.px,SF4_L4_dia_diff.c,type="l", col=1)
boxplot(SF4_L4_dia_diff.c,main="SF4_L4"); boxplot(SF4_L4_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_L4")
summary(SF4_L4_dia_diff.c)
SF4_L4_dia_diff.0 <- na.fill(SF4_L4_dia_diff.c,0);summary(SF4_L4_dia_diff.0)
SF4_L4_dia.c <- diffinv(SF4_L4_dia_diff.0)
SF4_L4_dia.NA <- is.na(dendro_SF4_raw.all$SF4_L4_dia_raw)
SF4_L4_dia.c <- ifelse(SF4_L4_dia.NA == TRUE, NA, SF4_L4_dia.c); summary(SF4_L4_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_L4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF4L_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of SF4 (raw data)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L2_dia_raw,type="l", col=2)
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L3_dia_raw,type="l", col=3)
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L4_dia_raw,type="l", col=4)

# Plot all trees of SF4 (differences)
plot(SF4_Date_diff.px,SF4_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF4_Date_diff.px,SF4_L2_dia_diff,type="l", col=2)
lines(SF4_Date_diff.px,SF4_L3_dia_diff,type="l", col=3)
lines(SF4_Date_diff.px,SF4_L4_dia_diff,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF4 (cleaned data)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_L1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L2_dia.c,type="l", col=2)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L3_dia.c,type="l", col=3)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L4_dia.c,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF4L_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# SF4_Z1 ####
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z1_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw.all$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z1_dia_diff <- diff(dendro_SF4_raw.all$SF4_Z1_dia_raw)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF4_year_diff.px <- as.POSIXct(cut(SF4_Date_diff.px, "year"))
boxplot(SF4_Z1_dia_diff,main="SF4_Z1"); boxplot(SF4_Z1_dia_diff~SF4_year_diff.px,main="SF4_Z1"); 
boxplot(SF4_Z1_dia_diff,ylim=c(-1,1),main="SF4_Z1"); boxplot(SF4_Z1_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z1")
# check if 10 highest max and min values occured during field day
SF4_Z1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_Z1_maxvalue.i <- sort(SF4_Z1_dia_diff, TRUE)[i]
  SF4_Z1_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z1_dia_diff == SF4_Z1_maxvalue.i)]
  SF4_Z1_maxdate.i.1 <- as.POSIXct(cut(SF4_Z1_maxdate.i, "day"))
  SF4_Z1_minvalue.i <- sort(SF4_Z1_dia_diff, FALSE)[i]
  SF4_Z1_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z1_dia_diff == SF4_Z1_minvalue.i)]
  SF4_Z1_mindate.i.1 <- as.POSIXct(cut(SF4_Z1_mindate.i, "day"))
  SF4_Z1_check[i,] <- c(SF4_Z1_maxvalue.i,as.character(SF4_Z1_maxdate.i),match(SF4_Z1_maxdate.i.1,field_days$Date),
                        SF4_Z1_minvalue.i,as.character(SF4_Z1_mindate.i),match(SF4_Z1_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_Z1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_Z1_check)

SF4_Z1_NA <- is.na(dendro_SF4_raw.all$SF4_Z1_dia_raw)
SF4_Z1_NAdiff <- SF4_Z1_NA[-1]
SF4_Z1_noNA <- na.locf(dendro_SF4_raw.all$SF4_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z1_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z1_noNA,type="l", col=2)
SF4_Z1_dia_diff_noNA <- diff(SF4_Z1_noNA)
SF4_Z1_dia_diff.c <-ifelse((SF4_Z1_dia_diff_noNA > 0.5 & SF4_Z1_NAdiff == FALSE) | (SF4_Z1_dia_diff_noNA < -0.5 & SF4_Z1_NAdiff == FALSE),NA,SF4_Z1_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_Z1_dia_diff.c,type="l", col=1)
boxplot(SF4_Z1_dia_diff.c,main="SF4_Z1"); boxplot(SF4_Z1_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z1")
summary(SF4_Z1_dia_diff.c)
SF4_Z1_dia_diff.0 <- na.fill(SF4_Z1_dia_diff.c,0);summary(SF4_Z1_dia_diff.0)
SF4_Z1_dia.c <- diffinv(SF4_Z1_dia_diff.0)
SF4_Z1_dia.NA <- is.na(dendro_SF4_raw.all$SF4_Z1_dia_raw)
SF4_Z1_dia.c <- ifelse(SF4_Z1_dia.NA == TRUE, NA, SF4_Z1_dia.c); summary(SF4_Z1_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_Z1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_Z1_dia.c)

# SF4_Z2
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z2_dia_raw,type="l", col=1)
SF4_Z2_dia_diff <- diff(dendro_SF4_raw.all$SF4_Z2_dia_raw)
plot(SF4_Date_diff.px,SF4_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z2_dia_diff,main="SF4_Z2"); boxplot(SF4_Z2_dia_diff~SF4_year_diff.px,main="SF4_Z2")
boxplot(SF4_Z2_dia_diff,ylim=c(-1,1),main="SF4_Z2"); boxplot(SF4_Z2_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z2")
# check if 10 highest max and min values occured during field day
SF4_Z2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_Z2_maxvalue.i <- sort(SF4_Z2_dia_diff, TRUE)[i]
  SF4_Z2_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z2_dia_diff == SF4_Z2_maxvalue.i)]
  SF4_Z2_maxdate.i.1 <- as.POSIXct(cut(SF4_Z2_maxdate.i, "day"))
  SF4_Z2_minvalue.i <- sort(SF4_Z2_dia_diff, FALSE)[i]
  SF4_Z2_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z2_dia_diff == SF4_Z2_minvalue.i)]
  SF4_Z2_mindate.i.1 <- as.POSIXct(cut(SF4_Z2_mindate.i, "day"))
  SF4_Z2_check[i,] <- c(SF4_Z2_maxvalue.i,as.character(SF4_Z2_maxdate.i),match(SF4_Z2_maxdate.i.1,field_days$Date),
                        SF4_Z2_minvalue.i,as.character(SF4_Z2_mindate.i),match(SF4_Z2_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_Z2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_Z2_check)
SF4_Z2_NA <- is.na(dendro_SF4_raw.all$SF4_Z2_dia_raw)
SF4_Z2_NAdiff <- SF4_Z2_NA[-1]
SF4_Z2_noNA <- na.locf(dendro_SF4_raw.all$SF4_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z2_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z2_noNA,type="l", col=2)
SF4_Z2_dia_diff_noNA <- diff(SF4_Z2_noNA)
SF4_Z2_dia_diff.c <-ifelse((SF4_Z2_dia_diff_noNA > 1 & SF4_Z2_NAdiff == FALSE) | (SF4_Z2_dia_diff_noNA < -1 & SF4_Z2_NAdiff == FALSE),NA,SF4_Z2_dia_diff_noNA) 
plot(SF4_Date_diff.px,SF4_Z2_dia_diff.c,type="l", col=1)
boxplot(SF4_Z2_dia_diff.c,main="SF4_Z2"); boxplot(SF4_Z2_dia_diff.c~SF4_year_diff.px,main="SF4_Z2")
summary(SF4_Z2_dia_diff.c)
SF4_Z2_dia_diff.0 <- na.fill(SF4_Z2_dia_diff.c,0);summary(SF4_Z2_dia_diff.0)
SF4_Z2_dia.c <- diffinv(SF4_Z2_dia_diff.0)
SF4_Z2_dia.NA <- is.na(dendro_SF4_raw.all$SF4_Z2_dia_raw)
SF4_Z2_dia.c <- ifelse(SF4_Z2_dia.NA == TRUE, NA, SF4_Z2_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_Z2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_Z2_dia.c)

# SF4_Z3
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z3_dia_raw,type="l", col=1)
SF4_Z3_dia_diff <- diff(dendro_SF4_raw.all$SF4_Z3_dia_raw)
plot(SF4_Date_diff.px,SF4_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z3_dia_diff,main="SF4_Z3"); boxplot(SF4_Z3_dia_diff~SF4_year_diff.px,main="SF4_Z3"); 
boxplot(SF4_Z3_dia_diff,ylim=c(-1,1),main="SF4_Z3"); boxplot(SF4_Z3_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z3")
# check if 10 highest max and min values occured during field day
SF4_Z3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_Z3_maxvalue.i <- sort(SF4_Z3_dia_diff, TRUE)[i]
  SF4_Z3_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z3_dia_diff == SF4_Z3_maxvalue.i)]
  SF4_Z3_maxdate.i.1 <- as.POSIXct(cut(SF4_Z3_maxdate.i, "day"))
  SF4_Z3_minvalue.i <- sort(SF4_Z3_dia_diff, FALSE)[i]
  SF4_Z3_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z3_dia_diff == SF4_Z3_minvalue.i)]
  SF4_Z3_mindate.i.1 <- as.POSIXct(cut(SF4_Z3_mindate.i, "day"))
  SF4_Z3_check[i,] <- c(SF4_Z3_maxvalue.i,as.character(SF4_Z3_maxdate.i),match(SF4_Z3_maxdate.i.1,field_days$Date),
                        SF4_Z3_minvalue.i,as.character(SF4_Z3_mindate.i),match(SF4_Z3_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_Z3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_Z3_check)
SF4_Z3_NA <- is.na(dendro_SF4_raw.all$SF4_Z3_dia_raw)
SF4_Z3_NAdiff <- SF4_Z3_NA[-1]
SF4_Z3_noNA <- na.locf(dendro_SF4_raw.all$SF4_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z3_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z3_noNA,type="l", col=2)
SF4_Z3_dia_diff_noNA <- diff(SF4_Z3_noNA)
SF4_Z3_dia_diff.c <-ifelse((SF4_Z3_dia_diff_noNA > 1 & SF4_Z3_NAdiff == FALSE) | (SF4_Z3_dia_diff_noNA < -1 & SF4_Z3_NAdiff == FALSE),NA,SF4_Z3_dia_diff_noNA) 
SF4_Z3_dia_diff.c[which(SF4_Z3_dia_diff == sort(SF4_Z3_dia_diff, FALSE)[5])] <- NA
plot(SF4_Date_diff.px,SF4_Z3_dia_diff.c,type="l", col=1)
boxplot(SF4_Z3_dia_diff.c,main="SF4_Z3"); boxplot(SF4_Z3_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z3")
summary(SF4_Z3_dia_diff.c)
SF4_Z3_dia_diff.0 <- na.fill(SF4_Z3_dia_diff.c,0);summary(SF4_Z3_dia_diff.0)
SF4_Z3_dia.c <- diffinv(SF4_Z3_dia_diff.0)
SF4_Z3_dia.NA <- is.na(dendro_SF4_raw.all$SF4_Z3_dia_raw)
SF4_Z3_dia.c <- ifelse(SF4_Z3_dia.NA == TRUE, NA, SF4_Z3_dia.c); summary(SF4_Z3_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_Z3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_Z3_dia.c)

# SF4_Z4
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z4_dia_raw,type="l", col=1)
SF4_Date_diff <- dendro_SF4_raw.all$Date_Time[-1]
SF4_Date_diff.px <- as.POSIXct(SF4_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF4_Z4_dia_diff <- diff(dendro_SF4_raw.all$SF4_Z4_dia_raw)
plot(SF4_Date_diff.px,SF4_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF4_Z4_dia_diff,main="SF4_Z4"); boxplot(SF4_Z4_dia_diff~SF4_year_diff.px,main="SF4_Z4"); 
boxplot(SF4_Z4_dia_diff,ylim=c(-1,1),main="SF4_Z4"); boxplot(SF4_Z4_dia_diff~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z4")
# check if 10 highest max and min values occured during field day
SF4_Z4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF4_Z4_maxvalue.i <- sort(SF4_Z4_dia_diff, TRUE)[i]
  SF4_Z4_maxdate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z4_dia_diff == SF4_Z4_maxvalue.i)]
  SF4_Z4_maxdate.i.1 <- as.POSIXct(cut(SF4_Z4_maxdate.i, "day"))
  SF4_Z4_minvalue.i <- sort(SF4_Z4_dia_diff, FALSE)[i]
  SF4_Z4_mindate.i <- dendro_SF4_raw.all$Date_Time.px[which(SF4_Z4_dia_diff == SF4_Z4_minvalue.i)]
  SF4_Z4_mindate.i.1 <- as.POSIXct(cut(SF4_Z4_mindate.i, "day"))
  SF4_Z4_check[i,] <- c(SF4_Z4_maxvalue.i,as.character(SF4_Z4_maxdate.i),match(SF4_Z4_maxdate.i.1,field_days$Date),
                        SF4_Z4_minvalue.i,as.character(SF4_Z4_mindate.i),match(SF4_Z4_mindate.i.1,field_days$Date)) 
} 
colnames(SF4_Z4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF4_Z4_check)

SF4_Z4_NA <- is.na(dendro_SF4_raw.all$SF4_Z4_dia_raw)
SF4_Z4_NAdiff <- SF4_Z4_NA[-1]
SF4_Z4_noNA <- na.locf(dendro_SF4_raw.all$SF4_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z4_dia_raw,type="l", col=1)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z4_noNA,type="l", col=2)
SF4_Z4_dia_diff_noNA <- diff(SF4_Z4_noNA)
SF4_Z4_dia_diff.c <-ifelse((SF4_Z4_dia_diff_noNA > 1 & SF4_Z4_NAdiff == FALSE) | (SF4_Z4_dia_diff_noNA < -1 & SF4_Z4_NAdiff == FALSE),NA,SF4_Z4_dia_diff_noNA) 
SF4_Z4_dia_diff.c[which(SF4_Z4_dia_diff == sort(SF4_Z4_dia_diff, FALSE)[5])] <- NA
plot(SF4_Date_diff.px,SF4_Z4_dia_diff.c,type="l", col=1)
boxplot(SF4_Z4_dia_diff.c,main="SF4_Z4"); boxplot(SF4_Z4_dia_diff.c~SF4_year_diff.px,ylim=c(-1,1),main="SF4_Z4")
summary(SF4_Z4_dia_diff.c)
SF4_Z4_dia_diff.0 <- na.fill(SF4_Z4_dia_diff.c,0);summary(SF4_Z4_dia_diff.0)
SF4_Z4_dia.c <- diffinv(SF4_Z4_dia_diff.0)
SF4_Z4_dia.NA <- is.na(dendro_SF4_raw.all$SF4_Z4_dia_raw)
SF4_Z4_dia.c <- ifelse(SF4_Z4_dia.NA == TRUE, NA, SF4_Z4_dia.c); summary(SF4_Z4_dia.c)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_Z4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF4_Z4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF4Z_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of SF4 (raw data)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z2_dia_raw,type="l", col=2)
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z3_dia_raw,type="l", col=3)
lines(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z4_dia_raw,type="l", col=4)

# Plot all trees of SF4 (differences)
plot(SF4_Date_diff.px,SF4_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF4_Date_diff.px,SF4_Z2_dia_diff,type="l", col=2)
lines(SF4_Date_diff.px,SF4_Z3_dia_diff,type="l", col=3)
lines(SF4_Date_diff.px,SF4_Z4_dia_diff,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF4 (cleaned data)
plot(dendro_SF4_raw.all$Date_Time.px,SF4_Z1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z2_dia.c,type="l", col=2)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z3_dia.c,type="l", col=3)
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z4_dia.c,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF4Z_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF4_raw.all$Date_Time.px,dendro_SF4_raw.all$SF4_Z4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF4_raw.all$Date_Time.px,SF4_Z4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# SF5 - merge data ####
names(dendro_SF5_raw);names(dendro_SF5_raw2015)
tail(dendro_SF5_raw[1],2); head(dendro_SF5_raw2015[1],2); tail(dendro_SF5_raw2015[1],2)

dendro_SF5_raw.all <- rbind(dendro_SF5_raw[,c(10,2,8,3,4,6,7,9,5)],
                            dendro_SF5_raw2015[,c(18,2,4,6,8,10,12,14,16)])
summary(dendro_SF5_raw.all)
summary(dendro_SF5_raw)
summary(dendro_SF5_raw2015)

# SF5_L1
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw.all$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L1_dia_diff <- diff(dendro_SF5_raw.all$SF5_L1_dia_raw)
plot(SF5_Date_diff.px,SF5_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_year_diff.px <- as.POSIXct(cut(SF5_Date_diff.px, "year"))
boxplot(SF5_L1_dia_diff,main="SF5_L1"); boxplot(SF5_L1_dia_diff~SF5_year_diff.px,main="SF5_L1"); 
boxplot(SF5_L1_dia_diff,ylim=c(-1,1),main="SF5_L1"); boxplot(SF5_L1_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L1")
# check if 10 highest max and min values occured during field day
SF5_L1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_L1_maxvalue.i <- sort(SF5_L1_dia_diff, TRUE)[i]
  SF5_L1_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L1_dia_diff == SF5_L1_maxvalue.i)]
  SF5_L1_maxdate.i.1 <- as.POSIXct(cut(SF5_L1_maxdate.i, "day"))
  SF5_L1_minvalue.i <- sort(SF5_L1_dia_diff, FALSE)[i]
  SF5_L1_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L1_dia_diff == SF5_L1_minvalue.i)]
  SF5_L1_mindate.i.1 <- as.POSIXct(cut(SF5_L1_mindate.i, "day"))
  SF5_L1_check[i,] <- c(SF5_L1_maxvalue.i,as.character(SF5_L1_maxdate.i),match(SF5_L1_maxdate.i.1,field_days$Date),
                        SF5_L1_minvalue.i,as.character(SF5_L1_mindate.i),match(SF5_L1_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_L1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_L1_check)

SF5_L1_NA <- is.na(dendro_SF5_raw.all$SF5_L1_dia_raw)
SF5_L1_NAdiff <- SF5_L1_NA[-1]
SF5_L1_noNA <- na.locf(dendro_SF5_raw.all$SF5_L1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L1_noNA,type="l", col=2)
SF5_L1_dia_diff_noNA <- diff(SF5_L1_noNA)
SF5_L1_dia_diff.c <-ifelse((SF5_L1_dia_diff_noNA > 1 & SF5_L1_NAdiff == FALSE) | (SF5_L1_dia_diff_noNA < -1 & SF5_L1_NAdiff == FALSE),NA,SF5_L1_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_L1_dia_diff.c,type="l", col=1)
boxplot(SF5_L1_dia_diff.c,main="SF5_L1"); boxplot(SF5_L1_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L1")
summary(SF5_L1_dia_diff.c)
SF5_L1_dia_diff.0 <- na.fill(SF5_L1_dia_diff.c,0);summary(SF5_L1_dia_diff.0)
SF5_L1_dia.c <- diffinv(SF5_L1_dia_diff.0)
SF5_L1_dia.NA <- is.na(dendro_SF5_raw.all$SF5_L1_dia_raw)
SF5_L1_dia.c <- ifelse(SF5_L1_dia.NA == TRUE, NA, SF5_L1_dia.c); summary(SF5_L1_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_L1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_L1_dia.c)

# SF5_L2
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L2_dia_raw,type="l", col=1)
SF5_L2_dia_diff <- diff(dendro_SF5_raw.all$SF5_L2_dia_raw)
plot(SF5_Date_diff.px,SF5_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L2_dia_diff,main="SF5_L2"); boxplot(SF5_L2_dia_diff~SF5_year_diff.px,main="SF5_L2")
boxplot(SF5_L2_dia_diff,ylim=c(-1,1),main="SF5_L2"); boxplot(SF5_L2_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L2")
# check if 10 highest max and min values occured during field day
SF5_L2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_L2_maxvalue.i <- sort(SF5_L2_dia_diff, TRUE)[i]
  SF5_L2_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L2_dia_diff == SF5_L2_maxvalue.i)]
  SF5_L2_maxdate.i.1 <- as.POSIXct(cut(SF5_L2_maxdate.i, "day"))
  SF5_L2_minvalue.i <- sort(SF5_L2_dia_diff, FALSE)[i]
  SF5_L2_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L2_dia_diff == SF5_L2_minvalue.i)]
  SF5_L2_mindate.i.1 <- as.POSIXct(cut(SF5_L2_mindate.i, "day"))
  SF5_L2_check[i,] <- c(SF5_L2_maxvalue.i,as.character(SF5_L2_maxdate.i),match(SF5_L2_maxdate.i.1,field_days$Date),
                        SF5_L2_minvalue.i,as.character(SF5_L2_mindate.i),match(SF5_L2_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_L2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_L2_check)
SF5_L2_NA <- is.na(dendro_SF5_raw.all$SF5_L2_dia_raw)
SF5_L2_NAdiff <- SF5_L2_NA[-1]
SF5_L2_noNA <- na.locf(dendro_SF5_raw.all$SF5_L2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L2_noNA,type="l", col=2)
SF5_L2_dia_diff_noNA <- diff(SF5_L2_noNA)
SF5_L2_dia_diff.c <-ifelse((SF5_L2_dia_diff_noNA > 1.5 & SF5_L2_NAdiff == FALSE) | (SF5_L2_dia_diff_noNA < -1.5 & SF5_L2_NAdiff == FALSE),NA,SF5_L2_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_L2_dia_diff.c,type="l", col=1)
boxplot(SF5_L2_dia_diff.c,main="SF5_L2"); boxplot(SF5_L2_dia_diff.c~SF5_year_diff.px,main="SF5_L2")
summary(SF5_L2_dia_diff.c)
SF5_L2_dia_diff.0 <- na.fill(SF5_L2_dia_diff.c,0);summary(SF5_L2_dia_diff.0)
SF5_L2_dia.c <- diffinv(SF5_L2_dia_diff.0)
SF5_L2_dia.NA <- is.na(dendro_SF5_raw.all$SF5_L2_dia_raw)
SF5_L2_dia.c <- ifelse(SF5_L2_dia.NA == TRUE, NA, SF5_L2_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_L2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_L2_dia.c)

# SF5_L3
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L3_dia_raw,type="l", col=1)
SF5_L3_dia_diff <- diff(dendro_SF5_raw.all$SF5_L3_dia_raw)
plot(SF5_Date_diff.px,SF5_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L3_dia_diff,main="SF5_L3"); boxplot(SF5_L3_dia_diff~SF5_year_diff.px,main="SF5_L3"); 
boxplot(SF5_L3_dia_diff,ylim=c(-1,1),main="SF5_L3"); boxplot(SF5_L3_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L3")
# check if 10 highest max and min values occured during field day
SF5_L3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_L3_maxvalue.i <- sort(SF5_L3_dia_diff, TRUE)[i]
  SF5_L3_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L3_dia_diff == SF5_L3_maxvalue.i)]
  SF5_L3_maxdate.i.1 <- as.POSIXct(cut(SF5_L3_maxdate.i, "day"))
  SF5_L3_minvalue.i <- sort(SF5_L3_dia_diff, FALSE)[i]
  SF5_L3_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L3_dia_diff == SF5_L3_minvalue.i)]
  SF5_L3_mindate.i.1 <- as.POSIXct(cut(SF5_L3_mindate.i, "day"))
  SF5_L3_check[i,] <- c(SF5_L3_maxvalue.i,as.character(SF5_L3_maxdate.i),match(SF5_L3_maxdate.i.1,field_days$Date),
                        SF5_L3_minvalue.i,as.character(SF5_L3_mindate.i),match(SF5_L3_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_L3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_L3_check)
SF5_L3_NA <- is.na(dendro_SF5_raw.all$SF5_L3_dia_raw)
SF5_L3_NAdiff <- SF5_L3_NA[-1]
SF5_L3_noNA <- na.locf(dendro_SF5_raw.all$SF5_L3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L3_noNA,type="l", col=2)
SF5_L3_dia_diff_noNA <- diff(SF5_L3_noNA)
SF5_L3_dia_diff.c <-ifelse((SF5_L3_dia_diff_noNA > 1 & SF5_L3_NAdiff == FALSE) | (SF5_L3_dia_diff_noNA < -1 & SF5_L3_NAdiff == FALSE),NA,SF5_L3_dia_diff_noNA) 
SF5_L3_dia_diff.c[which(SF5_L3_dia_diff == sort(SF5_L3_dia_diff, FALSE)[5])] <- NA
plot(SF5_Date_diff.px,SF5_L3_dia_diff.c,type="l", col=1)
boxplot(SF5_L3_dia_diff.c,main="SF5_L3"); boxplot(SF5_L3_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L3")
summary(SF5_L3_dia_diff.c)
SF5_L3_dia_diff.0 <- na.fill(SF5_L3_dia_diff.c,0);summary(SF5_L3_dia_diff.0)
SF5_L3_dia.c <- diffinv(SF5_L3_dia_diff.0)
SF5_L3_dia.NA <- is.na(dendro_SF5_raw.all$SF5_L3_dia_raw)
SF5_L3_dia.c <- ifelse(SF5_L3_dia.NA == TRUE, NA, SF5_L3_dia.c); summary(SF5_L3_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_L3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_L3_dia.c)

# SF5_L4
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw.all$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_L4_dia_diff <- diff(dendro_SF5_raw.all$SF5_L4_dia_raw)
plot(SF5_Date_diff.px,SF5_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_L4_dia_diff,main="SF5_L4"); boxplot(SF5_L4_dia_diff~SF5_year_diff.px,main="SF5_L4"); 
boxplot(SF5_L4_dia_diff,ylim=c(-1,1),main="SF5_L4"); boxplot(SF5_L4_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L4")
# check if 10 highest max and min values occured during field day
SF5_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_L4_maxvalue.i <- sort(SF5_L4_dia_diff, TRUE)[i]
  SF5_L4_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L4_dia_diff == SF5_L4_maxvalue.i)]
  SF5_L4_maxdate.i.1 <- as.POSIXct(cut(SF5_L4_maxdate.i, "day"))
  SF5_L4_minvalue.i <- sort(SF5_L4_dia_diff, FALSE)[i]
  SF5_L4_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_L4_dia_diff == SF5_L4_minvalue.i)]
  SF5_L4_mindate.i.1 <- as.POSIXct(cut(SF5_L4_mindate.i, "day"))
  SF5_L4_check[i,] <- c(SF5_L4_maxvalue.i,as.character(SF5_L4_maxdate.i),match(SF5_L4_maxdate.i.1,field_days$Date),
                        SF5_L4_minvalue.i,as.character(SF5_L4_mindate.i),match(SF5_L4_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_L4_check)

SF5_L4_NA <- is.na(dendro_SF5_raw.all$SF5_L4_dia_raw)
SF5_L4_NAdiff <- SF5_L4_NA[-1]
SF5_L4_noNA <- na.locf(dendro_SF5_raw.all$SF5_L4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L4_noNA,type="l", col=2)
SF5_L4_dia_diff_noNA <- diff(SF5_L4_noNA)
SF5_L4_dia_diff.c <-ifelse((SF5_L4_dia_diff_noNA > 1 & SF5_L4_NAdiff == FALSE) | (SF5_L4_dia_diff_noNA < -1 & SF5_L4_NAdiff == FALSE),NA,SF5_L4_dia_diff_noNA) 
SF5_L4_dia_diff.c[which(SF5_L4_dia_diff == sort(SF5_L4_dia_diff, FALSE)[5])] <- NA
plot(SF5_Date_diff.px,SF5_L4_dia_diff.c,type="l", col=1)
boxplot(SF5_L4_dia_diff.c,main="SF5_L4"); boxplot(SF5_L4_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_L4")
summary(SF5_L4_dia_diff.c)
SF5_L4_dia_diff.0 <- na.fill(SF5_L4_dia_diff.c,0);summary(SF5_L4_dia_diff.0)
SF5_L4_dia.c <- diffinv(SF5_L4_dia_diff.0)
SF5_L4_dia.NA <- is.na(dendro_SF5_raw.all$SF5_L4_dia_raw)
SF5_L4_dia.c <- ifelse(SF5_L4_dia.NA == TRUE, NA, SF5_L4_dia.c); summary(SF5_L4_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_L4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF5L_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of SF5 (raw data)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L2_dia_raw,type="l", col=2)
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L3_dia_raw,type="l", col=3)
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L4_dia_raw,type="l", col=4)

# Plot all trees of SF5 (differences)
plot(SF5_Date_diff.px,SF5_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF5_Date_diff.px,SF5_L2_dia_diff,type="l", col=2)
lines(SF5_Date_diff.px,SF5_L3_dia_diff,type="l", col=3)
lines(SF5_Date_diff.px,SF5_L4_dia_diff,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF5 (cleaned data)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_L1_dia.c,type="l", col=1,ylim=c(-15,80))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L2_dia.c,type="l", col=2)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L3_dia.c,type="l", col=3)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L4_dia.c,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF5L_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# SF5_Z1 ####
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z1_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw.all$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z1_dia_diff <- diff(dendro_SF5_raw.all$SF5_Z1_dia_raw)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
SF5_year_diff.px <- as.POSIXct(cut(SF5_Date_diff.px, "year"))
boxplot(SF5_Z1_dia_diff,main="SF5_Z1"); boxplot(SF5_Z1_dia_diff~SF5_year_diff.px,main="SF5_Z1"); 
boxplot(SF5_Z1_dia_diff,ylim=c(-1,1),main="SF5_Z1"); boxplot(SF5_Z1_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z1")
# check if 10 highest max and min values occured during field day
SF5_Z1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_Z1_maxvalue.i <- sort(SF5_Z1_dia_diff, TRUE)[i]
  SF5_Z1_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z1_dia_diff == SF5_Z1_maxvalue.i)]
  SF5_Z1_maxdate.i.1 <- as.POSIXct(cut(SF5_Z1_maxdate.i, "day"))
  SF5_Z1_minvalue.i <- sort(SF5_Z1_dia_diff, FALSE)[i]
  SF5_Z1_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z1_dia_diff == SF5_Z1_minvalue.i)]
  SF5_Z1_mindate.i.1 <- as.POSIXct(cut(SF5_Z1_mindate.i, "day"))
  SF5_Z1_check[i,] <- c(SF5_Z1_maxvalue.i,as.character(SF5_Z1_maxdate.i),match(SF5_Z1_maxdate.i.1,field_days$Date),
                        SF5_Z1_minvalue.i,as.character(SF5_Z1_mindate.i),match(SF5_Z1_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_Z1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_Z1_check)

SF5_Z1_NA <- is.na(dendro_SF5_raw.all$SF5_Z1_dia_raw)
SF5_Z1_NAdiff <- SF5_Z1_NA[-1]
SF5_Z1_noNA <- na.locf(dendro_SF5_raw.all$SF5_Z1_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z1_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z1_noNA,type="l", col=2)
SF5_Z1_dia_diff_noNA <- diff(SF5_Z1_noNA)
SF5_Z1_dia_diff.c <-ifelse((SF5_Z1_dia_diff_noNA > 0.5 & SF5_Z1_NAdiff == FALSE) | (SF5_Z1_dia_diff_noNA < -0.5 & SF5_Z1_NAdiff == FALSE),NA,SF5_Z1_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_Z1_dia_diff.c,type="l", col=1)
boxplot(SF5_Z1_dia_diff.c,main="SF5_Z1"); boxplot(SF5_Z1_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z1")
summary(SF5_Z1_dia_diff.c)
SF5_Z1_dia_diff.0 <- na.fill(SF5_Z1_dia_diff.c,0);summary(SF5_Z1_dia_diff.0)
SF5_Z1_dia.c <- diffinv(SF5_Z1_dia_diff.0)
SF5_Z1_dia.NA <- is.na(dendro_SF5_raw.all$SF5_Z1_dia_raw)
SF5_Z1_dia.c <- ifelse(SF5_Z1_dia.NA == TRUE, NA, SF5_Z1_dia.c); summary(SF5_Z1_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_Z1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_Z1_dia.c)

# SF5_Z2
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z2_dia_raw,type="l", col=1)
SF5_Z2_dia_diff <- diff(dendro_SF5_raw.all$SF5_Z2_dia_raw)
plot(SF5_Date_diff.px,SF5_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z2_dia_diff,main="SF5_Z2"); boxplot(SF5_Z2_dia_diff~SF5_year_diff.px,main="SF5_Z2")
boxplot(SF5_Z2_dia_diff,ylim=c(-1,1),main="SF5_Z2"); boxplot(SF5_Z2_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z2")
# check if 10 highest max and min values occured during field day
SF5_Z2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_Z2_maxvalue.i <- sort(SF5_Z2_dia_diff, TRUE)[i]
  SF5_Z2_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z2_dia_diff == SF5_Z2_maxvalue.i)]
  SF5_Z2_maxdate.i.1 <- as.POSIXct(cut(SF5_Z2_maxdate.i, "day"))
  SF5_Z2_minvalue.i <- sort(SF5_Z2_dia_diff, FALSE)[i]
  SF5_Z2_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z2_dia_diff == SF5_Z2_minvalue.i)]
  SF5_Z2_mindate.i.1 <- as.POSIXct(cut(SF5_Z2_mindate.i, "day"))
  SF5_Z2_check[i,] <- c(SF5_Z2_maxvalue.i,as.character(SF5_Z2_maxdate.i),match(SF5_Z2_maxdate.i.1,field_days$Date),
                        SF5_Z2_minvalue.i,as.character(SF5_Z2_mindate.i),match(SF5_Z2_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_Z2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_Z2_check)
SF5_Z2_NA <- is.na(dendro_SF5_raw.all$SF5_Z2_dia_raw)
SF5_Z2_NAdiff <- SF5_Z2_NA[-1]
SF5_Z2_noNA <- na.locf(dendro_SF5_raw.all$SF5_Z2_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z2_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z2_noNA,type="l", col=2)
SF5_Z2_dia_diff_noNA <- diff(SF5_Z2_noNA)
SF5_Z2_dia_diff.c <-ifelse((SF5_Z2_dia_diff_noNA > 1 & SF5_Z2_NAdiff == FALSE) | (SF5_Z2_dia_diff_noNA < -1 & SF5_Z2_NAdiff == FALSE),NA,SF5_Z2_dia_diff_noNA) 
plot(SF5_Date_diff.px,SF5_Z2_dia_diff.c,type="l", col=1)
boxplot(SF5_Z2_dia_diff.c,main="SF5_Z2"); boxplot(SF5_Z2_dia_diff.c~SF5_year_diff.px,main="SF5_Z2")
summary(SF5_Z2_dia_diff.c)
SF5_Z2_dia_diff.0 <- na.fill(SF5_Z2_dia_diff.c,0);summary(SF5_Z2_dia_diff.0)
SF5_Z2_dia.c <- diffinv(SF5_Z2_dia_diff.0)
SF5_Z2_dia.NA <- is.na(dendro_SF5_raw.all$SF5_Z2_dia_raw)
SF5_Z2_dia.c <- ifelse(SF5_Z2_dia.NA == TRUE, NA, SF5_Z2_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_Z2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_Z2_dia.c)

# SF5_Z3
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z3_dia_raw,type="l", col=1)
SF5_Z3_dia_diff <- diff(dendro_SF5_raw.all$SF5_Z3_dia_raw)
plot(SF5_Date_diff.px,SF5_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z3_dia_diff,main="SF5_Z3"); boxplot(SF5_Z3_dia_diff~SF5_year_diff.px,main="SF5_Z3"); 
boxplot(SF5_Z3_dia_diff,ylim=c(-1,1),main="SF5_Z3"); boxplot(SF5_Z3_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z3")
# check if 10 highest max and min values occured during field day
SF5_Z3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_Z3_maxvalue.i <- sort(SF5_Z3_dia_diff, TRUE)[i]
  SF5_Z3_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z3_dia_diff == SF5_Z3_maxvalue.i)]
  SF5_Z3_maxdate.i.1 <- as.POSIXct(cut(SF5_Z3_maxdate.i, "day"))
  SF5_Z3_minvalue.i <- sort(SF5_Z3_dia_diff, FALSE)[i]
  SF5_Z3_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z3_dia_diff == SF5_Z3_minvalue.i)]
  SF5_Z3_mindate.i.1 <- as.POSIXct(cut(SF5_Z3_mindate.i, "day"))
  SF5_Z3_check[i,] <- c(SF5_Z3_maxvalue.i,as.character(SF5_Z3_maxdate.i),match(SF5_Z3_maxdate.i.1,field_days$Date),
                        SF5_Z3_minvalue.i,as.character(SF5_Z3_mindate.i),match(SF5_Z3_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_Z3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_Z3_check)
SF5_Z3_NA <- is.na(dendro_SF5_raw.all$SF5_Z3_dia_raw)
SF5_Z3_NAdiff <- SF5_Z3_NA[-1]
SF5_Z3_noNA <- na.locf(dendro_SF5_raw.all$SF5_Z3_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z3_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z3_noNA,type="l", col=2)
SF5_Z3_dia_diff_noNA <- diff(SF5_Z3_noNA)
SF5_Z3_dia_diff.c <-ifelse((SF5_Z3_dia_diff_noNA > 1 & SF5_Z3_NAdiff == FALSE) | (SF5_Z3_dia_diff_noNA < -1 & SF5_Z3_NAdiff == FALSE),NA,SF5_Z3_dia_diff_noNA) 
SF5_Z3_dia_diff.c[which(SF5_Z3_dia_diff == sort(SF5_Z3_dia_diff, FALSE)[5])] <- NA
plot(SF5_Date_diff.px,SF5_Z3_dia_diff.c,type="l", col=1)
boxplot(SF5_Z3_dia_diff.c,main="SF5_Z3"); boxplot(SF5_Z3_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z3")
summary(SF5_Z3_dia_diff.c)
SF5_Z3_dia_diff.0 <- na.fill(SF5_Z3_dia_diff.c,0);summary(SF5_Z3_dia_diff.0)
SF5_Z3_dia.c <- diffinv(SF5_Z3_dia_diff.0)
SF5_Z3_dia.NA <- is.na(dendro_SF5_raw.all$SF5_Z3_dia_raw)
SF5_Z3_dia.c <- ifelse(SF5_Z3_dia.NA == TRUE, NA, SF5_Z3_dia.c); summary(SF5_Z3_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_Z3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_Z3_dia.c)

# SF5_Z4
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z4_dia_raw,type="l", col=1)
SF5_Date_diff <- dendro_SF5_raw.all$Date_Time[-1]
SF5_Date_diff.px <- as.POSIXct(SF5_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
SF5_Z4_dia_diff <- diff(dendro_SF5_raw.all$SF5_Z4_dia_raw)
plot(SF5_Date_diff.px,SF5_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(SF5_Z4_dia_diff,main="SF5_Z4"); boxplot(SF5_Z4_dia_diff~SF5_year_diff.px,main="SF5_Z4"); 
boxplot(SF5_Z4_dia_diff,ylim=c(-1,1),main="SF5_Z4"); boxplot(SF5_Z4_dia_diff~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z4")
# check if 10 highest max and min values occured during field day
SF5_Z4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  SF5_Z4_maxvalue.i <- sort(SF5_Z4_dia_diff, TRUE)[i]
  SF5_Z4_maxdate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z4_dia_diff == SF5_Z4_maxvalue.i)]
  SF5_Z4_maxdate.i.1 <- as.POSIXct(cut(SF5_Z4_maxdate.i, "day"))
  SF5_Z4_minvalue.i <- sort(SF5_Z4_dia_diff, FALSE)[i]
  SF5_Z4_mindate.i <- dendro_SF5_raw.all$Date_Time.px[which(SF5_Z4_dia_diff == SF5_Z4_minvalue.i)]
  SF5_Z4_mindate.i.1 <- as.POSIXct(cut(SF5_Z4_mindate.i, "day"))
  SF5_Z4_check[i,] <- c(SF5_Z4_maxvalue.i,as.character(SF5_Z4_maxdate.i),match(SF5_Z4_maxdate.i.1,field_days$Date),
                        SF5_Z4_minvalue.i,as.character(SF5_Z4_mindate.i),match(SF5_Z4_mindate.i.1,field_days$Date)) 
} 
colnames(SF5_Z4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(SF5_Z4_check)

SF5_Z4_NA <- is.na(dendro_SF5_raw.all$SF5_Z4_dia_raw)
SF5_Z4_NAdiff <- SF5_Z4_NA[-1]
SF5_Z4_noNA <- na.locf(dendro_SF5_raw.all$SF5_Z4_dia_raw, na.rm = FALSE)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z4_dia_raw,type="l", col=1)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z4_noNA,type="l", col=2)
SF5_Z4_dia_diff_noNA <- diff(SF5_Z4_noNA)
SF5_Z4_dia_diff.c <-ifelse((SF5_Z4_dia_diff_noNA > 1 & SF5_Z4_NAdiff == FALSE) | (SF5_Z4_dia_diff_noNA < -1 & SF5_Z4_NAdiff == FALSE),NA,SF5_Z4_dia_diff_noNA) 
SF5_Z4_dia_diff.c[which(SF5_Z4_dia_diff == sort(SF5_Z4_dia_diff, FALSE)[5])] <- NA
plot(SF5_Date_diff.px,SF5_Z4_dia_diff.c,type="l", col=1)
boxplot(SF5_Z4_dia_diff.c,main="SF5_Z4"); boxplot(SF5_Z4_dia_diff.c~SF5_year_diff.px,ylim=c(-1,1),main="SF5_Z4")
summary(SF5_Z4_dia_diff.c)
SF5_Z4_dia_diff.0 <- na.fill(SF5_Z4_dia_diff.c,0);summary(SF5_Z4_dia_diff.0)
SF5_Z4_dia.c <- diffinv(SF5_Z4_dia_diff.0)
SF5_Z4_dia.NA <- is.na(dendro_SF5_raw.all$SF5_Z4_dia_raw)
SF5_Z4_dia.c <- ifelse(SF5_Z4_dia.NA == TRUE, NA, SF5_Z4_dia.c); summary(SF5_Z4_dia.c)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_Z4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (SF5_Z4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF5Z_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of SF5 (raw data)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z2_dia_raw,type="l", col=2)
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z3_dia_raw,type="l", col=3)
lines(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z4_dia_raw,type="l", col=4)

# Plot all trees of SF5 (differences)
plot(SF5_Date_diff.px,SF5_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(SF5_Date_diff.px,SF5_Z2_dia_diff,type="l", col=2)
lines(SF5_Date_diff.px,SF5_Z3_dia_diff,type="l", col=3)
lines(SF5_Date_diff.px,SF5_Z4_dia_diff,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of SF5 (cleaned data)
plot(dendro_SF5_raw.all$Date_Time.px,SF5_Z1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z2_dia.c,type="l", col=2)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z3_dia.c,type="l", col=3)
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z4_dia.c,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_SF5Z_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_SF5_raw.all$Date_Time.px,dendro_SF5_raw.all$SF5_Z4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_SF5_raw.all$Date_Time.px,SF5_Z4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# WgN ####
# names(dendro_WgN_raw);
names(dendro_WgN_raw2015)
# tail(dendro_WgN_raw[1],2); head(dendro_WgN_raw2015[1],2); tail(dendro_WgN_raw2015[1],2)
dendro_WgN_raw.all <- dendro_WgN_raw2015[,c(10,2,4,6,8)]
summary(dendro_WgN_raw.all)

# WgN_Z1 ####
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z1_dia_raw,type="l", col=1)
WgN_Date_diff <- dendro_WgN_raw.all$Date_Time[-1]
WgN_Date_diff.px <- as.POSIXct(WgN_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
WgN_Z1_dia_diff <- diff(dendro_WgN_raw.all$WgN_Z1_dia_raw)
plot(WgN_Date_diff.px,WgN_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
WgN_year_diff.px <- as.POSIXct(cut(WgN_Date_diff.px, "year"))
boxplot(WgN_Z1_dia_diff,main="WgN_Z1"); boxplot(WgN_Z1_dia_diff~WgN_year_diff.px,main="WgN_Z1"); 
boxplot(WgN_Z1_dia_diff,ylim=c(-1,1),main="WgN_Z1"); boxplot(WgN_Z1_dia_diff~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z1")
# check if 10 highest max and min values occured during field day
WgN_Z1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgN_Z1_maxvalue.i <- sort(WgN_Z1_dia_diff, TRUE)[i]
  WgN_Z1_maxdate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z1_dia_diff == WgN_Z1_maxvalue.i)]
  WgN_Z1_maxdate.i.1 <- as.POSIXct(cut(WgN_Z1_maxdate.i, "day"))
  WgN_Z1_minvalue.i <- sort(WgN_Z1_dia_diff, FALSE)[i]
  WgN_Z1_mindate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z1_dia_diff == WgN_Z1_minvalue.i)]
  WgN_Z1_mindate.i.1 <- as.POSIXct(cut(WgN_Z1_mindate.i, "day"))
  WgN_Z1_check[i,] <- c(WgN_Z1_maxvalue.i,as.character(WgN_Z1_maxdate.i),match(WgN_Z1_maxdate.i.1,field_days$Date),
                        WgN_Z1_minvalue.i,as.character(WgN_Z1_mindate.i),match(WgN_Z1_mindate.i.1,field_days$Date)) 
} 
colnames(WgN_Z1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgN_Z1_check)

WgN_Z1_NA <- is.na(dendro_WgN_raw.all$WgN_Z1_dia_raw)
WgN_Z1_NAdiff <- WgN_Z1_NA[-1]
WgN_Z1_noNA <- na.locf(dendro_WgN_raw.all$WgN_Z1_dia_raw, na.rm = FALSE)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z1_dia_raw,type="l", col=1)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z1_noNA,type="l", col=2)
WgN_Z1_dia_diff_noNA <- diff(WgN_Z1_noNA)
WgN_Z1_dia_diff.c <-ifelse((WgN_Z1_dia_diff_noNA > 0.5 & WgN_Z1_NAdiff == FALSE) | (WgN_Z1_dia_diff_noNA < -0.5 & WgN_Z1_NAdiff == FALSE),NA,WgN_Z1_dia_diff_noNA) 
plot(WgN_Date_diff.px,WgN_Z1_dia_diff.c,type="l", col=1)
boxplot(WgN_Z1_dia_diff.c,main="WgN_Z1"); boxplot(WgN_Z1_dia_diff.c~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z1")
summary(WgN_Z1_dia_diff.c)
WgN_Z1_dia_diff.0 <- na.fill(WgN_Z1_dia_diff.c,0);summary(WgN_Z1_dia_diff.0)
WgN_Z1_dia.c <- diffinv(WgN_Z1_dia_diff.0)
WgN_Z1_dia.NA <- is.na(dendro_WgN_raw.all$WgN_Z1_dia_raw)
WgN_Z1_dia.c <- ifelse(WgN_Z1_dia.NA == TRUE, NA, WgN_Z1_dia.c); summary(WgN_Z1_dia.c)
plot(dendro_WgN_raw.all$Date_Time.px,WgN_Z1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgN_Z1_dia.c)

# WgN_Z2
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z2_dia_raw,type="l", col=1)
WgN_Z2_dia_diff <- diff(dendro_WgN_raw.all$WgN_Z2_dia_raw)
plot(WgN_Date_diff.px,WgN_Z2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgN_Z2_dia_diff,main="WgN_Z2"); boxplot(WgN_Z2_dia_diff~WgN_year_diff.px,main="WgN_Z2")
boxplot(WgN_Z2_dia_diff,ylim=c(-1,1),main="WgN_Z2"); boxplot(WgN_Z2_dia_diff~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z2")
# check if 10 highest max and min values occured during field day
WgN_Z2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgN_Z2_maxvalue.i <- sort(WgN_Z2_dia_diff, TRUE)[i]
  WgN_Z2_maxdate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z2_dia_diff == WgN_Z2_maxvalue.i)]
  WgN_Z2_maxdate.i.1 <- as.POSIXct(cut(WgN_Z2_maxdate.i, "day"))
  WgN_Z2_minvalue.i <- sort(WgN_Z2_dia_diff, FALSE)[i]
  WgN_Z2_mindate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z2_dia_diff == WgN_Z2_minvalue.i)]
  WgN_Z2_mindate.i.1 <- as.POSIXct(cut(WgN_Z2_mindate.i, "day"))
  WgN_Z2_check[i,] <- c(WgN_Z2_maxvalue.i,as.character(WgN_Z2_maxdate.i),match(WgN_Z2_maxdate.i.1,field_days$Date),
                        WgN_Z2_minvalue.i,as.character(WgN_Z2_mindate.i),match(WgN_Z2_mindate.i.1,field_days$Date)) 
} 
colnames(WgN_Z2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgN_Z2_check)
WgN_Z2_NA <- is.na(dendro_WgN_raw.all$WgN_Z2_dia_raw)
WgN_Z2_NAdiff <- WgN_Z2_NA[-1]
WgN_Z2_noNA <- na.locf(dendro_WgN_raw.all$WgN_Z2_dia_raw, na.rm = FALSE)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z2_dia_raw,type="l", col=1)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z2_noNA,type="l", col=2)
WgN_Z2_dia_diff_noNA <- diff(WgN_Z2_noNA)
WgN_Z2_dia_diff.c <-ifelse((WgN_Z2_dia_diff_noNA > 1 & WgN_Z2_NAdiff == FALSE) | (WgN_Z2_dia_diff_noNA < -1 & WgN_Z2_NAdiff == FALSE),NA,WgN_Z2_dia_diff_noNA) 
plot(WgN_Date_diff.px,WgN_Z2_dia_diff.c,type="l", col=1)
boxplot(WgN_Z2_dia_diff.c,main="WgN_Z2"); boxplot(WgN_Z2_dia_diff.c~WgN_year_diff.px,main="WgN_Z2")
summary(WgN_Z2_dia_diff.c)
WgN_Z2_dia_diff.0 <- na.fill(WgN_Z2_dia_diff.c,0);summary(WgN_Z2_dia_diff.0)
WgN_Z2_dia.c <- diffinv(WgN_Z2_dia_diff.0)
WgN_Z2_dia.NA <- is.na(dendro_WgN_raw.all$WgN_Z2_dia_raw)
WgN_Z2_dia.c <- ifelse(WgN_Z2_dia.NA == TRUE, NA, WgN_Z2_dia.c)
plot(dendro_WgN_raw.all$Date_Time.px,WgN_Z2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgN_Z2_dia.c)

# WgN_Z3
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z3_dia_raw,type="l", col=1)
WgN_Z3_dia_diff <- diff(dendro_WgN_raw.all$WgN_Z3_dia_raw)
plot(WgN_Date_diff.px,WgN_Z3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgN_Z3_dia_diff,main="WgN_Z3"); boxplot(WgN_Z3_dia_diff~WgN_year_diff.px,main="WgN_Z3"); 
boxplot(WgN_Z3_dia_diff,ylim=c(-1,1),main="WgN_Z3"); boxplot(WgN_Z3_dia_diff~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z3")
# check if 10 highest max and min values occured during field day
WgN_Z3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgN_Z3_maxvalue.i <- sort(WgN_Z3_dia_diff, TRUE)[i]
  WgN_Z3_maxdate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z3_dia_diff == WgN_Z3_maxvalue.i)]
  WgN_Z3_maxdate.i.1 <- as.POSIXct(cut(WgN_Z3_maxdate.i, "day"))
  WgN_Z3_minvalue.i <- sort(WgN_Z3_dia_diff, FALSE)[i]
  WgN_Z3_mindate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z3_dia_diff == WgN_Z3_minvalue.i)]
  WgN_Z3_mindate.i.1 <- as.POSIXct(cut(WgN_Z3_mindate.i, "day"))
  WgN_Z3_check[i,] <- c(WgN_Z3_maxvalue.i,as.character(WgN_Z3_maxdate.i),match(WgN_Z3_maxdate.i.1,field_days$Date),
                        WgN_Z3_minvalue.i,as.character(WgN_Z3_mindate.i),match(WgN_Z3_mindate.i.1,field_days$Date)) 
} 
colnames(WgN_Z3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgN_Z3_check)
WgN_Z3_NA <- is.na(dendro_WgN_raw.all$WgN_Z3_dia_raw)
WgN_Z3_NAdiff <- WgN_Z3_NA[-1]
WgN_Z3_noNA <- na.locf(dendro_WgN_raw.all$WgN_Z3_dia_raw, na.rm = FALSE)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z3_dia_raw,type="l", col=1)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z3_noNA,type="l", col=2)
WgN_Z3_dia_diff_noNA <- diff(WgN_Z3_noNA)
WgN_Z3_dia_diff.c <-ifelse((WgN_Z3_dia_diff_noNA > 1 & WgN_Z3_NAdiff == FALSE) | (WgN_Z3_dia_diff_noNA < -1 & WgN_Z3_NAdiff == FALSE),NA,WgN_Z3_dia_diff_noNA) 
WgN_Z3_dia_diff.c[which(WgN_Z3_dia_diff == sort(WgN_Z3_dia_diff, FALSE)[5])] <- NA
plot(WgN_Date_diff.px,WgN_Z3_dia_diff.c,type="l", col=1)
boxplot(WgN_Z3_dia_diff.c,main="WgN_Z3"); boxplot(WgN_Z3_dia_diff.c~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z3")
summary(WgN_Z3_dia_diff.c)
WgN_Z3_dia_diff.0 <- na.fill(WgN_Z3_dia_diff.c,0);summary(WgN_Z3_dia_diff.0)
WgN_Z3_dia.c <- diffinv(WgN_Z3_dia_diff.0)
WgN_Z3_dia.NA <- is.na(dendro_WgN_raw.all$WgN_Z3_dia_raw)
WgN_Z3_dia.c <- ifelse(WgN_Z3_dia.NA == TRUE, NA, WgN_Z3_dia.c); summary(WgN_Z3_dia.c)
plot(dendro_WgN_raw.all$Date_Time.px,WgN_Z3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgN_Z3_dia.c)

# WgN_Z4
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z4_dia_raw,type="l", col=1)
WgN_Date_diff <- dendro_WgN_raw.all$Date_Time[-1]
WgN_Date_diff.px <- as.POSIXct(WgN_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
WgN_Z4_dia_diff <- diff(dendro_WgN_raw.all$WgN_Z4_dia_raw)
plot(WgN_Date_diff.px,WgN_Z4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgN_Z4_dia_diff,main="WgN_Z4"); boxplot(WgN_Z4_dia_diff~WgN_year_diff.px,main="WgN_Z4"); 
boxplot(WgN_Z4_dia_diff,ylim=c(-1,1),main="WgN_Z4"); boxplot(WgN_Z4_dia_diff~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z4")
# check if 10 highest max and min values occured during field day
WgN_Z4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgN_Z4_maxvalue.i <- sort(WgN_Z4_dia_diff, TRUE)[i]
  WgN_Z4_maxdate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z4_dia_diff == WgN_Z4_maxvalue.i)]
  WgN_Z4_maxdate.i.1 <- as.POSIXct(cut(WgN_Z4_maxdate.i, "day"))
  WgN_Z4_minvalue.i <- sort(WgN_Z4_dia_diff, FALSE)[i]
  WgN_Z4_mindate.i <- dendro_WgN_raw.all$Date_Time.px[which(WgN_Z4_dia_diff == WgN_Z4_minvalue.i)]
  WgN_Z4_mindate.i.1 <- as.POSIXct(cut(WgN_Z4_mindate.i, "day"))
  WgN_Z4_check[i,] <- c(WgN_Z4_maxvalue.i,as.character(WgN_Z4_maxdate.i),match(WgN_Z4_maxdate.i.1,field_days$Date),
                        WgN_Z4_minvalue.i,as.character(WgN_Z4_mindate.i),match(WgN_Z4_mindate.i.1,field_days$Date)) 
} 
colnames(WgN_Z4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgN_Z4_check)

WgN_Z4_NA <- is.na(dendro_WgN_raw.all$WgN_Z4_dia_raw)
WgN_Z4_NAdiff <- WgN_Z4_NA[-1]
WgN_Z4_noNA <- na.locf(dendro_WgN_raw.all$WgN_Z4_dia_raw, na.rm = FALSE)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z4_dia_raw,type="l", col=1)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z4_noNA,type="l", col=2)
WgN_Z4_dia_diff_noNA <- diff(WgN_Z4_noNA)
WgN_Z4_dia_diff.c <-ifelse((WgN_Z4_dia_diff_noNA > 1 & WgN_Z4_NAdiff == FALSE) | (WgN_Z4_dia_diff_noNA < -1 & WgN_Z4_NAdiff == FALSE),NA,WgN_Z4_dia_diff_noNA) 
WgN_Z4_dia_diff.c[which(WgN_Z4_dia_diff == sort(WgN_Z4_dia_diff, FALSE)[5])] <- NA
plot(WgN_Date_diff.px,WgN_Z4_dia_diff.c,type="l", col=1)
boxplot(WgN_Z4_dia_diff.c,main="WgN_Z4"); boxplot(WgN_Z4_dia_diff.c~WgN_year_diff.px,ylim=c(-1,1),main="WgN_Z4")
summary(WgN_Z4_dia_diff.c)
WgN_Z4_dia_diff.0 <- na.fill(WgN_Z4_dia_diff.c,0);summary(WgN_Z4_dia_diff.0)
WgN_Z4_dia.c <- diffinv(WgN_Z4_dia_diff.0)
WgN_Z4_dia.NA <- is.na(dendro_WgN_raw.all$WgN_Z4_dia_raw)
WgN_Z4_dia.c <- ifelse(WgN_Z4_dia.NA == TRUE, NA, WgN_Z4_dia.c); summary(WgN_Z4_dia.c)
plot(dendro_WgN_raw.all$Date_Time.px,WgN_Z4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgN_Z4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_WgNZ_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of WgN (raw data)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z2_dia_raw,type="l", col=2)
lines(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z3_dia_raw,type="l", col=3)
lines(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z4_dia_raw,type="l", col=4)

# Plot all trees of WgN (differences)
plot(WgN_Date_diff.px,WgN_Z1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(WgN_Date_diff.px,WgN_Z2_dia_diff,type="l", col=2)
lines(WgN_Date_diff.px,WgN_Z3_dia_diff,type="l", col=3)
lines(WgN_Date_diff.px,WgN_Z4_dia_diff,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of WgN (cleaned data)
plot(dendro_WgN_raw.all$Date_Time.px,WgN_Z1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z2_dia.c,type="l", col=2)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z3_dia.c,type="l", col=3)
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z4_dia.c,type="l", col=4)
legend("topleft",c("Z1","Z2","Z3","Z4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_WgNZ_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgN_raw.all$Date_Time.px,dendro_WgN_raw.all$WgN_Z4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgN_raw.all$Date_Time.px,WgN_Z4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

# WgS ####
# names(dendro_WgS_raw);
names(dendro_WgS_raw2015)
# tail(dendro_WgS_raw[1],2); head(dendro_WgS_raw2015[1],2); tail(dendro_WgS_raw2015[1],2)
dendro_WgS_raw.all <- dendro_WgS_raw2015[,c(10,2,4,6,8)]
summary(dendro_WgS_raw.all)

# WgS_L1 ####
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L1_dia_raw,type="l", col=1)
WgS_Date_diff <- dendro_WgS_raw.all$Date_Time[-1]
WgS_Date_diff.px <- as.POSIXct(WgS_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
WgS_L1_dia_diff <- diff(dendro_WgS_raw.all$WgS_L1_dia_raw)
plot(WgS_Date_diff.px,WgS_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
WgS_year_diff.px <- as.POSIXct(cut(WgS_Date_diff.px, "year"))
boxplot(WgS_L1_dia_diff,main="WgS_L1"); boxplot(WgS_L1_dia_diff~WgS_year_diff.px,main="WgS_L1"); 
boxplot(WgS_L1_dia_diff,ylim=c(-1,1),main="WgS_L1"); boxplot(WgS_L1_dia_diff~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L1")
# check if 10 highest max and min values occured during field day
WgS_L1_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgS_L1_maxvalue.i <- sort(WgS_L1_dia_diff, TRUE)[i]
  WgS_L1_maxdate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L1_dia_diff == WgS_L1_maxvalue.i)]
  WgS_L1_maxdate.i.1 <- as.POSIXct(cut(WgS_L1_maxdate.i, "day"))
  WgS_L1_minvalue.i <- sort(WgS_L1_dia_diff, FALSE)[i]
  WgS_L1_mindate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L1_dia_diff == WgS_L1_minvalue.i)]
  WgS_L1_mindate.i.1 <- as.POSIXct(cut(WgS_L1_mindate.i, "day"))
  WgS_L1_check[i,] <- c(WgS_L1_maxvalue.i,as.character(WgS_L1_maxdate.i),match(WgS_L1_maxdate.i.1,field_days$Date),
                        WgS_L1_minvalue.i,as.character(WgS_L1_mindate.i),match(WgS_L1_mindate.i.1,field_days$Date)) 
} 
colnames(WgS_L1_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgS_L1_check)

WgS_L1_NA <- is.na(dendro_WgS_raw.all$WgS_L1_dia_raw)
WgS_L1_NAdiff <- WgS_L1_NA[-1]
WgS_L1_noNA <- na.locf(dendro_WgS_raw.all$WgS_L1_dia_raw, na.rm = FALSE)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L1_dia_raw,type="l", col=1)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L1_noNA,type="l", col=2)
WgS_L1_dia_diff_noNA <- diff(WgS_L1_noNA)
WgS_L1_dia_diff.c <-ifelse((WgS_L1_dia_diff_noNA > 1 & WgS_L1_NAdiff == FALSE) | (WgS_L1_dia_diff_noNA < -1 & WgS_L1_NAdiff == FALSE),NA,WgS_L1_dia_diff_noNA) 
plot(WgS_Date_diff.px,WgS_L1_dia_diff.c,type="l", col=1)
boxplot(WgS_L1_dia_diff.c,main="WgS_L1"); boxplot(WgS_L1_dia_diff.c~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L1")
summary(WgS_L1_dia_diff.c)
WgS_L1_dia_diff.0 <- na.fill(WgS_L1_dia_diff.c,0);summary(WgS_L1_dia_diff.0)
WgS_L1_dia.c <- diffinv(WgS_L1_dia_diff.0)
WgS_L1_dia.NA <- is.na(dendro_WgS_raw.all$WgS_L1_dia_raw)
WgS_L1_dia.c <- ifelse(WgS_L1_dia.NA == TRUE, NA, WgS_L1_dia.c); summary(WgS_L1_dia.c)
plot(dendro_WgS_raw.all$Date_Time.px,WgS_L1_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgS_L1_dia.c)

# WgS_L2
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L2_dia_raw,type="l", col=1)
WgS_L2_dia_diff <- diff(dendro_WgS_raw.all$WgS_L2_dia_raw)
plot(WgS_Date_diff.px,WgS_L2_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgS_L2_dia_diff,main="WgS_L2"); boxplot(WgS_L2_dia_diff~WgS_year_diff.px,main="WgS_L2")
boxplot(WgS_L2_dia_diff,ylim=c(-1,1),main="WgS_L2"); boxplot(WgS_L2_dia_diff~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L2")
# check if 10 highest max and min values occured during field day
WgS_L2_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgS_L2_maxvalue.i <- sort(WgS_L2_dia_diff, TRUE)[i]
  WgS_L2_maxdate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L2_dia_diff == WgS_L2_maxvalue.i)]
  WgS_L2_maxdate.i.1 <- as.POSIXct(cut(WgS_L2_maxdate.i, "day"))
  WgS_L2_minvalue.i <- sort(WgS_L2_dia_diff, FALSE)[i]
  WgS_L2_mindate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L2_dia_diff == WgS_L2_minvalue.i)]
  WgS_L2_mindate.i.1 <- as.POSIXct(cut(WgS_L2_mindate.i, "day"))
  WgS_L2_check[i,] <- c(WgS_L2_maxvalue.i,as.character(WgS_L2_maxdate.i),match(WgS_L2_maxdate.i.1,field_days$Date),
                        WgS_L2_minvalue.i,as.character(WgS_L2_mindate.i),match(WgS_L2_mindate.i.1,field_days$Date)) 
} 
colnames(WgS_L2_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgS_L2_check)
WgS_L2_NA <- is.na(dendro_WgS_raw.all$WgS_L2_dia_raw)
WgS_L2_NAdiff <- WgS_L2_NA[-1]
WgS_L2_noNA <- na.locf(dendro_WgS_raw.all$WgS_L2_dia_raw, na.rm = FALSE)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L2_dia_raw,type="l", col=1)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L2_noNA,type="l", col=2)
WgS_L2_dia_diff_noNA <- diff(WgS_L2_noNA)
WgS_L2_dia_diff.c <-ifelse((WgS_L2_dia_diff_noNA > 1 & WgS_L2_NAdiff == FALSE) | (WgS_L2_dia_diff_noNA < -1 & WgS_L2_NAdiff == FALSE),NA,WgS_L2_dia_diff_noNA) 
plot(WgS_Date_diff.px,WgS_L2_dia_diff.c,type="l", col=1)
boxplot(WgS_L2_dia_diff.c,main="WgS_L2"); boxplot(WgS_L2_dia_diff.c~WgS_year_diff.px,main="WgS_L2")
summary(WgS_L2_dia_diff.c)
WgS_L2_dia_diff.0 <- na.fill(WgS_L2_dia_diff.c,0);summary(WgS_L2_dia_diff.0)
WgS_L2_dia.c <- diffinv(WgS_L2_dia_diff.0)
WgS_L2_dia.NA <- is.na(dendro_WgS_raw.all$WgS_L2_dia_raw)
WgS_L2_dia.c <- ifelse(WgS_L2_dia.NA == TRUE, NA, WgS_L2_dia.c)
plot(dendro_WgS_raw.all$Date_Time.px,WgS_L2_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgS_L2_dia.c)

# WgS_L3
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L3_dia_raw,type="l", col=1)
WgS_L3_dia_diff <- diff(dendro_WgS_raw.all$WgS_L3_dia_raw)
plot(WgS_Date_diff.px,WgS_L3_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgS_L3_dia_diff,main="WgS_L3"); boxplot(WgS_L3_dia_diff~WgS_year_diff.px,main="WgS_L3"); 
boxplot(WgS_L3_dia_diff,ylim=c(-1,1),main="WgS_L3"); boxplot(WgS_L3_dia_diff~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L3")
# check if 10 highest max and min values occured during field day
WgS_L3_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgS_L3_maxvalue.i <- sort(WgS_L3_dia_diff, TRUE)[i]
  WgS_L3_maxdate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L3_dia_diff == WgS_L3_maxvalue.i)]
  WgS_L3_maxdate.i.1 <- as.POSIXct(cut(WgS_L3_maxdate.i, "day"))
  WgS_L3_minvalue.i <- sort(WgS_L3_dia_diff, FALSE)[i]
  WgS_L3_mindate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L3_dia_diff == WgS_L3_minvalue.i)]
  WgS_L3_mindate.i.1 <- as.POSIXct(cut(WgS_L3_mindate.i, "day"))
  WgS_L3_check[i,] <- c(WgS_L3_maxvalue.i,as.character(WgS_L3_maxdate.i),match(WgS_L3_maxdate.i.1,field_days$Date),
                        WgS_L3_minvalue.i,as.character(WgS_L3_mindate.i),match(WgS_L3_mindate.i.1,field_days$Date)) 
} 
colnames(WgS_L3_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgS_L3_check)
WgS_L3_NA <- is.na(dendro_WgS_raw.all$WgS_L3_dia_raw)
WgS_L3_NAdiff <- WgS_L3_NA[-1]
WgS_L3_noNA <- na.locf(dendro_WgS_raw.all$WgS_L3_dia_raw, na.rm = FALSE)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L3_dia_raw,type="l", col=1)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L3_noNA,type="l", col=2)
WgS_L3_dia_diff_noNA <- diff(WgS_L3_noNA)
WgS_L3_dia_diff.c <-ifelse((WgS_L3_dia_diff_noNA > 1 & WgS_L3_NAdiff == FALSE) | (WgS_L3_dia_diff_noNA < -1 & WgS_L3_NAdiff == FALSE),NA,WgS_L3_dia_diff_noNA) 
WgS_L3_dia_diff.c[which(WgS_L3_dia_diff == sort(WgS_L3_dia_diff, FALSE)[5])] <- NA
plot(WgS_Date_diff.px,WgS_L3_dia_diff.c,type="l", col=1)
boxplot(WgS_L3_dia_diff.c,main="WgS_L3"); boxplot(WgS_L3_dia_diff.c~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L3")
summary(WgS_L3_dia_diff.c)
WgS_L3_dia_diff.0 <- na.fill(WgS_L3_dia_diff.c,0);summary(WgS_L3_dia_diff.0)
WgS_L3_dia.c <- diffinv(WgS_L3_dia_diff.0)
WgS_L3_dia.NA <- is.na(dendro_WgS_raw.all$WgS_L3_dia_raw)
WgS_L3_dia.c <- ifelse(WgS_L3_dia.NA == TRUE, NA, WgS_L3_dia.c); summary(WgS_L3_dia.c)
plot(dendro_WgS_raw.all$Date_Time.px,WgS_L3_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgS_L3_dia.c)

# WgS_L4
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L4_dia_raw,type="l", col=1)
WgS_Date_diff <- dendro_WgS_raw.all$Date_Time[-1]
WgS_Date_diff.px <- as.POSIXct(WgS_Date_diff,format="%Y-%m-%d %H:%M", tz="GMT") 
WgS_L4_dia_diff <- diff(dendro_WgS_raw.all$WgS_L4_dia_raw)
plot(WgS_Date_diff.px,WgS_L4_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6)
boxplot(WgS_L4_dia_diff,main="WgS_L4"); boxplot(WgS_L4_dia_diff~WgS_year_diff.px,main="WgS_L4"); 
boxplot(WgS_L4_dia_diff,ylim=c(-1,1),main="WgS_L4"); boxplot(WgS_L4_dia_diff~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L4")
# check if 10 highest max and min values occured during field day
WgS_L4_check <- data.frame(matrix(nrow=10,ncol=6))
for (i in 1:10)
{
  WgS_L4_maxvalue.i <- sort(WgS_L4_dia_diff, TRUE)[i]
  WgS_L4_maxdate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L4_dia_diff == WgS_L4_maxvalue.i)]
  WgS_L4_maxdate.i.1 <- as.POSIXct(cut(WgS_L4_maxdate.i, "day"))
  WgS_L4_minvalue.i <- sort(WgS_L4_dia_diff, FALSE)[i]
  WgS_L4_mindate.i <- dendro_WgS_raw.all$Date_Time.px[which(WgS_L4_dia_diff == WgS_L4_minvalue.i)]
  WgS_L4_mindate.i.1 <- as.POSIXct(cut(WgS_L4_mindate.i, "day"))
  WgS_L4_check[i,] <- c(WgS_L4_maxvalue.i,as.character(WgS_L4_maxdate.i),match(WgS_L4_maxdate.i.1,field_days$Date),
                        WgS_L4_minvalue.i,as.character(WgS_L4_mindate.i),match(WgS_L4_mindate.i.1,field_days$Date)) 
} 
colnames(WgS_L4_check) <- c("max-value","max-date","field-day?","min-value","min-date","field-day?")
print(WgS_L4_check)

WgS_L4_NA <- is.na(dendro_WgS_raw.all$WgS_L4_dia_raw)
WgS_L4_NAdiff <- WgS_L4_NA[-1]
WgS_L4_noNA <- na.locf(dendro_WgS_raw.all$WgS_L4_dia_raw, na.rm = FALSE)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L4_dia_raw,type="l", col=1)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L4_noNA,type="l", col=2)
WgS_L4_dia_diff_noNA <- diff(WgS_L4_noNA)
WgS_L4_dia_diff.c <-ifelse((WgS_L4_dia_diff_noNA > 1 & WgS_L4_NAdiff == FALSE) | (WgS_L4_dia_diff_noNA < -1 & WgS_L4_NAdiff == FALSE),NA,WgS_L4_dia_diff_noNA) 
WgS_L4_dia_diff.c[which(WgS_L4_dia_diff == sort(WgS_L4_dia_diff, FALSE)[5])] <- NA
plot(WgS_Date_diff.px,WgS_L4_dia_diff.c,type="l", col=1)
boxplot(WgS_L4_dia_diff.c,main="WgS_L4"); boxplot(WgS_L4_dia_diff.c~WgS_year_diff.px,ylim=c(-1,1),main="WgS_L4")
summary(WgS_L4_dia_diff.c)
WgS_L4_dia_diff.0 <- na.fill(WgS_L4_dia_diff.c,0);summary(WgS_L4_dia_diff.0)
WgS_L4_dia.c <- diffinv(WgS_L4_dia_diff.0)
WgS_L4_dia.NA <- is.na(dendro_WgS_raw.all$WgS_L4_dia_raw)
WgS_L4_dia.c <- ifelse(WgS_L4_dia.NA == TRUE, NA, WgS_L4_dia.c); summary(WgS_L4_dia.c)
plot(dendro_WgS_raw.all$Date_Time.px,WgS_L4_dia.c,type="l", col=1); abline(v = field_days$Date, lty = 3); abline(v = jun26.2015, lty = 3, col="red"); abline(v = mar23.2015, lty = 3, col="red")
summary (WgS_L4_dia.c)

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_WgSL_corr.emf", height=8, width=12, pointsize=14)
par(mfcol=c(3,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
# Plot all trees of WgS (raw data)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L1_dia_raw,type="l", col=1,ylim=c(0,60))
lines(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L2_dia_raw,type="l", col=2)
lines(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L3_dia_raw,type="l", col=3)
lines(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L4_dia_raw,type="l", col=4)

# Plot all trees of WgS (differences)
plot(WgS_Date_diff.px,WgS_L1_dia_diff,type="l", col=1,ylim=c(-1,1.5)); abline(h=0.5); abline(h=0.4); abline(h=0.6) 
lines(WgS_Date_diff.px,WgS_L2_dia_diff,type="l", col=2)
lines(WgS_Date_diff.px,WgS_L3_dia_diff,type="l", col=3)
lines(WgS_Date_diff.px,WgS_L4_dia_diff,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")

# Plot all trees of WgS (cleaned data)
plot(dendro_WgS_raw.all$Date_Time.px,WgS_L1_dia.c,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L2_dia.c,type="l", col=2)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L3_dia.c,type="l", col=3)
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L4_dia.c,type="l", col=4)
legend("topleft",c("L1","L2","L3","L4"),pch=15,col=c(1,2,3,4),cex=1,bty="n")
dev.off()

win.metafile("H:/Data/Matsch_Catchment/SF/figures_dendro/Dendro2012-2015_WgSL_corr2.emf", height=12, width=12, pointsize=14)
par(mfcol=c(4,1), mar=c(3,3,3,2), oma=c(2,2,2,1), mgp=c(2,1,0),tcl=-0.2)
plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L1_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L1_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L2_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L2_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L3_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L3_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")

plot(dendro_WgS_raw.all$Date_Time.px,dendro_WgS_raw.all$WgS_L4_dia_raw,type="l", col=1,ylim=c(-15,60))
lines(dendro_WgS_raw.all$Date_Time.px,WgS_L4_dia.c,type="l", col=2)
legend("topleft",c("raw","cleared"),pch=15,col=c(1,2),cex=1,bty="n")
dev.off()

#####################################################################################################################

# combine and export cleaned data (differences + absolut values) ####
# absolut values
exp_SF1_dendro_abs <- data.frame(dendro_SF1_raw.all$Date_Time.px,SF1_L2_dia.c, SF1_L4_dia.c, SF1_L5_dia.c, SF1_L6_dia.c); colnames(exp_SF1_dendro_abs)[1] <-"Date_Time"
exp_SF2_dendro_abs <- data.frame(dendro_SF2_raw.all$Date_Time.px,SF2_L1_dia.c, SF2_L4_dia.c, SF2_L5_dia.c, SF2_L7_dia.c); colnames(exp_SF2_dendro_abs)[1] <-"Date_Time"
exp_SF3_dendro_abs <- data.frame(dendro_SF3_raw.all$Date_Time.px,SF3_L3_dia.c, SF3_L4_dia.c, SF3_L5_dia.c, SF3_L6_dia.c); colnames(exp_SF3_dendro_abs)[1] <-"Date_Time"
exp_SF4_dendro_abs <- data.frame(dendro_SF4_raw.all$Date_Time.px,SF4_L1_dia.c, SF4_L2_dia.c, SF4_L3_dia.c, SF4_L4_dia.c
                                 ,SF4_Z1_dia.c, SF4_Z2_dia.c, SF4_Z3_dia.c, SF4_Z4_dia.c); colnames(exp_SF4_dendro_abs)[1] <-"Date_Time"
exp_SF5_dendro_abs <- data.frame(dendro_SF5_raw.all$Date_Time.px,SF5_L1_dia.c, SF5_L2_dia.c, SF5_L3_dia.c, SF5_L4_dia.c
                                 ,SF5_Z1_dia.c, SF5_Z2_dia.c, SF5_Z3_dia.c, SF5_Z4_dia.c); colnames(exp_SF5_dendro_abs)[1] <-"Date_Time"
exp_WgN_dendro_abs <- data.frame(dendro_WgN_raw.all$Date_Time.px,WgN_Z1_dia.c, WgN_Z2_dia.c, WgN_Z3_dia.c, WgN_Z4_dia.c); colnames(exp_WgN_dendro_abs)[1] <-"Date_Time"
exp_WgS_dendro_abs <- data.frame(dendro_WgS_raw.all$Date_Time.px,WgS_L1_dia.c, WgS_L2_dia.c, WgS_L3_dia.c, WgS_L4_dia.c); colnames(exp_WgS_dendro_abs)[1] <-"Date_Time"

exp_dendro_abs <- merge(exp_SF1_dendro_abs,exp_SF2_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF3_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF4_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_SF5_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_WgN_dendro_abs,by="Date_Time",all=TRUE)
exp_dendro_abs <- merge(exp_dendro_abs,exp_WgS_dendro_abs,by="Date_Time",all=TRUE)

# differences
SF1_L5_dia_diff.c2 <- ifelse(SF1_L5_NAdiff == TRUE, NA, SF1_L5_dia_diff.c)
summary(SF1_L5_dia_diff.c); summary(SF1_L5_NAdiff); summary(SF1_L5_dia_diff.c2)
SF1_L2_dia_diff.c2 <- ifelse(SF1_L2_NAdiff == TRUE, NA, SF1_L2_dia_diff.c)
summary(SF1_L2_dia_diff.c); summary(SF1_L2_NAdiff); summary(SF1_L2_dia_diff.c2)
SF1_L6_dia_diff.c2 <- ifelse(SF1_L6_NAdiff == TRUE, NA, SF1_L6_dia_diff.c)
summary(SF1_L6_dia_diff.c); summary(SF1_L6_NAdiff); summary(SF1_L6_dia_diff.c2)
SF1_L4_dia_diff.c2 <- ifelse(SF1_L4_NAdiff == TRUE, NA, SF1_L4_dia_diff.c)
summary(SF1_L4_dia_diff.c); summary(SF1_L4_NAdiff); summary(SF1_L4_dia_diff.c2)

exp_SF1_dendro_diff <- data.frame(SF1_Date_diff.px,SF1_L2_dia_diff.c, SF1_L4_dia_diff.c, SF1_L5_dia_diff.c, SF1_L6_dia_diff.c); colnames(exp_SF1_dendro_diff)[1] <-"Date_Time"

SF2_L5_dia_diff.c2 <- ifelse(SF2_L5_NAdiff == TRUE, NA, SF2_L5_dia_diff.c)
summary(SF2_L5_dia_diff.c); summary(SF2_L5_NAdiff); summary(SF2_L5_dia_diff.c2)
SF2_L1_dia_diff.c2 <- ifelse(SF2_L1_NAdiff == TRUE, NA, SF2_L1_dia_diff.c)
summary(SF2_L1_dia_diff.c); summary(SF2_L1_NAdiff); summary(SF2_L1_dia_diff.c2)
SF2_L7_dia_diff.c2 <- ifelse(SF2_L7_NAdiff == TRUE, NA, SF2_L7_dia_diff.c)
summary(SF2_L7_dia_diff.c); summary(SF2_L7_NAdiff); summary(SF2_L7_dia_diff.c2)
SF2_L4_dia_diff.c2 <- ifelse(SF2_L4_NAdiff == TRUE, NA, SF2_L4_dia_diff.c)
summary(SF2_L4_dia_diff.c); summary(SF2_L4_NAdiff); summary(SF2_L4_dia_diff.c2)

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

WgS_L1_dia_diff.c2 <- ifelse(WgS_L1_NAdiff == TRUE, NA, WgS_L1_dia_diff.c)
summary(WgS_L1_dia_diff.c); summary(WgS_L1_NAdiff); summary(WgS_L1_dia_diff.c2)
WgS_L2_dia_diff.c2 <- ifelse(WgS_L2_NAdiff == TRUE, NA, WgS_L2_dia_diff.c)
summary(WgS_L2_dia_diff.c); summary(WgS_L2_NAdiff); summary(WgS_L2_dia_diff.c2)
WgS_L3_dia_diff.c2 <- ifelse(WgS_L3_NAdiff == TRUE, NA, WgS_L3_dia_diff.c)
summary(WgS_L3_dia_diff.c); summary(WgS_L3_NAdiff); summary(WgS_L3_dia_diff.c2)
WgS_L4_dia_diff.c2 <- ifelse(WgS_L4_NAdiff == TRUE, NA, WgS_L4_dia_diff.c)
summary(WgS_L4_dia_diff.c); summary(WgS_L4_NAdiff); summary(WgS_L4_dia_diff.c2)

WgN_Z1_dia_diff.c2 <- ifelse(WgN_Z1_NAdiff == TRUE, NA, WgN_Z1_dia_diff.c)
summary(WgN_Z1_dia_diff.c); summary(WgN_Z1_NAdiff); summary(WgN_Z1_dia_diff.c2)
WgN_Z2_dia_diff.c2 <- ifelse(WgN_Z2_NAdiff == TRUE, NA, WgN_Z2_dia_diff.c)
summary(WgN_Z2_dia_diff.c); summary(WgN_Z2_NAdiff); summary(WgN_Z2_dia_diff.c2)
WgN_Z3_dia_diff.c2 <- ifelse(WgN_Z3_NAdiff == TRUE, NA, WgN_Z3_dia_diff.c)
summary(WgN_Z3_dia_diff.c); summary(WgN_Z3_NAdiff); summary(WgN_Z3_dia_diff.c2)
WgN_Z4_dia_diff.c2 <- ifelse(WgN_Z4_NAdiff == TRUE, NA, WgN_Z4_dia_diff.c)
summary(WgN_Z4_dia_diff.c); summary(WgN_Z4_NAdiff); summary(WgN_Z4_dia_diff.c2)

exp_WgS_dendro_diff <- data.frame(WgS_Date_diff.px,WgS_L1_dia_diff.c2, WgS_L2_dia_diff.c2, WgS_L3_dia_diff.c2, WgS_L4_dia_diff.c2); colnames(exp_WgS_dendro_diff)[1] <-"Date_Time"
exp_WgN_dendro_diff <- data.frame(WgN_Date_diff.px,WgN_Z1_dia_diff.c2, WgN_Z2_dia_diff.c2, WgN_Z3_dia_diff.c2, WgN_Z4_dia_diff.c2); colnames(exp_WgN_dendro_diff)[1] <-"Date_Time"

exp_dendro_diff <- merge(exp_SF1_dendro_diff,exp_SF2_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF3_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF4_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_SF5_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_WgN_dendro_diff,by="Date_Time",all=TRUE)
exp_dendro_diff <- merge(exp_dendro_diff,exp_WgS_dendro_diff,by="Date_Time",all=TRUE)

# combine stem temperature data for export ####
names(dendro_SF1_raw);names(dendro_SF1_raw2015_1);names(dendro_SF1_raw2015_2)
dendro_temp_SF1 <- rbind(dendro_SF1_raw[,c(10,3,5,7,9)],
                            dendro_SF1_raw2015_1[c((match(tail(dendro_SF1_raw$Date_Time.px,1),dendro_SF1_raw2015_1$Date_Time.px)+1):(dim(dendro_SF1_raw2015_1)[1])),c(10,3,5,7,9)])
summary(dendro_temp_SF1)

names(dendro_SF2_raw);names(dendro_SF2_raw2015_1);names(dendro_SF2_raw2015_2)
dendro_temp_SF2 <- rbind(dendro_SF2_raw[,c(10,3,5,7,9)],
                            dendro_SF2_raw2015_1[c((match(tail(dendro_SF2_raw$Date_Time.px,1),dendro_SF2_raw2015_1$Date_Time.px)+1):(dim(dendro_SF2_raw2015_1)[1])),c(10,3,5,7,9)])
summary(dendro_temp_SF2)
names(dendro_SF3_raw);names(dendro_SF3_raw2015)
dendro_temp_SF3 <- rbind(dendro_SF3_raw[,c(10,3,5,7,9)],
                            dendro_SF3_raw2015[c((match(tail(dendro_SF3_raw$Date_Time.px,1),dendro_SF3_raw2015$Date_Time.px)+1):(dim(dendro_SF3_raw2015)[1])),c(10,3,5,7,9)])
summary(dendro_temp_SF3)

dendro_temp_SF5 <- dendro_SF5_raw2015[,c(18,3,5,7,9,11,13,15,17)]
summary(dendro_temp_SF5)

names(dendro_WgS_raw2015); names(dendro_WgN_raw2015)
dendro_temp_WgS <- dendro_WgS_raw2015[,c(10,3,5,7,9)]
dendro_temp_WgN <- dendro_WgN_raw2015[,c(10,3,5,7,9)]
summary(dendro_temp_WgS); summary(dendro_temp_WgN)

exp_dendro_temp <- merge(dendro_temp_SF1,dendro_temp_SF2,by="Date_Time.px",all=TRUE)
exp_dendro_temp <- merge(exp_dendro_temp,dendro_temp_SF3,by="Date_Time.px",all=TRUE)
exp_dendro_temp <- merge(exp_dendro_temp,dendro_temp_SF5,by="Date_Time.px",all=TRUE)
exp_dendro_temp <- merge(exp_dendro_temp,dendro_temp_WgN,by="Date_Time.px",all=TRUE)
exp_dendro_temp <- merge(exp_dendro_temp,dendro_temp_WgS,by="Date_Time.px",all=TRUE)

summary(exp_dendro_abs)
summary(exp_dendro_diff)
 
outfile.dendro_abs <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_2012-2015_abs.csv")  
write.csv(exp_dendro_abs,file=outfile.dendro_abs)

outfile.dendro_diff <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_2012-2015_diff.csv")  
write.csv(exp_dendro_diff,file=outfile.dendro_diff)

outfile.dendro_temp <- ("H:/Data/Matsch_Catchment/SF/dendro files for R/dendro_all_d2012-2015_temp.csv")  
write.csv(exp_dendro_temp,file=outfile.dendro_temp)

# continue to Dendrometer_data_load_merge_and-figures.R/Dendrometer_data_load_merge_and-figures_2012-2015.R 
# for figures and miss_values dendro.R/ miss_values dendro2021-2015.R for data imputation   

                         
                    