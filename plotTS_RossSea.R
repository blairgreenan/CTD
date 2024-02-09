# plotTS_RossSea.R
# Blair Greenan
# Fisheries and Oceans Canada
# 03 Dec 2023
# Description: The script loads the NBP1201 CTD data from a .mat file and 
# water depth from a txt file that I created based on the soundings from the
# event log. The oce package is used to create ctd objects and the oce plotTS
# function is used to create TS plots.
# 
#
# load libraries
library(oce)
library(R.matlab)
library(R.oo)
library(tidyverse)
library(lubridate)

# load CTD Data from the .mat file
NBP1201_ctd <- readMat("nbp1201_ctd.mat")

# load the water depth at CTD station locations
# this information was compiled from the Cruise Event Log spreadsheet
Water_depth <- read.delim("CTD_STN_WATER_DEPTH.txt", header = FALSE, sep = "\n")

# create a data frame with the CTD data
data <- data.frame(NBP1201_ctd$ctd.data)

# extract the names of the columns in the CTD data and use the trimws
# function to trim the white space at the start and end of the names
names(data) <- trimws(substr(NBP1201_ctd$data.variables, 5, 14))

# CTD metadata (lat, lon, etc.) from the mat file
stn_list <- data.frame(NBP1201_ctd$ctd.list)

# extract the names of the columns in the CTD metadata and use the trimws
# function to trim the white space at the start and end of the names
names(stn_list) <- trimws(substr(NBP1201_ctd$list.variables, 5, 19))

# create a set of unique cast numbers (note that there is a cast 4.2,
# so that means there are 118 locations with 119 CTD casts)
cast <- unique(data[['CTD cast']])

# Edit the longitude so that it matches the convention in the Ross Sea bathymetry data
for (i in 1:length(stn_list[['longitude (deg)']])){
  if(stn_list[['longitude (deg)']][i]>0)
  {stn_list[['longitude (deg)']][i]<-stn_list[['longitude (deg)']][i]-360}
}

# create an empty list for the ctd object
ctd <- list()

# loop to populate the ctd object with data and metadata for each CTD cast
for (i in seq_along(cast)) {
  cat('Cast', cast[i], '...')
  II <- data[['CTD cast']] == cast[i]
  ## create the ctd object
  tmp <- as.ctd(salinity=data$Sal[II],
                temperature=data$Temp[II],
                pressure=data$Pres[II],
                station = data$`CTD cast`[II][1],
                cruise = "NBP1201",
                ship = "R/V Nathaniel B. Palmer") # just use the first element of the station vector in the data - oddity of the MAT file having a station # for each point in the CTD profile
  # add the other fields that were collected by the CTD system on the R/V NBP
  # I am going to ignore the Time vector in the data since it is not particularly useful
  fields <- names(data)
  fields <- fields[-which(fields %in% c('Station', 'Sal', 'Temp', 'Pres', 'Time'))]
  for (f in fields) {
    tmp <- oceSetData(tmp, f, data[[f]][II])
  }
  # add metadata latitude, longitude and start time of the CTD cast to the ctd object
  tmp <- oceSetMetadata(tmp, 'longitude', stn_list[['longitude (deg)']][i])
  tmp <- oceSetMetadata(tmp, 'latitude', stn_list[['latitude (deg)']][i])
  tmp <- oceSetMetadata(tmp, 'waterDepth', Water_depth[i,1])
  tmp <- oceSetMetadata(tmp, 'startTime',
                        as.POSIXct(
                          paste0(stn_list$year[i], '-', stn_list$month[i], '-', stn_list$day[i], ' ', stn_list$hour[i],
                                 ':', stn_list$minute[i], ':', stn_list$second[i]),
                          tz='UTC'))
  ctd[[i]] <- tmp
  cat('\n')
}

# Create a section using the function from the oce package
ctd_section <- as.section(ctd)
# Create a subset for the 2nd occupation of NE-SW sections over Ross Bank CTD casts 37-44 & 74-80
RB_37_44 <- subset(ctd_section, 37 <= stationId & stationId <= 44 & stationId != 42)
RB_74_80 <- subset(ctd_section, 74 <= stationId & stationId <= 80)
# Compare the same station occupied on Leg 1 and Leg 2 of Ross Bank Survey
RB_37 <- subset(ctd_section, stationId == 37)
RB_38 <- subset(ctd_section, stationId == 38)
RB_39 <- subset(ctd_section, stationId == 39)
RB_40 <- subset(ctd_section, stationId == 40)
RB_41 <- subset(ctd_section, stationId == 41)
RB_43 <- subset(ctd_section, stationId == 43)
RB_44 <- subset(ctd_section, stationId == 44)
RB_74 <- subset(ctd_section, stationId == 74)
RB_75 <- subset(ctd_section, stationId == 75)
RB_76 <- subset(ctd_section, stationId == 76)
RB_77 <- subset(ctd_section, stationId == 77)
RB_78 <- subset(ctd_section, stationId == 78)
RB_79 <- subset(ctd_section, stationId == 79)
RB_80 <- subset(ctd_section, stationId == 80)

# make some plots
png("TS_RB_Leg1_Leg2.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_37_44, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_74_80, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_37_74.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_37, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_74, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_38_75.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_38, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_75, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_39_76.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_39, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_76, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_40_77.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_40, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_77, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_41_78.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_41, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_78, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_43_79.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_43, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_79, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()

# make some plots
png("TS_RB_44_80.png", width = 4800, height = 2400, res = 600)
par(mfrow=c(1,2))
plotTS(RB_44, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
plotTS(RB_80, Slim = c(34.15, 34.75), Tlim = c(-2.1, 0.8))
dev.off()




