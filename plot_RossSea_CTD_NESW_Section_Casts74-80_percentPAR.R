# plot_RossSea_CTD_Data.R
# Blair Greenan
# Fisheries and Oceans Canada
# 19 Jan 2023
# Update: 30 Jul 2023 - seems that there has been a change in the oce package in terms
# of how it handles water depth (pos/neg), so this required some changes to the script.
#
# Update: 29 Jan 2024 - applying the bottle calibrations for Chl and O2 that I got from 
# Dennis (email received on 23 Jan 2024).
#
# Update: 20 Feb 2024 - changed the T and S (UNESCO) plots to CT and SA (gsw) 
# NOTE: to see what derived values are available in the RBgrid2 section type RBgrid2[["?"]]
#
# load libraries
library(oce)
library(R.matlab)
library(R.oo)
library(tidyverse)
library(lubridate)
library(cmocean)

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

# 29 Jan 2024 - apply bottle calibrations
data$Fluor <- (data$Fluor+0.36)/1.8
data$Oxy <- (data$Oxy - 1.17)/0.8

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
# Create a subset for the 2nd occupation of NE-SW section over Ross Bank CTD casts 74-80
RB <- subset(ctd_section, 74 <= stationId & stationId <= 80)
# Grid the data in the subset using the oce SectionGrid function
RBgrid <- sectionGrid(RB, p=seq(0,1000,10))
# Reverse the order of the stations so that it is presented in descending order which presents better as West on left and East on right side of plot
RBgrid2 <- sectionSort(RBgrid, decreasing = TRUE)
# Add topography to the plot
RossSeaBathy <- read.topo(file = "C:/Science Projects/Ross Sea/Data/Ross Sea Bathymetry/topo_198W_174W_78.5S_72.5S_1min.nc")
RossSeaBathy@data$z <- -1*RossSeaBathy@data$z
#dev.new()
#dev.new()
# set option to gsw
options(oceEOS="gsw")
# Print figure to a TIFF file format
tiff("CTD74-80_gsw_percentPAR.tiff", width=6, height=6, units='in', res=1200, compression = 'lzw')
# Calculate the percent PAR (i.e., CTD PAR divided by Surface PAR)
percentPAR <- (RBgrid2[["Par"]]/RBgrid2[["Spar"]])*100
# Zoom in on the 0-10% PAR range and depth of 1-100m to get an idea about where the 1% light level is
plot(percentPAR, RBgrid2[["pressure"]], xlim = c(0, 10), ylim = c(0, 100))
# Close of the TIFF image to force the write to file
dev.off()



