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
# Create a subset for the E-W section over Ross Bank CTD casts 51-57
RB <- subset(ctd_section, 51 <= stationId & stationId <= 57)
# Grid the data in the subset using the oce SectionGrid function
RBgrid <- sectionGrid(RB, p=seq(0,1000,10))
# Reverse the order of the stations so that it is presented in descending order which presents better as West on left and East on right side of plot
RBgrid2 <- sectionSort(RBgrid, decreasing = TRUE)
# Add topography to the plot
RossSeaBathy <- read.topo(file = "C:/Science Projects/Ross Sea/Data/Ross Sea Bathymetry/topo_198W_174W_78.5S_72.5S_1min.nc")
RossSeaBathy@data$z <- -1*RossSeaBathy@data$z
#dev.new()
#dev.new()
# Print figure to a TIFF file format
tiff("CTD51-57.tiff", width=6, height=6, units='in', res=1200, compression = 'lzw')
par(mfrow=c(3,2))
plot(RBgrid2, which="temperature", ztype = "image", zcol = cmocean('thermal'), zbreaks=seq(-2.2, 0.0, 0.1), showBottom = RossSeaBathy, legend.text = 'A', xlab="", ylim = c(600, 0))
text(2,550,expression("Temperature (\u00B0C)"), adj=0)
plot(RBgrid2, which="Fluor", ztype = "image", zcol = cmocean('algae'), zbreaks=seq(0, 7, 0.1), showBottom = RossSeaBathy, legend.text = 'B', xlab="", ylim = c(600, 0))
text(2,550,expression(paste("Fluorescence (mg Chl-a/", m^3,")")), adj=0)
plot(RBgrid2, which="salinity", ztype = "image", zcol = cmocean('haline'), zbreaks=seq(34.4, 34.6, 0.01), showBottom = RossSeaBathy, legend.text = 'C', xlab="", ylim = c(600, 0))
text(2,550,"Salinity", adj=0)
# 29 Jan 2024 - change from using secondary oxygen sensor to primary
#vplot(RBgrid2, which="Oxy_s", ztype = "image", zcol = cmocean('oxy'), zbreaks=seq(6, 8, 0.1), showBottom = RossSeaBathy, legend.text = 'D', xlab="", ylim = c(600, 0))
plot(RBgrid2, which="Oxy", ztype = "image", zcol = cmocean('oxy'), zbreaks=seq(6, 8.5, 0.1), showBottom = RossSeaBathy, legend.text = 'D', xlab="", ylim = c(600, 0))
text(2,550,"Oxygen (ml/L)", adj=0)
plot(RBgrid2, which="Sig", ztype = "image", zcol = cmocean('dense'), zbreaks=seq(27.6, 27.9, 0.01), showBottom = RossSeaBathy, legend.text = 'E', ylim = c(600, 0))
text(2,550,"Sigmat", adj=0)
plot(RBgrid2, which="Trans", ztype = "image", zcol = cmocean('matter'), zbreaks=seq(90, 100, 0.5), showBottom = RossSeaBathy, legend.text = 'F', ylim = c(600, 0))
text(2,550,"Transmission (%)", adj=0)
# Close of the TIFF image to force the write to file
dev.off()



