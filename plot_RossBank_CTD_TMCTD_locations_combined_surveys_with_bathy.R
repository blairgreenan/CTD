# plot_RossBank_CTD_TMCTD_locations_combined_surveys_with_bathy.R
# Blair Greenan
# Fisheries and Oceans Canada
# 12 Jul 2023
#
# Description: This script plot the CTD and TMCTD cast locations during the two occupations
# of Ross Bank on NBP1201. The first survey also show the location of the SeaHorse
# mooring (red dot) - there was a CTD cast (#42) at that location, but no bottle 
# data was collected.

library(ggplot2)
library(R.matlab)
library(R.oo)
# load the tidyverse and lubridate packages
library(tidyverse)
library(lubridate)

dev.new()

# SeaHorse mooring position
dt <- data.frame(lat = -76.6601, lon = 179.2532)

# load the CTD data mat file
CTDData <- readMat("nbp1201_ctd.mat")

# Select surveys 1 & 2 of Ross Bank
CTD_RB1 <- which(CTDData$ctd.list[,1] %in% c(42,45,46,47,49,50,51,52,54,55,56,57))
# Repeat stations
CTD_RB1_repeat1 <- which(CTDData$ctd.list[,1] %in% c(37,74))
CTD_RB1_repeat2 <- which(CTDData$ctd.list[,1] %in% c(38,75))
CTD_RB1_repeat3 <- which(CTDData$ctd.list[,1] %in% c(40,77))
CTD_RB1_repeat4 <- which(CTDData$ctd.list[,1] %in% c(41,78))
CTD_RB1_repeat5 <- which(CTDData$ctd.list[,1] %in% c(43,79))
CTD_RB1_repeat6 <- which(CTDData$ctd.list[,1] %in% c(44,80))
# Central station on Ross Bank
CTD_RB1_center <- which(CTDData$ctd.list[,1] %in% c(39,48,53,76))
# stations with trace metal CTD casts, note that since the survey 2 occupation
# of the central station on Ross Bank (index 77 in CTDData$ctd.list[,1]) is 
# cast TM37.
CTD_RB1_TMCTD <- which(CTDData$ctd.list[,1] %in% c(37,39,41,46,50,77)) 

# latitude in decimal degrees
lat_dec_deg <- CTDData$ctd.list[CTD_RB1,8]
# latitude of repeat1 station in decimal degrees
lat_dec_deg_repeat1 <- CTDData$ctd.list[CTD_RB1_repeat1[1],8]
# latitude of repeat2 station in decimal degrees
lat_dec_deg_repeat2 <- CTDData$ctd.list[CTD_RB1_repeat2[1],8]
# latitude of repeat3 station in decimal degrees
lat_dec_deg_repeat3 <- CTDData$ctd.list[CTD_RB1_repeat3[1],8]
# latitude of repeat4 station in decimal degrees
lat_dec_deg_repeat4 <- CTDData$ctd.list[CTD_RB1_repeat4[1],8]
# latitude of repeat5 station in decimal degrees
lat_dec_deg_repeat5 <- CTDData$ctd.list[CTD_RB1_repeat5[1],8]
# latitude of repeat6 station in decimal degrees
lat_dec_deg_repeat6 <- CTDData$ctd.list[CTD_RB1_repeat6[1],8]
# latitude of central station in decimal degrees
lat_dec_deg_center <- CTDData$ctd.list[CTD_RB1_center[1],8]
# latitude of TMCTD stations in decimal degrees
lat_dec_deg_TMCTD <- CTDData$ctd.list[CTD_RB1_TMCTD[1:5],8]
# longitude in decimal degrees
lon_dec_deg <- CTDData$ctd.list[CTD_RB1,9]
# longitude of repeat1 station in decimal degrees
lon_dec_deg_repeat1 <- CTDData$ctd.list[CTD_RB1_repeat1[1],9]
# longitude of repeat2 station in decimal degrees
lon_dec_deg_repeat2 <- CTDData$ctd.list[CTD_RB1_repeat2[1],9]
# longitude of repeat3 station in decimal degrees
lon_dec_deg_repeat3 <- CTDData$ctd.list[CTD_RB1_repeat3[1],9]
# longitude of repeat4 station in decimal degrees
lon_dec_deg_repeat4 <- CTDData$ctd.list[CTD_RB1_repeat4[1],9]
# longitude of repeat5 station in decimal degrees
lon_dec_deg_repeat5 <- CTDData$ctd.list[CTD_RB1_repeat5[1],9]
# longitude of repeat6 station in decimal degrees
lon_dec_deg_repeat6 <- CTDData$ctd.list[CTD_RB1_repeat6[1],9]
# longitude of central station in decimal degrees
lon_dec_deg_center <- CTDData$ctd.list[CTD_RB1_center[1],9]
# longitude of TMCTD stations in decimal degrees
lon_dec_deg_TMCTD <- CTDData$ctd.list[CTD_RB1_TMCTD[1:5],9]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg <- (lon_dec_deg < 0)
lon_dec_deg[lon_dec_deg_neg] = 360 + lon_dec_deg[lon_dec_deg_neg]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg_center <- (lon_dec_deg_center < 0)
lon_dec_deg_center[lon_dec_deg_neg_center] = 360 + lon_dec_deg_center[lon_dec_deg_neg_center]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg_TMCTD <- (lon_dec_deg_TMCTD < 0)
lon_dec_deg_TMCTD[lon_dec_deg_neg_TMCTD] = 360 + lon_dec_deg_TMCTD[lon_dec_deg_neg_TMCTD]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg_repeat1 <- (lon_dec_deg_repeat1 < 0)
lon_dec_deg_repeat1[lon_dec_deg_neg_repeat1] = 360 + lon_dec_deg_repeat1[lon_dec_deg_neg_repeat1]
lon_dec_deg_neg_repeat2 <- (lon_dec_deg_repeat2 < 0)
lon_dec_deg_repeat2[lon_dec_deg_neg_repeat2] = 360 + lon_dec_deg_repeat2[lon_dec_deg_neg_repeat2]
lon_dec_deg_neg_repeat3 <- (lon_dec_deg_repeat3 < 0)
lon_dec_deg_repeat3[lon_dec_deg_neg_repeat3] = 360 + lon_dec_deg_repeat3[lon_dec_deg_neg_repeat3]
lon_dec_deg_neg_repeat4 <- (lon_dec_deg_repeat4 < 0)
lon_dec_deg_repeat4[lon_dec_deg_neg_repeat4] = 360 + lon_dec_deg_repeat4[lon_dec_deg_neg_repeat4]
lon_dec_deg_neg_repeat5 <- (lon_dec_deg_repeat5 < 0)
lon_dec_deg_repeat5[lon_dec_deg_neg_repeat5] = 360 + lon_dec_deg_repeat5[lon_dec_deg_neg_repeat5]
lon_dec_deg_neg_repeat6 <- (lon_dec_deg_repeat6 < 0)
lon_dec_deg_repeat6[lon_dec_deg_neg_repeat6] = 360 + lon_dec_deg_repeat6[lon_dec_deg_neg_repeat6]


cast_labels <- as.character(CTDData$ctd.list[CTD_RB1,1])
cast_labels_repeat1 <- "37,74"
cast_labels_repeat2 <- "38,75"
cast_labels_repeat3 <- "40,77"
cast_labels_repeat4 <- "41,78"
cast_labels_repeat5 <- "43,79"
cast_labels_repeat6 <- "44,80"
cast_labels_center <- "39,48,53,76"
cast_labels_TMCTD <- c("TM26", "TM27,TM37", "TM28", "TM29", "TM30")

# Create a data frame for the CTD station positions
dt2 <- data.frame(lat_dec_deg, lon_dec_deg)
# Create a data frame for the CTD central station position
dt3 <- data.frame(lat_dec_deg_center, lon_dec_deg_center)
# Create a data frame for the TMCTD station positions
dt4 <- data.frame(lat_dec_deg_TMCTD, lon_dec_deg_TMCTD)
# Create a data frame for the repeat station positions
dt_repeat1 <- data.frame(lat_dec_deg_repeat1, lon_dec_deg_repeat1)
dt_repeat2 <- data.frame(lat_dec_deg_repeat2, lon_dec_deg_repeat2)
dt_repeat3 <- data.frame(lat_dec_deg_repeat3, lon_dec_deg_repeat3)
dt_repeat4 <- data.frame(lat_dec_deg_repeat4, lon_dec_deg_repeat4)
dt_repeat5 <- data.frame(lat_dec_deg_repeat5, lon_dec_deg_repeat5)
dt_repeat6 <- data.frame(lat_dec_deg_repeat6, lon_dec_deg_repeat6)


# load the GEBCO bathymetry
# following https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd
my_nc_data <- nc_open("C:/Science Projects/gebco_2023/gebco_2023_n-76.3_s-77.05_w176.8_e180.5.nc")
lat_bathy <- ncvar_get(my_nc_data, "lat")
lon_bathy <- ncvar_get(my_nc_data, "lon")
elevation_bathy <- ncvar_get(my_nc_data, "elevation")
# Create 2D matrix of lon and lat
lonlat <- as.matrix(expand.grid(lon_bathy,lat_bathy))
# Reshape the elevation array
elevation_long <- as.vector(elevation_bathy)
# Create data frame
elevation_df <- data.frame(cbind(lonlat, elevation_long))
# Name columns
colnames(elevation_df) <- c("Longitude","Latitude","Depth")


ggp <- ggplot(elevation_df, aes(x = Longitude, y = Latitude)) + 
  geom_raster(aes(fill = Depth)) + 
  xlab("Longitude (decimal degrees)") + 
  ylab("Latitude (decimal degrees)") + 
  scale_y_continuous(limits=c(-77.05,-76.3), breaks=c(-77.0, -76.8, -76.6, -76.4), labels=c("77.0S", "76.8S", "76.6S", "76.4S"))  + 
  scale_x_continuous(limits=c(176.8,180.5), breaks=c(177, 178, 179, 180), labels=c("177E", "178E", "179E", "180")) 
#  + ggtitle("Ross Bank Survey 1")

ggp <- ggp +
  geom_point(data=dt2, aes(x = lon_dec_deg, y = lat_dec_deg), color = "black", size = 1) + 
  geom_text(aes(label=cast_labels), nudge_x = 0.04, nudge_y = -0.02, size=3) + 
  geom_point(data=dt, aes(x = lon, y = lat), color="red", size=1)

# Add the central CTD station (3 casts)
ggp <- ggp +
  geom_point(data=dt3, aes(x = lon_dec_deg_center, y = lat_dec_deg_center), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_center, y = lat_dec_deg_center, label=cast_labels_center), nudge_x = 0, nudge_y = -0.02, size=3)

# Add the TMCTD stations (4 casts)
# for some reason ggplot will only allow the geom_text for the lenght of the original data (18) or length 1, so have to do this 4 times
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[1], y = lat_dec_deg_TMCTD[1]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[1], y = lat_dec_deg_TMCTD[1], label=cast_labels_TMCTD[1]), nudge_x = 0.15, nudge_y = 0.0, size=3)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[2], y = lat_dec_deg_TMCTD[2]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[2], y = lat_dec_deg_TMCTD[2], label=cast_labels_TMCTD[2]), nudge_x = 0.28, nudge_y = 0.0, size=3)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[3], y = lat_dec_deg_TMCTD[3]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[3], y = lat_dec_deg_TMCTD[3], label=cast_labels_TMCTD[3]), nudge_x = 0.15, nudge_y = 0.0, size=3)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[4], y = lat_dec_deg_TMCTD[4]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[4], y = lat_dec_deg_TMCTD[4], label=cast_labels_TMCTD[4]), nudge_x = 0.15, nudge_y = 0.0, size=3)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[5], y = lat_dec_deg_TMCTD[5]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[5], y = lat_dec_deg_TMCTD[5], label=cast_labels_TMCTD[5]), nudge_x = 0.15, nudge_y = 0.0, size=3)

# Add the repeat1 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat1, aes(x = lon_dec_deg_repeat1, y = lat_dec_deg_repeat1), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat1, y = lat_dec_deg_repeat1, label=cast_labels_repeat1), nudge_x = 0, nudge_y = -0.02, size=3)
# Add the repeat2 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat2, aes(x = lon_dec_deg_repeat2, y = lat_dec_deg_repeat2), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat2, y = lat_dec_deg_repeat2, label=cast_labels_repeat2), nudge_x = 0, nudge_y = -0.02, size=3)
# Add the repeat3 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat3, aes(x = lon_dec_deg_repeat3, y = lat_dec_deg_repeat3), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat3, y = lat_dec_deg_repeat3, label=cast_labels_repeat3), nudge_x = 0, nudge_y = -0.02, size=3)
# Add the repeat4 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat4, aes(x = lon_dec_deg_repeat4, y = lat_dec_deg_repeat4), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat4, y = lat_dec_deg_repeat4, label=cast_labels_repeat4), nudge_x = 0, nudge_y = -0.02, size=3)
# Add the repeat5 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat5, aes(x = lon_dec_deg_repeat5, y = lat_dec_deg_repeat5), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat5, y = lat_dec_deg_repeat5, label=cast_labels_repeat5), nudge_x = 0, nudge_y = -0.02, size=3)
# Add the repeat6 CTD station (2 casts)
ggp <- ggp +
  geom_point(data=dt_repeat6, aes(x = lon_dec_deg_repeat6, y = lat_dec_deg_repeat6), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_repeat6, y = lat_dec_deg_repeat6, label=cast_labels_repeat6), nudge_x = 0, nudge_y = -0.02, size=3)


# Use ggsave to save a high resolution png file
#ggsave("RossBank_CTD_Survey1_ggsave.png", width = 10, height = 8, units = c("cm"), dpi = 1200, bg = "white", scale = 1.25)

# dev.off()

