# plot_RossBank_CTD_TMCTD_locations_combined_surveys_with_bathy.R
# Blair Greenan
# Fisheries and Oceans Canada
# 05 Oct 2023
#
# Description: This script plot the CTD and TMCTD cast locations during the two occupations
# of Ross Bank on NBP1201. To be honest, this script is a bit of a hack. In order to be able
# to plot points on top of the filled contour map, I had to create a z-dimension for the data
# representing the points. This 3rd dimension gets ignored (after a warning message) and 
# geom_point seems happy enough to plot the point on top of the fill contour map.  If you try to do
# this with just x,y the function will fail because the the contour plot is a 3D surface mapped to 2D.
# Anyway, it seems to work.  Not elegantly.  There are a lot of lines in here from
# the previous version of this script that was just plotting points and adding geom_text labels.
# It could be cleaner, but not worth my time now.

library(ggplot2)
library(R.matlab)
library(R.oo)
# load the tidyverse and lubridate packages
library(tidyverse)
library(lubridate)
library(ncdf4)
library(cmocean)

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
save(dt2, file = "ctd_stations.rda")
# Create a data frame for the CTD central station position
dt3 <- data.frame(lat_dec_deg_center, lon_dec_deg_center)
save(dt3, file = "ctd_station_central.rda")
# Create a data frame for the TMCTD station positions
dt4 <- data.frame(lat_dec_deg_TMCTD, lon_dec_deg_TMCTD)
# Create a data frame for the repeat station positions
dt_repeat1 <- data.frame(lat_dec_deg_repeat1, lon_dec_deg_repeat1)
save(dt_repeat1, file = "ctd_station_repeat1.rda")
dt_repeat2 <- data.frame(lat_dec_deg_repeat2, lon_dec_deg_repeat2)
save(dt_repeat2, file = "ctd_station_repeat2.rda")
dt_repeat3 <- data.frame(lat_dec_deg_repeat3, lon_dec_deg_repeat3)
save(dt_repeat3, file = "ctd_station_repeat3.rda")
dt_repeat4 <- data.frame(lat_dec_deg_repeat4, lon_dec_deg_repeat4)
save(dt_repeat4, file = "ctd_station_repeat4.rda")
dt_repeat5 <- data.frame(lat_dec_deg_repeat5, lon_dec_deg_repeat5)
save(dt_repeat5, file = "ctd_station_repeat5.rda")
dt_repeat6 <- data.frame(lat_dec_deg_repeat6, lon_dec_deg_repeat6)
save(dt_repeat6, file = "ctd_station_repeat6.rda")


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


ggp <- ggplot(elevation_df, aes(x = Longitude, y = Latitude, z = Depth)) + 
#  geom_raster(aes(fill = Depth), interpolate = TRUE) + 
  geom_contour_filled() + 
#  scale_fill_cmocean(name = "deep", direction = -1) +
  #  geom_contour(aes(colour = after_stat(level))) +
  #  scale_color_cmocean(name = "deep", direction = -1) +
  xlab("Longitude (decimal degrees)") + 
  ylab("Latitude (decimal degrees)") +
  labs(fill = "Depth (m)") +
  scale_y_continuous(limits=c(-77.05,-76.3), breaks=c(-77.0, -76.8, -76.6, -76.4), labels=c("77.0S", "76.8S", "76.6S", "76.4S"))  + 
  scale_x_continuous(limits=c(176.8,180.5), breaks=c(177, 178, 179, 180), labels=c("177E", "178E", "179E", "180"))
#  scale_x_continuous(limits=c(176.8,180.5), breaks=c(177, 178, 179, 180), labels=c("177E", "178E", "179E", "180"), position = "top") +
#  theme(legend.position = "bottom")
#  + ggtitle("Ross Bank Survey 1")

# SeaHorse moooring location
dt_depth <- c(dt,0)
names(dt_depth) <- c("Lat", "Lon", "Depth")
dt_depth <- data.frame(dt_depth)
ggp <- ggp + geom_point(data=dt_depth, aes(x = Lon, y = Lat, z = Depth), color="black", size=2, shape = 15) +
  geom_label(data=dt_depth, aes(x = Lon, y = Lat, z = Depth, label="42"), nudge_x = 0, nudge_y = -0.025, size=3)

# CTD only stations
zero_depth2 <- c(0,0,0,0,0,0,0,0,0,0,0,0)
dt2_depth <- data.frame(dt2,zero_depth2)
# remove some stations that were also TMCTD and SeaHorse
dt2_depth <- dt2_depth[c(2,3,4,5,6,7,8,9,10,11,12),]
ggp <- ggp + geom_point(data=dt2_depth, aes(x = lon_dec_deg, y = lat_dec_deg, z = zero_depth2), color="red", size=2) +
  geom_label(data=dt2_depth, aes(x = lon_dec_deg, y = lat_dec_deg, z = zero_depth2, label=cast_labels[c(2,3,4,5,6,7,8,9,10,11,12)]), nudge_x = 0, nudge_y = -0.025, size=3)


# CTD station at top of the bank
#zero_depth3 <- 0
#dt3_depth <- data.frame(dt3,zero_depth3)
#ggp <- ggp + geom_point(data=dt3_depth, aes(x = lon_dec_deg_center, y = lat_dec_deg_center, z = zero_depth3), color="red", size=2)

# CTD station along diagonal section
zero_depthr2 <- 0
dtr2_depth <- data.frame(dt_repeat2,zero_depthr2)
ggp <- ggp + geom_point(data=dtr2_depth, aes(x = lon_dec_deg_repeat2, y = lat_dec_deg_repeat2, z = zero_depthr2), color="red", size=2) +
  geom_label(data=dtr2_depth, aes(x = lon_dec_deg_repeat2, y = lat_dec_deg_repeat2, z = zero_depthr2, label=cast_labels_repeat2), nudge_x = 0, nudge_y = -0.025, size=3)


# CTD station along diagonal section
zero_depthr3 <- 0
dtr3_depth <- data.frame(dt_repeat3,zero_depthr3)
ggp <- ggp + geom_point(data=dtr3_depth, aes(x = lon_dec_deg_repeat3, y = lat_dec_deg_repeat3, z = zero_depthr3), color="red", size=2) +
  geom_label(data=dtr3_depth, aes(x = lon_dec_deg_repeat3, y = lat_dec_deg_repeat3, z = zero_depthr3, label=cast_labels_repeat3), nudge_x = 0, nudge_y = -0.025, size=3)


# CTD station along diagonal section
zero_depthr5 <- 0
dtr5_depth <- data.frame(dt_repeat5,zero_depthr5)
ggp <- ggp + geom_point(data=dtr5_depth, aes(x = lon_dec_deg_repeat5, y = lat_dec_deg_repeat5, z = zero_depthr5), color="red", size=2) +
  geom_label(data=dtr5_depth, aes(x = lon_dec_deg_repeat5, y = lat_dec_deg_repeat5, z = zero_depthr5, label=cast_labels_repeat5), nudge_x = 0, nudge_y = -0.025, size=3)


# CTD station along diagonal section
zero_depthr6 <- 0
dtr6_depth <- data.frame(dt_repeat6,zero_depthr2)
ggp <- ggp + geom_point(data=dtr6_depth, aes(x = lon_dec_deg_repeat6, y = lat_dec_deg_repeat6, z = zero_depthr6), color="red", size=2) +
  geom_label(data=dtr6_depth, aes(x = lon_dec_deg_repeat6, y = lat_dec_deg_repeat6, z = zero_depthr6, label=cast_labels_repeat6), nudge_x = 0, nudge_y = -0.025, size=3)



# Trace metal stations
zero_depth4 <- c(0,0,0,0,0)
dt4_depth <- data.frame(dt4,zero_depth4)
ggp <- ggp + geom_point(data=dt4_depth, aes(x = lon_dec_deg_TMCTD, y = lat_dec_deg_TMCTD, z = zero_depth4), color="red", size=2) +
#  ggp <- ggp + geom_point(data=dt4_depth, aes(x = lon_dec_deg_TMCTD, y = lat_dec_deg_TMCTD, z = zero_depth4), color="red", size=2, shape=17) + # shape=17 is a triangle
  geom_label(data=dt4_depth, aes(x = lon_dec_deg_TMCTD, y = lat_dec_deg_TMCTD, z = zero_depth4, label=cast_labels_TMCTD), nudge_x = 0, nudge_y = -0.055, size=3)

# CTD casts at TMCTD stations
# central station
zero_depth_3 <- 0
dt3_depth <- data.frame(dt3,zero_depth_3)
ggp <- ggp + 
  geom_label(data=dt3_depth, aes(x = lon_dec_deg_center, y = lat_dec_deg_center, z = zero_depth_3, label=cast_labels_center), nudge_x = 0, nudge_y = -0.025, size=3)

# SE station
zero_depth_1 <- 0
dtr1_depth <- data.frame(dt_repeat1,zero_depth_1)
ggp <- ggp + 
  geom_label(data=dtr1_depth, aes(x = lon_dec_deg_repeat1, y = lat_dec_deg_repeat1, z = zero_depth_1, label=cast_labels_repeat1), nudge_x = 0, nudge_y = -0.025, size=3)

# S station
zero_depth_4 <- 0
dtr4_depth <- data.frame(dt_repeat1,zero_depth_4)
ggp <- ggp + 
  geom_label(data=dtr4_depth, aes(x = lon_dec_deg_repeat4, y = lat_dec_deg_repeat4, z = zero_depth_4, label=cast_labels_repeat4), nudge_x = 0, nudge_y = -0.025, size=3)



# Use ggsave to save a high resolution png file
#ggsave("RossBank_CTD_Surveys_ggsave.png", width = 10, height = 8, units = c("cm"), dpi = 1200, bg = "white", scale = 1.25)

# dev.off()

