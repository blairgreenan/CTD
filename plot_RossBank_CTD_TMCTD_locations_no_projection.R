# plot_RossSea_CTD_locations.R
# Blair Greenan
# Fisheries and Oceans Canada
# 8 May 2023
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

# Select survey 1 of Ross Bank
#CTD_RB1 <- which(CTDData$ctd.list[,1] %in% seq(37,57))
CTD_RB1 <- which(CTDData$ctd.list[,1] %in% c(37,38,40,41,42,43,44,45,46,47,49,50,51,52,54,55,56,57))
CTD_RB1_center <- which(CTDData$ctd.list[,1] %in% c(39,48,53))
CTD_RB1_TMCTD <- which(CTDData$ctd.list[,1] %in% c(37,39,41,46)) # stations with trace metal CTD casts

# latitude in decimal degrees
lat_dec_deg <- CTDData$ctd.list[CTD_RB1,8]
# latitude of central station in decimal degrees
lat_dec_deg_center <- CTDData$ctd.list[CTD_RB1_center[1],8]
# latitude of TMCTD stations in decimal degrees
lat_dec_deg_TMCTD <- CTDData$ctd.list[CTD_RB1_TMCTD,8]
# longitude in decimal degrees
lon_dec_deg <- CTDData$ctd.list[CTD_RB1,9]
# longitude of central station in decimal degrees
lon_dec_deg_center <- CTDData$ctd.list[CTD_RB1_center[1],9]
# longitude of TMCTD stations in decimal degrees
lon_dec_deg_TMCTD <- CTDData$ctd.list[CTD_RB1_TMCTD,9]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg <- (lon_dec_deg < 0)
lon_dec_deg[lon_dec_deg_neg] = 360 + lon_dec_deg[lon_dec_deg_neg]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg_center <- (lon_dec_deg_center < 0)
lon_dec_deg_center[lon_dec_deg_neg_center] = 360 + lon_dec_deg_center[lon_dec_deg_neg_center]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg_TMCTD <- (lon_dec_deg_TMCTD < 0)
lon_dec_deg_TMCTD[lon_dec_deg_neg_TMCTD] = 360 + lon_dec_deg_TMCTD[lon_dec_deg_neg_TMCTD]

cast_labels <- as.character(CTDData$ctd.list[CTD_RB1,1])
cast_labels_center <- "39,48,53"
cast_labels_TMCTD <- c("TM26", "TM27", "TM28", "TM29")
# Find Station numbers associated with the Ross Bank CTD casts
# Select survey 1 of Ross Bank
#STN_RB1 <- which(CTDData$ctd.data[,2] %in% seq(37,57))
#Station_RB1 <- unique(CTDData$ctd.data[STN_RB1,1])
# Select survey 2 of Ross Bank
#STN_RB2 <- which(CTDData$ctd.data[,2] %in% seq(74,80))
#Station_RB2 <- unique(CTDData$ctd.data[STN_RB2,1])

# Create a data frame for the CTD station positions
dt2 <- data.frame(lat_dec_deg, lon_dec_deg)
# Create a data frame for the CTD central station position
dt3 <- data.frame(lat_dec_deg_center, lon_dec_deg_center)
# Create a data frame for the TMCTD station positions
dt4 <- data.frame(lat_dec_deg_TMCTD, lon_dec_deg_TMCTD)

ggp <- ggplot(dt2, aes(x = lon_dec_deg, y = lat_dec_deg)) + 
  geom_point(color = "black", size = 1) + 
  geom_text(aes(label=cast_labels), nudge_x = 0.04, nudge_y = -0.02, size=2) + 
  geom_point(data=dt, aes(x = lon, y = lat), color="red", size=1) + 
  xlab("Longitude (decimal degrees)") + 
  ylab("Latitude (decimal degrees)") + 
  scale_y_continuous(limits=c(-77.05,-76.3), breaks=c(-77.0, -76.8, -76.6, -76.4), labels=c("77.0S", "76.8S", "76.6S", "76.4S"))  + 
  scale_x_continuous(limits=c(176.8,180.5), breaks=c(177, 178, 179, 180), labels=c("177E", "178E", "179E", "180")) 
#  + ggtitle("Ross Bank Survey 1")

# Add the central CTD station (3 casts)
ggp <- ggp +
  geom_point(data=dt3, aes(x = lon_dec_deg_center, y = lat_dec_deg_center), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_center, y = lat_dec_deg_center, label=cast_labels_center), nudge_x = 0, nudge_y = -0.02, size=2)

# Add the TMCTD stations (4 casts)
# for some reason ggplot will only allow the geom_text for the lenght of the original data (18) or length 1, so have to do this 4 times
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[1], y = lat_dec_deg_TMCTD[1]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[1], y = lat_dec_deg_TMCTD[1], label=cast_labels_TMCTD[1]), nudge_x = 0.15, nudge_y = 0.0, size=2)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[2], y = lat_dec_deg_TMCTD[2]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[2], y = lat_dec_deg_TMCTD[2], label=cast_labels_TMCTD[2]), nudge_x = 0.15, nudge_y = 0.0, size=2)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[3], y = lat_dec_deg_TMCTD[3]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[3], y = lat_dec_deg_TMCTD[3], label=cast_labels_TMCTD[3]), nudge_x = 0.15, nudge_y = 0.0, size=2)
ggp <- ggp +
  geom_point(data=dt4, aes(x = lon_dec_deg_TMCTD[4], y = lat_dec_deg_TMCTD[4]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg_TMCTD[4], y = lat_dec_deg_TMCTD[4], label=cast_labels_TMCTD[4]), nudge_x = 0.15, nudge_y = 0.0, size=2)

# Use ggsave to save a high resolution png file
#ggsave("RossBank_CTD_Survey1_ggsave.png", width = 10, height = 8, units = c("cm"), dpi = 1200, bg = "white", scale = 1.25)

# dev.off()

# Ross Bank Survey 2
dev.new()
# Select survey 2 of Ross Bank
CTD_RB2 <- which(CTDData$ctd.list[,1] %in% seq(74,80))
# Add trace metal CTD location for Survey 2
CTD_RB2_TMCTD <- CTDData$ctd.list[77,1]

# latitude in decimal degrees
lat_dec_deg2 <- CTDData$ctd.list[CTD_RB2,8]
# latitude in decimal degrees TMCTD
lat_dec_deg2_TMCTD <- CTDData$ctd.list[77,8]
# longitude in decimal degrees
lon_dec_deg2 <- CTDData$ctd.list[CTD_RB2,9]
# longitude in decimal degrees
lon_dec_deg2_TMCTD <- CTDData$ctd.list[77,9]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg2 <- (lon_dec_deg2 < 0)
lon_dec_deg2[lon_dec_deg_neg2] = 360 + lon_dec_deg2[lon_dec_deg_neg2]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg2_TMCTD <- (lon_dec_deg2_TMCTD < 0)
lon_dec_deg2_TMCTD[lon_dec_deg_neg2_TMCTD] = 360 + lon_dec_deg2_TMCTD[lon_dec_deg_neg2_TMCTD]

# labels for the dots on the plot (CTD casts # and TMCTD cast #)
cast_labels2 <- as.character(CTDData$ctd.list[CTD_RB2,1])
cast_labels2_TMCTD <- "TM37"

# Create a data frame for the CTD station positions
dt5 <- data.frame(lat_dec_deg2, lon_dec_deg2)
dt6 <- data.frame(lat_dec_deg2_TMCTD, lon_dec_deg_neg2_TMCTD)

ggp2 <- ggplot(dt5, aes(x = lon_dec_deg2, y = lat_dec_deg2)) + 
  geom_point(color = "black", size = 1) + 
  geom_text(aes(label=cast_labels2), nudge_x = 0.04, nudge_y = -0.02, size=2) + 
  xlab("Longitude (decimal degrees)") + 
  ylab("Latitude (decimal degrees)") + 
  scale_y_continuous(limits=c(-77.05,-76.3), breaks=c(-77.0, -76.8, -76.6, -76.4), labels=c("77.0S", "76.8S", "76.6S", "76.4S"))  + 
  scale_x_continuous(limits=c(176.8,180.5), breaks=c(177, 178, 179, 180), labels=c("177E", "178E", "179E", "180")) 
#  + ggtitle("Ross Bank Survey 2")

# Add the TMCTD station (1 casts)
ggp2 <- ggp2 +
  geom_point(data=dt6, aes(x = lon_dec_deg2_TMCTD[1], y = lat_dec_deg2_TMCTD[1]), color="black", size=1) +
  geom_text(aes(x = lon_dec_deg2_TMCTD[1], y = lat_dec_deg2_TMCTD[1], label=cast_labels2_TMCTD[1]), nudge_x = 0.15, nudge_y = 0.0, size=2)

# Use ggsave to save a high resolution png file
#ggsave("RossBank_CTD_Survey2_ggsave.png", width = 10, height = 8, units = c("cm"), dpi = 1200, bg = "white", scale = 1.25)