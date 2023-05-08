# plot_RossSea_CTD_locations.R
# Blair Greenan
# Fisheries and Oceans Canada
# 19 Sep 2022
#
# R script to plot a map of the Ross Sea using ETOPO1 and Natural Earth data
# https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/ice_surface/grid_registered/netcdf/
# http://www.naturalearthdata.com/downloads/10m-physical-vectors/ 
# The intent is to be able to create maps for plotting various figures
# related to data collected in the Ross Sea in 2011-12 at part of the PRISM
# field program
#
# NOTE: I seem to have an issue with the map being displayed in the figure
# window. If that happens, just run the basemap command below on its own.

# Set working directory to the location of the bathymetry data
setwd("C:/Users/greenanb/Documents/Science Projects/Current/Ross Sea/Data/Ross Sea Bathymetry")

library(ggplot2)
library(sf)
library(ggOceanMaps)
library(ggOceanMapsData)
library(ggspatial)

# path to shapefile for land, bathymetry and ice shelves
outPath <- "./Shapefiles"

# Set projection to Antarctic polar stereographic
projection <- "EPSG:3031"

dev.new()

# load the shapefile for the Ross Sea area based on extraction from the Natural Earth shapefiles
load(file = paste(outPath, "bs_shapes.rda", sep = "/"))

#dt <- data.frame(lon = c(160, 160, -160, -160), lat = c(60, 80, 80, 60))
# SeaHorse mooring position
dt <- data.frame(lon = 179.2532, lat = -76.6601)

# switch the working directory back to the location of the Figures directory for
# the Ross Bank paper
setwd("C:/Users/greenanb/Documents/Science Projects/Current/Ross Sea/Documents/Papers/Ross Bank/Figures/CTD")

# load the CTD data mat file
library(R.matlab)
library(R.oo)
CTDData <- readMat("nbp1201_ctd.mat")
# load the tidyverse and lubridate packages
library(tidyverse)
library(lubridate)

# Select survey 1 of Ross Bank
CTD_RB1 <- which(CTDData$ctd.list[,1] %in% seq(37,57))
# Select survey 2 of Ross Bank
CTD_RB2 <- which(CTDData$ctd.list[,1] %in% seq(74,80))
# latitude in decimal degrees
lat_dec_deg <- CTDData$ctd.list[CTD_RB1,8]
# longitude in decimal degrees
lon_dec_deg <- CTDData$ctd.list[CTD_RB1,9]
# convert negative longitude to positive by subtracting value from 360
lon_dec_deg_neg <- (lon_dec_deg < 0)
lon_dec_deg[lon_dec_deg_neg] = 360 + lon_dec_deg[lon_dec_deg_neg]

# Find Station numbers associated with the Ross Bank CTD casts
# Select survey 1 of Ross Bank
STN_RB1 <- which(CTDData$ctd.data[,2] %in% seq(37,57))
Station_RB1 <- unique(CTDData$ctd.data[STN_RB1,1])
# Select survey 2 of Ross Bank
STN_RB2 <- which(CTDData$ctd.data[,2] %in% seq(74,80))
Station_RB2 <- unique(CTDData$ctd.data[STN_RB2,1])

# Create a data frame for the CTD station positions
dt2 <- data.frame(lat_dec_deg, lon_dec_deg)

# Create plot using the ggOceanMaps & ggspatial packages
basemap(shapefiles = list(land = bs_land, glacier = bs_ice_shelves, bathy = bs_bathy, 
                          name = "AntarcticStereographic"), bathymetry = TRUE, glaciers = TRUE, 
        limits = c(165, -165, -78, -72), lon.interval = 5, lat.interval = 1, rotate = TRUE, base_size = 8) +  
  geom_spatial_point(data = dt2, aes(x = lon_dec_deg, y = lat_dec_deg), color = "black", size = 0.1) +
  geom_spatial_point(data = dt, aes(x = lon, y = lat), color = "red", size = 0.1)

# Use ggsave to save a high resolution png file
ggsave("RossBank_CTD_ggsave.png", width = 10, height = 8, units = c("cm"), dpi = 1200, bg = "white")

# dev.off()


