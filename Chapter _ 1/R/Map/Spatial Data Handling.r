
##########################
################################################################################
# SCRIPT: Convert CSV Files to Spatial (sf) Objects and Save as Shapefiles
# AUTHOR: Ryuto Minami
# DATE: [Insert Date]
# DESCRIPTION: 
# This script converts household survey data from CSV format into spatial (sf) 
# objects in R. It performs the following steps:
# 1. Reads CSV files containing household geographic data (waves 1-4).
# 2. Checks for missing latitude and longitude columns to prevent errors.
# 3. Converts the data into an `sf` object with geographic coordinates.
# 4. Transforms the coordinate system to UTM Zone 32N (EPSG: 32632) for Nigeria.
# 5. Plot the spatial data on a map of Nigeria.
################################################################################
##########################


# Load required libraries
library(dbplyr)
library(tidyverse)
library(sf)
library(ggplot2)

# Load household geo-data for each wave
wave1_geo <- read.csv("mypath/nga_householdgeovariables_y1.csv")
wave2_geo <- read.csv("mypath/nga_householdgeovars_y2.csv")
wave3_geo <- read.csv("mypth/nga_householdgeovars_y3.csv")
wave4_geo <- read.csv("mypath/nga_householdgeovars_y4.csv")

# Load Nigeria shapefile
N_map <- st_read("mypath/gadm41_NGA_1.shp")

# Ensure column names are consistent across all datasets
colnames(wave1_geo) <- tolower(colnames(wave1_geo))
colnames(wave2_geo) <- tolower(colnames(wave2_geo))
colnames(wave3_geo) <- tolower(colnames(wave3_geo))
colnames(wave4_geo) <- tolower(colnames(wave4_geo))

# Check if coordinates exist in each dataset
if (!("lon_dd_mod" %in% colnames(wave4_geo)) | !("lat_dd_mod" %in% colnames(wave4_geo))) {
  stop("Error: Missing longitude/latitude columns in wave4_geo dataset.")
}
if (!("lon_dd_mod" %in% colnames(wave3_geo)) | !("lat_dd_mod" %in% colnames(wave3_geo))) {
  stop("Error: Missing longitude/latitude columns in wave3_geo dataset.")
}
if (!("lon_dd_mod" %in% colnames(wave2_geo)) | !("lat_dd_mod" %in% colnames(wave2_geo))) {
  stop("Error: Missing longitude/latitude columns in wave2_geo dataset.")
}
if (!("lon_dd_mod" %in% colnames(wave1_geo)) | !("lat_dd_mod" %in% colnames(wave1_geo))) {
  stop("Error: Missing longitude/latitude columns in wave1_geo dataset.")
}

# Convert each dataset into sf objects
wave1_sf <- convert_to_sf("mypath/nga_householdgeovariables_y1.csv")
wave2_sf <- convert_to_sf("mypath/nga_householdgeovars_y2.csv")
wave3_sf <- convert_to_sf("mypath/nga_householdgeovars_y3.csv")
wave4_sf <- convert_to_sf("mypath/nga_householdgeovars_y4.csv")

# View structure of the spatial data
print(st_geometry(wave1_sf))
print(st_geometry(wave2_sf))
print(st_geometry(wave3_sf))
print(st_geometry(wave4_sf))

# Plot Wave 4 Data
ggplot(N_map) + 
  geom_sf(fill = "gray90", color = "black") + 
  geom_point(data = wave4_geo, aes(x = lon_dd_mod, y = lat_dd_mod), 
             color = "#5500ff", size = 1, alpha = 0.7) +
  theme_bw() +
  labs(title = "Wave 4 Household Locations", x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("~/Desktop/plot_wave4.png", width = 10, height = 8, dpi = 300)

# Plot Wave 3 Data
ggplot(N_map) + 
  geom_sf(fill = "gray90", color = "black") + 
  geom_point(data = wave3_geo, aes(x = lon_dd_mod, y = lat_dd_mod), 
             color = "#040008", size = 0.8, alpha = 0.7) +
  theme_bw() +
  labs(title = "Wave 3 Household Locations", x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("~/Desktop/plot_wave3.png", width = 10, height = 8, dpi = 300)

# Plot Wave 2 Data
ggplot(N_map) + 
  geom_sf(fill = "gray90", color = "black") + 
  geom_point(data = wave2_geo, aes(x = lon_dd_mod, y = lat_dd_mod), 
             color = "#0095ff", size = 0.8, alpha = 0.7) +
  theme_void() +
  labs(title = "Wave 2 Household Locations", x = "Longitude", y = "Latitude")

ggsave("~/Desktop/plot_wave2.pdf", width = 10, height = 8, dpi = 300)

# Plot Wave 1 Data
ggplot(N_map) + 
  geom_sf(fill = "gray90", color = "black") + 
  geom_point(data = wave1_geo, aes(x = lon_dd_mod, y = lat_dd_mod), 
             color = "#4c00ff", size = 0.8, alpha = 0.7) +
  theme_bw() +
  labs(title = "Wave 1 Household Locations", x = "Longitude", y = "Latitude") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
