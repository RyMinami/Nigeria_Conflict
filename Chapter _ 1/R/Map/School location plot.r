################################################################################
# SCRIPT: Conflict Intensity and School Location Visualization
# AUTHOR: Ryuto Minami
# DATE: 2024-12-12
# DESCRIPTION:
# This script visualizes conflict intensity across Nigerian states and overlays 
# school locations using both ggplot2 and tmap for static and interactive maps.
#
# STEPS:
# Loads the required spatial libraries (`ggplot2`, `sf`, `ggspatial`, `tmap`).
# Reads in polygon (N_map) and point (school_sf) spatial data.
# Categorizes conflict intensity into five levels (Very Low → Very High).
# Creates a **ggplot** static map with:
#    - Conflict intensity (choropleth)
#    - School locations (point overlay)
#    - Aesthetic improvements (themes, legends, north arrow, scale).
# Uses **tmap** for an interactive OpenStreetMap visualization.
#############################################

# Load required libraries
library(ggplot2)
library(sf)
library(ggspatial)

###########################################################
##Check the point data (xSchool) and the polygon data (N_map)
ggplot() +
  geom_sf(data = N_map) +
  geom_sf(data = school_sf, color = "#0d0d0d", size = 0.5)

#  `conflict_colors` is defined here
# Define categorical bins for conflict intensity
N_map$conflict_category <- cut(N_map$Event_by_state,
                                          breaks = c(-Inf, 50, 100, 500, 1000, Inf),
                                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                                          include.lowest = TRUE)

# Define custom colors from white to red
conflict_colors <- c("Very Low" = "white",
                     "Low" = "#FFDDDD",
                     "Moderate" = "#FF9999",
                     "High" = "#FF4444",
                     "Very High" = "red")

# Plot the conflict intensity and school location map
ggplot() +
  geom_sf(data = N_map, aes(fill = conflict_category), color = "#877272", size = 0.3) +
  scale_fill_manual(name = "Conflict Intensity", values = conflict_colors, na.value = "gray90") +
  # Overlay school locations 
  geom_sf(data = school_sf, color = "#6c6f6c", size = 0.5, alpha = 0.3) +  
  # Improve aesthetics
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  # Add scale and north arrow
  annotation_scale(location = "bl") +  
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())

###########################################################################
##tmap creation
install.packages("tmap")
library(tmap)

tmap_mode("view")  # Interactive map mode
tm_shape(school_sf) + 
  tm_dots(col = "blue", size = 0.1) +
  tm_basemap("OpenStreetMap")

ggplot() + 
  geom_sf(data = N_map,point = "#ffffff", color = "black", size = 2) +
  geom_sf(data = school_sf, color = "#837e7e", size = 0.5) +  # Schools
  geom_sf(data = N_map, aes(fill = Event_by_state), color = "#030303", size = 0.3) +
 
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) 
