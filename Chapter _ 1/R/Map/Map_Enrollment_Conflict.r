
################################################################################
# SCRIPT: Conflict Intensity and School Location Visualization
# AUTHOR: Ryuto Minami
# DATE: 2024-12-13
# DESCRIPTION:
# This script visualizes conflict intensity across Nigerian states and overlays 
#
# STEPS:
# Categorizes conflict intensity into five levels (Very Low → Very High).
# Creates a **ggplot** static map with:
#    - Conflict intensity (choropleth)
#    - School locations (point overlay)
#    - Aesthetic improvements (themes, legends, north arrow, scale).
# Uses **tmap** for an interactive OpenStreetMap visualization.
################################################################################

# Load required libraries
library(ggplot2)
library(sf)
library(ggspatial)
##############################################################
##Plot for conflict event and primary net enrollment
ggplot() +
  # Choropleth layer for conflict events with categorical scale
  geom_sf(data = N_map_enrollment, aes(fill = Primary_Net_Enrollment), color = "#494747") +
  scale_fill_gradient(low = "white", high = "blue", name = "Enrollment") +
  # Proportional symbols for fatalities with circular legend
  geom_sf(data = conflict_summary, aes(size = total_events), shape = 21, color = "#f7f9f7", fill = "#000a007b") +
  scale_size_continuous(
    name = "Conflict_Events",
    range = c(1, 20),  
  ) +
  guides(
    size = guide_legend(
      override.aes = list(shape = 21, fill = "gray"),
      title.position = "top",
      title.hjust = 0.5  # Center align legend title
    )
  ) +
  # Customize the map appearance
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

ggsave("mypath/conflict_fatalities_map.png", width = 10, height = 8, dpi = 300)
