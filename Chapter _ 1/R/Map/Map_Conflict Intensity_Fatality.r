
################################################################################
# SCRIPT: Conflict Intensity & Fatalities Visualization Map
# AUTHOR: Ryuto Minami
# DATE: 2024-10-01
# DESCRIPTION:
# This script visualizes conflict intensity across Nigerian states using a 
#
# 1. Loads required libraries (`ggplot2`, `sf`, `ggspatial`).
# 2. Ensures the `Conflict_Level` variable is a factor with predefined categories.
# 3. Defines a color scale for different conflict intensity levels.
# 4. Categorizes conflict events based on predefined breakpoints.
# 5. Creates a **ggplot** map:
#    - Choropleth layer for conflict intensity using `geom_sf()`.
#    - Proportional symbols representing fatalities using `geom_sf()`.
# 6. Customizes the map appearance with appropriate legends and themes.
################################################################################

# Load required libraries
library(ggplot2)
library(sf)
library(ggspatial)

# Ensure Conflict_Level is a factor
N_map$Conflict_Level <- factor(N_map$Conflict_Level, 
                               levels = c("No", "Very Low", "Low", "Moderate", "High", "Very High", "Extreme"))

# Define color scale for conflict events
conflict_colors <- c("No" = "#ffffff",
                     "Very Low" = "#f8dfe5",
                     "Low" = "#f7a8a8",
                     "Moderate" = "#fc9191",
                     "High" = "#dd4e4e",
                     "Very High" = "#ff352e",
                     "Extreme" = "#ff0008")

# Define categorical breaks explicitly
N_map$Conflict_Level <- cut(N_map$Event_by_state, 
                             breaks = c(0, 100, 250, 500, 700, 1000, 2000, Inf), 
                             labels = c("No", "Very Low", "Low", "Moderate", "High", "Very High", "Extreme"),
                             include.lowest = TRUE)

### Plot conflict intensity and fatalities
ggplot() +
  # Choropleth layer for conflict events with categorical scale
  geom_sf(data = N_map, aes(fill = Conflict_Level), color = "#494747") +
  scale_fill_manual(
    name = "Conflict Events",
    values = c("No"= "#ffffff",
               "Very Low" = "#f8dfe5",
               "Low" = "#f7a8a8", 
               "Moderate" = "#fc9191", 
               "High" = "#dd4e4e", 
               "Very High" = "#ff352e", 
               "Extreme" = "#ff0008"),
    drop = FALSE  # Ensure all levels are displayed
  ) +
  # Proportional symbols for fatalities with circular legend
  geom_sf(data = conflict_summary, aes(size = total_fatalities), shape = 21, color = "#f7f9f7", fill = "#000a007b") +
  scale_size_continuous(
    name = "Fatalities",
    range = c(1, 10),  
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
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

