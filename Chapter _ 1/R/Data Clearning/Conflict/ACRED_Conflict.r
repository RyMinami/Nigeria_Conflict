################################################################################
# SCRIPT: Conflict Fatalities (ACRED) Mapping in Nigeria (2009-2019)
# AUTHOR: Ryuto Minami
# DATE: [2024_09_09]
# DESCRIPTION:
# 1. Filters conflict data for Nigeria (2009-2019)
# 2. Aggregates fatalities by district (ADMIN2)
# 3. Loads Nigeria's administrative shapefile
# 4. Merges conflict data with spatial data
# 5. Generates maps showing fatalities across multiple waves
################################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggspatial)
library(sf)
library(RColorBrewer)

################################################################################
# Load and Filter Conflict Data
################################################################################

# Load conflict dataset
full_conf_df <- read.csv("lacod_full.csv")

# Filter for Nigeria (2009-2019)
Nigeria_ACRED <- full_conf_df |> 
  filter(COUNTRY == "Nigeria", YEAR >= 2009, YEAR <= 2019)

################################################################################
###  Load and Process Shapefile Data
################################################################################

# Load Nigeria administrative boundaries shapefile
N_map <- st_read("pass/gadm41_NGA_2.shp")

# Rename column for consistency
N_map <- N_map |> rename(ADMIN2 = NAME_2)

################################################################################
###  Function to Aggregate Fatalities by District
################################################################################

aggregate_fatalities <- function(data, start_year, end_year) {
  data |> 
    filter(YEAR >= start_year, YEAR <= end_year) |> 
    group_by(ADMIN2) |> 
    summarise(FATALITIES = sum(FATALITIES, na.rm = TRUE)) |> 
    ungroup()
}

################################################################################
### Function to Generate Conflict Maps
################################################################################

generate_conflict_map <- function(N_map, fatalities_df, title, filename) {
  # Merge fatalities with spatial data
  N_map <- left_join(N_map, fatalities_df, by = "ADMIN2")
  N_map$FATALITIES[is.na(N_map$FATALITIES)] <- 0  # Replace NA with zero fatalities

  # Plot the map
  plot <- ggplot(N_map) +
    geom_sf(aes(fill = FATALITIES), color = "black", size = 0.2) +
    scale_fill_continuous(name = "Fatalities", high = "#c11333", low = "#f2f0f7") +
    labs(title = title, subtitle = paste(start_year, "-", end_year)) +
    theme_minimal() +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering())

  # Save the plot as PNG
  ggsave(filename = filename, plot = plot, width = 10, height = 8, dpi = 300)
}

################################################################################
### Generate Maps for Overall and Wave-Based Fatalities
################################################################################

# Overall Fatalities (2009-2019)
fatalities_overall <- aggregate_fatalities(Nigeria_ACREDA, 2009, 2019)
generate_conflict_map(N_map, fatalities_overall, 
                      title = "Figure 1: Fatalities by District in Nigeria (2009-2019)", 
                      filename = "pass/plot_overall.png")

# Define wave time periods
waves <- list(
  list("wave" = "Wave 1", "start_year" = 2009, "end_year" = 2010, "filename" = "pass/plot_wave1.png"),
  list("wave" = "Wave 2", "start_year" = 2012, "end_year" = 2013, "filename" = "pass/plot_wave2.png"),
  list("wave" = "Wave 3", "start_year" = 2015, "end_year" = 2016, "filename" = "pass/plot_wave3.png"),
  list("wave" = "Wave 4", "start_year" = 2018, "end_year" = 2019, "filename" = "pass/plot_wave4.png")
)

# Generate maps for each wave
for (wave in waves) {
  fatalities_wave <- aggregate_fatalities(Nigeria_ACREDA, wave$start_year, wave$end_year)
  generate_conflict_map(N_map, fatalities_wave, 
                        title = paste("Figure:", wave$wave, "Fatalities by District in Nigeria"),
                        filename = wave$filename)
}

################################################################################
### Fatalities by Quantiles (2018-2019)
################################################################################

# Merge wave 4 data for quantile analysis
N_map_wave4 <- left_join(N_map, fatalities_wave, by = "ADMIN2")
N_map_wave4$FATALITIES[is.na(N_map_wave4$FATALITIES)] <- 0

# Create quantile-based bins for fatalities
N_map_wave4$FATALITIES_QUINTILE <- cut(N_map_wave4$FATALITIES, 
                                       breaks = quantile(N_map_wave4$FATALITIES, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                       labels = FALSE, include.lowest = TRUE)

# Plot quantile-based fatalities map
ggplot(N_map_wave4) +
  geom_sf(aes(fill = factor(FATALITIES_QUINTILE)), color = "black", size = 0.2) +
  scale_fill_brewer(name = "Fatalities Quintile", palette = "Reds") +
  labs(title = "Figure: Fatalities by District (Quintile - 2018-2019)", 
       subtitle = "Categorized into 5 groups") +
  theme_minimal() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())