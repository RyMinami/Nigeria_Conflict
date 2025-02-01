
################################################################################
# SCRIPT: Household & Conflict Data Processing with Multiple Buffer Zones_2
# AUTHOR: [Ryuto Minami]
# DATE: [2024-08-12]
# DESCRIPTION:
# This script performs the following tasks:
# 1. **Loads household & conflict datasets** 
# 2. **Prepares spatial data** (Convert to sf objects, handle missing values)
# 3. **Filters data by waves & applies spatial joins**:
#    - Finds **conflict events within 10 km, 20 km, and 50 km buffers** of households
#    - Counts conflict events **in the last year** for each household 
# 4. **Extracts primary school-aged children (6-11 years old)**  
################################################################################


###  Load Household & Conflict Data
df_household <- read.csv("pass/household_data.csv")
df_lacod <- read.csv("pass/lacod_data.csv")

###  Data Cleaning & Preprocessing

# Drop missing values in household data
df_household <- df_household |> drop_na(lat_dd_mod, lon_dd_mod)

# Convert household data to sf object (WGS 84 CRS) and transform to UTM
household_sf <- df_household |> 
    st_as_sf(coords = c("lat_dd_mod", "lon_dd_mod"), crs = 4326) |> 
    st_transform(crs = 32632)

# Drop missing values in conflict data
df_ACRED <- df_ACRED |> drop_na(latitude, longitude)

# Convert conflict data to sf object (WGS 84 CRS) and transform to UTM
conflict_sf_utm <- df_ACRED |> 
    st_as_sf(coords = c("latitude", "longitude"), crs = 4326) |> 
    st_transform(crs = 32632)

### Define a Function for Spatial Join & Conflict Count for Multiple Buffers
count_conflicts <- function(household_sf, conflict_sf, wave_year) {
    # Filter household and conflict data by wave/year
    wave_household_sf <- household_sf |> filter(wave == wave_year$wave)
    wave_conflict_sf <- conflict_sf |> filter(year == wave_year$year)
    
    # Create buffers (10km, 20km, 50km)
    buffers <- list(
        "conflicts_10km" = st_buffer(wave_household_sf, dist = 10000),
        "conflicts_20km" = st_buffer(wave_household_sf, dist = 20000),
        "conflicts_50km" = st_buffer(wave_household_sf, dist = 50000)
    )

    # Perform spatial joins and count conflicts for each buffer
    conflicts <- map(buffers, ~ 
        st_join(.x, wave_conflict_sf, join = st_within) |> 
        group_by(hhid) |> 
        summarise(count = n(), .groups = 'drop')
    )

    # Rename conflict count columns
    names(conflicts) <- names(buffers)

    # Merge conflict counts with household data
    wave_data <- df_household |> filter(wave == wave_year$wave)
    
    wave_data <- reduce(conflicts, left_join, by = "hhid") |> 
        mutate(across(starts_with("conflicts"), ~ replace_na(.x, 0)))

    return(wave_data)
}

###  Apply Function to Each Wave
waves_info <- list(
    list(wave = 2, year = 2011),
    list(wave = 3, year = 2014),
    list(wave = 4, year = 2017)
)

# Process each wave and combine results
combined_df_final <- waves_info |> 
    map(~ count_conflicts(household_sf, conflict_sf_utm, .x)) |> 
    bind_rows()

###  Extract Primary School-Aged Children (6-11 Years Old)
primary <- combined_df_final |> 
    filter(Age >= 6 & Age <= 11) |> 
    arrange(hhid, wave)

# View Final Processed Dataset
view(primary)





