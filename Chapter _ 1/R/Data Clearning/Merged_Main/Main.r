
# ====================================================
# DATA CLEANING AND TRANSFORMATION SCRIPT

# Description:
# Data clearning for household and individual-level survey data.
# It performs the following steps:
# 1. Modify key variables (e.g., gender, enrollment, religion).
# 2. Rename key variables for clarity.
# 3. Generate new features, including household size and years of schooling.
# 4. Create a balanced panel dataset.
# ====================================================



# Recode Gender Variable: Convert ha_05 to "female" (1 = Male, 2 = Female → 0 = Male, 1 = Female)
data <- rename(data, female = ha_05)
data$female <- ifelse(data$female == 1, 0, ifelse(data$female == 2, 1, NA))

# Recode Enrollment Status: Convert (1 = No, 2 = Yes → 0 = No, 1 = Yes)
data$current_enrollment <- ifelse(data$current_enrollment == 1, 0, ifelse(data$current_enrollment == 2, 1, NA))

# Recode Religion: Keep only Cristian and non-Cristian groups, removing categories 3 and 4
data <- rename(data, religion = hb_23a)
data <- rename(data, Cristian = religion)
data <- filter(data, !(Cristian %in% c(3, 4)))
data$Cristian <- ifelse(data$Cristian == 2, 0, 1)

##Rename variables
colnames(data)[colnames(data) == "hb_03"] <- "marital_status"
colnames(data)[colnames(data) == "ha_06"] <- "relationship_head"
colnames(data)[colnames(data) == "sector"] <- "urban"

# Convert urban variable (1 = Urban, 2 = Rural → 1 = Urban, 0 = Rural)
data$urban <- ifelse(data$urban == 2, 0, 1)

colnames(data)[colnames(data) == "hd_43"] <- "main_job_type"
colnames(data)[colnames(data) == "ha_07"] <- "Age"

# STEP 3: Generate New Features
# Generate Household ID by wave
data$hhid_wave <- data$hhid * data$wave

# Calculate Household Size for each wave
data <- group_by(data, hhid_wave)
data$hh_size <- n()
data <- ungroup(data)

# Construct Years of Schooling Variable Based on Education Level
data$years_school <- case_when(
  data$highest_edu_completed %in% c(0) ~ 0,
  data$highest_edu_completed %in% c(1) ~ 1,
  data$highest_edu_completed %in% c(2) ~ 2,
  data$highest_edu_completed %in% c(3) ~ 3,
  data$highest_edu_completed %in% c(11) ~ 4,
  data$highest_edu_completed %in% c(12) ~ 5,
  data$highest_edu_completed %in% c(13) ~ 6,
  data$highest_edu_completed %in% c(14) ~ 7,
  data$highest_edu_completed %in% c(15) ~ 8,
  data$highest_edu_completed %in% c(16, 51, 52, 61) ~ 9,
  data$highest_edu_completed %in% c(21) ~ 10,
  data$highest_edu_completed %in% c(22) ~ 11,
  data$highest_edu_completed %in% c(23, 27, 321) ~ 12,
  data$highest_edu_completed %in% c(24) ~ 13,
  data$highest_edu_completed %in% c(25) ~ 14,
  data$highest_edu_completed %in% c(26, 28) ~ 15,
  data$highest_edu_completed %in% c(31, 32, 33, 34, 35, 41, 42) ~ 16,
  data$highest_edu_completed %in% c(322, 411, 412) ~ 17,
  data$highest_edu_completed %in% c(422, 423, 424) ~ 18,
  TRUE ~ NA_real_
)

# Generate Household Head Characteristics
data <- group_by(data, hhid_wave)
data$Age_hh <- max(ifelse(data$relationship_head == 1, data$Age, NA), na.rm = TRUE)
data$years_school_hh <- max(ifelse(data$relationship_head == 1, data$years_school, NA), na.rm = TRUE)
data$Cristian_hh <- max(ifelse(data$relationship_head == 1, data$Cristian, NA), na.rm = TRUE)
data <- ungroup(data)

# Define Primary School Age Group (6 to 11 years old)
data$Primary_age <- ifelse(data$Age >= 6 & data$Age <= 11, 1, NA)

#  Create Balanced Panel
# Keep relevant waves (2, 3, and 4)
data <- filter(data, wave %in% c(2, 3, 4))

# Generate unique household-individual ID
data$hhid_indiv <- data$hhid * data$indiv

# Count occurrences of each individual across waves and keep only balanced panel
data <- group_by(data, hhid_indiv)
data$count <- n()
data <- ungroup(data)
data <- filter(data, count == 3)

# Remove duplicate entries within the same wave
data <- group_by(data, hhid_indiv, wave)
data$isdup <- n() - 1
data <- ungroup(data)
data <- filter(data, isdup == 0)

#  Generate Indicators


# Generate Out-of-School Children Indicator
data$OOSC <- ifelse(data$edu_level_current_sy >= 11 & data$edu_level_current_sy <= 26, 0, 1)

# Identify Female Household Head
data$head_female1 <- ifelse(data$HH_head == 1, data$q01ac03, NA)
data <- group_by(data, hhid)
data$head_female <- max(data$head_female1, na.rm = TRUE)
data <- ungroup(data)
data <- select(data, -head_female1)

# Rename Education Level Column
data <- rename(data, level_edu_comp = q02c06)

# Early Childhood Care and Education (ECCE)
data$ECCE_C <- case_when(
  data$level_edu_comp == 0 ~ 1,
  data$level_edu_comp <= 5 ~ 1,
  data$level_edu_comp >= 6 ~ 0
)

# Primary Completion Indicator
data$PrimaryC <- ifelse(data$level_edu_comp >= 6 & data$level_edu_comp <= 8, 1, 0)

# Lower Secondary Completion Indicator
data$L_SecondaryC <- ifelse(data$level_edu_comp >= 9 & data$level_edu_comp <= 11 | data$level_edu_comp == 13, 1, 0)

# Upper Secondary Completion Indicator
data$U_SecondaryC <- ifelse(data$level_edu_comp == 12 | data$level_edu_comp == 14, 1, 0)

# Lower and Upper TVET Completion Indicators
data$L_TVET_C <- ifelse(data$level_edu_comp == 15, 1, 0)
data$U_TVET_C <- ifelse(data$level_edu_comp == 16, 1, 0)

# Female household head
data <- data |> mutate(head_female1 = ifelse(HH_head == 1, q01ac03, NA))
data <- data |> group_by(hhid) |> mutate(head_female = max(head_female1, na.rm = TRUE)) |> ungroup()
data <- data |> select(head_female1)


### 1. Merge Household Education & Labor Data
main_df <- left_join(main_df, education_h_df, by = c("wave", "hhid", "indiv")) |>
           left_join(Labor, by = c("wave", "hhid", "indiv"))

### 2. Rename Columns for Readability
main_df <- main_df |>
  rename(
    still_member = ha_02, gender = ha_05, relation_to_head = ha_06, new_member = ha_07, 
    Age = ha_08, marital = hb_11, Poly = hb_12, Num_wife = hb_13, Religion = hb_23a,
    Dad_ID = hb_25, Dad_Alive = hb_26, Dad_Edu_level = hb_27, Dad_Industry = hb_28_os, 
    Mom_live = hb_29, Mom_ID = hb_30, Mom_Alive = hb_31, Mom_Edu = hb_32, 
    Mom_Industry = hb_33_os, age3_above = hc_02, age5_above = hc_03, Own_Response = hc_04, 
    Response_On_Behalf = hc_05, read_ornot = hc_06, ever_attend_school = hc_07, 
    Start_age_school = hc_09, edu_completed = hc_10, Highest_qualification = hc_11, 
    Current_school_2019 = hc_12, Attend_school_2011_12 = hc_13, Edu_Level = hc_14, 
    Type_School_2019 = hc_15, Attended_school_2017_18 = hc_16, What_Level_2017_18 = hc_19, 
    Distance_school = hc_22, Ever_repeat = hc_29, Level_Last_repeated = hc_30, 
    How_many_repeat = hc_32, Plan_School_Nextyear = hc_33
  )

### 3. Select Relevant Variables
main_df <- main_df |>
  select(
    wave, visit.x, zone.x, state.x, lga.x, sector.x, ea.x, hhid, indiv, still_member, 
    gender, relation_to_head, new_member, Age, marital, Poly, Num_wife, Religion, 
    Dad_ID, Dad_Alive, Dad_Edu_level, Dad_Industry, Mom_live, Mom_ID, Mom_Alive, 
    Mom_Edu, Mom_Industry, age3_above, age5_above, Own_Response, Response_On_Behalf, 
    read_ornot, ever_attend_school, Start_age_school, edu_completed, 
    Highest_qualification, Current_school_2019, Attend_school_2011_12, Edu_Level, 
    Type_School_2019, Attended_school_2017_18, What_Level_2017_18, Distance_school, 
    Ever_repeat, Level_Last_repeated, How_many_repeat, Plan_School_Nextyear
  )

### 4. Recode Categorical Variables
main_df <- main_df |>
  mutate(
    # Convert Zones to Factor
    zone.x = factor(zone.x, levels = 1:6, 
                    labels = c("North Central", "North East", "North West", 
                               "South East", "South South", "South West")),
    
    # Convert States to Factor
    state.x = factor(state.x, levels = 1:37, 
                     labels = c("Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi", 
                                "Bayelsa", "Benue", "Borno", "Cross River", "Delta", 
                                "Ebonyi", "Edo", "Ekiti", "Enugu", "Gombe", "Imo", 
                                "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", 
                                "Kogi", "Kwara", "Lagos", "Nasarawa", "Niger", "Ogun", 
                                "Ondo", "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", 
                                "Taraba", "Yobe", "Zamfara", "FCT")),
    
    # Convert Religion to Factor
    Religion = factor(Religion, levels = 1:4, 
                      labels = c("Christianity", "Islam", "Traditional", "Others")),
    
    # Convert Urban/Rural to Factor
    sector.x = factor(sector.x, levels = 1:2, labels = c("Urban", "Rural")),
    
    # Convert Gender (Binary: 1 = Female, 0 = Male)
    female = factor(ifelse(gender == 2, 1, 0), levels = 0:1, labels = c("Male", "Female")),

    # Convert Marital Status to Factor
    marital = factor(marital, levels = 1:7, 
                     labels = c("Married Monogamous", "Married Poly", "Informal", 
                                "Divorced", "Widowed", "Separated", "Never Married"))
  )

### 5. Merge Household Data with Geospatial Data (Wave 4)
wave4_geo <- read.csv("pass/nga_householdgeovars_y4.csv")

# Merge geospatial data
main_geo_df <- left_join(main_df, wave4_geo, by = "hhid") |>
  select(-c(dist_road2:h2018_ndvi_max)) |>
  filter(!is.na(lat_dd_mod))  # Remove missing geospatial coordinates

# Convert Relationship to Head
main_geo_df <- main_geo_df |>
  mutate(relation_to_head = factor(relation_to_head, levels = 1:15, 
                                   labels = c("Head", "Spouse", "Own Child", "Step Child",
                                              "Adopted Child", "Grandchild", "Sibling", 
                                              "Niece/Nephew", "Sibling-in-law", "Parent", 
                                              "Parent-in-law", "Resident Domestic Help", 
                                              "Non-Resident Domestic Help", 
                                              "Other Relation", "Other Non-Relation")))

### Generate Household-Level Summary
main_geo_df <- main_geo_df |>
  mutate(hhid_wave = paste(hhid, wave, sep = "_")) |>
  group_by(hhid_wave) |>
  mutate(No_of_household_members = n()) |>
  ungroup()

  ##household data
df_household <- read.csv("pass/export_data.csv")
   
   ##Separate based on each wave
    df_wave2 <- df_household |>
    filter(wave == 2)

    df_wave3 <- df_household |>
    filter(wave == 3)

    df_wave4 <- df_household |>
    filter(wave == 4)


##conflict data
df_ACRED <- read.csv("pass/export_data2.csv")

#drop the missing values
df_household <- df_household |> 
    drop_na(lat_dd_mod, lon_dd_mod)
df_household

#converting to sf object
household_sf <- st_as_sf(df_household, coords = c("lat_dd_mod", "lon_dd_mod"), crs = 4326)
household_sf <- st_transform(household_sf, crs = 32632)

#drop the missing values
df_ACRED <- df_ACRED |> 
    drop_na(latitude, longitude)
df_ACRED

#converting to sf object
ACRED_sf <- st_as_sf(df_ACRED, coords = c("latitude", "longitude"), crs = 4326)
conflict_sf_utm <- st_transform(ACRED_sf, crs = 32632)

# Create 10 km buffer
household_sf$geometry <- st_buffer(household_sf$geometry, dist = 10000)  

##Filter wave 2
wave2_household_sf <- household_sf |> 
     filter(wave == 2)
wave2_ACRED <- conflict_sf_utm |> 
     filter(year == 2011)

# Spatial join: conflicts within buffers of households
joined_data_wave2 <- st_join(wave2_household_sf, wave2_ACRED, join = st_within)
    
# Count conflicts for each household
conflicts_count_wave2 <- joined_data_wave2 |>
     group_by(hhid) |>
     summarise(conflicts_last_year = n(), .groups = 'drop')

#merge back
     new_wave2 <- merge(df_wave2, conflicts_count_wave2, by = "hhid", all.x = TRUE)
     new_wave2$conflicts_last_year[is.na(new_wave2$conflicts_last_year)] <- 0


##Filter wave 3
wave3_household_sf <- household_sf |> 
     filter(wave == 3)
wave3_ACRED <- conflict_sf_utm |> 
     filter(year == 2014)

# Spatial join: conflicts within buffers of households
joined_data_wave3 <- st_join(wave3_household_sf, wave3_ACRED, join = st_within)
# Count conflicts for each household
conflicts_count_wave3 <- joined_data_wave3 |>
     group_by(hhid) |>
     summarise(conflicts_last_year = n(), .groups = 'drop')
    
#merge back
     new_wave3 <- merge(df_wave3, conflicts_count_wave3, by = "hhid", all.x = TRUE)
     new_wave3$conflicts_last_year[is.na(new_wave3$conflicts_last_year)] <- 0

##Filter wave 4
wave4_household_sf <- household_sf |> 
     filter(wave == 4)
wave4_ACRED <- conflict_sf_utm |> 
     filter(year == 2017)

# Spatial join: conflicts within buffers of households
joined_data_wave4 <- st_join(wave4_household_sf, wave4_ACRED, join = st_within)
# Count conflicts for each household
conflicts_count_wave4 <- joined_data_wave4 |>
     group_by(hhid) |>
     summarise(conflicts_last_year = n(), .groups = 'drop')
    

#merge back
     new_wave4 <- merge(df_wave4, conflicts_count_wave4, by = "hhid", all.x = TRUE)
     new_wave4$conflicts_last_year[is.na(new_wave4$conflicts_last_year)] <- 0

combined_df_1 <- bind_rows(new_wave2, new_wave3)
combined_df_final <- bind_rows(combined_df_1, new_wave4)

primary <- combined_df_final |>
filter(Age >= 6 & Age <= 11) 
view(primary)

 primary |>
    arrange(hhid, wave) 
view(primary)