
################################################################################
# SCRIPT: Mapping Primary School Completion and Enrollment Rates in Nigeria
# AUTHOR: Ryuto Minami
# DATE: 2024-09-09
# DESCRIPTION:
# This script visualizes **primary school completion rates** and 
# **net enrollment rates** across Nigerian states using choropleth maps.
#
# STEPS:
# 1. Loads required spatial libraries (`ggplot2`, `sf`, `ggspatial`).
# 2. Reads in Nigerian state boundary shapefile (`N_map`).
# 3. Assigns **Primary Completion Rate** and **Net Enrollment Rate** to states.
# 4. Creates **ggplot2** maps:
#    - Choropleth map for **Primary Completion Rate**.
#    - Choropleth map for **Primary Net Enrollment Rate**.
# 5. Groups states into **six geopolitical regions** and calculates regional averages.
################################################################################

# Load Nigerian state boundary shapefile
N_map <- st_read("pass/gadm41_NGA_1.shp")

# View structure of the spatial data
print(N_map)

### **1. Assign Primary School Completion Rates to States**
N_map <- N_map |> 
  mutate(Primary_Completion_Rate = case_when(
    NAME_1 == "Borno" ~ 31, NAME_1 == "Yobe" ~ 40, NAME_1 == "Adamawa" ~ 73,
    NAME_1 == "Gombe" ~ 50, NAME_1 == "Taraba" ~ 63, NAME_1 == "Bauchi" ~ 45,
    NAME_1 == "Plateau" ~ 79, NAME_1 == "Kaduna" ~ 71, NAME_1 == "Kano" ~ 69,
    NAME_1 == "Katsina" ~ 63, NAME_1 == "Zamfara" ~ 45, NAME_1 == "Sokoto" ~ 36,
    NAME_1 == "Kebbi" ~ 40, NAME_1 == "Jigawa" ~ 38, NAME_1 == "Niger" ~ 59,
    NAME_1 == "Kogi" ~ 84, NAME_1 == "Kwara" ~ 81, NAME_1 == "Nasarawa" ~ 69,
    NAME_1 == "Benue" ~ 74, NAME_1 == "Oyo" ~ 91, NAME_1 == "Osun" ~ 92,
    NAME_1 == "Ondo" ~ 91, NAME_1 == "Ekiti" ~ 95, NAME_1 == "Ogun" ~ 86,
    NAME_1 == "Lagos" ~ 98, NAME_1 == "Rivers" ~ 96, NAME_1 == "Bayelsa" ~ 86,
    NAME_1 == "Delta" ~ 92, NAME_1 == "Edo" ~ 90, NAME_1 == "Anambra" ~ 99,
    NAME_1 == "Enugu" ~ 95, NAME_1 == "Ebonyi" ~ 96, NAME_1 == "Imo" ~ 98,
    NAME_1 == "Abia" ~ 94, NAME_1 == "Akwa Ibom" ~ 94, NAME_1 == "Cross River" ~ 93,
    NAME_1 == "Federal Capital Territory" ~ 91, NAME_1 == "National" ~ 73
  ))

# View updated dataset
print(N_map)

### **2. Plot Primary School Completion Rate**
ggplot(N_map) +
  geom_sf(aes(fill = Primary_Completion_Rate)) +
  labs(title = "Primary School Completion by State") +
  scale_fill_continuous(high = "#047422", low = "#f2f0f7", name = "Completion Rate (%)") +
  theme_minimal() +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(3, "cm"))

# Save plot
ggsave("~/Desktop/primary_completion.pdf", width = 10, height = 10, dpi = 300)

### **3. Assign Primary Net Enrollment Rates to States**
N_map_enrollment <- N_map |> 
  mutate(Primary_Net_Enrollment = case_when(
    NAME_1 == "Abia" ~ 88.3, NAME_1 == "Adamawa" ~ 65.8, NAME_1 == "Akwa Ibom" ~ 85.5,
    NAME_1 == "Anambra" ~ 75.7, NAME_1 == "Bauchi" ~ 38.2, NAME_1 == "Bayelsa" ~ 86.4,
    NAME_1 == "Benue" ~ 75.7, NAME_1 == "Borno" ~ 34.5, NAME_1 == "Cross River" ~ 85.9,
    NAME_1 == "Delta" ~ 82.6, NAME_1 == "Ebonyi" ~ 83.7, NAME_1 == "Edo" ~ 87.5,
    NAME_1 == "Ekiti" ~ 84.5, NAME_1 == "Enugu" ~ 86.3, NAME_1 == "Gombe" ~ 43.6,
    NAME_1 == "Imo" ~ 85.7, NAME_1 == "Jigawa" ~ 53.5, NAME_1 == "Kaduna" ~ 77.0,
    NAME_1 == "Kano" ~ 63.5, NAME_1 == "Katsina" ~ 64.0, NAME_1 == "Kebbi" ~ 44.0,
    NAME_1 == "Kogi" ~ 85.3, NAME_1 == "Kwara" ~ 72.2, NAME_1 == "Lagos" ~ 92.9,
    NAME_1 == "Nasarawa" ~ 64.7, NAME_1 == "Niger" ~ 55.7, NAME_1 == "Ogun" ~ 79.3,
    NAME_1 == "Ondo" ~ 85.1, NAME_1 == "Osun" ~ 71.6, NAME_1 == "Oyo" ~ 80.7,
    NAME_1 == "Plateau" ~ 78.1, NAME_1 == "Rivers" ~ 87.5, NAME_1 == "Sokoto" ~ 44.2,
    NAME_1 == "Taraba" ~ 64.0, NAME_1 == "Yobe" ~ 41.4, NAME_1 == "Zamfara" ~ 37.5,
    NAME_1 == "Federal Capital Territory" ~ 86.9
  ))

### **4. Plot Primary Net Enrollment Rate**
ggplot(N_map_enrollment) +
  geom_sf(aes(fill = Primary_Net_Enrollment)) +
  labs(title = "Primary Net Enrollment Rate by State") +
  scale_fill_continuous(high = "#740458", low = "#f2f0f7", name = "Enrollment Rate (%)") +
  theme_minimal() +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(3, "cm"))

# Save plot
ggsave("~/Desktop/primary_net_enrollment.pdf", width = 10, height = 10, dpi = 300)

### **5. Group States by Region and Calculate Averages**
N_map <- N_map |> 
  mutate(region = case_when(
    NAME_1 %in% c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "FCT") ~ "North Central",
    NAME_1 %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe") ~ "North East",
    NAME_1 %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara") ~ "North West",
    NAME_1 %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo") ~ "South East",
    NAME_1 %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers") ~ "South South",
    NAME_1 %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo") ~ "South West",
    TRUE ~ "Unknown"
  ))

print(N_map)