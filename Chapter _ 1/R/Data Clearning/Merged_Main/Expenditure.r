
################################################################################
# SCRIPT: Household Expenditure Data Cleaning
# AUTHOR: [Ryuto Minami]
# DATE: [2024_08_07]
# DESCRIPTION:
# This script processes household expenditure data from multiple survey waves.
################################################################################

# Load necessary libraries
library(dplyr)
library(haven)

################################################################################
### 1. Load and Process Wave 4 Data
################################################################################

# Load wave4 dataset
df_wave4_nonfood <- read.csv("pass/S01C_HHRecallNonFood.csv")
df_wave4_food <- read.csv("pass/S01B_HHFoodConsumption.csv")

# Merge food and non-food data
df_wave4 <- df_wave4_food |> 
  left_join(df_wave4_nonfood, by = "hhid") |> 
  select(-urban, -strata, -zone, -province, -district, -commune, -village, 
         -operator_code, -hw20a, -hpw20a, -_merge)

# Assign survey wave
df_wave4$year <- 2

# Rename key variables
df_wave4 <- df_wave4 |> 
  rename(HH_Nonfood_total = q01cc06, HH_food_total = q01bc7) |> 
  mutate(Total_Expenditure = HH_food_total + HH_Nonfood_total)

################################################################################
### 2. Load and Process Wave 2 Data
################################################################################

# Load food consumption data
wave2_food <- read.csv("pass/hhfoodconsumption.csv")

# Aggregate food expenditure per household
wave2_food <- wave2_food |> 
  group_by(hhid) |> 
  summarise(HH_food_total = sum(q01bc05, na.rm = TRUE)) |> 
  ungroup()

# Load non-food expenditure data
wave2_nonfood <- read.csv("pass/hhrecallnonfood.csv")

# Aggregate non-food expenditure per household
wave2_nonfood <- wave2_nonfood |> 
  group_by(hhid) |> 
  summarise(HH_Nonfood_total = sum(q01cc06, na.rm = TRUE)) |> 
  ungroup()

# Merge datasets for wave 2
df_wave2 <- wave2_food |> 
  left_join(wave2_nonfood, by = "hhid") |> 
  mutate(
    HH_Nonfood_total = ifelse(is.na(HH_Nonfood_total), 0, HH_Nonfood_total),
    Total_Expenditure = HH_food_total + HH_Nonfood_total,
    year = 0
  )

################################################################################
### 3. Load and Process Wave 3 Data
################################################################################

# Load food consumption data
wave3_food <- read.csv("pass/hhfoodconsumption.dta")

# Aggregate food expenditure per household
wave3_food <- wave3_food |> 
  group_by(hhid) |> 
  summarise(HH_food_total = sum(q01bc05, na.rm = TRUE)) |> 
  ungroup()

# Remove unnecessary columns
wave3_food <- wave3_food |> select(-psu, -q01bc1, -q01cc01, -q01cc04)

# Assign survey wave
wave3_food$year <- 1

# Load non-food expenditure data
wave3_nonfood <- read.csv("pass/hhrecallnonfood.dta")

# Aggregate non-food expenditure per household
wave3_nonfood <- wave3_nonfood |> 
  group_by(hhid) |> 
  summarise(HH_Nonfood_total = sum(non_foodtotal, na.rm = TRUE)) |> 
  ungroup()

# Merge datasets for wave 3
df_wave3 <- wave3_food |> 
  left_join(wave3_nonfood, by = "hhid") |> 
  mutate(
    Total_Expenditure = HH_Nonfood_total + HH_food_total
  )

# Remove redundant columns
df_wave3 <- df_wave3 |> 
  select(-q01cc01, -q01cc04, -q01cc05, -q01bc01, -q01bc03, -q01bc04, -q01bc05, 
         -q01bc06, -non_foodtotal)

################################################################################
### 4. Combine All Waves into a Single Dataset
################################################################################

# Combine all datasets
df_combined <- bind_rows(df_wave2, df_wave3, df_wave4)

# Verify structure
str(df_combined)
