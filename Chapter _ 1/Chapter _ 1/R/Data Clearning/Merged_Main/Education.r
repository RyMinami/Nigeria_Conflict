

###Education datafile cleaning
# Load dataset (assuming it is a CSV file)

education_data <- read.csv("mypath/NGHS_education.csv")

education_data <- education_data |> 
  mutate(
    HH_head = ifelse(q01ac06 == 1, 1, 0),
    level_edu_comp = q02c06,
    ECCE_C = case_when(
      level_edu_comp == 0 ~ 1,
      level_edu_comp <= 5 ~ 1,
      level_edu_comp >= 6 ~ 0
    ),
    PrimaryC = case_when(
      level_edu_comp >= 6 & level_edu_comp <= 8 ~ 1,
      TRUE ~ 0
    ),
    L_SecondaryC = case_when(
      level_edu_comp >= 9 & level_edu_comp <= 11 ~ 1,
      level_edu_comp == 13 ~ 1,
      TRUE ~ 0
    ),
    U_SecondaryC = case_when(
      level_edu_comp == 12 ~ 1,
      level_edu_comp == 14 ~ 1,
      TRUE ~ 0
    ),
    L_TVET_C = ifelse(level_edu_comp == 15, 1, 0),
    U_TVET_C = ifelse(level_edu_comp == 16, 1, 0)
  )

data <- data |> mutate(U_TVET_C = ifelse(level_edu_comp == 16, 1, 0))

# Save cleaned dataset
write.csv(data, "cleaned_data.csv", row.names = FALSE)