

###Education datafile cleaning
# Load dataset (assuming it is a CSV file)
data <- read.csv("mypath/NGHS_education.csv")

# Household head
data <- data |> mutate(HH_head = ifelse(q01ac06 == 1, 1, 0))

# Female household head
data <- data |> mutate(head_female1 = ifelse(HH_head == 1, q01ac03, NA))
data <- data |> group_by(hhid) |> mutate(head_female = max(head_female1, na.rm = TRUE)) |> ungroup()
data <- data |> select(-head_female1)

# Rename education level column
data <- data |> rename(level_edu_comp = q02c06)

# ECCE (Early Childhood Care and Education) variable
data <- data |> mutate(ECCE_C = case_when(
  level_edu_comp == 0 ~ 1,
  level_edu_comp <= 5 ~ 1,
  level_edu_comp >= 6 ~ 0
))

# Primary Completed
data <- data |> mutate(PrimaryC = case_when(
  level_edu_comp >= 6 & level_edu_comp <= 8 ~ 1,
  TRUE ~ 0
))

# Lower Secondary Completed
data <- data |> mutate(L_SecondaryC = case_when(
  level_edu_comp >= 9 & level_edu_comp <= 11 ~ 1,
  level_edu_comp == 13 ~ 1,
  TRUE ~ 0
))

# Upper Secondary Completed
data <- data |> mutate(U_SecondaryC = case_when(
  level_edu_comp == 12 ~ 1,
  level_edu_comp == 14 ~ 1,
  TRUE ~ 0
))

# Lower TVET Completed
data <- data |> mutate(L_TVET_C = ifelse(level_edu_comp == 15, 1, 0))

# Upper TVET Completed
data <- data |> mutate(U_TVET_C = ifelse(level_edu_comp == 16, 1, 0))

# Save cleaned dataset
write.csv(data, "cleaned_data.csv", row.names = FALSE)