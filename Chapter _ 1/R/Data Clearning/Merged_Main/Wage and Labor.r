


# Additional renaming and transformation
waged_data <- waged_data |> rename(
  Industry_Sector = m4ac8a,
  social_insurance = m4ac13c,
  official_contract = m4ac13a,
  monthly_wage = m4ac10,
  yearly_wage = m4ac11,
  paid_leave = m4ac13b,
  public_holiday = m4ac12a,
  currently_student = m2vc4,
  occupation_code = m4ac3,
  industry_code = m4ac4,
  relationship = m1ac3,
  marital_status = m1ac8,
  complete_grade = m2vc1,
  working_status = m4ac2,
  yearly_working_day = m4ac3a,
  working_hour_perdata = m4ac7,
  working_days_monthly = m4ac6,
  waged_job = m4ac9
)

# Compute additional variables
waged_data <- waged_data |> mutate(
  hourly_wage_2 = yearly_wage / 12 / (working_hour_perdata * 22),
  log_hourly_wage2 = log(hourly_wage_2),
  work_experience = case_when(
    Edu_general_comp %in% c(0, 1, 2) ~ age - 15,
    Edu_general_comp == 3 ~ age - 18,
    Edu_general_comp == 8 ~ age - 20,
    Edu_general_comp == 9 ~ age - 22,
    TRUE ~ NA_real_
  ),
  workexperience_squared = work_experience^2
)
# Rename variables
colnames(# Female household head
data <- data |> mutate(head_female1 = ifelse(HH_head == 1, q01ac03, NA))
data <- data |> group_by(hhid) |> mutate(head_female = max(head_female1, na.rm = TRUE)) |> ungroup()
data <- data |> select(head_female1)) <- c(
  "Industry_Sector", "social_insurance", "official_contract", "monthly_wage", "yearly_wage", 
  "paid_leave", "public_holiday", "currently_student", "occupation_code", "industry_code", 
  "relationship", "marital_status", "complete_grade", "working_status", "yearly_working_day", 
  "working_hour_perdata", "working_days_monthly", "waged_job"
)

# Create hourly wage variable
waged_data <- waged_data |> 
  mutate(
    hourly_wage_2 = yearly_wage / 12 / (working_hour_perdata * 22),
    log_hourly_wage2 = log(hourly_wage_2),
    work_experience = case_when(
      Edu_general_comp %in% c(0, 1, 2) ~ age - 15,
      Edu_general_comp == 3 ~ age - 18,
      Edu_general_comp == 8 ~ age - 20,
      Edu_general_comp == 9 ~ age - 22,
      TRUE ~ NA_real_
    ),
    # Create work experience variable
    workexperience_squared = work_experience^2,
    # Recode unemployment benefit
    unemployment_benefit = if_else(unemployment_benefit == 2, 0, unemployment_benefit),
     # Rename enterprise type
    type_enterprise = Industry_Sector,
    # Categorizing industry sectors
    industry_sector = case_when(
      industry_code %in% c(1, 2, 3, 110, 140, 160, 170) ~ 0,
      industry_code %in% c(5, 6, 7, 8, 9) ~ 1,
      industry_code %in% c(10, 11, 13:33) ~ 2,
      industry_code %in% c(35:39, 41:43) ~ 3,
      industry_code %in% c(45:47) ~ 4,
      industry_code %in% c(49:53) ~ 5,
      industry_code %in% c(55, 56) ~ 6,
      industry_code %in% c(58:63) ~ 7,
      industry_code %in% c(64:82) ~ 8,
      industry_code %in% c(84:96) ~ 9,
      industry_code %in% c(97, 98) ~ 10,
      TRUE ~ NA_real_
    ),
    # Industry classification
    industry_sector = if_else(industry_sector == 10, 0, industry_sector),
    industry_new = case_when(
      industry_sector == 0 ~ 0,
      industry_sector == 2 ~ 1,
      industry_sector == 9 ~ 2,
      !industry_sector %in% c(0, 1, 2) ~ 3,
      TRUE ~ NA_real_
    ),
    # Industry sector classification excluding agriculture
    industry_sector_drop_agri = case_when(
      industry_code %in% c(5:9) ~ 1,
      industry_code %in% c(10, 11, 13:33) ~ 2,
      industry_code %in% c(35:39, 41:43) ~ 3,
      industry_code %in% c(45:47) ~ 4,
      industry_code %in% c(49:53) ~ 5,
      industry_code %in% c(55, 56) ~ 6,
      industry_code %in% c(58:63) ~ 7,
      industry_code %in% c(64:82) ~ 8,
      industry_code %in% c(84:96) ~ 9,
      industry_code %in% c(97, 98) ~ 10,
      TRUE ~ NA_real_
    ),
    # Further refine ownership classification
    ownership_new = case_when(
      type_enterprise == 2 ~ 0,
      type_enterprise == 4 ~ 1,
      type_enterprise == 5 ~ 2,
      type_enterprise == 6 ~ 3,
      TRUE ~ NA_real_
    ),
    # Further refine ownership classification
    ownership_new_2 = case_when(
      enterprise_owner == 1 ~ 0,
      enterprise_owner == 3 ~ 1,
      enterprise_owner == 4 ~ 2,
      enterprise_owner == 5 ~ 3,
      TRUE ~ NA_real_
    ),
    # Industry sector classification for services
    industry_sector_service = case_when(
      industry_sector_drop_agri == 6 ~ 0,
      industry_sector_drop_agri == 1 ~ 1,
      industry_sector_drop_agri == 2 ~ 2,
      industry_sector_drop_agri == 3 ~ 3,
      industry_sector_drop_agri == 4 ~ 4,
      industry_sector_drop_agri == 5 ~ 5,
      industry_sector_drop_agri == 7 ~ 6,
      industry_sector_drop_agri == 8 ~ 7,
      industry_sector_drop_agri == 9 ~ 8,
      TRUE ~ NA_real_
    ),
    # Industry reference variable
    industry_service_ref = if_else(industry_sector_drop_agri == 6, 0, industry_sector_drop_agri)
  )
