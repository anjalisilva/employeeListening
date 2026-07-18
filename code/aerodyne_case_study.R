# Anjali Silva
# 17 July 2026

# packages ------------------------------------------------------------
  
library(readxl)
library(dplyr)
library(janitor)
library(here)

# options ------------------------------------------------------------
# suppresses informational messages produced by dplyr::summarise()
# scipen = 999 stop scientific notation for salary data
options(dplyr.summarise.inform = FALSE, na.action = "na.exclude", scipen = 999)

# define workbook path ------------------------------------------------
  
workbook_path <- here::here(
  "data",
  "Case Dataset Aerodyne.xlsx")

# if file not found print an error to user
if (!file.exists(workbook_path)) {
  stop("File not found: ", workbook_path)}

# check workbook tabs ----------------------------------------------------

required_sheets <- c(
  "README",
  "Data_Dictionary",
  "Employees",
  "Job_Architecture",
  "Listening_Survey")

available_sheets <- readxl::excel_sheets(workbook_path)

missing_sheets <- setdiff(
  required_sheets,
  available_sheets)

# if file missing tabs, print message to user
if (length(missing_sheets) > 0) {
  stop("Missing workbook tabs: ", 
       paste(missing_sheets, collapse = ", ")
)} else {
  cat("All 5 required workbook tabs are present:\n",
  paste(available_sheets, collapse = "\n"),"\n")}


# read workbook tabs -----------------------------------------------------

readme <- readxl::read_excel(
  workbook_path,
  sheet = "README")

data_dictionary <- readxl::read_excel(
  workbook_path,
  sheet = "Data_Dictionary")

employees_raw <- readxl::read_excel(
  workbook_path,
  sheet = "Employees")

job_architecture_raw <- readxl::read_excel(
  workbook_path,
  sheet = "Job_Architecture")

listening_survey_raw <- readxl::read_excel(
  workbook_path,
  sheet = "Listening_Survey")

# check sheet sizes ------------------------------------------------------

sheet_sizes <- data.frame(
  sheet = required_sheets,
  rows = c(
    nrow(readme),
    nrow(data_dictionary),
    nrow(employees_raw),
    nrow(job_architecture_raw),
    nrow(listening_survey_raw)
  ),
  columns = c(
    ncol(readme),
    ncol(data_dictionary),
    ncol(employees_raw),
    ncol(job_architecture_raw),
    ncol(listening_survey_raw)
  )
)

# provide user with sheet sizes
cat("5 sheet sizes:\n")
print(sheet_sizes)

# inspect imported data --------------------------------------------------

# standardize column names into consistent lowercase snake_case
# done for consistency for columns that contains spaces, 
# capitalization, or special characters
employees_raw <- employees_raw |>
  janitor::clean_names()

job_architecture_raw <- job_architecture_raw |>
  janitor::clean_names()

listening_survey_raw <- listening_survey_raw |>
  janitor::clean_names()

# print column names for each analytical sheet
cat("\nEmployees columns:\n")
print(names(employees_raw))

cat("\nJob Architecture columns:\n")
print(names(job_architecture_raw))

cat("\nListening Survey columns:\n")
print(names(listening_survey_raw))


# inspect data types and sample values
cat("\nEmployees structure:\n")
dplyr::glimpse(employees_raw)

cat("\nJob Architecture structure:\n")
dplyr::glimpse(job_architecture_raw)

cat("\nListening Survey structure:\n")
dplyr::glimpse(listening_survey_raw)

# data quality checks ----------------------------------------------

# check if any duplicate employee IDs
employee_duplicates <- employees_raw |>
  dplyr::count(employee_id) |>
  dplyr::filter(n > 1)

survey_duplicates <- listening_survey_raw |>
  dplyr::count(employee_id) |>
  dplyr::filter(n > 1)

# check duplicate job family and level combinations
architecture_duplicates <- job_architecture_raw |>
  dplyr::count(job_family, level_code) |>
  dplyr::filter(n > 1)

duplicate_summary <- data.frame(
  dataset = c(
    "Employees",
    "Listening Survey",
    "Job Architecture"),
  duplicate_check = c(
    "Employee IDs",
    "Employee IDs",
    "Family and level combinations"),
  duplicates= c(
    nrow(employee_duplicates),
    nrow(survey_duplicates),
    nrow(architecture_duplicates)))

# no duplicate values present
print(duplicate_summary, row.names = FALSE) 

# summarize missing values -----------------------------------------------

# counts number of missing values in every column
employee_missing <- data.frame(
  variable = names(employees_raw),
  missing = colSums(is.na(employees_raw))) |>
  dplyr::filter(missing > 0)

architecture_missing <- data.frame(
  variable = names(job_architecture_raw),
  missing = colSums(is.na(job_architecture_raw))) |>
  dplyr::filter(missing > 0)

survey_missing <- data.frame(
  variable = names(listening_survey_raw),
  missing = colSums(is.na(listening_survey_raw))) |>
  dplyr::filter(missing > 0)

cat("\nMissing values in Employees:\n")
print(employee_missing)
# time_in_level_months     138
# months_since_last_promotion    1131
# base_salary     175

cat("\nMissing values in Job Architecture:\n")
print(architecture_missing)

cat("\nMissing values in Listening Survey:\n")
print(survey_missing)

# check binary employee variables ----------------------------------------

# below variables should only contain only 0 and 1
# useNA = "ifany" include missing values (NA) as a separate category
cat("\nManager flag values:\n")
print(table(employees_raw$manager_flag, useNA = "ifany"))

cat("\nPromotion outcome values:\n")
print(table(employees_raw$promoted_last_24mo, useNA = "ifany"))

cat("\nVoluntary turnover values:\n")
print(table(employees_raw$voluntary_turnover, useNA = "ifany"))

# check key numeric ranges -----------------------------------------------

# range check to ID implausible values
employee_ranges <- employees_raw |>
  dplyr::summarise(
    tenure_min = min(tenure_months, na.rm = TRUE),
    tenure_max = max(tenure_months, na.rm = TRUE),
    time_in_level_min = min(time_in_level_months, na.rm = TRUE),
    time_in_level_max = max(time_in_level_months, na.rm = TRUE),
    performance_min = min(performance_rating, na.rm = TRUE),
    performance_max = max(performance_rating, na.rm = TRUE),
    salary_min = min(base_salary, na.rm = TRUE),
    salary_max = max(base_salary, na.rm = TRUE))

cat("\nEmployee variable ranges:\n")
dplyr::glimpse(employee_ranges)


# check survey response ranges -------------------------------------------

# survey items should use a 1-to-5 scale
# finds min and max for every survey column except employee_id
survey_ranges <- listening_survey_raw |>
  dplyr::summarise(dplyr::across(-employee_id,
      list(minimum = ~ min(.x, na.rm = TRUE),
          maximum = ~ max(.x, na.rm = TRUE))))
cat("\nSurvey response ranges:\n")
dplyr::glimpse(survey_ranges)

# check survey coverage --------------------------------------------------

# compares employee IDs in survey with employee IDs in employee table
survey_coverage <- employees_raw |>
  dplyr::summarise(
    employees = dplyr::n(),
    survey_responses = sum(employee_id %in% listening_survey_raw$employee_id),
    response_rate = survey_responses / employees)

cat("\nSurvey coverage:\n")
print(survey_coverage)

# standardize text fields -----------------------------------------------

# important before joining employees_raw with job_architecture_raw
# because differences in capitalization and spacing can cause valid 
# records not to match

# get  distinct job-family label in alphabetical order
cat("\nEmployee job families:\n")
print(sort(unique(employees_raw$job_family)))

cat("\nJob Architecture job families:\n")
print(sort(unique(job_architecture_raw$job_family)))

cat("\nWorker types:\n")
print(sort(unique(employees_raw$worker_type)))

cat("\nSites:\n")
print(sort(unique(employees_raw$site)))

cat("\nJob levels:\n")
print(sort(unique(employees_raw$job_level)))

# standardized versions below
# - trimws() removes spaces at beginning or end of text value
# - tolower() converts text to lowercase
# - toupper() convert text to upper case; uppercase is appropriate for 
#   abbreviated job-level codes
employees <- employees_raw |>
  mutate(
    job_family = trimws(tolower(job_family)),
    job_level = trimws(toupper(job_level)),
    worker_type = trimws(tolower(worker_type)),
    business_unit = trimws(business_unit),
    site = trimws(site),
    gender = trimws(gender),
    age_band = trimws(age_band))

job_architecture <- job_architecture_raw |>
  mutate(job_family = trimws(tolower(job_family)),
         level_code = trimws(toupper(level_code)))

listening_survey <- listening_survey_raw


# confirm standardized job families -------------------------------------

cat("\nStandardized employee job families:\n")
print(sort(unique(employees$job_family)))

cat("\nStandardized architecture job families:\n")
print(sort(unique(job_architecture$job_family)))

# compare job family counts
family_count_check <- data.frame(
  table = c(
    "Employees before cleaning",
    "Employees after cleaning",
    "Job Architecture before cleaning",
    "Job Architecture after cleaning"),
  distinct_job_families = c(
    n_distinct(employees_raw$job_family),
    n_distinct(employees$job_family),
    n_distinct(job_architecture_raw$job_family),
    n_distinct(job_architecture$job_family)))

cat("\nDistinct job family counts:\n")
print(family_count_check)

# validate architecture matches ------------------------------------------

# employee records without a matching record in job-architecture table
unmatched_architecture <- employees |>
  dplyr::anti_join(job_architecture,
    by = c("job_family","job_level" = "level_code")) 

cat("\nEmployees without a matching job architecture record:",
  nrow(unmatched_architecture),"\n")

# stop analysis if any employee records do not match
if (nrow(unmatched_architecture) > 0) {
  print(unmatched_architecture |> distinct(job_family, job_level))
  stop("Employee records do not fully match the job architecture")}


# prepare job architecture -----------------------------------------------

# - highest_level_order ID highest level available within each job family
# - employee is at the top of the ladder when level_order == highest_level_order

# - levels_in_family counts how many job levels exist in each family and
#   this help ID families which have shorter career ladders
job_architecture <- job_architecture |>
  dplyr::group_by(job_family) |>
  dplyr::mutate(highest_level_order = max(level_order, na.rm = TRUE),
         levels_in_family = n_distinct(level_code)) |> 
  dplyr::ungroup()


# join architecture to employees -----------------------------------------

# left_join() because it keep all employees and adds the matching architecture fields
employees <- employees |>
  dplyr::left_join(job_architecture,
  by = c("job_family","job_level" = "level_code"))
cat("\nRows after architecture join:", nrow(employees),"\n") # 6000

# create structural ceiling measures -------------------------------------

# define analysis thresholds 
# - long time in level defined as 48 months (4 years) or more
# - 48-month threshold is an assumption and should later be tested 
#   using alternatives such as 36 and 60 months
long_time_threshold <- 48

# near top of pay band defined as 90% or more
pay_band_threshold <- 0.90

employees <- employees |>
  dplyr::mutate(
    # employee is at highest available level in job family
    top_of_ladder = level_order == highest_level_order,
    
    # employee has remained in current level for at least 48 months
    long_time_in_level = case_when(
      # preserves missing values without treating it as FALSE
      is.na(time_in_level_months) ~ NA,
      time_in_level_months >= long_time_threshold ~ TRUE,
      # for remaining rows that did not match earlier condition return FALSE
      TRUE ~ FALSE),
    
    # relative salary position within the assigned pay band
    pay_band_position = dplyr::case_when(
      # if base_salary is missing return a missing numeric value
      is.na(base_salary) ~ NA_real_,
      # if either salary-band minimum or maximum is missing return NA
      is.na(band_min) | is.na(band_max) ~ NA_real_,
      # if band maximum is equal to or less than band minimum return NA
      # done to prevent invalid calculation like division by zero
      band_max <= band_min ~ NA_real_,
      # for remaining valid remaining rows, calculate the employee’s position
      # within salary band
      TRUE ~ (base_salary - band_min) / (band_max - band_min)),
    
    # employee is at or above 90% of the pay band
    near_top_of_pay_band = dplyr::case_when(
      is.na(pay_band_position) ~ NA,
      pay_band_position >= pay_band_threshold ~ TRUE,
      TRUE ~ FALSE),
    
    # number of structural ceiling conditions experienced
    ceiling_count =
      dplyr::coalesce(as.integer(top_of_ladder), 0L) +
      # coalesce replaces NA with 0 
      dplyr::coalesce(as.integer(long_time_in_level), 0L) +
      dplyr::coalesce(as.integer(near_top_of_pay_band), 0L),
    
    # employee experiences at least two ceiling conditions
    high_constraint = ceiling_count >= 2)

# review structural ceiling measures 

cat("\nSummary of table:\n")
dplyr::glimpse(employees)

cat("\nTop of ladder:\n")
print(table(employees$top_of_ladder, useNA = "ifany"))

cat("\nLong time in level:\n")
print(table(employees$long_time_in_level, useNA = "ifany"))

cat("\nNear top of pay band:\n")
print(table(employees$near_top_of_pay_band, useNA = "ifany"))

cat("\nCeiling count:\n")
print(table(employees$ceiling_count, useNA = "ifany"))

cat("\nHigh structural constraint:\n")
print(table(employees$high_constraint, useNA = "ifany"))

cat("\nPay band position summary:\n")
print(summary(employees$pay_band_position))

# check salary values outside assigned pay bands 
pay_band_exceptions <- employees |>
  filter(!is.na(pay_band_position),
          pay_band_position < 0 | pay_band_position > 1)

cat("\nEmployees outside assigned pay bands:",
  nrow(pay_band_exceptions),"\n") # 209

# summarize structural ceilings - question 1 in case study -------------

# summarize by job family 
family_ceiling_summary <- employees |>
  # divide employee table into job-family groups
  dplyr::group_by(job_family) |>
  dplyr::summarise(
    # number of employees in each group
    employees = dplyr::n(),
    # first() returns common value without adding it to group_by()
    # levels_in_family is presumably same for every employee within a job family
    levels_in_family = dplyr::first(levels_in_family),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    # pct = percentage; proportion of employees at the top of the ladder
    top_of_ladder_pct = round(mean(top_of_ladder, na.rm = TRUE) * 100, 2),
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    # percentage of employees who have spent at least 48 months in current role 
    long_time_pct = round(mean(long_time_in_level, na.rm = TRUE) * 100, 2),
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    # percentage of employees whose salary is at or above 90% of assigned pay 
    near_band_top_pct = round(mean(near_top_of_pay_band, na.rm = TRUE) * 100, 2),
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    # percentage of employees who meet definition of high structural constraint
    # ceiling_count >= 2
    high_constraint_pct = round(mean(high_constraint, na.rm = TRUE) * 100, 2) ) |>
  dplyr::arrange(desc(high_constraint_pct))

# summarize by worker type
worker_type_ceiling_summary <- employees |>
  dplyr::group_by(worker_type) |>
  dplyr::summarise(
    employees = dplyr::n(),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    top_of_ladder_pct = round(mean(top_of_ladder, na.rm = TRUE) * 100, 2),
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    long_time_pct = round(mean(long_time_in_level, na.rm = TRUE) * 100, 2),
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    near_band_top_pct = round(mean(near_top_of_pay_band, na.rm = TRUE) * 100, 2),
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    high_constraint_pct = round(mean(high_constraint, na.rm = TRUE) * 100, 2)) |>
  dplyr::arrange(desc(high_constraint_pct))

# summarize by site
site_ceiling_summary <- employees |>
  dplyr::group_by(site) |>
  dplyr::summarise(
    employees = dplyr::n(),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    top_of_ladder_pct = round(mean(top_of_ladder, na.rm = TRUE) * 100, 2),
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    long_time_pct = round(mean(long_time_in_level, na.rm = TRUE) * 100, 2),
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    near_band_top_pct = round(mean(near_top_of_pay_band, na.rm = TRUE) * 100, 2),
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    high_constraint_pct = round(mean(high_constraint, na.rm = TRUE) * 100, 2)) |>
  dplyr::arrange(desc(high_constraint_pct))

# print ceiling summaries 
cat("\nStructural ceilings by job family:\n")
print(family_ceiling_summary |>
        arrange(desc(employees)))

cat("\nStructural ceilings by worker type:\n")
print(worker_type_ceiling_summary)

cat("\nStructural ceilings by site:\n")
print(site_ceiling_summary |>
        arrange(desc(employees)))

# save structural ceiling summaries
write.csv(family_ceiling_summary,
  here::here("tables", "family_ceiling_summary.csv"),
  row.names = FALSE)

write.csv(worker_type_ceiling_summary,
  here::here("tables", "worker_type_ceiling_summary.csv"),
  row.names = FALSE)

write.csv(site_ceiling_summary,
  here::here("tables", "site_ceiling_summary.csv"),
  row.names = FALSE)
