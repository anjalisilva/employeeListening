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
  anti_join(job_architecture,
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
  group_by(job_family) |>
  mutate(highest_level_order = max(level_order, na.rm = TRUE),
         levels_in_family = n_distinct(level_code)) |> 
  ungroup()


# join architecture to employees -----------------------------------------

employees <- employees |>
  left_join(job_architecture,
    by = c("job_family","job_level" = "level_code"))
cat("\nRows after architecture join:", nrow(employees),"\n")
