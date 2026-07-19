# Aerodyne employee listening and job architecture case study
# Senior Research Data Scientist candidate analysis
#
# Author: Anjali Silva
#
# Purpose
# Investigate where employees experience structural career ceilings and assess
# how these conditions relate to career-growth sentiment, promotion and turnover
#
# Data
# Case Dataset Aerodyne.xlsx
# Employees, Job_Architecture and Listening_Survey tabs
#
# Output
# Reproducible summary tables saved to the tables folder
# Figures for the final presentation saved to the figures folder
#
# Analysis date: 17 July 2026

# packages ------------------------------------------------------------
  
library(readxl)
library(dplyr)
library(janitor)
library(here)
library(broom)
library(stats)

# assumptions ----------------------------------------------------------

# long time in level defined as 48 months or more
long_time_threshold <- 48

# near top of pay band defined as 90% or more
pay_band_threshold <- 0.90

# minimum group size used for subgroup reporting
minimum_group_size <- 30

# assume for Listening_Survey, a 1 to 5 agreement scale
# interpret 1 as strongly disagree and 5 as strongly agree for all listening items 
# higher values are favorable except for stuck_1: 
# - 1 Strongly disagree (unfavorable, except stuck 1 favorable)
# - 5 Strongly agree  (favorable, except stuck 1 unfavorable)

# employee experiences at least two ceiling conditions
# high_constraint = ceiling_count >= 2

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

# check workbook tabs and read content ---------------------------------------

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

# read workbook content 
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

# check sheet sizes 
sheet_sizes <- data.frame(
  sheet = required_sheets,
  rows = c(
    nrow(readme),
    nrow(data_dictionary),
    nrow(employees_raw),
    nrow(job_architecture_raw),
    nrow(listening_survey_raw)),
  columns = c(
    ncol(readme),
    ncol(data_dictionary),
    ncol(employees_raw),
    ncol(job_architecture_raw),
    ncol(listening_survey_raw)))

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


# question 1: identify and map structural ceilings -----------------------

# - prepare job architecture
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
# long_time_threshold <- 48

# near top of pay band defined as 90% or more
# pay_band_threshold <- 0.90

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

# summarize structural ceilings --------------------------

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
    top_of_ladder_pct = mean(top_of_ladder, na.rm = TRUE) * 100,
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    # percentage of employees who have spent at least 48 months in current role 
    long_time_pct = mean(long_time_in_level, na.rm = TRUE) * 100,
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    # percentage of employees whose salary is at or above 90% of assigned pay 
    near_band_top_pct = mean(near_top_of_pay_band, na.rm = TRUE) * 100,
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    # percentage of employees who meet definition of high structural constraint
    # ceiling_count >= 2
    high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100) |>
  dplyr::mutate(across(ends_with("_pct"), ~ round(.x, 2))) |>
  dplyr::arrange(desc(high_constraint_pct))

# summarize by worker type
worker_type_ceiling_summary <- employees |>
  dplyr::group_by(worker_type) |>
  dplyr::summarise(
    employees = dplyr::n(),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    top_of_ladder_pct = mean(top_of_ladder, na.rm = TRUE) * 100,
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    long_time_pct = mean(long_time_in_level, na.rm = TRUE) * 100,
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    near_band_top_pct = mean(near_top_of_pay_band, na.rm = TRUE) * 100,
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100) |>
  dplyr::mutate(across(ends_with("_pct"), ~ round(.x, 2))) |>
  dplyr::arrange(desc(high_constraint_pct))

# summarize by site
site_ceiling_summary <- employees |>
  dplyr::group_by(site) |>
  dplyr::summarise(
    employees = dplyr::n(),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    top_of_ladder_pct = mean(top_of_ladder, na.rm = TRUE) * 100,
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    long_time_pct = mean(long_time_in_level, na.rm = TRUE) * 100,
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    near_band_top_pct = mean(near_top_of_pay_band, na.rm = TRUE) * 100,
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100) |>
  dplyr::mutate(across(ends_with("_pct"), ~ round(.x, 2))) |>
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
  here::here("tables", "question1_table1_family_ceiling_summary.csv"),
  row.names = FALSE)

write.csv(worker_type_ceiling_summary,
  here::here("tables", "question1_table2_worker_type_ceiling_summary.csv"),
  row.names = FALSE)

write.csv(site_ceiling_summary,
  here::here("tables", "question1_table3_site_ceiling_summary.csv"),
  row.names = FALSE)

# summarize ceilings by job family and site -------------------------------

# - done to separate true site effect from differences caused by  mix 
#   of job families at each site
# - a site could have a high ceiling rate because it employs many people in 
#   short-ladder technician families, which doesn't mean the site has weaker management
family_site_ceiling_summary <- employees |>
  dplyr::group_by(site, job_family) |>
  dplyr::summarise(
    employees = dplyr::n(),
    top_of_ladder_n = sum(top_of_ladder, na.rm = TRUE),
    top_of_ladder_pct = mean(top_of_ladder, na.rm = TRUE) * 100,
    long_time_n = sum(long_time_in_level, na.rm = TRUE),
    long_time_pct = mean(long_time_in_level, na.rm = TRUE) * 100,
    near_band_top_n = sum(near_top_of_pay_band, na.rm = TRUE),
    near_band_top_pct = mean(near_top_of_pay_band, na.rm = TRUE) * 100,
    high_constraint_n = sum(high_constraint, na.rm = TRUE),
    high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100) |>
  dplyr::mutate(across(ends_with("_pct"), ~ round(.x, 2))) |>
  dplyr::arrange(site, desc(high_constraint_pct))


# print family and site summary 
cat("\nStructural ceilings by site and job family:\n")
print(family_site_ceiling_summary)


# identify larger comparison groups
# small groups can produce unstable percentages e.g., 2/4 = 50%
family_site_priority <- family_site_ceiling_summary |>
  filter(employees >= minimum_group_size) |>
  arrange(desc(high_constraint_pct))

cat("\nHighest constraint site and family groups with at least 30 employees:\n")
print(family_site_priority)

# save family and site summaries 
write.csv(family_site_ceiling_summary,
  here::here("tables", "question1_table4_family_site_ceiling_summary.csv"),
  row.names = FALSE)

write.csv(family_site_priority,
  here::here("tables", "question1_table5_family_site_priority.csv"),
  row.names = FALSE)


# question 2: prepare listening survey measures --------------------------

# define survey items used in each measure
career_growth_items <- c("grow_1", "grow_2", "grow_3")
engagement_items <- c("eng_1", "eng_2")

# create survey measures
listening_survey <- listening_survey |>
  dplyr::mutate(
    # average of three career growth items
    career_growth_score = rowMeans(
      dplyr::across(all_of(career_growth_items)), na.rm = TRUE),
    
    # proportion of career growth items rated favorable (responses 4 or 5)
    career_growth_favorable = rowMeans(
      dplyr::across(all_of(career_growth_items), ~ .x >= 4), 
      na.rm = TRUE) * 100,
    
    # average of two engagement items
    engagement_score = rowMeans(
      dplyr::across(all_of(engagement_items)), na.rm = TRUE),
    
    # reverse score so higher values indicate feeling less stuck
    stuck_reversed = 6 - stuck_1)

# review listening survey measures 

# career-growth scores between 1 and 5
cat("\nCareer growth score summary:\n")
print(summary(listening_survey$career_growth_score))

# favorability between 0 and 100
cat("\nCareer growth favorable percentage summary:\n")
print(summary(listening_survey$career_growth_favorable))

# engagement scores between 1 and 5
cat("\nEngagement score summary:\n")
print(summary(listening_survey$engagement_score))

# reversed stuck values between 1 and 5
cat("\nReversed feeling stuck summary:\n")
print(summary(listening_survey$stuck_reversed))

# replace undefined row means with missing values 

listening_survey <- listening_survey |>
  dplyr::mutate(
    career_growth_score = ifelse(
      is.nan(career_growth_score),
      NA_real_,
      career_growth_score),
    career_growth_favorable = ifelse(
      is.nan(career_growth_favorable),
      NA_real_,
      career_growth_favorable),
    engagement_score = ifelse(
      is.nan(engagement_score),
      NA_real_,
      engagement_score))


# join survey measures to employee data ----------------------------------

analysis_data <- employees |>
  # left join retains all 6000 employees
  # inner_join() would remove nonrespondents and reduce employee
  # analysis population
  dplyr::left_join(
    listening_survey,
    by = "employee_id") |>
  dplyr::mutate(survey_respondent = !is.na(career_growth_score))

cat("\nRows in combined analysis data:", nrow(analysis_data), "\n")

cat("\nSurvey respondent status:\n")
print(table(analysis_data$survey_respondent, useNA = "ifany"))

# check survey records without matching employees 
unmatched_survey_records <- listening_survey |>
  dplyr::anti_join(employees, by = "employee_id")

cat("\nSurvey records without matching employees:", 
    nrow(unmatched_survey_records), "\n") # 0


# check survey coverage by employee group --------------------------------

# this analysis help ID if some groups are underrepresented in 
# listening-survey results

# survey response rate by job family
survey_coverage_family <- analysis_data |>
  dplyr::group_by(job_family) |>
  dplyr::summarise(
    # count employees in each group
    employees = dplyr::n(),
    # count survey respondents
    survey_responses = sum(survey_respondent),
    # calculate response rate
    response_rate_pct = mean(survey_respondent) * 100) |>
  dplyr::mutate(response_rate_pct = round(response_rate_pct, 2)) |>
  # sort from lowest response rate
  dplyr::arrange(response_rate_pct)


# survey response rate by worker type
survey_coverage_worker_type <- analysis_data |>
  dplyr::group_by(worker_type) |>
  dplyr::summarise(
    employees = dplyr::n(),
    survey_responses = sum(survey_respondent),
    response_rate_pct = mean(survey_respondent) * 100) |>
  dplyr::mutate(response_rate_pct = round(response_rate_pct, 2)) |>
  dplyr::arrange(response_rate_pct)


# survey response rate by site
survey_coverage_site <- analysis_data |>
  dplyr::group_by(site) |>
  dplyr::summarise(
    employees = dplyr::n(),
    survey_responses = sum(survey_respondent),
    response_rate_pct = mean(survey_respondent) * 100) |>
  dplyr::mutate(response_rate_pct = round(response_rate_pct, 2)) |>
  dplyr::arrange(response_rate_pct)


# print survey coverage summaries 
cat("\nSurvey coverage by job family:\n")
print(survey_coverage_family)

cat("\nSurvey coverage by worker type:\n")
print(survey_coverage_worker_type)

cat("\nSurvey coverage by site:\n")
print(survey_coverage_site)

# save survey coverage summaries
write.csv(survey_coverage_family,
          here::here("tables", "question2_table1_survey_coverage_family.csv"),
          row.names = FALSE)

write.csv(survey_coverage_worker_type,
          here::here("tables", "question2_table2_survey_coverage_worker_type.csv"),
          row.names = FALSE)

write.csv(survey_coverage_site,
          here::here("tables", "question2_table3_survey_coverage_site.csv"),
          row.names = FALSE)

# calculate overall survey response rate ---------------------------------

overall_survey_coverage <- analysis_data |>
  dplyr::summarise(
    employees = dplyr::n(),
    survey_responses = sum(survey_respondent),
    response_rate_pct = mean(survey_respondent) * 100) |>
  dplyr::mutate(response_rate_pct = round(response_rate_pct, 1))

cat("\nOverall survey coverage:\n")
print(overall_survey_coverage) # 92.9

# summarize career growth by ceiling status ------------------------------

# compare career-growth sentiment by structural ceiling indicators

# compare career-growth sentiment by top of ladder status
growth_by_top_of_ladder <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(top_of_ladder) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    # calculate mean career-growth score
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    # calculates percentage favorable
    career_growth_favorable_pct = mean(career_growth_favorable, na.rm = TRUE)) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2))

# compare career-growth sentiment by long time in level status
growth_by_long_time <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(long_time_in_level) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE)) |>
  dplyr::mutate(career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2))

# compare career-growth sentiment by pay band position
growth_by_pay_band <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(near_top_of_pay_band) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE)) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2))

# compare career-growth sentiment by high constraint status
growth_by_constraint <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(high_constraint) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE)) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2))


# - summarize career growth by number of ceiling conditions 
# - done to examine if sentiment goes down as employees  
#   experience more ceiling conditions
growth_by_ceiling_count <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(ceiling_count) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable,na.rm = TRUE)) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct,
      1)) |>
  dplyr::arrange(ceiling_count)

# print career growth comparisons 
cat("\nCareer growth by top of ladder status:\n")
print(growth_by_top_of_ladder)

cat("\nCareer growth by long time in level status:\n")
print(growth_by_long_time)

cat("\nCareer growth by pay band position:\n")
print(growth_by_pay_band)

cat("\nCareer growth by high constraint status:\n")
print(growth_by_constraint)

cat("\nCareer growth by number of ceiling conditions:\n")
print(growth_by_ceiling_count)

# save career growth summaries 
write.csv(growth_by_top_of_ladder,
  here::here("tables", "question2_table4_growth_by_top_of_ladder.csv"),
  row.names = FALSE)

write.csv(growth_by_long_time,
  here::here("tables", "question2_table5_growth_by_long_time_in_level.csv"),
  row.names = FALSE)

write.csv(growth_by_pay_band,
  here::here("tables", "question2_table6_growth_by_pay_band_position.csv"),
  row.names = FALSE)

write.csv(growth_by_constraint,
  here::here("tables", "question2_table7_growth_by_high_constraint.csv"),
  row.names = FALSE)

write.csv(growth_by_ceiling_count,
  here::here("tables", "question2_table8_growth_by_ceiling_count.csv"),
  row.names = FALSE)

# summarize career growth by job family ----------------------

# summarize career growth by job family
growth_by_family <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(job_family) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE),
    high_constraint_pct = mean(
      high_constraint,
      na.rm = TRUE) * 100) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2),
    high_constraint_pct = round(
      high_constraint_pct, 2)) |>
  dplyr::arrange(career_growth_mean)


# summarize career growth by worker type
growth_by_worker_type <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(worker_type) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE),
    high_constraint_pct = mean(
      high_constraint,
      na.rm = TRUE) * 100) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2),
    high_constraint_pct = round(
      high_constraint_pct, 2)) |>
  dplyr::arrange(career_growth_mean)


# summarize career growth by site
growth_by_site <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(site) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(career_growth_score, na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable, na.rm = TRUE),
    high_constraint_pct = mean(
      high_constraint,
      na.rm = TRUE) * 100) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2),
    high_constraint_pct = round(
      high_constraint_pct, 2)) |>
  dplyr::arrange(career_growth_mean)

cat("\nCareer growth by job family:\n")
print(growth_by_family)

cat("\nCareer growth by worker type:\n")
print(growth_by_worker_type)

cat("\nCareer growth by site:\n")
print(growth_by_site)

# save career growth group summaries
write.csv(growth_by_family,
  here::here("tables", "question2_table9_growth_by_family.csv"),
  row.names = FALSE)

write.csv(growth_by_worker_type,
  here::here("tables", "question2_table10_growth_by_worker_type.csv"),
  row.names = FALSE)

write.csv(growth_by_site,
  here::here("tables", "question2_table11_growth_by_site.csv"),
  row.names = FALSE)

# summarize career growth by family and site -----------------------------

growth_by_family_site <- analysis_data |>
  dplyr::filter(survey_respondent) |>
  dplyr::group_by(site,job_family) |>
  dplyr::summarise(
    respondents = dplyr::n(),
    career_growth_mean = mean(
      career_growth_score,
      na.rm = TRUE),
    career_growth_favorable_pct = mean(
      career_growth_favorable,
      na.rm = TRUE),
    high_constraint_pct = mean(
      high_constraint,
      na.rm = TRUE) * 100) |>
  dplyr::mutate(
    career_growth_mean = round(career_growth_mean, 2),
    career_growth_favorable_pct = round(
      career_growth_favorable_pct, 2),
    high_constraint_pct = round(
      high_constraint_pct, 2)) |>
  dplyr::filter(respondents >= minimum_group_size) |>
  dplyr::arrange(site, career_growth_mean)

cat("\nCareer growth by site and job family:\n")
print(growth_by_family_site)

write.csv(growth_by_family_site,
  here::here("tables", "question2_table12_growth_by_family_site.csv"),
  row.names = FALSE)


# promotion and turnover by ceiling status -------------------
# compare structural ceiling exposure with 2 business outcomes
# - recent promotion
# - voluntary turnover

# outcomes by high constraint status
outcomes_by_constraint <- analysis_data |>
  dplyr::group_by(high_constraint) |>
  dplyr::summarise(
    employees = dplyr::n(),
    promoted_n = sum(promoted_last_24mo, na.rm = TRUE),
    promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
    turnover_n = sum(voluntary_turnover, na.rm = TRUE),
    turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
  dplyr::mutate(
    promoted_pct = round(promoted_pct, 2),
    turnover_pct = round(turnover_pct, 2))


# outcomes by number of ceiling conditions
outcomes_by_ceiling_count <- analysis_data |>
  dplyr::group_by(ceiling_count) |>
  dplyr::summarise(
    employees = dplyr::n(),
    promoted_n = sum(promoted_last_24mo, na.rm = TRUE),
    promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
    turnover_n = sum(voluntary_turnover, na.rm = TRUE),
    turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
  dplyr::mutate(
    promoted_pct = round(promoted_pct, 2),
    turnover_pct = round(turnover_pct, 2)) |>
  dplyr::arrange(ceiling_count)

# print promotion and turnover summaries 
cat("\nPromotion and turnover by high constraint status:\n")
print(outcomes_by_constraint)

cat("\nPromotion and turnover by number of ceiling conditions:\n")
print(outcomes_by_ceiling_count)

# summarize outcomes by individual ceiling measures 

outcomes_by_top_of_ladder <- analysis_data |>
  dplyr::group_by(top_of_ladder) |>
  dplyr::summarise(
    employees = dplyr::n(),
    promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
    turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
  dplyr::mutate(
    promoted_pct = round(promoted_pct, 2),
    turnover_pct = round(turnover_pct, 2))


outcomes_by_long_time <- analysis_data |>
  dplyr::group_by(long_time_in_level) |>
  dplyr::summarise(
    employees = dplyr::n(),
    promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
    turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
  dplyr::mutate(
    promoted_pct = round(promoted_pct, 2),
    turnover_pct = round(turnover_pct, 2))


outcomes_by_pay_band <- analysis_data |>
  dplyr::group_by(near_top_of_pay_band) |>
  dplyr::summarise(
    employees = dplyr::n(),
    promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
    turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
  dplyr::mutate(
    promoted_pct = round(promoted_pct, 2),
    turnover_pct = round(turnover_pct, 2))

cat("\nPromotion and turnover by top of ladder status:\n")
print(outcomes_by_top_of_ladder)

cat("\nPromotion and turnover by long time in level status:\n")
print(outcomes_by_long_time)

cat("\nPromotion and turnover by pay band position:\n")
print(outcomes_by_pay_band)

# save promotion and turnover summaries
write.csv(outcomes_by_constraint,
  here::here("tables", "question2_table13_outcomes_by_high_constraint.csv"),
  row.names = FALSE)

write.csv(outcomes_by_ceiling_count,
  here::here("tables", "question2_table14_outcomes_by_ceiling_count.csv"),
  row.names = FALSE)

write.csv(outcomes_by_top_of_ladder,
  here::here("tables", "question2_table15_outcomes_by_top_of_ladder.csv"),
  row.names = FALSE)

write.csv(outcomes_by_long_time,
  here::here("tables", "question2_table16_outcomes_by_long_time_in_level.csv"),
  row.names = FALSE)

write.csv(outcomes_by_pay_band,
  here::here("tables", "question2_table17_outcomes_by_pay_band_position.csv"),
  row.names = FALSE)

# adjust outcome models ------------------------------------
  
# test if high structural constraint is associated with career growth, 
# promotion and voluntary turnover after accounting for employee and
# organizational differences

# turn tenure from months to years for easy analysis
analysis_data <- analysis_data |>
  dplyr::mutate(tenure_years = tenure_months / 12,
    manager_flag = factor(manager_flag),
    gender = factor(gender),
    age_band = factor(age_band),
    site = factor(site),
    job_family = factor(job_family))

# model career growth among survey respondents
career_growth_model <- stats::lm(
  career_growth_score ~
    high_constraint +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data |>
    dplyr::filter(survey_respondent))

cat("\nAdjusted career growth model:\n")
print(summary(career_growth_model))

# model probability of promotion during the last 24 months
# promotion is coded as 0 or 1 so logistic regression 
promotion_model <- stats::glm(
  promoted_last_24mo ~
    high_constraint +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data,
  family = stats::binomial())

cat("\nAdjusted promotion model:\n")
print(summary(promotion_model))

# model probability of voluntary turnover
turnover_model <- stats::glm(
  voluntary_turnover ~
    high_constraint +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data,
  family = stats::binomial())

cat("\nAdjusted voluntary turnover model:\n")
print(summary(turnover_model))

# extract adjusted career growth results
career_growth_results <- broom::tidy(
  career_growth_model,
  conf.int = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2)))

cat("\nAdjusted career growth results:\n")
print(career_growth_results)

# convert promotion coefficients to odds ratios
promotion_results <- broom::tidy(
  promotion_model,
  conf.int = TRUE,
  # exponentiate = TRUE so model coefficients are converted to odds ratios
  exponentiate = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2)))

cat("\nAdjusted promotion odds ratios:\n")
print(promotion_results)


# convert turnover coefficients to odds ratios
turnover_results <- broom::tidy(
  turnover_model,
  conf.int = TRUE,
  exponentiate = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2)))

cat("\nAdjusted turnover odds ratios:\n")
print(turnover_results)

# extract primary structural constraint results
career_growth_constraint_result <- career_growth_results |>
  dplyr::filter(term == "high_constraintTRUE")

promotion_constraint_result <- promotion_results |>
  dplyr::filter(term == "high_constraintTRUE")

turnover_constraint_result <- turnover_results |>
  dplyr::filter(term == "high_constraintTRUE")

# combine primary adjusted results
adjusted_constraint_summary <- dplyr::bind_rows(
  career_growth_constraint_result |>
    dplyr::transmute(
      outcome = "Career growth score",
      measure = "Adjusted mean difference",
      estimate,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value),
  
  promotion_constraint_result |>
    dplyr::transmute(
      outcome = "Promotion",
      measure = "Adjusted odds ratio",
      estimate,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value),
  
  turnover_constraint_result |>
    dplyr::transmute(
      outcome = "Voluntary turnover",
      measure = "Adjusted odds ratio",
      estimate,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value))

cat("\nAdjusted high constraint summary:\n")
print(adjusted_constraint_summary)

# save adjusted model results
write.csv(career_growth_results,
  here::here("tables", "question2_table18_career_growth_model_results.csv"),
  row.names = FALSE)

write.csv(promotion_results,
  here::here("tables", "question2_table19_promotion_model_results.csv"),
  row.names = FALSE)

write.csv(turnover_results,
  here::here("tables", "question2_table20_turnover_model_results.csv"),
  row.names = FALSE)

write.csv(adjusted_constraint_summary,
  here::here("tables", "question2_table21_adjusted_constraint_summary.csv"),
  row.names = FALSE)

# compare individual ceiling measures ------------------------

# model career growth using separate ceiling indicators
career_growth_components_model <- stats::lm(
  career_growth_score ~
    top_of_ladder +
    long_time_in_level +
    near_top_of_pay_band +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data |> dplyr::filter(survey_respondent))


# model promotion using separate ceiling indicators
promotion_components_model <- stats::glm(
  promoted_last_24mo ~
    top_of_ladder +
    long_time_in_level +
    near_top_of_pay_band +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data,
  family = stats::binomial())


# model voluntary turnover using separate ceiling indicators
turnover_components_model <- stats::glm(
  voluntary_turnover ~
    top_of_ladder +
    long_time_in_level +
    near_top_of_pay_band +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data,
  family = stats::binomial())

# extract career growth component estimates 
career_growth_component_results <- broom::tidy(
  career_growth_components_model,
  conf.int = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE")) |>
  dplyr::mutate(
    ceiling_measure = dplyr::case_when(
      term == "top_of_ladderTRUE" ~ "Top of ladder",
      term == "long_time_in_levelTRUE" ~ "Long time in level",
      term == "near_top_of_pay_bandTRUE" ~ "Near top of pay band"),
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2))) |>
  dplyr::select(
    ceiling_measure, estimate, conf.low, conf.high, p.value)

cat("\nAdjusted career growth results by ceiling measure:\n")
print(career_growth_component_results)

# extract promotion odds ratios 
promotion_component_results <- broom::tidy(
  promotion_components_model,
  conf.int = TRUE,
  exponentiate = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE")) |>
  dplyr::mutate(
    ceiling_measure = dplyr::case_when(
      term == "top_of_ladderTRUE" ~ "Top of ladder",
      term == "long_time_in_levelTRUE" ~ "Long time in level",
      term == "near_top_of_pay_bandTRUE" ~ "Near top of pay band"),
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2))) |>
  dplyr::select(ceiling_measure, estimate, conf.low, conf.high, p.value)

cat("\nAdjusted promotion odds ratios by ceiling measure:\n")
print(promotion_component_results)

# extract voluntary turnover odds ratios 
turnover_component_results <- broom::tidy(
  turnover_components_model,
  conf.int = TRUE,
  exponentiate = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE")) |>
  dplyr::mutate(
    ceiling_measure = dplyr::case_when(
      term == "top_of_ladderTRUE" ~ "Top of ladder",
      term == "long_time_in_levelTRUE" ~ "Long time in level",
      term == "near_top_of_pay_bandTRUE" ~ "Near top of pay band"),
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2))) |>
  dplyr::select(ceiling_measure, estimate, conf.low, conf.high, p.value)

cat("\nAdjusted turnover odds ratios by ceiling measure:\n")
print(turnover_component_results)

# combine individual ceiling results 
ceiling_component_summary <- dplyr::bind_rows(
  career_growth_component_results |>
    dplyr::mutate(
      outcome = "Career growth score",
      measure = "Adjusted mean difference"),
  
  promotion_component_results |>
    dplyr::mutate(
      outcome = "Promotion",
      measure = "Adjusted odds ratio"),
  
  turnover_component_results |>
    dplyr::mutate(
      outcome = "Voluntary turnover",
      measure = "Adjusted odds ratio")) |>
  dplyr::select(
    outcome, ceiling_measure, measure, estimate,
    conf.low,
    conf.high,
    p.value)

cat("\nAdjusted results for individual ceiling measures:\n")
print(ceiling_component_summary)

# save individual ceiling model results 

write.csv(
  career_growth_component_results,
  here::here("tables", "question2_table22_career_growth_ceiling_components.csv"),
  row.names = FALSE)

write.csv(
  promotion_component_results,
  here::here("tables", "question2_table23_promotion_ceiling_components.csv"),
  row.names = FALSE)

write.csv(
  turnover_component_results,
  here::here("tables", "question2_table24_turnover_ceiling_components.csv"),
  row.names = FALSE)

write.csv(
  ceiling_component_summary,
  here::here("tables", "question2_table25_ceiling_component_summary.csv"),
  row.names = FALSE)


# compare structure and manager support ----------------------

# model career growth using manager support 
career_growth_manager_model <- stats::lm(
  career_growth_score ~
    mgr_1 +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data |>
    dplyr::filter(survey_respondent))


# model career growth using both structural ceilings and manager support
career_growth_full_model <- stats::lm(
  career_growth_score ~
    top_of_ladder +
    long_time_in_level +
    near_top_of_pay_band +
    mgr_1 +
    tenure_years +
    performance_rating +
    manager_flag +
    gender +
    age_band +
    site +
    job_family,
  data = analysis_data |>
    dplyr::filter(survey_respondent))

# extract manager support result 
manager_support_result <- broom::tidy(
  career_growth_manager_model,
  conf.int = TRUE) |>
  dplyr::filter(term == "mgr_1") |>
  dplyr::mutate(
    model = "Manager support only",
    measure = "Adjusted mean difference",
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2)))


# extract results from combined model 
structure_manager_results <- broom::tidy(
  career_growth_full_model,
  conf.int = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE",
      "mgr_1")) |>
  dplyr::mutate(
    predictor = dplyr::case_when(
      term == "top_of_ladderTRUE" ~ "Top of ladder",
      term == "long_time_in_levelTRUE" ~ "Long time in level",
      term == "near_top_of_pay_bandTRUE" ~ "Near top of pay band",
      term == "mgr_1" ~ "Manager support"),
    model = "Structure and manager support",
    measure = "Adjusted mean difference",
    dplyr::across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 2))) |>
  dplyr::select(
    predictor,
    model,
    measure,
    estimate,
    conf.low,
    conf.high,
    p.value)

cat("\nManager support model result:\n")
print(manager_support_result)

cat("\nCombined structure and manager support results:\n")
print(structure_manager_results)

# compare model explanatory power 
career_growth_model_comparison <- data.frame(
  model = c(
    "Structural ceilings only",
    "Manager support only",
    "Structure and manager support"),
  adjusted_r_squared = c(
    summary(career_growth_components_model)$adj.r.squared,
    summary(career_growth_manager_model)$adj.r.squared,
    summary(career_growth_full_model)$adj.r.squared),
  sample_size = c(
    stats::nobs(career_growth_components_model),
    stats::nobs(career_growth_manager_model),
    stats::nobs(career_growth_full_model))) |>
  dplyr::mutate(
    adjusted_r_squared = round(adjusted_r_squared, 2))

cat("\nCareer growth model comparison:\n")
print(career_growth_model_comparison)

# compare estimates before and after manager support 
structure_only_estimates <- broom::tidy(
  career_growth_components_model,
  conf.int = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE")) |>
  dplyr::transmute(
    term, structure_only_estimate = estimate)


structure_with_manager_estimates <- broom::tidy(
  career_growth_full_model,
  conf.int = TRUE) |>
  dplyr::filter(
    term %in% c(
      "top_of_ladderTRUE",
      "long_time_in_levelTRUE",
      "near_top_of_pay_bandTRUE")) |>
  dplyr::transmute(
    term, structure_with_manager_estimate = estimate)


structure_estimate_comparison <- structure_only_estimates |>
  dplyr::left_join(
    structure_with_manager_estimates,
    by = "term") |>
  dplyr::mutate(
    ceiling_measure = dplyr::case_when(
      term == "top_of_ladderTRUE" ~ "Top of ladder",
      term == "long_time_in_levelTRUE" ~ "Long time in level",
      term == "near_top_of_pay_bandTRUE" ~ "Near top of pay band"),
    change_after_manager_support =
      structure_with_manager_estimate -
      structure_only_estimate,
    dplyr::across(
      c(structure_only_estimate,
        structure_with_manager_estimate,
        change_after_manager_support), ~ round(.x, 2))) |>
  dplyr::select(
    ceiling_measure,
    structure_only_estimate,
    structure_with_manager_estimate,
    change_after_manager_support)

cat("\nStructural estimates before and after manager support:\n")
print(structure_estimate_comparison)

# save structure and manager support results 
write.csv(manager_support_result,
  here::here("tables", "question2_table26_manager_support_result.csv"),
  row.names = FALSE)

write.csv(structure_manager_results,
  here::here("tables", "question2_table27_structure_manager_results.csv"),
  row.names = FALSE)

write.csv(career_growth_model_comparison,
  here::here("tables", "question2_table28_career_growth_model_comparison.csv"),
  row.names = FALSE)

write.csv(structure_estimate_comparison,
  here::here("tables", "question2_table29_structure_estimate_comparison.csv"),
  row.names = FALSE)


#####END ####
# [END]