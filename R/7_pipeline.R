# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: function to run all analyses

run_analysis <- function(config) {
  required_config <- c(
    "long_time_threshold",
    "pay_band_threshold",
    "minimum_group_size",
    "save_tables",
    "save_figures")

  missing_config <- setdiff(required_config, names(config))

  if (length(missing_config) > 0) {
    stop(
      "Missing configuration values: ",
      paste(missing_config, collapse = ", "))
  }
  workbook_data <- load_workbook_data()
  
  prepared_data <- inspect_and_prepare_data(
    readme = workbook_data$readme,
    data_dictionary = workbook_data$data_dictionary,
    employees_raw = workbook_data$employees_raw,
    job_architecture_raw = workbook_data$job_architecture_raw,
    listening_survey_raw = workbook_data$listening_survey_raw)
  
  question1_results <- run_question1(
    employees = prepared_data$employees,
    job_architecture = prepared_data$job_architecture,
    long_time_threshold = config$long_time_threshold,
    pay_band_threshold = config$pay_band_threshold,
    minimum_group_size = config$minimum_group_size,
    save_tables = config$save_tables,
    save_figures = config$save_figures)
  
  question2_results <- run_question2(
    employees = question1_results$employees,
    listening_survey = prepared_data$listening_survey,
    minimum_group_size = config$minimum_group_size,
    save_tables = save_tables)
  
  question3_results <- run_question3(
    analysis_data = question2_results$analysis_data,
    minimum_group_size = config$minimum_group_size,
    save_tables = save_tables)
  
  question4_results <- run_question4(
    analysis_data = question2_results$analysis_data,
    minimum_group_size = config$minimum_group_size,
    save_tables = save_tables)
  
  list(workbook_data = workbook_data,
       prepared_data = prepared_data,
       question1_results = question1_results,
       question2_results = question2_results,
       question3_results = question3_results,
       question4_results = question4_results)
}

# [END]