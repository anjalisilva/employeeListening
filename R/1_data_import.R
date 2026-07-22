# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: load and check data 

load_workbook_data <- function() {
  # define workbook path 
  
  workbook_path <- here::here("data", "Case Dataset Aerodyne.xlsx")
  
  # if file not found print an error to user
  if (!file.exists(workbook_path)) {
    stop("File not found: ", workbook_path)}
  
  # check workbook tabs and read content 
  required_sheets <- c(
    "README",
    "Data_Dictionary",
    "Employees",
    "Job_Architecture",
    "Listening_Survey")
  
  available_sheets <- readxl::excel_sheets(workbook_path)
  
  missing_sheets <- setdiff(required_sheets, available_sheets)
  
  # if file missing tabs, print message to user
  if (length(missing_sheets) > 0) {
    stop("Missing workbook tabs: ", 
         paste(missing_sheets, collapse = ", "))
    } else {cat("All 5 required workbook tabs are present:\n",
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
  
  # returns to user from this function 
  list(workbook_path = workbook_path,
       required_sheets = required_sheets,
       available_sheets = available_sheets,
       readme = readme,
       data_dictionary = data_dictionary,
       employees_raw = employees_raw,
       job_architecture_raw = job_architecture_raw,
       listening_survey_raw = listening_survey_raw,
       sheet_sizes = sheet_sizes)
}

# [END]