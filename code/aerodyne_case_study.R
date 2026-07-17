# Anjali Silva
# 17 July 2026

# set up
library(readxl)

# Issue:
# conflicts between promoted_last_24mo and months_since_last_promotion
# employees marked as promoted during the past 24 months whose months-since-promotion
# value is greater than 24; employees marked as not promoted despite a 
# months-since-promotion value below 24
#  - Use promoted_last_24mo as the supplied promotion outcome
#  - Can't use months_since_last_promotion as a reliable measure of promotion velocity



# Define file path
file_path <- "data/Case Dataset Aerodyne.xlsx"

# 1. Get all sheet names
sheet_names <- excel_sheets(file_path)

# 2. Read each sheet into a list
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# 3. Name the list elements after the sheets
names(all_sheets) <- sheet_names