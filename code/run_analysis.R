# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Purpose: Load functions and run complete analysis

# R dependency packages --------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(here)
library(broom)
library(stats)
library(psych)
library(ggplot2)
library(tidyr)

# options ----------------------------------------------------------------

# suppresses informational messages produced by dplyr::summarise()
# scipen = 999 stop scientific notation for salary data
options(
  dplyr.summarise.inform = FALSE,
  na.action = "na.exclude",
  scipen = 999)

# load analysis functions ------------------------------------------------

# get file names in R folder
r_files <- list.files(
  path = here::here("R"),
  pattern = "\\.R$",
  full.names = TRUE)

r_files <- sort(r_files)

if (length(r_files) == 0) {
  stop("No R function files found in R folder")
}

# loads every R script listed in r_files
invisible(lapply(r_files, source))

# user inputs ------------------------------------------------------------

# ask if tables should be saved
save_tables <- ask_yes_no(
  "Save tables to the tables folder? [y/N]: ")

cat("Save tables:", if (save_tables) "Yes" else "No", "\n")

# ask if figures should be saved
save_figures <- ask_yes_no(
  "Save figures to the figures folder? [y/N]: ")

cat("Save figures:", if (save_figures) "Yes" else "No", "\n")

# assumptions ------------------------------------------------------------

# long time in level defined as 48 months or more
long_time_threshold <- 48

# near top of pay band defined as 90% or more
pay_band_threshold <- 0.90

# minimum group size used for subgroup reporting
minimum_group_size <- 30

# analysis configuration -------------------------------------------------

config <- list(
  long_time_threshold = long_time_threshold,
  pay_band_threshold = pay_band_threshold,
  minimum_group_size = minimum_group_size,
  save_tables = save_tables,
  save_figures = save_figures)

# create output folders when selected
if (save_tables) {
  dir.create(
    here::here("tables"),
    recursive = TRUE,
    showWarnings = FALSE)
}

if (save_figures) {
  dir.create(
    here::here("figures"),
    recursive = TRUE,
    showWarnings = FALSE)
}

# run full analysis -------------------------------------------------------

analysis_results <- run_analysis(config)
