# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: case study question 3 analysis: assess equity 
# exposure and outcomes 

run_question3 <- function(
    analysis_data,
    minimum_group_size,
    save_tables) {
  
  # summarize equity outcomes by gender
  equity_by_gender <- analysis_data |>
    dplyr::group_by(gender) |>
    dplyr::summarise(
      employees = dplyr::n(),
      high_constraint_n = sum(high_constraint, na.rm = TRUE),
      high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100,
      career_growth_mean = mean(career_growth_score, na.rm = TRUE),
      promoted_n = sum(promoted_last_24mo, na.rm = TRUE),
      promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
      turnover_n = sum(voluntary_turnover, na.rm = TRUE),
      turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
    dplyr::mutate(
      group_type = "Gender",
      high_constraint_pct = round(high_constraint_pct, 2),
      career_growth_mean = round(career_growth_mean, 2),
      promoted_pct = round(promoted_pct, 2),
      turnover_pct = round(turnover_pct, 2)) |>
    dplyr::select(
      group_type,
      group = gender,
      employees,
      high_constraint_n,
      high_constraint_pct,
      career_growth_mean,
      promoted_n,
      promoted_pct,
      turnover_n,
      turnover_pct)
  
  # summarize equity outcomes by age band 
  equity_by_age_band <- analysis_data |>
    dplyr::group_by(age_band) |>
    dplyr::summarise(
      employees = dplyr::n(),
      high_constraint_n = sum(high_constraint, na.rm = TRUE),
      high_constraint_pct = mean(high_constraint, na.rm = TRUE) * 100,
      career_growth_mean = mean(career_growth_score, na.rm = TRUE),
      promoted_n = sum(promoted_last_24mo, na.rm = TRUE),
      promoted_pct = mean(promoted_last_24mo, na.rm = TRUE) * 100,
      turnover_n = sum(voluntary_turnover, na.rm = TRUE),
      turnover_pct = mean(voluntary_turnover, na.rm = TRUE) * 100) |>
    dplyr::mutate(
      group_type = "Age band",
      high_constraint_pct = round(high_constraint_pct, 2),
      career_growth_mean = round(career_growth_mean, 2),
      promoted_pct = round(promoted_pct, 2),
      turnover_pct = round(turnover_pct, 2)) |>
    dplyr::select(
      group_type,
      group = age_band,
      employees,
      high_constraint_n,
      high_constraint_pct,
      career_growth_mean,
      promoted_n,
      promoted_pct,
      turnover_n,
      turnover_pct)
  
  # combine demographic summaries
  equity_outcome_summary <- dplyr::bind_rows(
    equity_by_gender,
    equity_by_age_band) |>
    dplyr::filter(employees >= minimum_group_size) |>
    dplyr::arrange(
      group_type,
      dplyr::desc(high_constraint_pct))
  
  cat("\nEquity outcomes by demographic group:\n")
  print(equity_outcome_summary)
  
  # adjust structural ceiling exposure for workforce composition 
  equity_constraint_model <- stats::glm(
    high_constraint ~
      gender +
      age_band +
      worker_type +
      tenure_years +
      performance_rating +
      manager_flag +
      site,
    data = analysis_data,
    family = stats::binomial())
  
  
  # extract adjusted demographic ceiling results
  equity_constraint_results <- broom::tidy(
    equity_constraint_model,
    conf.int = TRUE,
    exponentiate = TRUE) |>
    dplyr::filter(
      grepl("^gender", term) |
        grepl("^age_band", term)) |>
    dplyr::mutate(
      demographic_group = dplyr::case_when(
        grepl("^gender", term) ~ "Gender",
        grepl("^age_band", term) ~ "Age band"),
      dplyr::across(
        c(estimate, std.error, conf.low, conf.high, p.value),
        ~ round(.x, 2))) |>
    dplyr::select(
      demographic_group,
      term,
      estimate,
      conf.low,
      conf.high,
      p.value)
  
  cat("\nAdjusted structural constraint odds by demographic group:\n")
  print(equity_constraint_results)
  
  # assess demographic differences in promotion 
  equity_promotion_model <- stats::glm(
    promoted_last_24mo ~
      gender +
      age_band +
      high_constraint +
      tenure_years +
      performance_rating +
      manager_flag +
      site +
      job_family,
    data = analysis_data,
    family = stats::binomial())
  
  
  # extract adjusted demographic promotion results
  equity_promotion_results <- broom::tidy(
    equity_promotion_model,
    conf.int = TRUE,
    exponentiate = TRUE) |>
    dplyr::filter(
      grepl("^gender", term) |
        grepl("^age_band", term)) |>
    dplyr::mutate(
      demographic_group = dplyr::case_when(
        grepl("^gender", term) ~ "Gender",
        grepl("^age_band", term) ~ "Age band"),
      dplyr::across(
        c(estimate, std.error, conf.low, conf.high, p.value),
        ~ round(.x, 2))) |>
    dplyr::select(
      demographic_group,
      term,
      estimate,
      conf.low,
      conf.high,
      p.value)
  
  cat("\nAdjusted promotion odds by demographic group:\n")
  print(equity_promotion_results)
  # genderMale;     1.24     1.05      1.47    0.01
  # age_band40-49; 1.29     1.05      1.59    0.02
  
  # review model reference groups 
  
  cat("\nGender reference group and levels:\n")
  print(levels(analysis_data$gender))
  
  cat("\nAge band reference group and levels:\n")
  print(levels(analysis_data$age_band))
  
  # save question 3 tables 
  if (save_tables) {
    write.csv(equity_outcome_summary,
              here::here("tables",
                         "question3_table1_equity_outcome_summary.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(equity_constraint_results,
              here::here("tables",
                         "question3_table2_adjusted_constraint_results.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(equity_promotion_results,
              here::here("tables",
                         "question3_table3_adjusted_promotion_results.csv"),
              row.names = FALSE)
  }
  
  
  list( equity_by_gender = equity_by_gender,
    equity_by_age_band = equity_by_age_band,
    equity_outcome_summary = equity_outcome_summary,
    equity_constraint_model = equity_constraint_model,
    equity_constraint_results = equity_constraint_results,
    equity_promotion_model = equity_promotion_model,
    equity_promotion_results = equity_promotion_results)
}

# [END]
