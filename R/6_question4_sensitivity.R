# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: Case study question 4: sensitivity analysis and 
# action priorities 

run_question4 <- function(
    analysis_data,
    minimum_group_size,
    save_tables) {
  
  # define:               time         pay-band threshold
  # Less restrictive      36 months     85% 
  # Primary               48 months     90% 
  # More restrictive      60 months     95% 
  
  # test alternate ceiling thresholds
  sensitivity_thresholds <- data.frame(
    scenario = c(
      "Less restrictive",
      "Primary definition",
      "More restrictive"),
    long_time_months = c(36, 48, 60),
    pay_band_cutoff = c(0.85, 0.90, 0.95))
  
  # calculate high constraint under each scenario
  sensitivity_results <- sensitivity_thresholds |>
    dplyr::rowwise() |>
    dplyr::mutate(
      employees = nrow(analysis_data),
      high_constraint_n = sum(
        (as.integer(analysis_data$top_of_ladder) +
           dplyr::coalesce(
             as.integer(
               analysis_data$time_in_level_months >= long_time_months), 0L) +
           dplyr::coalesce(
             as.integer(
               analysis_data$pay_band_position >= pay_band_cutoff), 0L)) >= 2,
        na.rm = TRUE),
      
      high_constraint_pct =
        high_constraint_n / employees * 100) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      high_constraint_pct = round(high_constraint_pct, 2))
  
  cat("\nSensitivity of high constraint definition:\n")
  print(sensitivity_results)
  
  # identify priority job families 
  priority_family_summary <- analysis_data |>
    dplyr::group_by(job_family) |>
    dplyr::summarise(
      employees = dplyr::n(),
      levels_in_family = dplyr::first(levels_in_family),
      high_constraint_pct =
        mean(high_constraint, na.rm = TRUE) * 100,
      career_growth_mean =
        mean(career_growth_score, na.rm = TRUE),
      promoted_pct =
        mean(promoted_last_24mo, na.rm = TRUE) * 100,
      turnover_pct =
        mean(voluntary_turnover, na.rm = TRUE) * 100) |>
    dplyr::filter(employees >= minimum_group_size) |>
    dplyr::mutate(
      high_constraint_pct = round(high_constraint_pct, 2),
      career_growth_mean = round(career_growth_mean, 2),
      promoted_pct = round(promoted_pct, 2),
      turnover_pct = round(turnover_pct, 2)) |>
    dplyr::arrange(
      dplyr::desc(high_constraint_pct),
      career_growth_mean)
  
  cat("\nPriority job families:\n")
  print(priority_family_summary)
  
  # flag job families for targeted action 
  # - a family is classified as High priority
  #    - above-median structural constraint
  #    - below-median career-growth sentiment
  #    - above-median voluntary turnover
  
  # - a family is classified as Review 
  #    - when at least one major concern is present
  
  # - a family is classified as Monitor
  #    - when available indicators do not show a strong concern
  
  priority_family_summary <- priority_family_summary |>
    dplyr::mutate(
      action_priority = dplyr::case_when(
        high_constraint_pct >=
          median(high_constraint_pct, na.rm = TRUE) &
          career_growth_mean <=
          median(career_growth_mean, na.rm = TRUE) &
          turnover_pct >=
          median(turnover_pct, na.rm = TRUE) ~
          "High priority",
        
        high_constraint_pct >=
          median(high_constraint_pct, na.rm = TRUE) |
          career_growth_mean <=
          median(career_growth_mean, na.rm = TRUE) ~
          "Review", TRUE ~ "Monitor")) |>
    dplyr::arrange(
      factor(
        action_priority,
        levels = c(
          "High priority",
          "Review",
          "Monitor")),
      dplyr::desc(high_constraint_pct))
  
  cat("\nRecommended action priority by job family:\n")
  print(priority_family_summary)
  
  
  # save question 4 tables 
  if (save_tables) {
    write.csv(sensitivity_results,
              here::here("tables",
                         "question4_table1_threshold_sensitivity.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(priority_family_summary,
              here::here("tables",
                         "question4_table2_priority_family_summary.csv"),
              row.names = FALSE)
  }
  
  list(sensitivity_thresholds = sensitivity_thresholds,
    sensitivity_results = sensitivity_results,
    priority_family_summary = priority_family_summary)
}
