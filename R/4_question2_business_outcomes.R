# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: case study question 2 analysis

run_question2 <- function(
    employees,
    listening_survey,
    minimum_group_size,
    save_tables) {
  
  # question 2: prepare listening survey measures 
  # define survey items used in each measure
  career_growth_items <- c("grow_1", "grow_2", "grow_3")
  engagement_items <- c("eng_1", "eng_2")
  
  # assess internal consistency of career-growth items 
  # Cronbach's alpha
  career_growth_alpha <- psych::alpha(
    listening_survey |>
      dplyr::select(dplyr::all_of(career_growth_items)),
    check.keys = FALSE,
    warnings = FALSE,
    use = "pairwise")
  
  career_growth_reliability <- data.frame(
    measure = "Career growth",
    items = paste(career_growth_items, collapse = ", "),
    n_items = length(career_growth_items),
    cronbach_alpha = round(
      career_growth_alpha$total$raw_alpha, 2),
    standardized_alpha = round(
      career_growth_alpha$total$std.alpha, 2))
  
  cat("\nCareer-growth scale reliability:\n")
  print(career_growth_reliability)
  # Cronbach’s alpha of 0.84 suggests 3 items are related
  # enough to be combined into one career-growth score
  
  if (save_tables) {
    write.csv(career_growth_reliability,
              here::here("tables",
                         "question2_table0_career_growth_reliability.csv"),
              row.names = FALSE)
  }
  
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
  
  
  # join survey measures to employee data 
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
  
  
  # check survey coverage by employee group 
  
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
  if (save_tables) {
    write.csv(survey_coverage_family,
              here::here("tables", "question2_table1_survey_coverage_family.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(survey_coverage_worker_type,
              here::here("tables", "question2_table2_survey_coverage_worker_type.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(survey_coverage_site,
              here::here("tables", "question2_table3_survey_coverage_site.csv"),
              row.names = FALSE)
  }
  
  # calculate overall survey response rate 
  overall_survey_coverage <- analysis_data |>
    dplyr::summarise(
      employees = dplyr::n(),
      survey_responses = sum(survey_respondent),
      response_rate_pct = mean(survey_respondent) * 100) |>
    dplyr::mutate(response_rate_pct = round(response_rate_pct, 1))
  
  cat("\nOverall survey coverage:\n")
  print(overall_survey_coverage) # 92.9
  
  # summarize career growth by ceiling status 
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
  if (save_tables) {
    write.csv(growth_by_top_of_ladder,
              here::here("tables", "question2_table4_growth_by_top_of_ladder.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_long_time,
              here::here("tables", "question2_table5_growth_by_long_time_in_level.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_pay_band,
              here::here("tables", "question2_table6_growth_by_pay_band_position.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_constraint,
              here::here("tables", "question2_table7_growth_by_high_constraint.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_ceiling_count,
              here::here("tables", "question2_table8_growth_by_ceiling_count.csv"),
              row.names = FALSE)
  }
  
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
  if (save_tables) {
    write.csv(growth_by_family,
              here::here("tables", "question2_table9_growth_by_family.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_worker_type,
              here::here("tables", "question2_table10_growth_by_worker_type.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(growth_by_site,
              here::here("tables", "question2_table11_growth_by_site.csv"),
              row.names = FALSE)
  }
  
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
  
  if (save_tables) {
    write.csv(growth_by_family_site,
              here::here("tables", "question2_table12_growth_by_family_site.csv"),
              row.names = FALSE)
  }
  
  
  # promotion and turnover by ceiling status 
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
  if (save_tables) {
    write.csv(outcomes_by_constraint,
              here::here("tables", "question2_table13_outcomes_by_high_constraint.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(outcomes_by_ceiling_count,
              here::here("tables", "question2_table14_outcomes_by_ceiling_count.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(outcomes_by_top_of_ladder,
              here::here("tables", "question2_table15_outcomes_by_top_of_ladder.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(outcomes_by_long_time,
              here::here("tables", "question2_table16_outcomes_by_long_time_in_level.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(outcomes_by_pay_band,
              here::here("tables", "question2_table17_outcomes_by_pay_band_position.csv"),
              row.names = FALSE)
  }
  
  # adjust outcome models 
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
  if (save_tables) {
    write.csv(career_growth_results,
              here::here("tables", "question2_table18_career_growth_model_results.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(promotion_results,
              here::here("tables", "question2_table19_promotion_model_results.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(turnover_results,
              here::here("tables", "question2_table20_turnover_model_results.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(adjusted_constraint_summary,
              here::here("tables", "question2_table21_adjusted_constraint_summary.csv"),
              row.names = FALSE)
  }
  
  # compare individual ceiling measures 
  
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
  
  if (save_tables) {
    write.csv(
      career_growth_component_results,
      here::here("tables", "question2_table22_career_growth_ceiling_components.csv"),
      row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(
      promotion_component_results,
      here::here("tables", "question2_table23_promotion_ceiling_components.csv"),
      row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(
      turnover_component_results,
      here::here("tables", "question2_table24_turnover_ceiling_components.csv"),
      row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(
      ceiling_component_summary,
      here::here("tables", "question2_table25_ceiling_component_summary.csv"),
      row.names = FALSE)
  }
  
  
  # compare structure and manager support 
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
  if (save_tables) {
    write.csv(manager_support_result,
              here::here("tables", "question2_table26_manager_support_result.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(structure_manager_results,
              here::here("tables", "question2_table27_structure_manager_results.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(career_growth_model_comparison,
              here::here("tables", "question2_table28_career_growth_model_comparison.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(structure_estimate_comparison,
              here::here("tables", "question2_table29_structure_estimate_comparison.csv"),
              row.names = FALSE)
  }
  
  
  list(listening_survey = listening_survey,
       analysis_data = analysis_data,
       career_growth_reliability = career_growth_reliability,
       unmatched_survey_records = unmatched_survey_records,
       survey_coverage_family = survey_coverage_family,
       survey_coverage_worker_type = survey_coverage_worker_type,
       survey_coverage_site = survey_coverage_site,
       overall_survey_coverage = overall_survey_coverage,
       growth_by_top_of_ladder = growth_by_top_of_ladder,
       growth_by_long_time = growth_by_long_time,
       growth_by_pay_band = growth_by_pay_band,
       growth_by_constraint = growth_by_constraint,
       growth_by_ceiling_count = growth_by_ceiling_count,
       growth_by_family = growth_by_family,
       growth_by_worker_type = growth_by_worker_type,
       growth_by_site = growth_by_site,
       growth_by_family_site = growth_by_family_site,
       outcomes_by_constraint = outcomes_by_constraint,
       outcomes_by_ceiling_count = outcomes_by_ceiling_count,
       outcomes_by_top_of_ladder = outcomes_by_top_of_ladder,
       outcomes_by_long_time = outcomes_by_long_time,
       outcomes_by_pay_band = outcomes_by_pay_band,
       career_growth_model = career_growth_model,
       promotion_model = promotion_model,
       turnover_model = turnover_model,
       career_growth_results = career_growth_results,
       promotion_results = promotion_results,
       turnover_results = turnover_results,
       adjusted_constraint_summary = adjusted_constraint_summary,
       career_growth_components_model = career_growth_components_model,
       promotion_components_model = promotion_components_model,
       turnover_components_model = turnover_components_model,
       career_growth_component_results = career_growth_component_results,
       promotion_component_results = promotion_component_results,
       turnover_component_results = turnover_component_results,
       ceiling_component_summary = ceiling_component_summary,
       career_growth_manager_model = career_growth_manager_model,
       career_growth_full_model = career_growth_full_model,
       manager_support_result = manager_support_result,
       structure_manager_results = structure_manager_results,
       career_growth_model_comparison = career_growth_model_comparison,
       structure_estimate_comparison = structure_estimate_comparison)
}

# [END]
