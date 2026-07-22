# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: question 1: identify and map structural ceilings 

run_question1 <- function(
    employees,
    job_architecture,
    long_time_threshold,
    pay_band_threshold,
    minimum_group_size,
    save_tables,
    save_figures) {
  
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
  if (save_tables) {
    write.csv(family_ceiling_summary,
              here::here("tables", "question1_table1_family_ceiling_summary.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(worker_type_ceiling_summary,
              here::here("tables", "question1_table2_worker_type_ceiling_summary.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(site_ceiling_summary,
              here::here("tables", "question1_table3_site_ceiling_summary.csv"),
              row.names = FALSE)
  }
  
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
  if (save_tables) {
    write.csv(family_site_ceiling_summary,
              here::here("tables", "question1_table4_family_site_ceiling_summary.csv"),
              row.names = FALSE)
  }
  
  if (save_tables) {
    write.csv(family_site_priority,
              here::here("tables", "question1_table5_family_site_priority.csv"),
              row.names = FALSE)
  }
  
  # create structural bottleneck visual - heatmap -------------------------------
  
  # show most constrained job families and ceilings
  bottleneck_plot_data <- family_ceiling_summary |>
    dplyr::slice_max(
      order_by = high_constraint_pct,
      n = 8,
      with_ties = FALSE) |>
    dplyr::mutate(
      job_family_label = paste0(
        tools::toTitleCase(job_family),
        " (", levels_in_family, " levels)")) |>
    dplyr::select(
      job_family_label,
      top_of_ladder_pct,
      long_time_pct,
      near_band_top_pct,
      high_constraint_pct) |>
    tidyr::pivot_longer(
      cols = c(
        top_of_ladder_pct,
        long_time_pct,
        near_band_top_pct,
        high_constraint_pct),
      names_to = "ceiling_measure",
      values_to = "percent") |>
    dplyr::mutate(
      ceiling_measure = dplyr::case_when(
        ceiling_measure == "top_of_ladder_pct" ~ "Top of ladder",
        ceiling_measure == "long_time_pct" ~ "Long time in level",
        ceiling_measure == "near_band_top_pct" ~ "Near pay-band max",
        ceiling_measure == "high_constraint_pct" ~ "High constraint"),
      ceiling_measure = factor(
        ceiling_measure,
        levels = c(
          "Top of ladder",
          "Long time in level",
          "Near pay-band max",
          "High constraint")),
      job_family_label = factor(
        job_family_label,
        levels = rev(unique(job_family_label))),
      percent_label = paste0(round(percent, 2), "%"))
  
  
  # heatmap of ceiling prevalence by job family
  bottleneck_heatmap <- ggplot2::ggplot(
    bottleneck_plot_data,
    aes(
      x = ceiling_measure,
      y = job_family_label,
      fill = percent)) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.7) +
    ggplot2::geom_text(
      aes(label = percent_label),
      size = 3.0) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#3333B3",
      name = "% employees") +
    ggplot2::labs(
      title = "Structural ceiling prevalence by job family",
      subtitle = "Top 8 job families ranked by high structural constraint",
      x = NULL,
      y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9),
      axis.text.x = element_text(size = 8, angle = 25, hjust = 1),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7))
  
  if (save_figures) {
    # save bottleneck visual
    ggplot2::ggsave(
      filename = here::here("figures",
                            "question1_figure1_structural_bottleneck_heatmap.png"),
      plot = bottleneck_heatmap,
      width = 8.5,
      height = 4.8,
      dpi = 300)
  }
  
  # visualize structural bottlenecks by job family -------------------------
  
  # calculate overall high-constraint rate across employees
  overall_constraint_pct <- employees |>
    dplyr::summarise(
      high_constraint_pct =
        mean(high_constraint, na.rm = TRUE) * 100) |>
    dplyr::pull(high_constraint_pct)
  
  # prep data for plotting
  bottleneck_bar_data <- family_ceiling_summary |>
    dplyr::arrange(
      dplyr::desc(high_constraint_pct)) |>
    dplyr::mutate(
      # create presentation-friendly job-family names
      job_family_label = dplyr::case_when(
        job_family == "maintenance & mro" ~
          "Maintenance & MRO",
        job_family == "manufacturing/production tech" ~
          "Manufacturing/Production Tech",
        TRUE ~ tools::toTitleCase(job_family)),
      # add number of levels to each family label
      job_family_label = paste0(
        job_family_label,
        " (", levels_in_family, " levels)"),
      # classify families using structural evidence
      priority_group = dplyr::case_when(
        high_constraint_pct >= overall_constraint_pct &
          levels_in_family == 3 ~
          "High structural priority",
        high_constraint_pct >= overall_constraint_pct &
          levels_in_family > 3 ~
          "Structural review",
        TRUE ~ "Monitor"),
      # control legend order
      priority_group = factor(
        priority_group,
        levels = c(
          "High structural priority",
          "Structural review",
          "Monitor")),
      # order bars from highest to lowest after coord_flip()
      job_family_label = factor(
        job_family_label,
        levels = rev(job_family_label)),
      # show percentage, constrained count and family total
      bar_label = paste0(
        sprintf("%.2f", high_constraint_pct),
        "% (", high_constraint_n, " of ", employees, ")"))
  
  
  # horizontal bar chart
  structural_bottleneck_bar_chart <- ggplot2::ggplot(
    bottleneck_bar_data,
    ggplot2::aes(
      x = job_family_label,
      y = high_constraint_pct,
      fill = priority_group)) +
    ggplot2::geom_col(
      width = 0.70) +
    # add percentage and employee counts for each bar
    ggplot2::geom_text(
      ggplot2::aes(label = bar_label),
      hjust = -0.10,
      size = 2.8) +
    ggplot2::coord_flip(
      clip = "off") +
    # presentation blue and color-blind friendly emphasis colors
    ggplot2::scale_fill_manual(
      values = c(
        "High structural priority" = "#E69F00",
        "Structural review" = "#999999",
        "Monitor" = "#3333B3"),
      breaks = c(
        "High structural priority",
        "Structural review",
        "Monitor")) +
    # extra space allows the longer labels to fit
    ggplot2::scale_y_continuous(
      limits = c(0,max(bottleneck_bar_data$high_constraint_pct) + 8),
      breaks = seq(0, 30, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(
        mult = c(0, 0.02))) +
    ggplot2::labs(
      title = paste0(
        "Structural Ceilings Prevalence by ",
        "Job Family"),
      x = NULL,
      y = paste0(
        "Percentage within each job family meeting ",
        "the high-constraint definition"),
      fill = NULL,
      caption = paste0(
        "Overall high-constraint rate = ",
        sprintf("%.2f", overall_constraint_pct),
        "% (n = 6,000). High constraint = at least 2 of 3 conditions: ",
        "top of ladder, 48+ months in level, or 90%+ of pay band.")) +
    ggplot2::theme_minimal(
      base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13),
      plot.caption = ggplot2::element_text(
        size = 7,
        hjust = 0),
      axis.title.x = ggplot2::element_text(
        size = 9),
      axis.text.x = ggplot2::element_text(
        size = 8),
      axis.text.y = ggplot2::element_text(
        size = 8),
      panel.grid.major.y =
        ggplot2::element_blank(),
      panel.grid.minor =
        ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        size = 8),
      plot.margin = ggplot2::margin(
        t = 6,
        r = 65,
        b = 6,
        l = 6))
  
  
  # display chart only during an interactive R session
  if (interactive()) {
    print(structural_bottleneck_bar_chart)
  }
  
  if (save_figures) {
    # save chart
    ggplot2::ggsave(
      filename = here::here(
        "figures",
        "question1_figure2_structural_bottlenecks_by_job_family.png"),
      plot = structural_bottleneck_bar_chart,
      width = 9.5,
      height = 5.4,
      units = "in",
      dpi = 300,
      bg = "white")
  }
  
  list(employees = employees,
       job_architecture = job_architecture,
       pay_band_exceptions = pay_band_exceptions,
       family_ceiling_summary = family_ceiling_summary,
       worker_type_ceiling_summary = worker_type_ceiling_summary,
       site_ceiling_summary = site_ceiling_summary,
       family_site_ceiling_summary = family_site_ceiling_summary,
       family_site_priority = family_site_priority,
       bottleneck_plot_data = bottleneck_plot_data,
       bottleneck_heatmap = bottleneck_heatmap,
       overall_constraint_pct = overall_constraint_pct,
       bottleneck_bar_data = bottleneck_bar_data,
       structural_bottleneck_bar_chart = structural_bottleneck_bar_chart)
}

# [END]
