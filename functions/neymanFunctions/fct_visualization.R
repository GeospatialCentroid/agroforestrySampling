library(ggplot2)
library(dplyr)
library(tidyr)

#' Plot the Accuracy Convergence Curve
#' Shows how accuracy improves as sample size increases
plot_convergence_curve <- function(results_df) {
  
  # Reshape data to long format for faceting
  long_df <- results_df %>%
    tidyr::pivot_longer(
      cols = c("percentTRUE", "percentCOVERED"),
      names_to = "metric_name",
      values_to = "metric_value"
    ) %>%
    mutate(
      metric_label = case_when(
        metric_name == "percentTRUE" ~ "Accuracy (Est within +/- 10%)",
        metric_name == "percentCOVERED" ~ "Reliability (Truth within 95% CI)"
      )
    )
  
  ggplot(long_df, aes(x = runNumber, y = metric_value, color = method)) +
    
    # Facet by Metric (Stacked vertically)
    facet_grid(metric_label ~ ., scales = "free_y") +
    
    # Reference Lines
    # 90% for Accuracy, 95% for Coverage
    geom_hline(data = filter(long_df, metric_name == "percentTRUE"), 
               yintercept = 90, linetype = "dashed", color = "gray50") +
    geom_hline(data = filter(long_df, metric_name == "percentCOVERED"), 
               yintercept = 95, linetype = "dashed", color = "gray50") +
    
    # Main trend lines
    geom_line(linewidth = 1, alpha = 0.8) +
    # geom_point(size = 1, alpha = 0.6) + # Optional dots
    
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
    scale_x_continuous(breaks = seq(0, max(results_df$runNumber), 10)) +
    scale_color_brewer(palette = "Set1") +
    
    theme_light() +
    labs(
      title = "Sampling Performance: Accuracy vs. Reliability",
      subtitle = "Top: Does the estimate hit the target? | Bottom: Are the error bars honest?",
      x = "Sample Size Iteration (~1% increments)",
      y = "% of Simulations",
      color = "Stratification Method"
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "#333333"),
      strip.text = element_text(color = "white", face = "bold")
    )
}

#' Plot Snapshot at Specific Sample Budgets
#' Facets are ordered strictly by the input 'target_runs' vector
plot_efficiency_bar <- function(results_df, target_runs = c(5, 10, 20)) {
  
  # 1. Prepare Labels
  ordered_labels <- paste0("Sample Size: ~", target_runs, "%")
  
  # 2. Filter & Reshape
  subset_df <- results_df %>%
    filter(runNumber %in% target_runs) %>%
    pivot_longer(
      cols = c("percentTRUE", "percentCOVERED"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      label_text = paste0("Sample Size: ~", runNumber, "%"),
      run_label = factor(label_text, levels = ordered_labels),
      # Clean names for legend
      metric_clean = ifelse(metric == "percentTRUE", "Accuracy", "Coverage")
    )
  
  ggplot(subset_df, aes(x = reorder(method, -value), y = value, fill = method, alpha = metric_clean)) +
    geom_col(position = "dodge", color = "white") +
    
    facet_wrap(~run_label) +
    
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_brewer(palette = "Set1") +
    
    # Use Alpha to distinguish Accuracy vs Coverage
    scale_alpha_manual(values = c("Accuracy" = 1, "Coverage" = 0.4), name = "Metric") +
    
    coord_flip() + 
    
    theme_light() +
    labs(
      title = "Efficiency Snapshot: Accuracy vs. Coverage",
      subtitle = "Solid bars = Accuracy | Faded bars = Coverage (95% CI)",
      x = NULL,
      y = "Percent Success"
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "#333333"),
      strip.text = element_text(color = "white", face = "bold")
    )
}


#' Identify Minimum Viable Sample Size
#' Finds the first sample size where a method crosses the thresholds AND stays there.
#' Identify Minimum Viable Sample Size (with % Population)
get_viability_thresholds <- function(results_df, total_population, acc_threshold = 90, cov_threshold = 90) {
  
  results_df %>%
    group_by(method) %>%
    arrange(method, runNumber) %>%
    mutate(
      is_successful = percentTRUE >= acc_threshold & percentCOVERED >= cov_threshold,
      future_success = rev(cummin(rev(is_successful)))
    ) %>%
    filter(future_success == 1) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      # Calculate the Raw Sample Count
      raw_samples = runNumber * 100,
      
      # Calculate the True Sampling Percentage (Sampling Fraction)
      pct_of_population = round((raw_samples / total_population) * 100, 2)
    ) %>%
    select(
      method, 
      run_number = runNumber,
      raw_samples,
      pct_of_population,        # <--- New Column
      accuracy_at_min = percentTRUE, 
      coverage_at_min = percentCOVERED
    ) %>%
    arrange(pct_of_population)
}



plot_efficiency_thresholds <- function(results_df, viability_df) {
  
  ggplot(results_df, aes(x = runNumber, y = percentTRUE, color = method)) +
    geom_line(alpha = 0.3) + # Faded lines for context
    
    # Highlight the "Viable" point
    # UPDATE: Use 'run_number' instead of 'min_sample_size'
    geom_point(data = viability_df, 
               aes(x = run_number, y = accuracy_at_min, fill = method),
               shape = 24, size = 5, color = "black", stroke = 1) +
    
    # UPDATE: Use 'pct_of_population' for the label text
    geom_text(data = viability_df,
              aes(x = run_number, y = accuracy_at_min + 7, 
                  label = paste0(pct_of_population, "%")),
              color = "black", fontface = "bold", size = 4) +
    
    scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    
    theme_light() +
    labs(
      title = "Minimum Viable Sample Size",
      subtitle = "Triangles mark where method meets 90% Accuracy AND 90% Coverage",
      x = "Sample Size Iteration (1 unit = 100 samples)", 
      y = "Accuracy (%)"
    )
}

