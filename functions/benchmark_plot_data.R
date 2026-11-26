# Prepare the WSV main-analysis results consistently across the benchmark figures.
prepare_benchmark_plot_data <- function(results) {
  design <- attr(results, "benchmark_design")
  k_levels <- c(as.character(1:10), "automatic")
  results <- results %>%
    mutate(
      method = recode(as.character(method), EARTs = "ARTEs", RT = "RTs"),
      k = if_else(k_modus %in% c("automatic", "automatically", "automaticaly"),
                  "automatic", as.character(k_used)),
      task_name = recode(as.character(task_name),
                         "SAT11-HAND-runtime-regression" = "SAT11",
                         "Mercedes_Benz_Greener_Manufacturing" = "Mercedes_Benz")
    ) %>%
    filter(metric == "weighted splitting variables",
           method %in% c("ARTEs", "MRTEs", "MRTs", "RTs"),
           method != "ARTEs" | (imp.num.var == "automatic" &
                                 metric_cluster == "weighted splitting variables"))

  # Count each RF once when applying the dataset-level OOB R-squared threshold.
  eligible_tasks <- results %>%
    distinct(task_id, fold, r_squared_rf) %>%
    group_by(task_id) %>%
    summarise(mean_oob_r_squared = mean(r_squared_rf, na.rm = TRUE), .groups = "drop") %>%
    filter(mean_oob_r_squared >= 0.3)
  results <- semi_join(results, eligible_tasks, by = "task_id")

  # Use the recorded design when available. Older exports contain only the finished jobs.
  expected_folds <- if (is.null(design)) unique(results$fold) else design$folds
  fixed_k <- if (is.null(design)) unique(results$k[results$k != "automatic"]) else as.character(design$k)
  arte_k <- if (is.null(design)) {
    union(fixed_k, results$k[results$method == "ARTEs"])
  } else {
    ifelse(is.na(design$k_art), "automatic", as.character(design$k_art))
  }
  complete_methods <- results %>%
    group_by(task_id, method) %>%
    summarise(
      complete = {
        expected_k <- if (first(method) == "ARTEs") arte_k else fixed_k
        n() == length(expected_folds) * length(expected_k) &&
          setequal(fold, expected_folds) && setequal(k, expected_k) &&
          n_distinct(fold, k) == n()
      },
      .groups = "drop"
    ) %>%
    filter(complete)
  results <- semi_join(results, complete_methods, by = c("task_id", "method"))
  if (!nrow(results)) {
    stop("No complete benchmark method results meet the RF OOB R-squared threshold.", call. = FALSE)
  }
  mutate(results, k = factor(k, levels = k_levels))
}

benchmark_k_levels <- c(as.character(1:10), "automatic")
benchmark_colours <- setNames(
  c("#FCB812", "#EF5B0F", "#E1051B", "#E60070", "#492480", "#0089BE",
    "#00C8AA", "#00693E", "#64C846", "#97FF0D", "#000000"),
  benchmark_k_levels
)

# Facet by dataset and retain the original layout and ensemble-size legend.
benchmark_panel <- function(results, measure, y_label, position = "dodge2", outlier_size = 1.5) {
  ggplot(results, aes(x = method, y = .data[[measure]], colour = k)) +
    geom_boxplot(position = position, outlier.size = outlier_size) +
    theme_bw(base_size = 16) +
    facet_wrap(~ task_name, scales = "free_y", ncol = 4) +
    labs(colour = "used k", x = NULL, y = y_label) +
    scale_colour_manual(values = benchmark_colours, breaks = benchmark_k_levels) +
    guides(colour = guide_legend(nrow = 1)) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.x = element_text(size = 12),
      legend.position = "bottom",
      legend.key.size = grid::unit(0.6, "cm"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}
