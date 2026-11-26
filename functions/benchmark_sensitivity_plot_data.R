# Prepare the fixed-size ARTE comparisons used in Figures B8–B15.
benchmark_clustering_metrics <- c("weighted splitting variables", "prediction", "random")
benchmark_clustering_labels <- c("WSV", "Prediction", "Random")
benchmark_clustering_colours <- setNames(
  c("#492480", "#FCB812", "#E60070"), benchmark_clustering_labels
)

prepare_benchmark_sensitivity_plot_data <- function(results) {
  # Retain the dataset exclusions used in the original sensitivity figures.
  excluded_datasets <- c(
    "Airlines_DepDelay_10M", "MIP-2016-regression", "Santander_transaction_value",
    "Yolanda", "quake", "sensory", "topo_2_1", "wine_quality", "yprop_4_1",
    "nyc-taxi-green-dec-2016", "Allstate_Claims_Severity", "black_friday",
    "Buzzinsocialmedia_Twitter", "Mercedes_Benz_Greener_Manufacturing"
  )
  data <- results %>%
    filter(
      method %in% c("EARTs", "ARTEs"),
      metric == "weighted splitting variables",
      imp.num.var == "automatic",
      metric_cluster %in% benchmark_clustering_metrics,
      !k_modus %in% c("automatic", "automatically", "automaticaly"),
      k_used %in% 1:10,
      !task_name %in% excluded_datasets
    ) %>%
    mutate(
      task_name = recode(as.character(task_name), "SAT11-HAND-runtime-regression" = "SAT11"),
      k = factor(k_used, levels = 1:10),
      clustering = factor(metric_cluster, levels = benchmark_clustering_metrics,
                          labels = benchmark_clustering_labels)
    )
  if (!nrow(data)) {
    stop("No eligible fixed-size ARTE sensitivity results. Run 07run_benchmark_sensitivity_analysis.R.",
         call. = FALSE)
  }
  data
}

benchmark_sensitivity_panel <- function(results, measure, y_label) {
  ggplot(results, aes(x = k, y = .data[[measure]], fill = clustering)) +
    geom_boxplot(outlier.size = 0.8) +
    theme_bw(base_size = 16) +
    facet_wrap(~ task_name, scales = "free_y", ncol = 4) +
    labs(x = "used k", y = y_label, fill = "Clustering metric") +
    scale_fill_manual(values = benchmark_clustering_colours) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(strip.background = element_blank(), strip.placement = "outside",
          text = element_text(size = 13), legend.position = "bottom",
          legend.key.size = grid::unit(0.6, "cm"))
}
