# Reproduce Figure A4 from the local simulation sensitivity analysis.
# Run 03run_simulation_sensitivity_analysis.R to generate the metrics and ensembles.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, cowplot, ranger, WGCNA, this.path)

path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

# Use only the two exports produced by the sensitivity simulation.
results_path <- file.path(proc_dir, "sensitivity_results.Rds")
ensembles_path <- file.path(proc_dir, "sensitivity_arte_results.Rds")
if (!all(file.exists(results_path, ensembles_path))) {
  stop("Run 03run_simulation_sensitivity_analysis.R to generate the Figure A4 inputs.",
       call. = FALSE)
}
results <- readRDS(results_path)
ensembles <- readRDS(ensembles_path)
job_ids <- as.character(results$job.id)
stopifnot(length(job_ids) == nrow(results), !anyNA(job_ids), !anyDuplicated(job_ids),
          !anyDuplicated(names(ensembles)), setequal(job_ids, names(ensembles)))

# Compare the three clustering settings under Boruta variable selection.
clustering_metrics <- c("weighted splitting variables", "prediction", "random")
clustering_levels <- c("WSV", "Prediction", "Random")
scenario_levels <- c("linear", "non-linear")
k_levels <- c(as.character(1:10), "automatic")
results <- results %>%
  filter(method %in% c("ARTEs", "EARTs"),
         metric_cluster %in% clustering_metrics,
         imp.num.var == "automatic") %>%
  mutate(
    simulation = factor(simulation, levels = scenario_levels),
    k = if_else(k_modus %in% c("automatic", "automatically", "automaticaly"),
                "automatic", as.character(k_used)),
    k = factor(k, levels = k_levels),
    clustering = factor(metric_cluster, levels = clustering_metrics, labels = clustering_levels),
    # Pairwise tree distances require at least two trees.
    dist_variables = if_else(k_used >= 2, dist_variables, NA_real_),
    dist_weighted_variables = if_else(k_used >= 2, dist_weighted_variables, NA_real_),
    covered_correlated_vars = if_else(simulation == "non-linear", NA_real_, covered_correlated_vars)
  )
stopifnot(nrow(results) > 0L, !anyNA(results$simulation), !anyNA(results$k))

# Reuse the original A3 reference seeds and row offsets. No RF fit is needed.
n_reference <- 1000L
reference_seed <- 20261101L
generator_env <- new.env()
generator_env$ranger <- function(...) NULL
sys.source(file.path(path_code, "functions", "simulate_linear_effects.R"), envir = generator_env)
sys.source(file.path(path_code, "functions", "simulate_nonlinear_effects.R"), envir = generator_env)

linear_info <- results %>% filter(simulation == "linear") %>% slice_head(n = 1L)
stopifnot(nrow(linear_info) == 1L)
set.seed(reference_seed)
linear_reference <- generator_env$simulate_data(
  n_train = 1000L, n_test = n_reference, p_cor = linear_info$p_cor,
  nGenesncorr = 100L - 4L * linear_info$p_cor - 6L,
  minCor1 = 0.9, maxCor1 = 0.9, minCor2 = 0.3, maxCor2 = 0.3,
  b1 = linear_info$b1, b2 = linear_info$b2, mtry = linear_info$mtry,
  min.node.size = linear_info$min.node.size, num.trees = linear_info$num.trees
)$test_data
set.seed(reference_seed + 1L)
nonlinear_reference <- generator_env$generate_rulefit_data(100L + n_reference)[100L + seq_len(n_reference), ]
reference_data <- list(linear = linear_reference, `non-linear` = nonlinear_reference)

# Compare predictions across repetitions for each scenario, k and clustering setting.
results <- results %>%
  group_by(simulation, k, clustering) %>%
  group_modify(function(group, key) {
    if (nrow(group) < 2L) {
      warning("At least two models are required for stability in ",
              key$simulation, " / k = ", key$k, " / ", key$clustering, ".")
      group$prediction_stability <- NA_real_
      return(group)
    }

    newdata <- reference_data[[as.character(key$simulation)]]
    predictions <- vapply(seq_len(nrow(group)), function(i) {
      job_id <- as.character(group$job.id[i])
      model <- ensembles[[job_id]]$trees
      if (!inherits(model, "ranger") || model$num.trees != group$k_used[i]) {
        stop("Invalid ARTE ensemble for job ", job_id, ".", call. = FALSE)
      }
      variables <- model$forest$independent.variable.names
      stopifnot(length(variables) > 0L, all(variables %in% names(newdata)))
      predict(model, data = newdata[, variables, drop = FALSE], num.threads = 1)$predictions
    }, numeric(nrow(newdata)))
    stopifnot(all(is.finite(predictions)))
    # Include zero self-distances in each model's mean, as in the paper analysis.
    pairwise_mse <- as.matrix(stats::dist(t(predictions)))^2 / nrow(newdata)
    group$prediction_stability <- colMeans(pairwise_mse)
    group
  }) %>%
  ungroup()

# Arrange the eleven panels in the order used in Figure A4.
panel_labels <- c(
  mse_rf_pred = "Deviation from RF",
  mse_test_dat_tree = "MSE on test data",
  bic = "BIC",
  number_leaves = "Sum of all leaves in ensemble",
  dist_variables = "SV distance",
  dist_weighted_variables = "WSV distance",
  covered_effect_vars = "Covered effect variables",
  covered_correlated_vars = "Covered correlated variables",
  covered_noise_vars = "Covered noise variables",
  runtime = "Runtime (min)",
  prediction_stability = "Prediction stability"
)
clustering_colours <- setNames(c("#492480", "#FCB812", "#E60070"), clustering_levels)
make_panel <- function(metric) {
  ggplot(results, aes(x = k, y = .data[[metric]], fill = clustering)) +
    geom_boxplot(outlier.size = 0.8, na.rm = TRUE) +
    facet_grid(simulation ~ ., scales = "free_y") +
    scale_fill_manual(values = clustering_colours, drop = FALSE) +
    labs(x = "Ensemble size k", y = panel_labels[[metric]], fill = "Clustering method") +
    theme_bw(base_size = 15) +
    theme(strip.background = element_blank(), strip.placement = "outside",
          legend.position = "bottom")
}
plots <- lapply(names(panel_labels), make_panel)
legend <- cowplot::get_legend(plots[[1L]])
panels <- cowplot::plot_grid(
  plotlist = lapply(plots, function(plot) plot + theme(legend.position = "none")),
  ncol = 3, align = "hv"
)
figure_a4 <- cowplot::plot_grid(panels, legend, ncol = 1, rel_heights = c(1, 0.05))

ggsave(file.path(out_dir, "fig_A4_sim_cluster_metric_comparison.png"), plot = figure_a4,
       units = "cm", width = 40, height = 45, dpi = 300)

message("Please contact the authors to request the sensitivity analysis results used in the paper.")
