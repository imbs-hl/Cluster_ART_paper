# Reproduce Figure B10 of the benchmark sensitivity analysis.
# Run 07run_benchmark_sensitivity_analysis.R to generate the inputs.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, this.path, OpenML, ranger)

# Resolve input and output paths relative to this script.
path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

source(file.path(path_code, "functions", "benchmark_sensitivity_plot_data.R"), local = TRUE)
results <- readRDS(file.path(proc_dir, "benchmark_sensitivity_results.Rds"))
expected_folds <- attr(results, "benchmark_design")$folds
results <- prepare_benchmark_sensitivity_plot_data(results)
if (is.null(expected_folds)) {
  expected_folds <- sort(unique(results$fold))
}
if (length(expected_folds) < 2L) {
  stop("Prediction stability requires at least two planned benchmark folds.", call. = FALSE)
}

# Use only task and k combinations with all planned folds for all three variants.
stopifnot(!anyDuplicated(results[c("task_id", "k", "clustering", "fold")]))
complete_settings <- results %>%
  group_by(task_id, k, clustering) %>%
  summarise(complete = setequal(fold, expected_folds), .groups = "drop") %>%
  group_by(task_id, k) %>%
  summarise(complete = n() == length(benchmark_clustering_metrics) && all(complete),
            .groups = "drop") %>%
  filter(complete)
results <- semi_join(results, complete_settings, by = c("task_id", "k"))
if (!nrow(results)) {
  stop("No task and ensemble size has all planned folds for all three clustering methods.",
       call. = FALSE)
}

# Match each model to its metrics row by job ID.
ensemble_results <- readRDS(file.path(proc_dir, "benchmark_sensitivity_ensemble_results.Rds"))
job_ids <- as.character(results$job.id)
stopifnot(length(job_ids) == nrow(results), !anyNA(job_ids), !anyDuplicated(job_ids),
          !anyDuplicated(names(ensemble_results)), all(job_ids %in% names(ensemble_results)))

# Use all preprocessed observations for the task as a common reference.
source(file.path(path_code, "functions", "prepare_benchmark_task.R"), local = TRUE)
cache_dir <- file.path(path_code, "registries", "openml_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
invisible(utils::capture.output(OpenML::setOMLConfig(
  server = "https://www.openml.org/api/v1", cachedir = cache_dir
)))

stability_results <- bind_rows(lapply(split(results, results$task_id), function(rows) {
  task <- prepare_benchmark_task(OpenML::getOMLTask(task.id = rows$task_id[1L]))
  reference_data <- task$data
  stopifnot(all(as.character(rows$task_version) == as.character(task$metadata$task_version)),
            all(rows$n_na_free_unique_outlier == nrow(reference_data)))

  groups <- rows %>% group_by(k, clustering) %>% group_split()
  bind_rows(lapply(groups, function(group) {
    predictions <- vapply(seq_len(nrow(group)), function(i) {
      model <- ensemble_results[[as.character(group$job.id[i])]]$trees
      stopifnot(inherits(model, "ranger"), model$num.trees == group$k_used[i])
      variables <- model$forest$independent.variable.names
      stopifnot(length(variables) > 0L, all(variables %in% names(reference_data)))
      predict(model, data = reference_data[, variables, drop = FALSE], num.threads = 1L)$predictions
    }, numeric(nrow(reference_data)))
    stopifnot(all(is.finite(predictions)))

    # Include each model's zero self-distance in its mean pairwise MSE.
    pairwise_mse <- as.matrix(stats::dist(t(predictions)))^2 / nrow(reference_data)
    group %>% transmute(task_name, fold, k, clustering, prediction_distance = colMeans(pairwise_mse))
  }))
}))

source(file.path(path_code, "functions", "benchmark_plot_data.R"), local = TRUE)
figure_b10 <- ggplot(stability_results, aes(x = clustering, y = prediction_distance, colour = k)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge2(preserve = "single")) +
  facet_wrap(~ task_name, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = benchmark_colours) +
  labs(x = NULL, y = "Prediction stability", colour = "Ensemble size k") +
  theme_bw(base_size = 16) +
  theme(strip.background = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 14)) +
  guides(colour = guide_legend(nrow = 1))

ggsave(file.path(out_dir, "bench_cluster_metric_comparison_stability.png"), plot = figure_b10,
       width = 30, height = 30, units = "cm", dpi = 200)
