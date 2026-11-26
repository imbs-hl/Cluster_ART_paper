# Reproduce Figure B7 of the benchmark experiment.
# Run 05run_benchmark_experiment.R to generate the benchmark results.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, this.path, OpenML, ranger)

# Resolve input and output paths relative to this script.
path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

source(file.path(path_code, "functions", "benchmark_plot_data.R"), local = TRUE)
results <- prepare_benchmark_plot_data(readRDS(file.path(proc_dir, "benchmark_results.Rds")))

# Match models to the filtered metrics via the job IDs from batchtools.
ensemble_results <- readRDS(file.path(proc_dir, "benchmark_ensemble_results.Rds"))
job_ids <- as.character(results$job.id)
stopifnot(length(job_ids) == nrow(results), !anyNA(job_ids), !anyDuplicated(job_ids),
          !anyDuplicated(names(ensemble_results)), all(job_ids %in% names(ensemble_results)),
          !anyDuplicated(results[c("task_id", "method", "k", "fold")]))

# Reuse the OpenML cache and the preprocessing from the benchmark run.
source(file.path(path_code, "functions", "prepare_benchmark_task.R"), local = TRUE)
cache_dir <- file.path(path_code, "registries", "openml_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
invisible(utils::capture.output(OpenML::setOMLConfig(
  server = "https://www.openml.org/api/v1", cachedir = cache_dir
)))

# Predict on all preprocessed observations in each task, as in the paper.
task_results <- split(results, results$task_id)
stability_results <- bind_rows(lapply(task_results, function(rows) {
  task_id <- rows$task_id[1L]
  task <- prepare_benchmark_task(OpenML::getOMLTask(task.id = task_id))
  stopifnot(all(as.character(rows$task_version) == as.character(task$metadata$task_version)),
            all(rows$n_na_free_unique_outlier == nrow(task$data)))
  reference_data <- task$data

  groups <- rows %>% group_by(method, k) %>% group_split()
  bind_rows(lapply(groups, function(group) {
    if (nrow(group) < 2L) {
      warning("Skipping a stability group with fewer than two folds: ", group$task_name[1L],
              " / ", group$method[1L], " / k = ", group$k[1L], ".", call. = FALSE)
      return(NULL)
    }

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
    group %>% transmute(task_name, method, k, prediction_distance = colMeans(pairwise_mse))
  }))
}))
if (!nrow(stability_results)) {
  stop("No benchmark stability groups have at least two fitted models.", call. = FALSE)
}
stability_results <- stability_results %>%
  mutate(method = factor(method, levels = c("ARTEs", "MRTEs", "MRTs", "RTs")))
figure_b7 <- benchmark_panel(
  stability_results, "prediction_distance", "Prediction stability",
  position = position_dodge2(preserve = "single"), outlier_size = 0.6
) +
  labs(colour = "Ensemble size k") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "benchmark_prediction_stability.png"), plot = figure_b7,
       width = 30, height = 30, units = "cm", dpi = 200)
