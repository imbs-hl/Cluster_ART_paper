# Reproduce Figure B5 of the benchmark experiment.
# Run 05run_benchmark_experiment.R to generate the benchmark results.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, this.path)

# Resolve input and output paths relative to this script.
path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

source(file.path(path_code, "functions", "benchmark_plot_data.R"), local = TRUE)
results <- prepare_benchmark_plot_data(readRDS(file.path(proc_dir, "benchmark_results.Rds")))

# Add one RF test MSE per task and fold using the MRT results for k = 1.
rf_reference <- results %>%
  filter(method == "MRTs", k == "1") %>%
  transmute(task_id, task_name, fold, method = "RF", k, mse_test_dat_tree = mse_test_dat_rf)
results_mse <- bind_rows(results, rf_reference)
figure_b5 <- benchmark_panel(results_mse, "mse_test_dat_tree", "MSE on test data")

ggsave(file.path(out_dir, "benchmark_mse_test_data.png"), plot = figure_b5,
       width = 30, height = 30, units = "cm", dpi = 200)
