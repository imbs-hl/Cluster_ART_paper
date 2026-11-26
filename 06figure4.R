# Reproduce Figure 4 of the benchmark experiment.
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

# Compare ensemble predictions with predictions from the original RF.
figure4 <- benchmark_panel(results, "mse_rf_pred", "Deviation from RF")

ggsave(file.path(out_dir, "benchmark_deviation_rf.png"), plot = figure4,
       width = 30, height = 30, units = "cm", dpi = 200)
