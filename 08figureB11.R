# Reproduce Figure B11 of the benchmark sensitivity analysis.
# Run 07run_benchmark_sensitivity_analysis.R to generate the inputs.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, this.path)

# Resolve input and output paths relative to this script.
path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

source(file.path(path_code, "functions", "benchmark_sensitivity_plot_data.R"), local = TRUE)
results <- readRDS(file.path(proc_dir, "benchmark_sensitivity_results.Rds"))
results <- prepare_benchmark_sensitivity_plot_data(results)

figure_b11 <- benchmark_sensitivity_panel(results, "number_leaves", "Total number of terminal nodes")

ggsave(file.path(out_dir, "bench_cluster_metric_comparison_leaves.png"), plot = figure_b11,
       width = 30, height = 30, units = "cm", dpi = 200)
