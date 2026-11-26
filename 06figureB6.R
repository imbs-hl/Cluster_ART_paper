# Reproduce Figure B6 of the benchmark experiment.
# Run 05run_benchmark_experiment.R to generate the benchmark results.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, this.path, cowplot)

# Resolve input and output paths relative to this script.
path_code <- this.dir()
proc_dir <- file.path(path_code, "proc")
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

source(file.path(path_code, "functions", "benchmark_plot_data.R"), local = TRUE)
results <- prepare_benchmark_plot_data(readRDS(file.path(proc_dir, "benchmark_results.Rds")))

# Panel A. Mean number of splits per tree.
plt_number_splits <- benchmark_panel(results, "number_splits", "Mean number of splits",
                                    outlier_size = 0.8) +
  theme(legend.position = "none")

# Panel B. Total number of terminal nodes in the ensemble.
plt_number_leaves <- benchmark_panel(results, "number_leaves", "Total number of terminal nodes",
                                    outlier_size = 0.8)

figure_b6 <- cowplot::plot_grid(plt_number_splits, plt_number_leaves, labels = "AUTO",
                                rel_heights = c(1, 1.15), ncol = 1)

ggsave(file.path(out_dir, "benchmark_number_splits.png"), plot = figure_b6,
       width = 30, height = 50, units = "cm", dpi = 200)
