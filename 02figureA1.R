# Reproduce Figure A1 of the simulation study.
# Run 01run_simulation_study.R to generate proc/results.Rds.
# The default simulation uses fewer repetitions and smaller models than the paper run.

#---------------------------------------
# Load plotting and data preparation packages.
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(ggplot2)
pacman::p_load(dplyr)
pacman::p_load(cowplot)
pacman::p_load(this.path)

#---------------------------------------
# Resolve input and output paths relative to this script.
path_code <- this.dir()
if (!dir.exists(path_code)) {
  stop("Could not resolve the directory containing 02figureA1.R.")
}


# Directory containing the simulation results.
proc_dir <- file.path(path_code, "proc")
# Create the directory for exported figures.
dir.create(file.path(path_code, "output"), showWarnings = FALSE)
out_dir <- file.path(path_code, "output")


#---------------------------------------
# Choose between the local simulation and the precalculated paper results.
use_paper_results <- FALSE
results_file <- if (use_paper_results) "results_paper.Rds" else "results.Rds"
results_imp <- readRDS(file.path(proc_dir, results_file))

if (!"simulation" %in% names(results_imp)) {
  results_imp$simulation <- ifelse(results_imp$model == "Regression", "linear", "non-linear")
}

# The paper export leaves the WSV clustering metric blank for ARTEs.
if (use_paper_results) {
  missing_wsv <- results_imp$method %in% c("EARTs", "ARTEs") & is.na(results_imp$metric_cluster)
  results_imp$metric_cluster[missing_wsv] <- "weighted splitting variables"
}

# Keep the main-analysis ARTE configuration and label automatic k separately.
k_levels <- c(as.character(1:10), "automatic")
results <- results_imp %>%
  mutate(
    simulation = if_else(simulation == "non-linear effects", "non-linear", simulation),
    method = if_else(method == "EARTs", "ARTEs", method),
    k = if_else(k_modus %in% "automaticaly", "automatic", as.character(k_used)),
    k = factor(k, levels = k_levels)
  ) %>%
  filter(method != "ARTEs" |
           (metric_cluster == "weighted splitting variables" & imp.num.var == "automatic"))

#---------------------------------------
# Colours for fixed ensemble sizes 1–10 and automatic selection.
color_palette <- setNames(c(rgb(252, 184, 18, maxColorValue = 255),
                   rgb(239, 91, 15, maxColorValue = 255),
                   rgb(225, 5, 27, maxColorValue = 255),
                   rgb(230, 0, 112, maxColorValue = 255),
                   rgb(73, 36, 128, maxColorValue = 255),
                   rgb(0, 137, 190, maxColorValue = 255),
                   rgb(0, 200, 170, maxColorValue = 255),
                   rgb(0, 105, 62, maxColorValue = 255),
                   rgb(100, 200,70, maxColorValue = 255),
                   rgb(151, 255,13, maxColorValue = 255),
                   rgb(0, 0, 0, maxColorValue = 255)),
                          k_levels)

#---------------------------------------

# Panel A. Mean number of splits per tree.
plt_number_splits <- ggplot(results, aes(x = method, y = number_splits, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("Mean number of splits per tree") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 15), legend.position = "none",
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)

# Panel B. Total number of terminal nodes in the ensemble.
plt_number_leaves <- ggplot(results, aes(x = method, y = number_leaves, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("Total number of terminal nodes") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 15), legend.position = "right",
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)


figure_a1 <- plot_grid(plt_number_splits, plt_number_leaves, labels = "AUTO", rel_widths = c(1, 1.6))

ggsave(file.path(out_dir, "fig_A1_sim_number_splits.png"), plot = figure_a1,
       units = "cm", width = 25, height = 10, dpi = 1200)
