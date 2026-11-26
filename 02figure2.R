# Reproduce Figure 2 of the simulation study.
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
  stop("Could not resolve the directory containing 02figure2.R.")
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
# Panel A. Splitting-variable (SV) distances within ensembles.
plt1 = ggplot(results, aes(x = method, y = dist_variables, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("SV distance of trees in ensemble") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.title = element_text(size = 14),
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)

# Panel B. Weighted splitting-variable (WSV) distances within ensembles.
plt2 = ggplot(results, aes(x = method, y = dist_weighted_variables, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("WSV distance of trees in ensemble") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 16), legend.position = "right",
        axis.title = element_text(size = 14),
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)

# Arrange panels A and B in one row.
figure2 <- plot_grid(plt1, plt2, labels = "AUTO", rel_widths = c(1, 1.3))

ggsave(file.path(out_dir, "fig2_sim_distances_ensembles.png"), plot = figure2,
       units = "cm", width = 30, height = 10, dpi = 1200)
