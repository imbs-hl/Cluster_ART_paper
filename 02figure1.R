# Reproduce Figure 1 of the simulation study.
# Run 01run_simulation_study.R to generate proc/results.Rds.
# The default simulation uses fewer repetitions and smaller models than the paper run.

#---------------------------------------
# Load plotting and data preparation packages.
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(ggplot2)
pacman::p_load(devtools)
pacman::p_load(rpart)
pacman::p_load(dplyr)
pacman::p_load(cowplot)
pacman::p_load(this.path)

#---------------------------------------
# Resolve input and output paths relative to this script.
path_code <- this.dir()
if(!file.exists(path_code)){warning("Please change the directory path_code in simulation_study.R.")}


# Directory containing the simulation results.
dir.create(file.path(path_code, "proc"), showWarnings = FALSE)
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

# Label automatic selection separately from fixed ensemble sizes.
results <- results_imp %>%
  bind_rows() %>%
  mutate(k = ifelse(is.na(k_modus), k_used, ifelse(k_modus == "automaticaly", "automatic", k_used)),
         mse_rf_pred_weighted = ifelse(is.na(mse_rf_pred_weighted), mse_rf_pred_weighted, mse_rf_pred_weighted))


# Order fixed ensemble sizes numerically and place automatic selection last.
results$k = factor(results$k,
                   levels = c(as.character(1:10), "automatic"),
                   labels = c(as.character(1:10), "automatic"))

#---------------------------------------
# Colours for fixed ensemble sizes 1–10 and automatic selection.
color_palette <- c(rgb(252, 184, 18, maxColorValue = 255),
                   rgb(239, 91, 15, maxColorValue = 255),
                   rgb(225, 5, 27, maxColorValue = 255),
                   rgb(230, 0, 112, maxColorValue = 255),
                   rgb(73, 36, 128,, maxColorValue = 255),
                   rgb(0, 137, 190, maxColorValue = 255),
                   rgb(0, 200, 170, maxColorValue = 255),
                   rgb(0, 105, 62, maxColorValue = 255),
                   rgb(100, 200,70, maxColorValue = 255),
                   rgb(151, 255,13, maxColorValue = 255),
                   rgb(0, 0, 0, maxColorValue = 255)

)



# Panel A. Mean squared deviation from the random forest predictions.
plt1 = ggplot(results,
              aes(x = method, y = mse_rf_pred, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("Deviation from RF") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 14), legend.position = "none",
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)


# Add one RF test MSE per scenario and repetition using the MRT jobs with k = 1.
rf_reference <- results %>%
  filter(method == "MRTs", k == "1") %>%
  transmute(method = "RF", simulation, mse_test_dat_tree = mse_test_dat_rf, k)
results_mse <- bind_rows(results, rf_reference)

# Panel B. Test MSE for the ensembles and the original random forest.
plt2 = ggplot(results_mse, aes(x = method, y = mse_test_dat_tree, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("MSE on test data") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 14), legend.position = "none",
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)


# Panel C. BIC-type criterion combining test MSE and ensemble complexity.
plt3 = ggplot(results, aes(x = method, y = bic, colour = k)) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(colour = "used k", x = "") +
  ylab("BIC") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 14), legend.position = "right",
        legend.key.size =unit(0.6, 'cm'))+
  scale_colour_manual(values = color_palette)+
  facet_grid(simulation~.)

# Arrange panels A–C in one row.
figure1 <- plot_grid(plt1, plt2, plt3, labels = "AUTO", rel_widths = c(1, 1, 1.4), nrow = 1)

# Save the three-panel figure as a PNG.
ggsave(file.path(out_dir, "fig1_sim_deviation_mse_bic.png"), plot = figure1,
       units = "cm", width = 30, height = 9, dpi = 1200)




