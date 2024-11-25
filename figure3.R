##' With this script the figure 3 from Kronziel et al. "Improving Random Forest 
##' Interpretability with Clustering-Based Ensembles of Artificial 
##' Representative Trees" can be reproduced. Given a simulated data set. 
##' Run simulation_study.R to get such a data set. 
##' With the standard parameters in simulation_study.R, only a small number of 
##' repetitions are performed. In addition, the hyperparameters of the ARTEs and 
##' the RF were adjusted for runtime reasons, which means that the results may 
##' differ from those in the paper.

#---------------------------------------
## Define directories
## Please define your main directory here. 
## This should be the directory you cloned the git repository into.
path_code <- this.path::this.dir()
if(!file.exists(path_code)){warning("Please change the directory path_code in simulation_study.R.")}


## Create and define proc directory
dir.create(file.path(path_code, "proc"), showWarnings = FALSE)
proc_dir <- file.path(path_code, "proc")
## Create and define output directory
dir.create(file.path(path_code, "output"), showWarnings = FALSE)
out_dir <- file.path(path_code, "output")

#---------------------------------------
## Load libraries
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(ggplot2)
pacman::p_load(devtools)
pacman::p_load(rpart)
pacman::p_load(dplyr)
pacman::p_load(cowplot)

#---------------------------------------
## Load and prepare data
# data from publication
results <- read.csv2(file.path(proc_dir, "simulation_data.csv"))

# data produced by simulations.R
results <- readRDS(file.path(proc_dir, "results.Rds")) %>% 
  bind_rows()

#---------------------------------------
# plot data and save plot

plt1 <- ggplot(results, aes(x = method, y = covered_effect_vars, col = factor(k_used))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab("Covered effect variables") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 13), legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

plt2 <- ggplot(results, aes(x = method, y = covered_correlated_vars, col = factor(k_used))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab("Covered correlated variables") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 13), legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

plt3 <- ggplot(results, aes(x = method, y = covered_noise_vars, col = factor(k_used))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab("Covered noise variables") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 13), legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

plot_grid(plt1, plt2, plt3, labels = "AUTO", rel_widths = c(1, 1, 1.2), ncol = 3)

ggsave(file.path(out_dir, "abb3_covered_vars.png"), units = "cm", width = 30, height = 9, dpi=1200)

