##' With this script the figure 2 from Kronziel et al. "Improving Random Forest 
##' Interpretability with Clustering-Based Ensembles of Artificial 
##' Representative Trees" can be reproduced. Given a simulated data set. 
##' Run simulation_study.R to get such a data set. 
##' With the standard parameters in simulation_study.R, only a small number of 
##' repetitions are performed. In addition, the hyperparameters of the ARTEs and 
##' the RF were adjusted for runtime reasons, which means that the results may 
##' differ from those in the paper.

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
pacman::p_load(this.path())
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
## Load and prepare data
# data from publication
results <- readRDS(file.path(proc_dir, "results_paper.Rds"))

# data produced by simulations.R
results <- readRDS(file.path(proc_dir, "results.Rds")) %>% 
  bind_rows()

#---------------------------------------
# plot data and save plot
plt1 <- ggplot(results, aes(x = method, y = dist_variables, col = factor(k_used))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab("SV distance of trees in ensemble") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 13), legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

plt2 <- ggplot(results, aes(x = method, y = dist_weighted_variables, col = factor(k_used))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab("WSV distance of trees in ensemble") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  theme(text = element_text(size = 13), legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

plot_grid(plt1, plt2, labels = "AUTO", rel_widths = c(1, 1.15))

ggsave(file.path(out_dir, "abb2_distances_ensembles.png"), units = "cm", width = 30, height = 10, dpi=1200)
