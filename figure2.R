##' With this script the figure 2 from Kronziel et al. "Improving Random Forest 
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
path_code <- "/Users/kronziel/mywork/diss/ERT/R/paper_code/Cluster_ART_paper"
if(!file.exists(path_code)){warning("Please change the directory path_code in figure1.R.")}
setwd(path_code)

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

#---------------------------------------
## Load and prepare data
# data from publication
results <- readRDS(file.path(proc_dir, "results_paper.Rds"))

# data produced by simulations.R
results <- readRDS(file.path(proc_dir, "results.Rds"))

## Transform results from list to data.frame
## melt data frame to use it for ggplot
plot_data <- results %>% 
  bind_rows() %>% 
  select(method, k_used, dist_variables, dist_weighted_variables) %>% 
  mutate(k_used = as.factor(k_used)) %>% 
  melt(id = c("method", "k_used")) %>%
  mutate(
    x_label = case_when(
      variable == "dist_variables" ~ "A",
      variable == "dist_weighted_variables" ~ "B"
    )
  )


#---------------------------------------
# plot data and save plot
ggplot(plot_data, aes(x = method, y = value, col = k_used)) +
  geom_boxplot(outlier.size = 0.8) +
  facet_wrap(variable~.,
             strip.position = "left",
             scales = "free_y",
             labeller = as_labeller(c("dist_variables" = "SV distance of trees in ensemble", 
                                      "dist_weighted_variables" = "WSV distance of trees in ensemble"))) +
  theme_bw() +
  labs(col = "used k", x = "") +
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  # Texte oben links in die Facets setzen
  geom_text(
    data = plot_data %>% distinct(variable, x_label), 
    aes(x = -Inf, y = Inf, label = x_label), 
    hjust = -0.5, vjust = 1.5, 
    inherit.aes = FALSE,
    size = 5
  ) +
  theme(text = element_text(size = 18), legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size =unit(0.6, 'cm'))

ggsave(file.path(out_dir, "abb2_distances_ensembles.png"), units = "cm", width = 30, height = 10, dpi=1200)
