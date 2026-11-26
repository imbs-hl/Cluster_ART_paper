# Reproduce Figure A2 of the simulation study.
# This figure uses both the simulation metrics and the fitted ensembles.

#---------------------------------------
# Load plotting and data preparation packages.
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(ggplot2)
pacman::p_load(dplyr)
pacman::p_load(ranger)
pacman::p_load(WGCNA)
pacman::p_load(this.path)

#---------------------------------------
# Resolve input and output paths relative to this script.
path_code <- this.dir()
if (!dir.exists(path_code)) {
  stop("Could not resolve the directory containing 02figureA2.R.")
}


# Directory containing the simulation results.
proc_dir <- file.path(path_code, "proc")
# Create the directory for exported figures.
dir.create(file.path(path_code, "output"), showWarnings = FALSE)
out_dir <- file.path(path_code, "output")


#---------------------------------------
# Load metrics and fitted ensembles from the local simulation.
results_imp <- readRDS(file.path(proc_dir, "results.Rds"))

ensemble_results <- c(
  readRDS(file.path(proc_dir, "arte_results.Rds")),
  readRDS(file.path(proc_dir, "mrte_results.Rds")),
  readRDS(file.path(proc_dir, "mrt_results.Rds")),
  readRDS(file.path(proc_dir, "rt_results.Rds"))
)

trees_imp <- lapply(ensemble_results, function(result) result$trees)

if (!"simulation" %in% names(results_imp)) {
  results_imp$simulation <- ifelse(results_imp$model == "Regression", "linear", "non-linear")
}

# Match ensemble models to result rows by batchtools job ID before filtering.
job_ids <- as.character(results_imp$job.id)
stopifnot(is.data.frame(results_imp), length(job_ids) == nrow(results_imp),
          !anyNA(job_ids), !anyDuplicated(job_ids), !anyDuplicated(names(trees_imp)),
          setequal(job_ids, names(trees_imp)))
trees_imp <- trees_imp[job_ids]

# Keep the WSV and Boruta configuration for ARTEs.
k_levels <- c(as.character(1:10), "automatic")
method_levels <- c("ARTEs", "MRTEs", "MRTs", "RT")
scenario_levels <- c("linear", "non-linear")
results <- results_imp %>%
  mutate(
    model_index = row_number(),
    simulation = if_else(simulation == "non-linear effects", "non-linear", simulation),
    method = if_else(method == "EARTs", "ARTEs", method),
    k = if_else(k_modus %in% c("automatic", "automatically", "automaticaly"),
                "automatic", as.character(k_used)),
    k = factor(k, levels = k_levels)
  ) %>%
  filter(method %in% method_levels,
         method != "ARTEs" |
           (imp.num.var == "automatic" &
              metric_cluster == "weighted splitting variables"))
stopifnot(nrow(results) > 0L, all(results$simulation %in% scenario_levels), !anyNA(results$k))

# Generate reference observations using the same seeds and row selection as ERT.
n_reference <- 1000L
reference_seed <- 20261101L
generator_env <- new.env()
# The linear generator calls ranger by name. No forest is needed for reference data.
generator_env$ranger <- function(...) NULL
sys.source(file.path(path_code, "functions", "simulate_linear_effects.R"), envir = generator_env)
sys.source(file.path(path_code, "functions", "simulate_nonlinear_effects.R"), envir = generator_env)

linear_info <- results %>% filter(simulation == "linear") %>% slice_head(n = 1L)
stopifnot(nrow(linear_info) == 1L)
set.seed(reference_seed)
linear_reference <- generator_env$simulate_data(
  n_train = 1L, n_test = n_reference, p_cor = linear_info$p_cor,
  nGenesncorr = 100L - 4L * linear_info$p_cor - 6L,
  minCor1 = 0.9, maxCor1 = 0.9, minCor2 = 0.3, maxCor2 = 0.3,
  b1 = linear_info$b1, b2 = linear_info$b2, mtry = linear_info$mtry,
  min.node.size = linear_info$min.node.size, num.trees = linear_info$num.trees
)$test_data
set.seed(reference_seed + 1L)
nonlinear_reference <- generator_env$generate_rulefit_data(n_reference + 1L)[-1L, ]
reference_data <- list(linear = linear_reference, `non-linear` = nonlinear_reference)

# Compare ensembles trained in different repetitions on the same reference data.
groups <- results %>% group_by(simulation, method, k) %>% group_split()
stability_results <- bind_rows(lapply(groups, function(group) {
  group_label <- sprintf("%s / %s / k = %s", group$simulation[1], group$method[1], group$k[1])
  if (nrow(group) < 2L) {
    warning("At least two fitted ensembles are required for ", group_label, ". Skipping.")
    return(NULL)
  }

  newdata <- reference_data[[group$simulation[1]]]
  predictions <- vapply(seq_len(nrow(group)), function(i) {
    model <- trees_imp[[group$model_index[i]]]
    if (!inherits(model, "ranger") || model$num.trees != group$k_used[i]) {
      stop("Invalid ensemble for ", group_label, " at export row ", group$model_index[i], ".")
    }
    variables <- model$forest$independent.variable.names
    stopifnot(length(variables) > 0L, all(variables %in% names(newdata)))
    predict(model, data = newdata[, variables, drop = FALSE], num.threads = 1)$predictions
  }, numeric(nrow(newdata)))
  stopifnot(all(is.finite(predictions)))

  # Include zero self-distances in each model's mean, as in the ERT analysis.
  pairwise_mse <- as.matrix(stats::dist(t(predictions)))^2 / nrow(newdata)
  group %>% transmute(simulation, method, k, prediction_distance = colMeans(pairwise_mse))
}))
if (nrow(stability_results) == 0L) {
  stop("No stability groups have at least two fitted ensembles.", call. = FALSE)
}
stability_results <- stability_results %>%
  mutate(simulation = factor(simulation, levels = scenario_levels),
         method = factor(method, levels = method_levels))

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

# Plot the distribution of model-level stability scores across repetitions.
figure_a2 <- ggplot(stability_results, aes(x = method, y = prediction_distance, colour = k)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge2(preserve = "single")) +
  facet_grid(simulation ~ ., scales = "free_y") +
  scale_colour_manual(values = color_palette) +
  labs(x = NULL, y = "Prediction stability", colour = "Ensemble size k") +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

ggsave(file.path(out_dir, "fig_A2_sim_prediction_stability.png"), plot = figure_a2,
       units = "cm", width = 15, height = 9, dpi = 300)

message("Please contact the authors to request the fitted ensemble models used for Figure A2 in the paper.")
