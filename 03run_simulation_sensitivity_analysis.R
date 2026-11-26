# Run the two ARTE sensitivity analyses from the simulation study.

## Local simulation settings
n_train_linear <- 200L
n_train_nonlinear <- 200L
n_test <- 200L
num.trees <- 20L
min.node.size_linear <- 100L
min.node.size_nonlinear <- 100L
repls <- 2L
k <- 2  # One ensemble size with distinct clustering configurations.
min.bucket <- 20L
probs_quantiles <- list(0.5)

## Original ERT settings
# n_train_linear <- 1000L
# n_train_nonlinear <- 5000L
# n_test <- 1000L
# num.trees <- 500L
# min.node.size_linear <- 100L
# min.node.size_nonlinear <- 500L  # ERT job script; the manuscript reports 100.
# repls <- 100L
# k <- as.numeric(1:10)
# min.bucket <- 100L
# probs_quantiles <- list(c(0.25, 0.5, 0.75))

## Settings shared by both configurations
p <- 100L
p_cor <- 3L
mtry <- sqrt(p)
metric <- "weighted splitting variables"
conf_file <- NA_character_  # Set the batchtools configuration path for a cluster run.

## Load the generators and ART ensemble code used in the main simulation
path_code <- this.path::this.dir()
function_dir <- file.path(path_code, "functions")
source(file.path(function_dir, "simulate_linear_effects.R"))
source(file.path(function_dir, "simulate_nonlinear_effects.R"))
source(file.path(function_dir, "get_artes.R"))

reg_dir <- file.path(path_code, "registries")
proc_dir <- file.path(path_code, "proc")
dir.create(reg_dir, showWarnings = FALSE)
dir.create(proc_dir, showWarnings = FALSE)

reg <- batchtools::makeExperimentRegistry(
  file.dir = tempfile("simulation_sensitivity_", tmpdir = reg_dir),
  work.dir = proc_dir,
  conf.file = conf_file,
  source = file.path(function_dir, c(
    "simulate_nonlinear_effects.R", "tree_subsection.R", "combine_tree.R"
  )),
  packages = c("ranger", "timbR", "rpart", "dplyr", "WGCNA", "Boruta"),
  seed = 12345L
)

batchtools::addProblem(name = "linear", data = 1, fun = simulate_data, seed = 12345, reg = reg)
batchtools::addProblem(
  name = "nonlinear", data = 1, fun = simulate_nonlinear_effects, seed = 12345, reg = reg
)
batchtools::addAlgorithm(name = "ARTEs", fun = get_artes, reg = reg)

prob.designs <- list(
  linear = data.frame(
    n_train = n_train_linear, n_test = n_test, p_cor = p_cor,
    nGenesncorr = p - 4L * p_cor - 6L,
    minCor1 = 0.9, maxCor1 = 0.9, minCor2 = 0.3, maxCor2 = 0.3,
    b1 = 1, b2 = 1, mtry = mtry,
    min.node.size = min.node.size_linear, num.trees = num.trees
  ),
  nonlinear = data.frame(
    n_train = n_train_nonlinear, n_test = n_test, mtry = mtry,
    min.node.size = min.node.size_nonlinear, num.trees = num.trees
  )
)

# Vary variable preselection under WSV clustering.
variable_design <- expand.grid(
  metric = metric, metric_cluster = metric, k = k,
  imp.num.var = list(10, "automatic", 100),
  probs_quantiles = probs_quantiles, min.bucket = min.bucket,
  stringsAsFactors = FALSE
)

# Vary clustering under Boruta preselection. The WSV/Boruta jobs above
# provide the baseline for both comparisons.
clustering_design <- expand.grid(
  metric = metric, metric_cluster = c("prediction", "random"), k = k,
  imp.num.var = list("automatic"),
  probs_quantiles = probs_quantiles, min.bucket = min.bucket,
  stringsAsFactors = FALSE
)
algo.designs <- list(ARTEs = rbind(variable_design, clustering_design))

ids <- batchtools::addExperiments(
  prob.designs = prob.designs, algo.designs = algo.designs, repls = repls, reg = reg
)
batchtools::summarizeExperiments(reg = reg)
batchtools::submitJobs(ids = ids, reg = reg)
batchtools::waitForJobs(ids = ids, reg = reg)

done_ids <- batchtools::findDone(ids = ids, reg = reg)
if (nrow(done_ids) != nrow(ids)) {
  warning(sprintf("Completed %d of %d sensitivity jobs. See registry %s.",
                  nrow(done_ids), nrow(ids), reg$file.dir))
}
if (nrow(done_ids) == 0L) {
  stop("No sensitivity jobs completed successfully.", call. = FALSE)
}

# Export metrics and fitted ensembles with matching job IDs.
job_results <- batchtools::reduceResultsList(ids = done_ids, reg = reg)
names(job_results) <- as.character(done_ids$job.id)
results <- dplyr::bind_rows(lapply(job_results, function(result) {
  metrics <- result[[1L]]
  metrics$imp.num.var <- as.character(metrics$imp.num.var)
  metrics
}), .id = "job.id")
job_table <- batchtools::getJobTable(ids = done_ids, reg = reg)
results$repl <- job_table$repl[match(results$job.id, job_table$job.id)]

saveRDS(results, file.path(proc_dir, "sensitivity_results.Rds"))
saveRDS(job_results, file.path(proc_dir, "sensitivity_arte_results.Rds"))
