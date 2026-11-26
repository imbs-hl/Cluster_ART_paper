# Run the benchmark experiment on regression tasks downloaded from OpenML.

## Local benchmark settings
task_ids <- 359950L  # Boston, a small task from the paper.
num_folds <- 7L
folds_to_run <- 1:2
num.trees <- 20L
k <- as.numeric(1:2)
k_art <- k
probs_quantiles <- list(0.5)

## Original ERT benchmark settings
# task_ids <- c(
#   233211L, 359945L, 359933L, 359935L, 359952L, 359940L, 359931L,
#   233214L, 317614L, 359949L, 359934L, 359946L, 359938L, 359932L,
#   359943L, 359948L, 359944L, 359937L, 233215L, 233212L, 359936L,
#   359939L, 359930L, 359950L, 167210L, 233213L, 360945L, 359929L
# )
# num_folds <- 7L
# folds_to_run <- seq_len(num_folds)
# num.trees <- 500L
# k <- as.numeric(1:10)
# k_art <- c(k, NA_real_)
# probs_quantiles <- list(c(0.25, 0.5, 0.75))

## Shared settings
metric <- "weighted splitting variables"
imp.num.var <- "automatic"
min.bucket_perc <- 0.1
conf_file <- NA_character_  # Set the batchtools configuration path for a cluster run.
run_clustering_sensitivity <- FALSE
# run_clustering_sensitivity <- TRUE  # Add prediction-based and random clustering at fixed k.

required_packages <- c("batchtools", "caret", "dplyr", "OpenML", "ranger",
                       "timbR", "rpart", "Boruta", "this.path")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the required R packages before running the benchmark: ",
       paste(missing_packages, collapse = ", "), call. = FALSE)
}
stopifnot(length(task_ids) > 0L, !anyDuplicated(task_ids), length(folds_to_run) > 0L,
          all(folds_to_run %in% seq_len(num_folds)), !anyDuplicated(folds_to_run))

path_code <- this.path::this.dir()
function_dir <- file.path(path_code, "functions")
source(file.path(function_dir, "prepare_benchmark_task.R"))
benchmark_functions <- c("get_mrts_benchmark", "get_mrtes_benchmark",
                         "get_rt_benchmark", "get_artes_benchmark")
for (name in benchmark_functions) {
  source(file.path(function_dir, paste0(name, ".R")))
}

reg_dir <- file.path(path_code, "registries")
proc_dir <- file.path(path_code, "proc")
cache_dir <- file.path(reg_dir, "openml_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(proc_dir, showWarnings = FALSE)
invisible(utils::capture.output(OpenML::setOMLConfig(
  server = "https://www.openml.org/api/v1", cachedir = cache_dir
)))

reg <- batchtools::makeExperimentRegistry(
  file.dir = tempfile("benchmark_openml_", tmpdir = reg_dir),
  work.dir = proc_dir,
  conf.file = conf_file,
  source = file.path(function_dir, c(
    "select_trees.R", "tree_subsection.R", "combine_tree.R",
    "get_artes_automatic_clusters_benchmark.R"
  )),
  packages = c("ranger", "timbR", "rpart", "Boruta"),
  seed = 12345L
)

prob.designs <- register_benchmark_tasks(reg, task_ids, num_folds, folds_to_run, num.trees)

batchtools::addAlgorithm("MRTs", fun = get_mrts_benchmark, reg = reg)
batchtools::addAlgorithm("MRTEs", fun = get_mrtes_benchmark, reg = reg)
batchtools::addAlgorithm("RTs", fun = get_rt_benchmark, reg = reg)
batchtools::addAlgorithm("ARTEs", fun = get_artes_benchmark, reg = reg)

fixed_design <- data.frame(metric = metric, k = k)
arte_design <- expand.grid(
  metric = metric, metric_cluster = metric, k = k_art, imp.num.var = imp.num.var,
  probs_quantiles = probs_quantiles, min.bucket_perc = min.bucket_perc,
  stringsAsFactors = FALSE
)
if (run_clustering_sensitivity) {
  alternative_design <- expand.grid(
    metric = metric, metric_cluster = c("prediction", "random"), k = k,
    imp.num.var = imp.num.var, probs_quantiles = probs_quantiles,
    min.bucket_perc = min.bucket_perc, stringsAsFactors = FALSE
  )
  arte_design <- rbind(arte_design, alternative_design)
}
algo.designs <- list(MRTs = fixed_design, MRTEs = fixed_design, RTs = fixed_design,
                     ARTEs = arte_design)
ids <- batchtools::addExperiments(
  prob.designs = prob.designs, algo.designs = algo.designs, repls = 1L, reg = reg
)
batchtools::summarizeExperiments(reg = reg)
batchtools::submitJobs(ids = ids, reg = reg)
batchtools::waitForJobs(ids = ids, reg = reg)

done_ids <- batchtools::findDone(ids = ids, reg = reg)
if (!nrow(done_ids)) {
  stop("No benchmark jobs completed successfully. See registry ", reg$file.dir, ".", call. = FALSE)
}
if (nrow(done_ids) != nrow(ids)) {
  warning(sprintf("Completed %d of %d benchmark jobs. See registry %s.",
                  nrow(done_ids), nrow(ids), reg$file.dir), call. = FALSE)
}

# Keep the OOB R-squared in the export for dataset screening during analysis.
job_results <- batchtools::reduceResultsList(ids = done_ids, reg = reg)
names(job_results) <- as.character(done_ids$job.id)
results <- dplyr::bind_rows(lapply(job_results, function(result) result[[1L]]), .id = "job.id")
attr(results, "benchmark_design") <- list(
  folds = folds_to_run, k = k, k_art = k_art
)
saveRDS(results, file.path(proc_dir, "benchmark_results.Rds"))
saveRDS(job_results, file.path(proc_dir, "benchmark_ensemble_results.Rds"))
