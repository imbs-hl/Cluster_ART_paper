# Compare ARTE clustering methods on benchmark regression tasks from OpenML.

## Local sensitivity settings
task_ids <- 359950L  # Boston, a small task from the paper.
num_folds <- 7L
folds_to_run <- 1:2
num.trees <- 20L
k <- 2  # A single ensemble size at which clustering methods can differ.
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
# probs_quantiles <- list(c(0.25, 0.5, 0.75))

## Shared settings
metric <- "weighted splitting variables"
imp.num.var <- "automatic"
min.bucket_perc <- 0.1
conf_file <- NA_character_  # Set the batchtools configuration path for a cluster run.

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
source(file.path(function_dir, "get_artes_benchmark.R"))

reg_dir <- file.path(path_code, "registries")
proc_dir <- file.path(path_code, "proc")
cache_dir <- file.path(reg_dir, "openml_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(proc_dir, showWarnings = FALSE)
invisible(utils::capture.output(OpenML::setOMLConfig(
  server = "https://www.openml.org/api/v1", cachedir = cache_dir
)))

reg <- batchtools::makeExperimentRegistry(
  file.dir = tempfile("benchmark_sensitivity_", tmpdir = reg_dir),
  work.dir = proc_dir,
  conf.file = conf_file,
  source = file.path(function_dir, c(
    "tree_subsection.R", "combine_tree.R"
  )),
  packages = c("ranger", "timbR", "rpart", "Boruta"),
  seed = 12345L
)

prob.designs <- register_benchmark_tasks(reg, task_ids, num_folds, folds_to_run, num.trees)

batchtools::addAlgorithm("ARTEs", fun = get_artes_benchmark, reg = reg)

# Change the grouping of RF trees while retaining WSV and Boruta for ART construction.
clustering_metrics <- c(metric, "prediction", "random")
algo.designs <- list(ARTEs = expand.grid(
  metric = metric, metric_cluster = clustering_metrics, k = k,
  imp.num.var = imp.num.var, probs_quantiles = probs_quantiles,
  min.bucket_perc = min.bucket_perc, stringsAsFactors = FALSE
))
ids <- batchtools::addExperiments(
  prob.designs = prob.designs, algo.designs = algo.designs, repls = 1L, reg = reg
)
batchtools::summarizeExperiments(reg = reg)
batchtools::submitJobs(ids = ids, reg = reg)
batchtools::waitForJobs(ids = ids, reg = reg)

done_ids <- batchtools::findDone(ids = ids, reg = reg)
if (!nrow(done_ids)) {
  stop("No benchmark sensitivity jobs completed successfully. See registry ", reg$file.dir, ".", call. = FALSE)
}
if (nrow(done_ids) != nrow(ids)) {
  warning(sprintf("Completed %d of %d benchmark sensitivity jobs. See registry %s.",
                  nrow(done_ids), nrow(ids), reg$file.dir), call. = FALSE)
}

# Keep the OOB R-squared in the export for dataset screening during analysis.
job_results <- batchtools::reduceResultsList(ids = done_ids, reg = reg)
names(job_results) <- as.character(done_ids$job.id)
results <- dplyr::bind_rows(lapply(job_results, function(result) result[[1L]]), .id = "job.id")
attr(results, "benchmark_design") <- list(
  folds = folds_to_run, k = k, metric_cluster = clustering_metrics
)
saveRDS(results, file.path(proc_dir, "benchmark_sensitivity_results.Rds"))
saveRDS(job_results, file.path(proc_dir, "benchmark_sensitivity_ensemble_results.Rds"))
