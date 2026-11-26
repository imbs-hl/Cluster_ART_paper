## Local simulation settings
n_train_linear <- 200L
n_train_nonlinear <- 200L
n_test <- 200L
num.trees <- 20L
min.node.size_linear <- 100L
min.node.size_nonlinear <- 100L
repls <- 2L
k <- as.numeric(1:2)
min.bucket <- 20L
probs_quantiles <- list(0.5)

## Original ERT settings for the main simulation
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


#---------------------------------------
## load packages
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}
pacman::p_load(batchtools)
pacman::p_load(checkmate)
pacman::p_load(data.table)
pacman::p_load(ggplot2)
pacman::p_load(ranger)
pacman::p_load(bindata)
pacman::p_load(rpart)
pacman::p_load(plyr)
pacman::p_load(dplyr)
pacman::p_load(gridExtra)
pacman::p_load(WGCNA)
pacman::p_load(Boruta)
pacman::p_load(this.path)

if("timbR" %in% installed.packages()){
  library(timbR)
} else {
  devtools::install_github("imbs-hl/timbR", "master")
  library(timbR)
}
#---------------------------------------
## paths
## Define directories
## Please define your main directory here.
## This should be the directory you cloned the git repository into.
path_code <- this.dir()
if(!file.exists(path_code)){warning("Please change the directory path_code in simulation_study.R.")}

setwd(path_code)

## Create and define registry directory
dir.create(file.path(path_code, "registries"), showWarnings = FALSE)
reg_dir <- file.path(path_code, "registries")

## Create and define proc directory
dir.create(file.path(path_code, "proc"), showWarnings = FALSE)
proc_dir <- file.path(path_code, "proc")

## Create and define output directory
dir.create(file.path(path_code, "output"), showWarnings = FALSE)
out_dir <- file.path(path_code, "output")



#---------------------------------------
## load functions needed for the data simulation

## functions to simulate the data sets
source("functions/simulate_linear_effects.R")
source("functions/simulate_nonlinear_effects.R")
source("functions/combine_tree.R")
source("functions/tree_subsection.R")
source("functions/get_mrts.R")
source("functions/get_mrtes.R")
source("functions/select_trees.R")
source("functions/get_artes_automatic_clusters.R")
source("functions/get_artes.R")
source("functions/get_rt.R")

#---------------------------------------
## set parameters for simulation

# number of correlated variables with variables X1, X2, X5 and X6
p <- 100L
p_cor <- 3L

# number of additional noise variables (2 noise variables X5 and X6 with correlated variables not included)
nGenesncorr <- p - 4L * p_cor - 6L

# minimal and maximal correlation for correlated variables with X1 and X5
minCor1 <- 0.9
maxCor1 <- 0.9

# minimal and maximal correlation for correlated variables with X2 and X6
minCor2 <- 0.3
maxCor2 <- 0.3

# effect size for X1 and X3 and effect size for X2 and X4
b1 = 1
b2 = 1

# Settings shared by the local and paper runs
mtry <- sqrt(p)
imp.num.var <- "automatic"
metric <- "weighted splitting variables"

# Data Simulation

## In this part the data will be simulated and saved
## for later use

## Create registry
reg_name <- "simulate_ensembles"

reg_rf_rep_dist <-
  batchtools::makeExperimentRegistry(
    file.dir = file.path(reg_dir, reg_name),
    work.dir = proc_dir,
    conf.file = NA, ## If you have a batch system, please enter conf file here
    source = file.path(path_code, "functions", c(
      "simulate_nonlinear_effects.R", "tree_subsection.R",
      "combine_tree.R", "select_trees.R",
      "get_artes_automatic_clusters.R"
    )),
    packages = c("ranger", "timbR", "rpart", "dplyr", "WGCNA") ## Define which packages to use in your simulations
  )

## Add the linear and nonlinear simulation problems
batchtools::addProblem(
  name = "linear",
  reg = reg_rf_rep_dist,
  fun = simulate_data,
  data = 1,
  seed = 12345
)

batchtools::addProblem(
  name = "nonlinear",
  reg = reg_rf_rep_dist,
  fun = simulate_nonlinear_effects,
  data = 1,
  seed = 12345
)

## Add the ensemble algorithms
batchtools::addAlgorithm(reg = reg_rf_rep_dist,
                         name = "MRTs",
                         fun = get_mrts
)

batchtools::addAlgorithm(reg = reg_rf_rep_dist,
                         name = "MRTEs",
                         fun = get_mrtes
)

batchtools::addAlgorithm(reg = reg_rf_rep_dist,
                         name = "ARTEs",
                         fun = get_artes
)

batchtools::addAlgorithm(reg = reg_rf_rep_dist,
                         name = "RTE",
                         fun = get_rt
)

## define problem and algorithm designs
prob.designs <- list(
  linear = data.frame(
    n_train = n_train_linear,
    n_test = n_test,
    p_cor = p_cor,
    nGenesncorr = nGenesncorr,
    minCor1 = minCor1,
    maxCor1 = maxCor1,
    minCor2 = minCor2,
    maxCor2 = maxCor2,
    b1 = b1,
    b2 = b2,
    mtry = mtry,
    min.node.size = min.node.size_linear,
    num.trees = num.trees
  ),
  nonlinear = data.frame(
    n_train = n_train_nonlinear,
    n_test = n_test,
    mtry = mtry,
    min.node.size = min.node.size_nonlinear,
    num.trees = num.trees
  )
)


algo.designs <- list(
  MRTs = data.frame(metric           = metric,
                    k                = k,
                    stringsAsFactors = FALSE
  ),
  MRTEs = data.frame(metric           = metric,
                     k                = k,
                     stringsAsFactors = FALSE
  ),
  ARTEs = expand.grid(metric           = metric,
                      metric_cluster   = metric,
                      k                = c(k), # add NA_real_ to the vector to use automatic selection
                      imp.num.var      = imp.num.var,
                      probs_quantiles  = probs_quantiles,
                      min.bucket       = min.bucket,
                      stringsAsFactors = FALSE
  ),
  RTE = data.frame(metric = metric, k = k)
)

## Add the experiment
ids = batchtools::addExperiments(reg = reg_rf_rep_dist,
                                 prob.designs = prob.designs,
                                 algo.designs = algo.designs,
                                 repls = repls)


summarizeExperiments(reg = reg_rf_rep_dist)


## Test jobs before submission
# testJob(id = 1, reg = reg_rf_rep_dist)

## Please change this if you have a batch system.

# the defaults will take about 5minutes to run
submitJobs(ids = ids, reg = reg_rf_rep_dist)



# check job status
getStatus(reg = reg_rf_rep_dist)


## Collect and save results ----
waitForJobs(reg = reg_rf_rep_dist)
done_ids <- findDone(reg = reg_rf_rep_dist)
if (nrow(done_ids) != nrow(ids)) {
  warning("Some simulation jobs did not finish successfully. Saving completed jobs only.")
}
job_results <- reduceResultsList(ids = done_ids, reg = reg_rf_rep_dist)
names(job_results) <- as.character(done_ids$job.id)
job_table <- getJobTable(ids = done_ids, reg = reg_rf_rep_dist)
arte_results <- job_results[as.character(job_table$job.id[job_table$algorithm == "ARTEs"])]
mrte_results <- job_results[as.character(job_table$job.id[job_table$algorithm == "MRTEs"])]
mrt_results <- job_results[as.character(job_table$job.id[job_table$algorithm == "MRTs"])]
rt_results <- job_results[as.character(job_table$job.id[job_table$algorithm == "RTE"])]
results <- dplyr::bind_rows(lapply(job_results, function(result) {
  if (is.data.frame(result)) result else result[[1L]]
}), .id = "job.id")
saveRDS(results, file = file.path(proc_dir, "results.Rds"))
saveRDS(arte_results, file = file.path(proc_dir, "arte_results.Rds"))
saveRDS(mrte_results, file = file.path(proc_dir, "mrte_results.Rds"))
saveRDS(mrt_results, file = file.path(proc_dir, "mrt_results.Rds"))
saveRDS(rt_results, file = file.path(proc_dir, "rt_results.Rds"))

