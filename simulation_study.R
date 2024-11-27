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
source("functions/simulate_data.R")
source("functions/combine_tree.R")
source("functions/tree_subsection.R")
source("functions/get_mrts.R")
source("functions/get_mrtes.R")
source("functions/get_artes.R")

#---------------------------------------
## set parameters for simulation

## parameter of data set
# sample size of train and test data set
n_train   <- 1000
n_test    <- 1000

# number of correlated variables with variables X1, X2, X5 and X6
p_cor <- 3

# number of additional noise variables (2 noise variables X5 and X6 with correlated variables not included)
nGenesncorr <- 100 - 3*p_cor - 6 

# minimal and maximal correlation for correlated variables with X1 and X5
minCor1 <- 0.9
maxCor1 <- 0.9

# minimal and maximal correlation for correlated variables with X2 and X6
minCor2 <- 0.3
maxCor2 <- 0.3

# effect size for X1 and X3 and effect size for X2 and X4
b1 = 1
b2 = 1

## parameter of random forest(RF) 
## RF is generated together with data so that every algorithm of batchtools gets the same data and RF

# hyperparameter of ranger that defines the number of trees in the random forest
# you can save time here; in the paper num.trees = 500 was used
# if a lower value is used to save time, the results may change
num.trees <- 20

# hyperparameter of ranger to select mtry split candidates
mtry <- sqrt(100)

# hyperparameter of ranger to stop tree growth at minimal node size
min.node.size <- 100

## replicates of simulation
# you can save time here; in the paper repls = 100 was used
# if a lower value is used to save time, the results may change
repls <- 10

## parameter of MRTs, MRTEs, ARTEs
# number of trees to select or build
# you can save time here; in the paper k = 1, ... 10 was used
k <- as.numeric(1:2)

# number of most important variables that are used to build ARTEs
# you can save time here; in the paper imp.num.var = 10 was used
# if a lower value is used to save time, the results may change
imp.num.var <- 1

# quantiles that should be used for continuos variables to build ARTEs
# you can save time here; in the paper probs_quantiles = list(c(0.25,0.5,0.75)) was used
# if a lower value is used to save time, the results may change
probs_quantiles <- list(c(0.5)) 

# metric to measure the distance of the trees in RF
metric   <- c("weighted splitting variables")

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
    packages = c("ranger", "timbR", "rpart", "dplyr", "WGCNA") ## Define which packages to use in your simulations
  )

## Add the problem (data simulation)
batchtools::addProblem(name = "simulate_data",
                       reg = reg_rf_rep_dist, 
                       fun = simulate_data,
                       data = 1,
                       seed = 12345)

## Add the algorithms to solve the problem (MRTs, MRTEs, ARTEs)
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

## define problem and algorithm designs
prob.designs <- list(
  simulate_data = data.frame(   n_train    = n_train,
                                n_test     = n_test,
                                p_cor      = p_cor,
                                nGenesncorr= nGenesncorr,
                                minCor1    = minCor1,
                                maxCor1    = maxCor1,
                                minCor2    = minCor2,
                                maxCor2    = maxCor2,
                                b1         = b1,
                                b2         = b2,
                                mtry       = mtry,
                                min.node.size = min.node.size,
                                num.trees  = num.trees,
                                stringsAsFactors = FALSE
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
                      k                = k,
                      imp.num.var      = imp.num.var,
                      probs_quantiles  = probs_quantiles,
                      stringsAsFactors = FALSE
  )
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

submitJobs(ids = ids, reg = reg_rf_rep_dist)


##' With pre selected parameters it will take around 10 min to complete.
##' Please note, the run times for the other settings could differ. 
##' Anyway simulating data for the figures in the paper will probably run for several days on you computer. 

# check job status
getStatus()


## Collect and save results ----
results <- reduceResultsList(reg = reg_rf_rep_dist, missing.val = 0)
## Save results
saveRDS(results, file = file.path(proc_dir, "results.Rds"))
