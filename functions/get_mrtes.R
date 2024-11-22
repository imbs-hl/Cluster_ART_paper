#' select ensemble of k most representative trees (MRTs) for every cluster out of random forest (RF)
#' using batchtools to get object called instance
#' function gets: 
#'    - instance object returned by simulate_data_seifert
#'    - metric to measure distances between trees in RF
#'    - k (number of clusters to find and MRTs to select)
#' function returns:
#'    - k most representative trees (one per cluster) -> MRTEs
#' Step 1: measure_distances on RF with specified metric
#' Step 2: MDS on RF
#' Step 3: k-means on first two components of MDS
#' Step 4: build one MRT per cluster found by k-means -> MRTEs

get_mrtes <- function(data, instance, metric, k, ...){
  
  # Exctract data from instance
  information_df            <- instance[[1]]
  train_data                <- instance[[2]]
  test_data                 <- instance[[3]]
  effect_var_ids            <- instance[[4]]
  correlated_effect_var_ids <- instance[[5]]
  noise_var_ids             <- instance[[6]]
  rf                        <- instance[[7]]
  
  n_test <- nrow(test_data)
  
  # for k = 1 calculate MRT as usual
  # for k > 1 build ensemble of k MRTs 
  if(k == 1){ # select MRT for k = 1
    # measure runtime
    start <- proc.time()
    
    # measure_distances on RF with specified metric using timbR package
    d  <- measure_distances(rf = rf, metric = metric)
    
    # if only 1 MRTE is wanted, MDS and clustering isn't needed -> one MRT is selected
    mrtes <- select_trees(rf, num.trees = 1, distance.matrix = d)
    
    # runtime in minutes
    end <- proc.time()
    runtime <- as.numeric((end - start)[1])/60
    
    # calculate quality measures
    # no distance scores for single tree
    dist_variables <- 0
    dist_weighted_variables <- 0
    
    # number of splits in MRT
    number_splits <- nrow(treeInfo(mrtes) %>% filter(!terminal))
    
    # coverage: used effect variables
    covered_effect_vars <- sum(effect_var_ids %in% treeInfo(mrtes)$splitvarName)/length(effect_var_ids)
    
    # coverage: used correlated variables  
    covered_correlated_vars <- sum(correlated_effect_var_ids %in% treeInfo(mrtes)$splitvarName)/length(correlated_effect_var_ids)
    
    # coverage: used noise variables  
    covered_noise_vars <- sum(noise_var_ids %in% treeInfo(mrtes)$splitvarName)/length(noise_var_ids)

    # accuracy: prediction accuracy on test data set
    mse_test_dat_rf <- 1/nrow(test_data) * sum((test_data$y - predict(rf, data = test_data[,-1])$predictions)^2)
    mse_test_dat_mrt <- 1/nrow(test_data) * sum((test_data$y - predict(mrtes, data = test_data[,-1])$predictions)^2)
    
    # fidelity: MSE to rf prediction
    mse_rf_pred <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - predict(mrtes, data = test_data[,-1])$predictions)^2)

  }else if(k > 1){
    # measure runtime
    start <- proc.time()
    
    # step 1: measure_distances on RF with specified metric using timbR package
    d  <- measure_distances(rf = rf, metric = metric)
    
    # step 2: MDS on distance scores
    mds <- prcomp(d, rank = 2)
  
    # step 3: k-means on first teo components
    m <- kmeans(mds$rotation, centers = k)
    
    # step 4: MRT of each Cluster 
    # split RF into Cluster, select MRT from each tree, combine MRTs into MRTEs
    for (i in 1:k) {
      indices <- which(m$cluster == i)
      temp_forest <- tree_subsection(rf = rf, indices = indices)
      temp_tree <- select_trees(rf = temp_forest, num.trees = 1, 
                                distance.matrix = d[indices, indices])
      if(i > 1){
        temp_tree <- combine_tree(mrtes, temp_tree)
      }
      mrtes <- temp_tree
    }

    # runtime in minutes
    end <- proc.time()
    runtime <- as.numeric((end - start)[1])/60
    
    # similarity: distance scores of ensembles of MRTs
    dist_variables <- measure_distances(rf = mrtes, metric = "splitting variables") %>% mean()
    dist_weighted_variables <- measure_distances(rf = mrtes, metric = "weighted splitting variables") %>% mean()
    
    # coverage: used effect variables
    covered_effect_vars <- lapply(c(1:k), function(tree){sum(effect_var_ids %in% treeInfo(mrtes, tree)$splitvarName)/length(effect_var_ids)}) %>% unlist() %>% mean()
    
    # coverage: used correlated variables  
    covered_correlated_vars <- lapply(c(1:k), function(tree){sum(correlated_effect_var_ids %in% treeInfo(mrtes, tree)$splitvarName)/length(correlated_effect_var_ids)}) %>% unlist() %>% mean()
    
    # coverage: used noise variables  
    covered_noise_vars <- lapply(c(1:k), function(tree){sum(noise_var_ids %in% treeInfo(mrtes, tree)$splitvarName)/length(noise_var_ids)}) %>% unlist() %>% mean()

    # accuracy: prediction accuracy on test data set
    mse_test_dat_rf <- 1/nrow(test_data) * sum((test_data$y - predict(rf, data = test_data[,-1])$predictions)^2)
    mse_test_dat_mrt <- 1/nrow(test_data) * sum((test_data$y - predict(mrtes, data = test_data[,-1])$predictions)^2)
    
    # fidelity: MSE to rf prediction
    mse_rf_pred <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - predict(mrtes, data = test_data[,-1])$predictions)^2)
  }

  
  return(data.frame(metric                      = metric, 
                    method                      = "MRTEs",
                    dist_variables              = dist_variables,
                    dist_weighted_variables     = dist_weighted_variables,
                    min_node_size               = information_df$min.node.size,
                    mse_test_dat_rf             = mse_test_dat_rf,
                    mse_test_dat_tree           = mse_test_dat_mrt,
                    mse_rf_pred                 = mse_rf_pred,
                    k_used                      = k,
                    covered_effect_vars         = covered_effect_vars,
                    covered_correlated_vars     = covered_correlated_vars,
                    covered_noise_vars          = covered_noise_vars,
                    runtime                     = runtime,
                    information_df,
                    imp.num.var = NA,
                    probs_quantiles = NA
                    )
              )
   
}

