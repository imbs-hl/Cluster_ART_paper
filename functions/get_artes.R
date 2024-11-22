#' select ensemble of k artificial representative trees (ARTs) for every cluster out of random forest (RF)
#' using batchtools to get object called instance
#' function gets: 
#'    - instance object returned by simulate_data_seifert
#'    - metric to measure distances between trees in RF
#'    - k (number of clusters to find and ARTs to select)
#' function returns:
#'    - k artificial representative trees (one per cluster) -> ARTEs
#' Step 1: measure_distances on RF with specified metric
#' Step 2: MDS on RF
#' Step 3: k-means on first two components of MDS
#' Step 4: build one ART per cluster found by k-means -> ARTEs


get_artes <- function(data, instance, metric, k, imp.num.var, probs_quantiles, ...){
  
  # exctract data from instance
  information_df            <- instance[[1]]
  train_data                <- instance[[2]]
  test_data                 <- instance[[3]]
  effect_var_ids            <- instance[[4]]
  correlated_effect_var_ids <- instance[[5]]
  noise_var_ids             <- instance[[6]]
  rf                        <- instance[[7]]
  
  n_test <- nrow(test_data)
 
  # for k = 1 calculate ART as usual
  # for k > 1 use k ARTs for ensemble
  if(k == 1){ # select ART for k = 1
    # measure runtime
    start <- proc.time()
    
    # generate single ART
    artes <- generate_tree(rf = rf, metric = metric, train_data = train_data, test_data = test_data, dependent_varname = "y", 
                         importance.mode = TRUE, imp.num.var = imp.num.var, probs_quantiles = probs_quantiles, epsilon = 0.05)
    
    # runtime in minutes
    end <- proc.time()
    runtime <- as.numeric((end - start)[1])/60
    
    # calculate quality measures
    # no distance scores for single ART
    dist_variables <- 0
    dist_weighted_variables <- 0
    
    # number of splits in ART
    number_splits <- nrow(treeInfo(artes) %>% filter(!terminal))
    
    # coverage: used effect variables
    covered_effect_vars <- sum(effect_var_ids %in% treeInfo(artes)$splitvarName)/length(effect_var_ids)
    
    # coverage: used correlated variables  
    covered_correlated_vars <- sum(correlated_effect_var_ids %in% treeInfo(artes)$splitvarName)/length(correlated_effect_var_ids)
    
    # coverage: used noise variables  
    covered_noise_vars <- sum(noise_var_ids %in% treeInfo(artes)$splitvarName)/length(noise_var_ids)
    
    # accuracy: prediction accuracy on test data set
    mse_test_dat_rf <- 1/nrow(test_data) * sum((test_data$y - predict(rf, data = test_data[,-1])$predictions)^2)
    mse_test_dat_art <- 1/nrow(test_data) * sum((test_data$y - predict(artes, data = test_data[,-1])$predictions)^2)
    
    # fidelity: MSE to rf prediction
    mse_rf_pred <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - predict(artes, data = test_data[,-1])$predictions)^2)

    # no distance scores for single tree
    dist_variables <- 0
    dist_weighted_variables <- 0
  }else if(k > 1){
    # measure runtime
    start <- proc.time()
    
    # step 1: measure_distances on RF with specified metric using timbR package
    d  <- measure_distances(rf = rf, metric = metric)
    
    # step 2: MDS on distance scores
    mds <- prcomp(d, rank = 2)
  
    # step 3: k-means on first teo components
    m <- kmeans(mds$rotation, centers = k)
    
    # step 4: ART of each Cluster 
    # split RF into Cluster, select ART from each tree, combine ARTs into ARTEs
    for (i in 1:k) {
      indices <- which(m$cluster == i)
      temp_forest <- tree_subsection(rf = rf, indices = indices)
      temp_tree <- generate_tree(rf = temp_forest, metric = metric, train_data = train_data, test_data = test_data, dependent_varname = "y", 
                                 importance.mode = TRUE, imp.num.var = imp.num.var, probs_quantiles = probs_quantiles, epsilon = 0.05)
      
      if(i > 1){
        temp_tree <- combine_tree(artes, temp_tree)
      }
      artes <- temp_tree
    }

    # runtime in minutes
    end <- proc.time()
    runtime <- as.numeric((end - start)[1])/60
    
    # number of splits in ensemble of ARTs
    number_splits <- lapply(c(1:k), function(tree){nrow(treeInfo(artes, tree) %>% filter(!terminal))}) %>% unlist()
    
    # similarity: distance scores of ensembles of ARTs
    dist_variables <- measure_distances(rf = artes, metric = "splitting variables") %>% mean()
    dist_weighted_variables <- measure_distances(rf = artes, metric = "weighted splitting variables") %>% mean()
    
    # coverage: used effect variables
    covered_effect_vars <- lapply(c(1:k), function(tree){sum(effect_var_ids %in% treeInfo(artes, tree)$splitvarName)/length(effect_var_ids)}) %>% unlist() %>% mean()
    
    # coverage: used correlated variables   
    covered_correlated_vars <- lapply(c(1:k), function(tree){sum(correlated_effect_var_ids %in% treeInfo(artes, tree)$splitvarName)/length(correlated_effect_var_ids)}) %>% unlist() %>% mean()
    
    # coverage: used noise variables  
    covered_noise_vars <- lapply(c(1:k), function(tree){sum(noise_var_ids %in% treeInfo(artes, tree)$splitvarName)/length(noise_var_ids)}) %>% unlist() %>% mean()

    # accuracy: prediction accuracy on test data set
    mse_test_dat_rf <- 1/nrow(test_data) * sum((test_data$y - predict(rf, data = test_data[,-1])$predictions)^2)
    mse_test_dat_art <- 1/nrow(test_data) * sum((test_data$y - predict(artes, data = test_data[,-1])$predictions)^2)
    
    # fidelity: MSE to rf prediction
    mse_rf_pred <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - predict(artes, data = test_data[,-1])$predictions)^2)

    # distance score of all trees of RF
    d  <- measure_distances(rf = rf, metric = metric)
  }

  
  return(data.frame(metric                      = metric, 
                    method                      = "EARTs",
                    dist_variables              = dist_variables,
                    dist_weighted_variables     = dist_weighted_variables,
                    min_node_size               = information_df$min.node.size,
                    mse_test_dat_rf             = mse_test_dat_rf,
                    mse_test_dat_tree           = mse_test_dat_art,
                    mse_rf_pred                 = mse_rf_pred,
                    k_used                      = k,
                    covered_effect_vars         = covered_effect_vars,
                    covered_correlated_vars     = covered_correlated_vars,
                    covered_noise_vars          = covered_noise_vars,
                    runtime                     = runtime,
                    information_df,
                    imp.num.var = imp.num.var,
                    probs_quantiles = probs_quantiles
                    )
  )
   
}
