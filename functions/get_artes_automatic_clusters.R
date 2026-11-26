get_artes_automatic_clusters <- function(rf, metric, threshold_improvement, test_data, train_data, imp.num.var, probs_quantiles, min.bucket, importance_mode = TRUE, metric_cluster = metric){
  
  # Prediction-based clustering uses training observations only.
  cluster_data <- if (metric_cluster %in% c("prediction", "terminal nodes")) train_data else NULL

  # generate single ART
  art <- generate_tree(rf = rf, metric = metric, train_data = train_data, test_data = test_data, dependent_varname = "y", 
                       importance.mode = importance_mode, imp.num.var = imp.num.var, probs_quantiles = probs_quantiles, epsilon = 0.05, min.bucket = min.bucket)
  
  # ## Prediction accuracy on forest prediction
  # mse_ert <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - predict(mrt, data = test_data[,-1])$predictions)^2)
  
  # within cluster variation: quadratic distance between art and trees in rf
  d_within <- sum(measure_distances(rf = combine_tree(rf, art), metric = metric_cluster, test_data = cluster_data)[, rf$num.trees+1]^2)
  
  ## add more trees to MRT ensemble until improvement is below threshold
  ## compute pca 
  d  <- measure_distances(rf = rf, metric = metric_cluster, test_data = cluster_data)
  pca <- prcomp(d, rank = 2)
  accepted_ensemble <- art
  accepted_cluster_size <- rf$num.trees
  k <- 2
  while (is.finite(d_within) && d_within > 0 && k <= rf$num.trees) {
    # perform k-means on pca results
    set.seed(123)
    m <- kmeans(pca$rotation, centers = k)
    
    ## Split rf into Cluster, select MRT from each tree, combine MRT into ensemble 
    d_within_extended <- c()
    art_ids <- c()
    cluster_size <- c()
    for (i in 1:k) {
      indices <- which(m$cluster == i)
      cluster_size[i] <- length(indices)
      temp_forest <- tree_subsection(rf = rf, indices = indices)
      print(paste0("Es sind ", length(indices), " Baeume im Cluster."))
      temp_tree <- generate_tree(rf = temp_forest, metric = metric, train_data = train_data, test_data = test_data, dependent_varname = "y", 
                                 importance.mode = importance_mode, imp.num.var = imp.num.var, probs_quantiles = probs_quantiles, epsilon = 0.05, min.bucket = min.bucket)
      # calculate distance within cluster
      d_within_extended <- append(d_within_extended,
                                  sum(measure_distances(rf = combine_tree(temp_forest, temp_tree), metric = metric_cluster, test_data = cluster_data)[1:temp_forest$num.trees, temp_forest$num.trees+1]^2))
      # save ids of mrts for quality control
      art_ids <- append(art_ids, 
                        which(measure_distances(rf = combine_tree(temp_forest, temp_tree), metric = metric)[1:temp_forest$num.trees, temp_forest$num.trees+1]==0))
      
      if(i > 1){
        ert <- combine_tree(ert, temp_tree)
      }else{
        ert <- temp_tree
      }
    }
    
    # # Quality control plot
    # centers <- 1:500 %in% art_ids
    # ggplot(pca$rotation, aes(x=pca$rotation[,1], y = pca$rotation[,2], shape = centers, col = factor(m$cluster))) +
    #   geom_point()
    
    # within cluster variation with one cluster more
    d_within_extended <- sum(d_within_extended)
    
    # ## Prediction accuracy on forest prediction
    # mse_ert_new <- 1/nrow(test_data) * sum((predict(rf, data = test_data[,-1])$predictions - 
    #                                           predict(ert, data = test_data[,-1])$predictions)^2)
    
    # Accept only a strict reduction meeting the relative improvement threshold.
    improvement <- (d_within - d_within_extended) / d_within
    if (!is.finite(improvement) || improvement <= 0 ||
        improvement < threshold_improvement) {
      break
    }

    accepted_ensemble <- ert
    accepted_cluster_size <- cluster_size
    d_within <- d_within_extended
    k <- k + 1
  }

  return(list(accepted_ensemble, accepted_cluster_size))
}

