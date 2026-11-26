#' Select and evaluate an ensemble of most representative trees
#'
#' @param data Problem data supplied by batchtools.
#' @param instance Simulation result containing the fitted forest, data and predictor groups.
#' @param metric Distance metric used to cluster RF trees and select MRTs.
#' @param k Fixed number of clusters. Use 1 to select from the full forest.
#' @param ... Additional arguments supplied by batchtools.
#' @return A list with one metrics row, test predictions and the fitted ensemble in trees.
get_mrtes <- function(data, instance, metric, k, ...) {
  if (is.na(k)) {
    stop("MRTEs require a fixed number of clusters (k).")
  }

  # Read the fitted forest, data and predictor groups from the simulation instance.
  information_df <- instance[[1]]
  test_data <- instance[[3]]
  effect_var_ids <- instance[[4]]
  correlated_effect_var_ids <- instance[[5]]
  noise_var_ids <- instance[[6]]
  rf <- instance[[7]]
  n_test <- nrow(test_data)

  # Measure user CPU time for ensemble selection.
  start <- proc.time()
  tree_distances <- measure_distances(rf = rf, metric = metric)
  if (k == 1) {
    # Select one MRT from the full random forest.
    mrtes <- select_trees(rf, num.trees = 1, distance.matrix = tree_distances)
    cluster_size <- rf$num.trees
  } else {
    # Cluster the first two PCA loading coordinates, as in the original simulation.
    distance_pca <- prcomp(tree_distances, rank = 2)
    # Use the same k-means seed as the ARTE analysis.
    set.seed(123)
    clustering <- kmeans(distance_pca$rotation, centers = k)

    cluster_size <- integer(k)
    for (i in seq_len(k)) {
      indices <- which(clustering$cluster == i)
      cluster_size[i] <- length(indices)
      cluster_forest <- tree_subsection(rf = rf, indices = indices)
      # Select the MRT using distances between trees in this cluster.
      cluster_mrt <- select_trees(
        rf = cluster_forest, num.trees = 1,
        distance.matrix = tree_distances[indices, indices, drop = FALSE]
      )
      mrtes <- if (i == 1) cluster_mrt else combine_tree(mrtes, cluster_mrt)
    }
  }
  runtime <- as.numeric((proc.time() - start)[1]) / 60
  tree_info <- lapply(seq_len(k), function(tree) treeInfo(mrtes, tree))
  number_splits <- mean(vapply(tree_info, function(info) sum(!info$terminal), integer(1)))
  number_leaves <- sum(vapply(tree_info, function(info) sum(info$terminal), integer(1)))

  if (k == 1) {
    dist_variables <- 0
    dist_weighted_variables <- 0
  } else {
    dist_variables <- mean(measure_distances(rf = mrtes, metric = "splitting variables"))
    dist_weighted_variables <- mean(measure_distances(rf = mrtes, metric = "weighted splitting variables"))
  }

  # Average the share of predictors in each group used for splitting per tree.
  coverage <- function(predictors) {
    mean(vapply(tree_info, function(info) mean(predictors %in% info$splitvarName), numeric(1)))
  }
  covered_effect_vars <- coverage(effect_var_ids)
  # The nonlinear scenario has no correlated effect predictors.
  covered_correlated_vars <- NA_real_
  if (length(correlated_effect_var_ids) > 0L) {
    covered_correlated_vars <- coverage(correlated_effect_var_ids)
  }
  covered_noise_vars <- coverage(noise_var_ids)

  # Standard predictions weight MRTs equally. Cluster-weighted predictions weight
  # each MRT by the share of RF trees it represents.
  observed_y <- test_data$y
  test_predictors <- test_data[, -1, drop = FALSE]
  y_pred_rf <- predict(rf, data = test_predictors)$predictions
  y_pred_mrts <- predict(mrtes, data = test_predictors)$predictions
  mse_test_dat_rf <- 1 / n_test * sum((observed_y - y_pred_rf)^2)
  mse_test_dat_mrt <- 1 / n_test * sum((observed_y - y_pred_mrts)^2)
  mse_rf_pred <- 1 / n_test * sum((y_pred_rf - y_pred_mrts)^2)

  if (k == 1) {
    mse_test_dat_mrt_weighted <- mse_test_dat_mrt
    mse_rf_pred_weighted <- mse_rf_pred
  } else {
    tree_predictions <- predict(mrtes, data = test_predictors, predict.all = TRUE)$predictions
    weighted_predictions_mrt <- tree_predictions %*% matrix(cluster_size / information_df$num.trees, ncol = 1)
    mse_test_dat_mrt_weighted <- 1 / n_test * sum((observed_y - weighted_predictions_mrt)^2)
    mse_rf_pred_weighted <- 1 / n_test * sum((y_pred_rf - weighted_predictions_mrt)^2)
  }

  total_sum_of_squares <- sum((observed_y - mean(observed_y))^2)
  # The BIC-type penalty uses the total number of terminal nodes across the ensemble.
  bic <- n_test * log(mse_test_dat_mrt) + 4 * (number_leaves + 1) * log(n_test)

  # Return one metrics row, test predictions and the fitted MRT ensemble.
  list(
    data.frame(
      metric = metric,
      method = "MRTEs",
      dist_variables = dist_variables,
      dist_weighted_variables = dist_weighted_variables,
      min_node_size = information_df$min.node.size,
      mse_test_dat_rf = mse_test_dat_rf,
      mse_test_dat_tree = mse_test_dat_mrt,
      mse_test_dat_tree_weighted = mse_test_dat_mrt_weighted,
      mse_rf_pred = mse_rf_pred,
      mse_rf_pred_weighted = mse_rf_pred_weighted,
      k_used = k,
      k_modus = "manually",
      covered_effect_vars = covered_effect_vars,
      covered_correlated_vars = covered_correlated_vars,
      covered_noise_vars = covered_noise_vars,
      runtime = runtime,
      number_splits = number_splits,
      number_leaves = number_leaves,
      var_pop = total_sum_of_squares,
      r_squared = 1 - (n_test * mse_test_dat_mrt) / total_sum_of_squares,
      bic = bic,
      information_df,
      imp.num.var = NA,
      probs_quantiles = NA,
      min.bucket = NA
    ),
    data.frame(test_dat_y = observed_y, y_pred_rf = y_pred_rf, y_pred_mrts = y_pred_mrts),
    trees = mrtes
  )
}
