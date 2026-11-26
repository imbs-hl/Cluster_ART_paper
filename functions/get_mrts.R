#' Select and evaluate an ensemble of most representative trees
#'
#' @param data Problem data supplied by batchtools.
#' @param instance Simulation result containing the fitted forest, data and predictor groups.
#' @param metric Distance metric used to rank trees in the full random forest.
#' @param k Fixed number of trees to select.
#' @param ... Additional arguments supplied by batchtools.
#' @return A list with one metrics row, test predictions and the fitted ensemble in trees.
get_mrts <- function(data, instance, metric, k, ...) {
  if (is.na(k)) {
    stop("MRTs require a fixed number of trees (k).")
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
  # Select the k trees with the smallest total distance to all RF trees.
  tree_distances <- measure_distances(rf = rf, metric = metric)
  mrts <- select_trees(rf = rf, num.trees = k, distance.matrix = tree_distances)
  runtime <- as.numeric((proc.time() - start)[1]) / 60
  tree_info <- lapply(seq_len(k), function(tree) treeInfo(mrts, tree))
  number_splits <- mean(vapply(tree_info, function(info) sum(!info$terminal), integer(1)))
  number_leaves <- sum(vapply(tree_info, function(info) sum(info$terminal), integer(1)))

  # Use the same distance summary as in the original MRT analysis.
  dist_variables <- mean(measure_distances(rf = mrts, metric = "splitting variables"))
  dist_weighted_variables <- mean(measure_distances(rf = mrts, metric = "weighted splitting variables"))

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

  # The MRT ensemble averages the predictions of its selected trees.
  observed_y <- test_data$y
  test_predictors <- test_data[, -1, drop = FALSE]
  y_pred_rf <- predict(rf, data = test_predictors)$predictions
  y_pred_mrts <- predict(mrts, data = test_predictors)$predictions
  mse_test_dat_rf <- 1 / n_test * sum((observed_y - y_pred_rf)^2)
  mse_test_dat_mrt <- 1 / n_test * sum((observed_y - y_pred_mrts)^2)
  mse_rf_pred <- 1 / n_test * sum((y_pred_rf - y_pred_mrts)^2)

  total_sum_of_squares <- sum((observed_y - mean(observed_y))^2)
  # The BIC-type penalty uses the total number of terminal nodes across the ensemble.
  bic <- n_test * log(mse_test_dat_mrt) + 4 * (number_leaves + 1) * log(n_test)

  # Return one metrics row, test predictions and the fitted MRT ensemble.
  list(
    data.frame(
      metric = metric,
      method = "MRTs",
      dist_variables = dist_variables,
      dist_weighted_variables = dist_weighted_variables,
      min_node_size = information_df$min.node.size,
      mse_test_dat_rf = mse_test_dat_rf,
      mse_test_dat_tree = mse_test_dat_mrt,
      mse_rf_pred = mse_rf_pred,
      k_used = k,
      k_modus = NA,
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
    trees = mrts
  )
}
