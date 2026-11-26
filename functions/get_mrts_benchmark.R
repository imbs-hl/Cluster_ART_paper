#' Select and evaluate an ensemble of most representative trees
#'
#' @param data Problem data supplied by batchtools.
#' @param instance Benchmark fold containing metadata, training and test data, the fitted forest and target name.
#' @param metric Distance metric used to rank trees in the full random forest.
#' @param k Fixed number of trees to select.
#' @param ... Additional arguments supplied by batchtools.
#' @return A list with one metrics row and the fitted ensemble in trees.
get_mrts_benchmark <- function(data, instance, metric, k, ...) {
  if (is.na(k)) {
    stop("MRTs require a fixed number of trees (k).")
  }

  # Read the fitted forest, test data and target name from the benchmark fold.
  information_df <- instance[[1]]
  test_data <- instance[[3]]
  rf <- instance[[4]]
  dependent_varname <- instance[[5]]
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

  # The MRT ensemble averages the predictions of its selected trees.
  observed_y <- test_data[[dependent_varname]]
  test_predictors <- test_data[, rf$forest$independent.variable.names, drop = FALSE]
  y_pred_rf <- predict(rf, data = test_predictors)$predictions
  y_pred_mrts <- predict(mrts, data = test_predictors)$predictions
  mse_test_dat_rf <- 1 / n_test * sum((observed_y - y_pred_rf)^2)
  mse_test_dat_mrt <- 1 / n_test * sum((observed_y - y_pred_mrts)^2)
  mse_rf_pred <- 1 / n_test * sum((y_pred_rf - y_pred_mrts)^2)

  total_sum_of_squares <- sum((observed_y - mean(observed_y))^2)
  # The BIC-type penalty uses the total number of terminal nodes across the ensemble.
  bic <- n_test * log(mse_test_dat_mrt) + 4 * (number_leaves + 1) * log(n_test)

  # Return one metrics row and the fitted MRT ensemble.
  list(
    data.frame(
      metric = metric,
      method = "MRTs",
      dist_variables = dist_variables,
      dist_weighted_variables = dist_weighted_variables,
      mse_test_dat_rf = mse_test_dat_rf,
      mse_test_dat_tree = mse_test_dat_mrt,
      mse_rf_pred = mse_rf_pred,
      k_used = k,
      k_modus = NA,
      runtime = runtime,
      number_splits = number_splits,
      number_leaves = number_leaves,
      var_pop = total_sum_of_squares,
      r_squared = 1 - (n_test * mse_test_dat_mrt) / total_sum_of_squares,
      bic = bic,
      information_df[setdiff(names(information_df), "var_pop")],
      imp.num.var = NA,
      probs_quantiles = NA,
      min.bucket = NA
    ),
    trees = mrts
  )
}
