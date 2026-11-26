#' Sample and evaluate an ensemble of random forest trees
#'
#' @param data Problem data supplied by batchtools.
#' @param instance Benchmark fold containing metadata, training and test data, the fitted forest and target name.
#' @param metric Distance metric recorded in the results for comparison with other methods.
#' @param k Fixed number of trees to sample.
#' @param ... Additional arguments supplied by batchtools.
#' @return A list with one metrics row and the fitted ensemble in trees.
get_rt_benchmark <- function(data, instance, metric, k, ...) {
  if (is.na(k)) {
    stop("RT ensembles require a fixed number of trees (k).")
  }

  # Read the fitted forest, test data and target name from the benchmark fold.
  information_df <- instance[[1]]
  test_data <- instance[[3]]
  rf <- instance[[4]]
  dependent_varname <- instance[[5]]
  n_test <- nrow(test_data)

  # Measure user CPU time for ensemble selection.
  start <- proc.time()
  # Sample k trees without replacement from the fitted random forest.
  sampled_indices <- sample.int(rf$num.trees, size = k, replace = FALSE)
  rt <- tree_subsection(rf, sampled_indices)
  rt$predictions <- NULL
  rt$prediction.error <- NULL
  runtime <- as.numeric((proc.time() - start)[1]) / 60
  tree_info <- lapply(seq_len(k), function(tree) treeInfo(rt, tree))
  number_splits <- mean(vapply(tree_info, function(info) sum(!info$terminal), integer(1)))
  number_leaves <- sum(vapply(tree_info, function(info) sum(info$terminal), integer(1)))

  # Use the same distance summary as in the original RT analysis.
  dist_variables <- mean(measure_distances(rf = rt, metric = "splitting variables"))
  dist_weighted_variables <- mean(measure_distances(rf = rt, metric = "weighted splitting variables"))

  # The RT ensemble averages the predictions of its sampled trees.
  observed_y <- test_data[[dependent_varname]]
  test_predictors <- test_data[, rf$forest$independent.variable.names, drop = FALSE]
  y_pred_rf <- predict(rf, data = test_predictors)$predictions
  y_pred_rt <- predict(rt, data = test_predictors)$predictions
  mse_test_dat_rf <- 1 / n_test * sum((observed_y - y_pred_rf)^2)
  mse_test_dat_rt <- 1 / n_test * sum((observed_y - y_pred_rt)^2)
  mse_rf_pred <- 1 / n_test * sum((y_pred_rf - y_pred_rt)^2)

  total_sum_of_squares <- sum((observed_y - mean(observed_y))^2)
  # The BIC-type penalty uses the total number of terminal nodes across the ensemble.
  bic <- n_test * log(mse_test_dat_rt) + 4 * (number_leaves + 1) * log(n_test)

  # Return one metrics row and the fitted RT ensemble.
  list(
    data.frame(
      metric = metric,
      method = "RTs",
      dist_variables = dist_variables,
      dist_weighted_variables = dist_weighted_variables,
      mse_test_dat_rf = mse_test_dat_rf,
      mse_test_dat_tree = mse_test_dat_rt,
      mse_rf_pred = mse_rf_pred,
      k_used = k,
      k_modus = NA,
      runtime = runtime,
      number_splits = number_splits,
      number_leaves = number_leaves,
      var_pop = total_sum_of_squares,
      r_squared = 1 - (n_test * mse_test_dat_rt) / total_sum_of_squares,
      bic = bic,
      information_df[setdiff(names(information_df), "var_pop")],
      imp.num.var = NA,
      probs_quantiles = NA,
      min.bucket = NA
    ),
    trees = rt
  )
}
