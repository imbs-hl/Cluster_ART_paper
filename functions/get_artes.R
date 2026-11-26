#' Build and evaluate an ensemble of artificial representative trees
#'
#' @param data Problem data supplied by batchtools.
#' @param instance Simulation result containing the fitted forest, data and predictor groups.
#' @param metric Distance metric used to construct each ART.
#' @param k Number of clusters. Use 1 for the full forest or NA for automatic selection.
#' @param imp.num.var Fixed number of predictors or "automatic" for Boruta selection.
#' @param probs_quantiles Quantiles used as candidate splits for continuous predictors.
#' @param threshold_improvement Minimum relative improvement for increasing k.
#' @param min.bucket Minimum number of observations per ART terminal node.
#' @param metric_cluster Metric or random assignment used to group RF trees.
#' @param ... Additional arguments supplied by batchtools.
#' @return A list with one metrics row, test predictions and the fitted ensemble in trees.
get_artes <- function(data, instance, metric, k, imp.num.var, probs_quantiles,
                      threshold_improvement = 0.1, min.bucket,
                      metric_cluster = metric, ...) {
  # Read the fitted forest, data and predictor groups from the simulation instance.
  information_df            <- instance[[1]]
  train_data                <- instance[[2]]
  test_data                 <- instance[[3]]
  effect_var_ids            <- instance[[4]]
  correlated_effect_var_ids <- instance[[5]]
  noise_var_ids             <- instance[[6]]
  rf                        <- instance[[7]]
  automatic_k <- is.na(k)
  k_modus <- if (automatic_k) "automaticaly" else "manually"
  n_test <- nrow(test_data)

  # Unpack parameters supplied as list columns in the job design.
  probs_quantiles <- unlist(probs_quantiles)
  imp.num.var <- unlist(imp.num.var)

  # User CPU time includes variable selection and ART construction.
  start <- proc.time()
  rf_art <- rf
  art_imp_num_var <- imp.num.var
  if (isTRUE(imp.num.var == "automatic")) {
    # Select predictors once on the training data for all ARTs in this ensemble.
    selection <- Boruta::Boruta(
      x = train_data[, setdiff(names(train_data), "y"), drop = FALSE],
      y = train_data$y,
      getImp = Boruta::getImpRfZ
    )$finalDecision
    selected_vars <- names(selection)[selection == "Confirmed"]
    if (length(selected_vars) == 0L) {
      stop("Boruta confirmed no predictors for ART construction.")
    }
    if (is.null(names(rf$variable.importance)) ||
        !all(selected_vars %in% names(rf$variable.importance))) {
      stop("Selected variables are missing from the RF variable importance names.")
    }

    # generate_tree() accepts a count, not a preselected variable list.
    # Mark the selected variables in a private RF copy so its top-m filter
    # selects exactly these variables without running Boruta again.
    rf_art$variable.importance[] <- 0
    rf_art$variable.importance[selected_vars] <- 1
    art_imp_num_var <- length(selected_vars)
  }

  # Build the ART ensemble for the requested cluster count.
  if (automatic_k) {
    print("automatic selection of k")
    # Choose k using the relative reduction in within-cluster variation.
    artes_list <- get_artes_automatic_clusters(
      rf = rf_art, metric_cluster = metric_cluster, metric = metric,
      threshold_improvement = threshold_improvement, test_data = test_data,
      train_data = train_data, imp.num.var = art_imp_num_var,
      probs_quantiles = probs_quantiles, min.bucket = min.bucket
    )
    artes <- artes_list[[1]]
    cluster_size <- artes_list[[2]]
    k <- artes$num.trees
  } else if (k == 1) {
    # Build one ART from the full random forest.
    artes <- generate_tree(
      rf = rf_art, metric = metric, train_data = train_data, test_data = test_data,
      dependent_varname = "y", importance.mode = TRUE, imp.num.var = art_imp_num_var,
      probs_quantiles = probs_quantiles, epsilon = 0.05, min.bucket = min.bucket
    )
    cluster_size <- rf$num.trees
  } else {
    if (identical(metric_cluster, "random")) {
      # Randomly assign all RF trees to groups whose sizes differ by at most one.
      cluster_assignment <- sample(
        rep(seq_len(k), length.out = rf$num.trees)
      )
    } else {
      # Use training data for distances based on predictions or terminal nodes.
      tree_distances <- measure_distances(
        rf = rf,
        metric = metric_cluster,
        test_data = if (metric_cluster %in% c("prediction", "terminal nodes")) {
          train_data
        } else {
          NULL
        }
      )

      # Cluster the first two PCA loading coordinates, as in the original simulation.
      distance_pca <- prcomp(tree_distances, rank = 2)

      # Use the same k-means seed as the MRTE analysis in ERT.
      set.seed(123)
      clustering <- kmeans(distance_pca$rotation, centers = k)
      cluster_assignment <- clustering$cluster
    }

    # Build one ART per cluster and retain cluster sizes for weighted predictions.
    cluster_size <- integer(k)
    for (i in seq_len(k)) {
      indices <- which(cluster_assignment == i)
      cluster_size[i] <- length(indices)
      cluster_forest <- tree_subsection(rf = rf_art, indices = indices)
      cluster_art <- generate_tree(
        rf = cluster_forest, metric = metric, train_data = train_data, test_data = test_data,
        dependent_varname = "y", importance.mode = TRUE, imp.num.var = art_imp_num_var,
        probs_quantiles = probs_quantiles, epsilon = 0.05, min.bucket = min.bucket
      )

      artes <- if (i == 1) cluster_art else combine_tree(artes, cluster_art)
    }
  }
  runtime <- as.numeric((proc.time() - start)[1]) / 60
  tree_info <- lapply(seq_len(k), function(tree) treeInfo(artes, tree))
  number_splits <- mean(vapply(tree_info, function(info) sum(!info$terminal), integer(1)))
  number_leaves <- sum(vapply(tree_info, function(info) sum(info$terminal), integer(1)))

  if (!automatic_k && k == 1) {
    dist_variables <- 0
    dist_weighted_variables <- 0
  } else {
    dist_variables <- mean(measure_distances(rf = artes, metric = "splitting variables"))
    dist_weighted_variables <- mean(measure_distances(rf = artes, metric = "weighted splitting variables"))
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

  # Standard predictions weight ARTs equally. Cluster-weighted predictions weight
  # each ART by the share of RF trees it represents.
  observed_y <- test_data$y
  test_predictors <- test_data[, -1, drop = FALSE]
  y_pred_rf <- predict(rf, data = test_predictors)$predictions
  y_pred_arte <- predict(artes, data = test_predictors)$predictions
  mse_test_dat_rf <- 1 / n_test * sum((observed_y - y_pred_rf)^2)
  mse_test_dat_art <- 1 / n_test * sum((observed_y - y_pred_arte)^2)
  mse_rf_pred <- 1 / n_test * sum((y_pred_rf - y_pred_arte)^2)

  if (!automatic_k && k == 1) {
    mse_test_dat_art_weighted <- mse_test_dat_art
    mse_rf_pred_weighted <- mse_rf_pred
  } else {
    tree_predictions <- predict(artes, data = test_predictors, predict.all = TRUE)$predictions
    weighted_predictions_art <- tree_predictions %*% matrix(cluster_size / information_df$num.trees, ncol = 1)
    mse_test_dat_art_weighted <- 1 / n_test * sum((observed_y - weighted_predictions_art)^2)
    mse_rf_pred_weighted <- 1 / n_test * sum((y_pred_rf - weighted_predictions_art)^2)
  }

  total_sum_of_squares <- sum((observed_y - mean(observed_y))^2)
  # The BIC-type penalty uses the total number of terminal nodes across the ensemble.
  bic <- n_test * log(mse_test_dat_art) + 4 * (number_leaves + 1) * log(n_test)

  # Return one metrics row, test predictions and the fitted ART ensemble.
  list(
    data.frame(
      metric = metric,
      metric_cluster = metric_cluster,
      method = "ARTEs",
      dist_variables = dist_variables,
      dist_weighted_variables = dist_weighted_variables,
      min_node_size = information_df$min.node.size,
      mse_test_dat_rf = mse_test_dat_rf,
      mse_test_dat_tree = mse_test_dat_art,
      mse_test_dat_tree_weighted = mse_test_dat_art_weighted,
      mse_rf_pred = mse_rf_pred,
      mse_rf_pred_weighted = mse_rf_pred_weighted,
      k_used = k,
      k_modus = k_modus,
      covered_effect_vars = covered_effect_vars,
      covered_correlated_vars = covered_correlated_vars,
      covered_noise_vars = covered_noise_vars,
      runtime = runtime,
      information_df,
      imp.num.var = imp.num.var,
      min.bucket = min.bucket,
      number_splits = number_splits,
      number_leaves = number_leaves,
      var_pop = total_sum_of_squares,
      r_squared = 1 - (n_test * mse_test_dat_art) / total_sum_of_squares,
      bic = bic,
      probs_quantiles = ifelse(is.null(probs_quantiles), "NULL", paste(probs_quantiles, collapse = ","))
    ),
    data.frame(test_dat_y = observed_y, y_pred_rf = y_pred_rf, y_pred_arte = y_pred_arte),
    trees = artes
  )
}
