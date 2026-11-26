#' Simulate the nonlinear regression scenario and fit the reference random forest.
#'
#' The signal function follows Friedman, J. H., and Popescu, B. E. (2008),
#' "Predictive learning via rule ensembles", Ann. Appl. Stat. 2, 916–954.
#' The simulation settings follow the revised Cluster ARTs manuscript.
#'
#' @param n_train,n_test Number of training and test observations.
#' @param mtry,min.node.size,num.trees Hyperparameters passed to ranger.
#' @param ... Additional arguments supplied by batchtools.
#' @return The fitted forest, training and test data, simulation settings,
#'   and predictor names grouped by their role in the simulation.
simulate_nonlinear_effects <- function(n_train, n_test, mtry, min.node.size,
                                         num.trees = 500L, ...) {
  all_data <- generate_rulefit_data(n_train + n_test)
  train_data <- all_data[seq_len(n_train), ]
  test_data <- all_data[n_train + seq_len(n_test), ]
  rf <- ranger::ranger(
    y ~ ., data = train_data, importance = "permutation",
    mtry = mtry, num.trees = num.trees, min.node.size = min.node.size
  )

  list(
    information_df = data.frame(
      simulation = "non-linear", 
      n_train = n_train, 
      n_test = n_test,
      p_cor = NA_integer_, b1 = NA_real_, b2 = NA_real_, 
      num.trees = num.trees,
      min.node.size = min.node.size,
      mtry = mtry, 
      r_squared_rf = rf$r.squared
    ),
    train_data = train_data,
    test_data = test_data,
    effect_var_ids = paste0("x", 1:8),
    correlated_effect_var_ids = character(0),
    noise_var_ids = paste0("x", 9:100),
    rf = rf
  )
}

# Generate independent RuleFit observations without fitting a forest.
generate_rulefit_data <- function(n) {
  p <- 100L
  # Draw independent predictors uniformly from {0, 0.1, ..., 0.9}.
  x <- matrix(sample(0:9, n * p, replace = TRUE) / 10, nrow = n, ncol = p)

  # Only the first eight predictors contribute to the nonlinear signal.
  signal <- apply(x, 1, function(row) {
    9 * exp(-3 * (1 - row[1])^2) *
      exp(-3 * (1 - row[2])^2) *
      exp(-3 * (1 - row[3])^2) -
      0.8 * exp(-2 * (row[4] - row[5])) +
      2 * sin(pi * row[6])^2 -
      2.5 * (row[7] - row[8])
  })
  # Set the signal-to-noise variance ratio to 2:1.
  y <- signal + rnorm(n, mean = 0, sd = sqrt(var(signal) / 2))

  all_data <- data.frame(y = y, x)
  names(all_data) <- c("y", paste0("x", seq_len(p)))
  all_data
}
