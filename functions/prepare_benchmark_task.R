# Apply the ERT preprocessing to an OpenML regression task.
prepare_benchmark_task <- function(task) {
  data_set <- task$input$data.set
  target <- task$input$target.features
  if (length(target) != 1L) {
    stop("The OpenML task must have one regression target.", call. = FALSE)
  }
  raw <- as.data.frame(data_set$data)
  if (!target %in% names(raw) || !is.numeric(raw[[target]])) {
    stop("The OpenML regression target is missing or is not numeric.", call. = FALSE)
  }
  complete <- raw[stats::complete.cases(raw), , drop = FALSE]
  variable <- vapply(complete, function(column) length(unique(column)) > 1L, logical(1))
  nonconstant <- complete[, variable, drop = FALSE]
  if (!target %in% names(nonconstant) || ncol(nonconstant) < 2L) {
    stop("No varying target and predictors remain after removing missing values.", call. = FALSE)
  }

  y <- nonconstant[[target]]
  centre <- stats::median(y)
  radius <- 5 * stats::IQR(y)
  clean <- nonconstant[y > centre - radius & y < centre + radius, , drop = FALSE]
  if (nrow(clean) < 2L || length(unique(clean[[target]])) < 2L) {
    stop("Too few observations or a constant target after outlier removal.", call. = FALSE)
  }
  outcome_range <- range(clean[[target]])
  clean[[target]] <- (clean[[target]] - outcome_range[1L]) / diff(outcome_range)

  metadata <- data.frame(
    task_id = task$task.id, task_name = data_set$desc$name, task_version = data_set$desc$version,
    n = nrow(raw), p = ncol(raw), n_na_free = nrow(complete), p_na_free = ncol(complete),
    n_na_free_unique = nrow(nonconstant), p_na_free_unique = ncol(nonconstant),
    n_na_free_unique_outlier = nrow(clean), p_na_free_unique_outlier = ncol(clean),
    target_variable = target
  )
  list(data = clean, target = target, metadata = metadata)
}

# batchtools uses the same problem instance and seed for methods in a given fold.
fit_benchmark_fold <- function(data, current_fold, num.trees, ...) {
  test_indices <- data$folds[[current_fold]]
  train_data <- data$data[-test_indices, , drop = FALSE]
  test_data <- data$data[test_indices, , drop = FALSE]
  target <- data$target
  predictors <- setdiff(names(train_data), target)
  observed <- test_data[[target]]
  mtry <- floor(sqrt(length(predictors)))
  min_node_size <- floor(nrow(train_data) / 10)

  rf <- ranger::ranger(
    x = train_data[, predictors, drop = FALSE], y = train_data[[target]],
    num.trees = num.trees, mtry = mtry, min.node.size = min_node_size,
    importance = "permutation", num.threads = 1L
  )
  information_df <- cbind(
    data$metadata,
    data.frame(fold = current_fold, n_train = nrow(train_data), n_test = nrow(test_data),
               mean_test = mean(observed), var_test = stats::var(observed),
               min_node_size = min_node_size, mtry = mtry, num.trees = num.trees,
               r_squared_rf = rf$r.squared)
  )
  list(information_df, train_data, test_data, rf, target)
}

# Download and register each task once for all folds and algorithms.
register_benchmark_tasks <- function(reg, task_ids, num_folds, folds_to_run, num.trees) {
  prob.designs <- list()
  for (task_id in task_ids) {
    message("Loading OpenML task ", task_id, ".")
    task_data <- tryCatch(
      {
        prepared <- prepare_benchmark_task(OpenML::getOMLTask(task.id = task_id))
        set.seed(123)
        prepared$folds <- caret::createFolds(prepared$data[[prepared$target]],
                                             k = num_folds, returnTrain = FALSE)
        if (length(prepared$folds) != num_folds || any(lengths(prepared$folds) < 2L)) {
          stop("The task does not have enough observations for all requested folds.", call. = FALSE)
        }
        prepared
      },
      error = function(error) {
        warning("OpenML task ", task_id, " skipped. ", conditionMessage(error), call. = FALSE)
        NULL
      }
    )
    if (is.null(task_data)) {
      next
    }

    problem <- paste0("openml_", task_id)
    batchtools::addProblem(name = problem, data = task_data, fun = fit_benchmark_fold,
                           seed = 12345L, reg = reg)
    prob.designs[[problem]] <- data.frame(current_fold = folds_to_run, num.trees = num.trees)
  }
  if (!length(prob.designs)) {
    stop("No OpenML benchmark tasks could be prepared.", call. = FALSE)
  }
  prob.designs
}
