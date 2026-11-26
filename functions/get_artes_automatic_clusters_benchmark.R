get_artes_automatic_clusters_benchmark <- function(rf, metric, threshold_improvement,
                                                   test_data, train_data, imp.num.var,
                                                   probs_quantiles, min.bucket,
                                                   dependent_varname, importance_mode) {
  build_art <- function(forest) {
    generate_tree(
      rf = forest, metric = metric, train_data = train_data, test_data = test_data,
      dependent_varname = dependent_varname, importance.mode = importance_mode,
      imp.num.var = imp.num.var, probs_quantiles = probs_quantiles,
      epsilon = 0.05, min.bucket = min.bucket
    )
  }

  # Measure within-cluster variation as squared distances to the cluster ART.
  within_variation <- function(forest, art) {
    distances <- measure_distances(rf = combine_tree(forest, art), metric = metric)
    sum(distances[seq_len(forest$num.trees), forest$num.trees + 1L]^2)
  }

  accepted_ensemble <- build_art(rf)
  accepted_cluster_size <- rf$num.trees
  d_within <- within_variation(rf, accepted_ensemble)

  tree_distances <- measure_distances(rf = rf, metric = metric)
  distance_pca <- prcomp(tree_distances, rank = 2)
  coordinates <- distance_pca$rotation
  # k-means requires fewer clusters than trees and distinct starting centres.
  max_clusters <- min(nrow(coordinates) - 1L, nrow(unique(coordinates)))
  k <- 2L
  while (is.finite(d_within) && d_within > 0 && k <= max_clusters) {
    set.seed(123)
    clustering <- kmeans(coordinates, centers = k)
    cluster_size <- integer(k)
    cluster_variation <- numeric(k)

    for (i in seq_len(k)) {
      indices <- which(clustering$cluster == i)
      cluster_size[i] <- length(indices)
      forest <- tree_subsection(rf, indices)
      art <- build_art(forest)
      cluster_variation[i] <- within_variation(forest, art)
      ensemble <- if (i == 1L) art else combine_tree(ensemble, art)
    }

    new_variation <- sum(cluster_variation)
    improvement <- (d_within - new_variation) / d_within
    if (!is.finite(improvement) || improvement <= 0 || improvement < threshold_improvement) {
      break
    }

    accepted_ensemble <- ensemble
    accepted_cluster_size <- cluster_size
    d_within <- new_variation
    k <- k + 1L
  }

  list(accepted_ensemble, accepted_cluster_size)
}
