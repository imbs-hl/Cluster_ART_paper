# Reproduce Table B1 from the main benchmark results.
# Run 05run_benchmark_experiment.R to generate proc/benchmark_results.Rds.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(dplyr, this.path, knitr)

path_code <- this.dir()
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)
results <- readRDS(file.path(path_code, "proc", "benchmark_results.Rds"))

# Prefer the planned design so folds or settings with no completed jobs are counted.
design <- attr(results, "benchmark_design")
if (is.null(design)) {
  warning("The benchmark export has no planned design. Inferring folds and k from completed jobs.",
          call. = FALSE)
  design <- list(
    folds = sort(unique(results$fold)),
    k_art = c(sort(unique(results$k_used[!results$k_modus %in%
                                          c("automatic", "automatically", "automaticaly")])),
              if (any(results$k_modus %in% c("automatic", "automatically", "automaticaly"))) NA_real_)
  )
}
k_levels <- ifelse(is.na(design$k_art), "Auto", as.character(design$k_art))
stopifnot(length(k_levels) > 0L, length(design$folds) > 0L,
          !anyDuplicated(k_levels), !anyDuplicated(design$folds))

# Use the same eligible datasets as the main benchmark plots. Count the raw
# ARTE jobs before the plot code removes incomplete methods.
source(file.path(path_code, "functions", "benchmark_plot_data.R"), local = TRUE)
# Use the inferred design for older exports in both the plot and the table.
if (is.null(design$k)) {
  design$k <- design$k_art[!is.na(design$k_art)]
}
attr(results, "benchmark_design") <- design
datasets <- prepare_benchmark_plot_data(results) %>% distinct(task_id)
arte <- results %>%
  filter(method %in% c("EARTs", "ARTEs"), metric == "weighted splitting variables",
         metric_cluster == "weighted splitting variables", imp.num.var == "automatic") %>%
  mutate(k = if_else(k_modus %in% c("automatic", "automatically", "automaticaly"),
                     "Auto", as.character(k_used))) %>%
  filter(k %in% k_levels, fold %in% design$folds) %>%
  distinct(task_id, k, fold)
dataset_names <- results %>%
  semi_join(datasets, by = "task_id") %>%
  distinct(task_id, task_name) %>%
  arrange(task_name)
stopifnot(!anyDuplicated(dataset_names$task_id))

missing_by_k <- vapply(k_levels, function(setting) {
  available <- tabulate(match(arte$task_id[arte$k == setting], dataset_names$task_id),
                        nbins = nrow(dataset_names))
  length(design$folds) - available
}, integer(nrow(dataset_names)))
missing_by_k <- matrix(missing_by_k, nrow = nrow(dataset_names),
                       dimnames = list(NULL, k_levels))
missing_total <- rowSums(missing_by_k)
keep <- missing_total > 0
table_b1 <- data.frame(
  Dataset = c(dataset_names$task_name[keep], "Total missing"),
  rbind(missing_by_k[keep, , drop = FALSE], colSums(missing_by_k)),
  Total = c(missing_total[keep], sum(missing_total)),
  check.names = FALSE
)

write.csv(table_b1, file.path(out_dir, "benchmark_missing_arte.csv"), row.names = FALSE)
latex <- knitr::kable(
  table_b1, format = "latex", booktabs = TRUE, row.names = FALSE,
  align = c("l", rep("r", length(k_levels) + 1L)),
  caption = "Number of missing ART ensemble results for benchmark datasets with incomplete ART results.",
  label = "benchmark_missing_arte", linesep = ""
)
latex_lines <- strsplit(as.character(latex), "\n", fixed = TRUE)[[1L]]
latex_lines <- sub(paste0("{l", strrep("r", length(k_levels) + 1L), "}"),
                   paste0("{p{4cm}", strrep("r", length(k_levels) + 1L), "}"),
                   latex_lines, fixed = TRUE)
latex_lines <- gsub("\\_", "\\_\\allowbreak ", latex_lines, fixed = TRUE)
top_rule <- match("\\toprule", latex_lines)
group_header <- c(
  sprintf(" & \\multicolumn{%d}{c}{Ensemble size $k$} & \\\\", length(k_levels)),
  sprintf("\\cmidrule(lr){2-%d}", length(k_levels) + 1L)
)
latex_lines <- append(latex_lines, group_header, after = top_rule)
writeLines(append(latex_lines, c("\\small", "\\setlength{\\tabcolsep}{3pt}"), after = 1L),
           file.path(out_dir, "benchmark_missing_arte.tex"))
