#' Plot similarity over time for multiple dyads
#'
#' This function creates a ggplot object showing the similarity over time for multiple dyads.
#'
#' @param similarities A list of similarity sequences for each dyad
#' @param title A string specifying the plot title
#' @param y_label A string specifying the y-axis label
#' @return A ggplot object
#' @import ggplot2
#' @importFrom stats cor
#' @export
#'
#' @examples
#' similarities <- list(
#'   "1" = c(0.5, 0.6, 0.7),
#'   "2" = c(0.4, 0.5, 0.6)
#' )
#' plot_sim_time(similarities, "Topic Similarity", "Similarity Score")
#' print(plot)
plot_sim_time <- function(similarities, title, y_label) {
  df <- data.frame(
    dyad = rep(names(similarities), sapply(similarities, length)),
    time = unlist(lapply(similarities, seq_along)),
    similarity = unlist(similarities)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$similarity, color = .data$dyad)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = title, x = "Time", y = y_label) +
    ggplot2::theme_minimal()
}


#' Calculate summary statistics for similarities
#'
#' This function calculates summary statistics for the similarities of multiple dyads.
#'
#' @param similarities A list of similarity sequences for each dyad
#' @return A matrix with summary statistics for each dyad
#' @export
#'
#' @examples
#' similarities <- list(
#'   "1" = c(0.5, 0.6, 0.7),
#'   "2" = c(0.4, 0.5, 0.6)
#' )
#' calc_sum_stats(similarities)
#' print(plot)
calc_sum_stats <- function(similarities) {
  if (length(similarities) == 0) {
    stop("No data to calculate summary statistics")
  }

  na_present <- any(sapply(similarities, function(x) any(is.na(x))))
  if (na_present) {
    warning("NAs present in the data")
  }

  summary_stats <- lapply(similarities, function(x) {
    c(mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE))
  })

  do.call(rbind, summary_stats)
}

#' Plot summary statistics for similarities
#'
#' This function creates a ggplot object showing summary statistics for similarities of multiple dyads.
#'
#' @param summary_stats A data frame with summary statistics for each dyad
#' @param title A string specifying the plot title
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' similarities <- list(
#'   "1" = c(0.5, 0.6, 0.7),
#'   "2" = c(0.4, 0.5, 0.6)
#' )
#' stats <- calc_sum_stats(similarities)
#' plot_sum_stats(stats, "Summary Statistics of Similarities")
#' print(plot)
plot_sum_stats <- function(summary_stats, title) {
  df <- as.data.frame(summary_stats)
  df$dyad <- rownames(df)
  df_long <- data.frame(
    dyad = rep(df$dyad, each = 4),
    statistic = rep(c("mean", "sd", "min", "max"), nrow(df)),
    value = c(t(as.matrix(df[, c("mean", "sd", "min", "max")])))
  )

  ggplot2::ggplot(df_long, ggplot2::aes(x = .data$dyad, y = .data$value, fill = .data$statistic)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = title, x = "Dyad", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set2")
}

#' Compare multiple similarity measures
#'
#' This function compares multiple similarity measures for the same set of dyads.
#'
#' @param similarity_list A list of lists, where each inner list contains similarities for each dyad
#' @param measure_names A vector of names for each similarity measure
#' @return A data frame with all similarity measures for each dyad
#' @export
#'
#' @examples
#' topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
#' lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
#' compare_sim_meas(
#'   list(topic_similarities, lexical_similarities),
#'   c("Topic", "Lexical")
#' )
#' print(plot)
compare_sim_meas <- function(similarity_list, measure_names) {
  if (length(similarity_list) != length(measure_names)) {
    stop("The number of similarity lists must match the number of measure names.")
  }

  result <- data.frame(dyad = rep(names(similarity_list[[1]]),
                                  sapply(similarity_list[[1]], length)))

  for (i in seq_along(similarity_list)) {
    measure <- measure_names[i]
    similarities <- unlist(similarity_list[[i]])
    result[[measure]] <- similarities
  }

  result
}

#' Plot comparison of multiple similarity measures
#'
#' This function creates a ggplot object comparing multiple similarity measures for the same set of dyads.
#'
#' @param comparison_df A data frame output from compare_sim_meas()
#' @param title A string specifying the plot title
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
#' lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
#' comparison_df <- compare_sim_meas(
#'   list(topic_similarities, lexical_similarities),
#'   c("Topic", "Lexical")
#' )
#' plot_sim_comp(comparison_df, "Comparison of Similarity Measures")
#' print(plot)
plot_sim_comp <- function(comparison_df, title) {
  measures <- setdiff(names(comparison_df), "dyad")
  df_long <- data.frame(
    dyad = rep(comparison_df$dyad, length(measures)),
    measure = rep(measures, each = nrow(comparison_df)),
    similarity = unlist(comparison_df[, measures])
  )

  ggplot2::ggplot(df_long, ggplot2::aes(x = .data$dyad, y = .data$similarity, fill = .data$measure)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = title, x = "Dyad", y = "Similarity") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set3")
}

#' Calculate correlation between similarity measures
#'
#' This function calculates the correlation between different similarity measures.
#'
#' @param comparison_df A data frame output from compare_sim_meas()
#' @return A correlation matrix
#' @export
#'
#' @examples
#' topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
#' lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
#' comparison_df <- compare_sim_meas(
#'   list(topic_similarities, lexical_similarities),
#'   c("Topic", "Lexical")
#' )
#' calc_sim_cor(comparison_df)
#' print(plot)
calc_sim_cor <- function(comparison_df) {
  cor(comparison_df[, -1], use = "pairwise.complete.obs")
}

#' Plot heatmap of similarity measure correlations
#'
#' This function creates a ggplot object showing a heatmap of correlations between similarity measures.
#'
#' @param cor_matrix A correlation matrix output from calc_sim_cor()
#' @param title A string specifying the plot title
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
#' lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
#' comparison_df <- compare_sim_meas(
#'   list(topic_similarities, lexical_similarities),
#'   c("Topic", "Lexical")
#' )
#' cor_matrix <- calc_sim_cor(comparison_df)
#' plot_sim_cor_heatmap(cor_matrix, "Correlation of Similarity Measures")
#' print(plot)
plot_sim_cor_heatmap <- function(cor_matrix, title) {
  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "Correlation")

  ggplot2::ggplot(cor_df, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                  midpoint = 0, limit = c(-1,1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
