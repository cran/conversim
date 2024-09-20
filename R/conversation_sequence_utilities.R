#' Combine similarity measures for a single dyad
#'
#' This function combines multiple similarity measures into a single overall similarity score for a single dyad.
#'
#' @param similarities A list of similarity measures for a single dyad
#' @param weights A numeric vector of weights for each similarity measure (default is equal weights)
#' @return A list containing the combined sequence and average similarity
#' @export
#' @examples
#' sim1 <- list(sequence = c(0.8, 0.7, 0.9), average = 0.8)
#' sim2 <- list(sequence = c(0.6, 0.8, 0.7), average = 0.7)
#' combine_sim_seq(list(sim1, sim2))
#' print(plot)
#' @name combine_sim_seq
#' @title Combine Similarity Measures
combine_sim_seq <- function(similarities, weights = NULL) {
  if (length(similarities) == 0) {
    stop("At least one similarity measure is required")
  }

  if (is.null(weights)) {
    weights <- rep(1 / length(similarities), length(similarities))
  }

  sequence_lengths <- sapply(similarities, function(x) length(x$sequence))
  if (length(unique(sequence_lengths)) > 1) {
    warning("Sequence lengths do not match. Results may be unexpected.")
  }

  max_length <- max(sequence_lengths)

  combined_sequence <- numeric(max_length)
  for (i in seq_along(similarities)) {
    seq <- c(similarities[[i]]$sequence, rep(NA, max_length - length(similarities[[i]]$sequence)))
    combined_sequence <- combined_sequence + seq * weights[i]
  }

  combined_average <- sum(sapply(seq_along(similarities), function(i) {
    similarities[[i]]$average * weights[i]
  }))

  return(list(sequence = combined_sequence, average = combined_average))
}

#' Normalize similarity scores
#'
#' This function normalizes similarity scores to a 0-1 range.
#'
#' @param similarities A numeric vector of similarity scores
#' @return A numeric vector of normalized similarity scores
#' @export
#' @name norm_sim
#' @title Normalize Similarity Scores
#' @examples
#' similarities <- c(0.2, 0.5, 0.8, 1.0, 0.3)
#' norm_sim(similarities)
#' print(plot)
norm_sim <- function(similarities) {
  min_sim <- min(similarities, na.rm = TRUE)
  max_sim <- max(similarities, na.rm = TRUE)

  if (min_sim == max_sim) {
    return(rep(0, length(similarities)))
  }

  return((similarities - min_sim) / (max_sim - min_sim))
}

#' Plot similarity sequence for a single dyad
#'
#' This function creates a line plot of the similarity sequence for a single dyad.
#'
#' @param similarity A list containing the sequence of similarities and the average similarity
#' @param title A character string for the plot title
#' @return A ggplot object
#' @export
#' @name plot_sim_seq
#' @title Plot Similarity Sequence
#' @examples
#' sim_list <- list(
#'   sequence = c(0.5, 0.6, 0.7, 0.6, 0.8),
#'   average = 0.64
#' )
#'
#' # Plot the similarity sequence
#' plot <- plot_sim_seq(sim_list, "Dyad Similarity Sequence")
#' print(plot)
plot_sim_seq <- function(similarity, title) {
  if (length(similarity$sequence) == 0) {
    stop("Empty sequence. Cannot create plot.")
  }

  df <- data.frame(
    index = seq_along(similarity$sequence),
    similarity = similarity$sequence
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$similarity)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = similarity$average, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = "Sequence", y = "Similarity") +
    ggplot2::theme_minimal()
}

#' Plot multiple similarity measures for a single dyad
#'
#' This function creates a faceted plot of multiple similarity measures for a single dyad.
#'
#' @param similarities A list of similarity measures for a single dyad
#' @param titles A character vector of titles for each similarity measure
#' @return A ggplot object
#' @export
#' @name plot_sim_multi
#' @title Plot Multiple Similarity Measures
#' @examples
#' sim1 <- list(sequence = c(0.5, 0.6, 0.7, 0.6, 0.8), average = 0.64)
#' sim2 <- list(sequence = c(0.4, 0.5, 0.6, 0.7, 0.7), average = 0.58)
#' similarities <- list(sim1, sim2)
#' titles <- c("Measure 1", "Measure 2")
#'
#' # Plot multiple similarity measures
#' plot <- plot_sim_multi(similarities, titles)
#' print(plot)
plot_sim_multi <- function(similarities, titles) {
  if (length(similarities) == 0) {
    stop("Empty similarity list. Cannot create plot.")
  }

  df <- data.frame(
    index = rep(seq_along(similarities[[1]]$sequence), times = length(similarities)),
    measure = rep(titles, each = length(similarities[[1]]$sequence)),
    similarity = unlist(lapply(similarities, function(x) x$sequence))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$similarity, color = .data$measure)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ measure, scales = "free_y") +
    ggplot2::labs(x = "Sequence", y = "Similarity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' Create a heatmap of similarity measures for a single dyad
#'
#' This function creates a heatmap of multiple similarity measures for a single dyad.
#'
#' @param similarities A list of similarity measures for a single dyad
#' @param titles A character vector of titles for each similarity measure
#' @return A ggplot object
#' @export
#' @name heatmap_sim
#' @title Create Similarity Heatmap
#' @examples
#' sim1 <- list(sequence = c(0.5, 0.6, 0.7, 0.6, 0.8), average = 0.64)
#' sim2 <- list(sequence = c(0.4, 0.5, 0.6, 0.7, 0.7), average = 0.58)
#' similarities <- list(sim1, sim2)
#' titles <- c("Measure 1", "Measure 2")
#'
#' # Plot multiple similarity measures
#' plot <- plot_sim_multi(similarities, titles)
#' print(plot)
heatmap_sim <- function(similarities, titles) {
  if (length(similarities) == 0) {
    stop("Empty similarity list. Cannot create heatmap.")
  }

  df <- data.frame(
    index = rep(seq_along(similarities[[1]]$sequence), times = length(similarities)),
    measure = rep(titles, each = length(similarities[[1]]$sequence)),
    similarity = unlist(lapply(similarities, function(x) x$sequence))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$measure, fill = .data$similarity)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") +
    ggplot2::labs(x = "Sequence", y = "Similarity Measure", fill = "Similarity") +
    ggplot2::theme_minimal()
}

#' Create a radar chart of average similarities for a single dyad
#'
#' This function creates a radar chart of average similarities for multiple measures of a single dyad.
#'
#' @param similarities A list of similarity measures for a single dyad
#' @param titles A character vector of titles for each similarity measure
#' @return A ggplot object
#' @export
#' @name radar_sim
#' @title Create Radar Chart of Average Similarities
#' @examples
#' sim1 <- list(sequence = c(0.5, 0.6, 0.7, 0.6, 0.8), average = 0.64)
#' sim2 <- list(sequence = c(0.4, 0.5, 0.6, 0.7, 0.7), average = 0.58)
#' sim3 <- list(sequence = c(0.6, 0.7, 0.8, 0.7, 0.9), average = 0.74)
#' sim4 <- list(sequence = c(0.3, 0.4, 0.5, 0.6, 0.6), average = 0.48)
#' similarities <- list(sim1, sim2, sim3, sim4)
#' titles <- c("Measure 1", "Measure 2", "Measure 3", "Measure 4")
#'
#' # Create radar chart
#' radar <- radar_sim(similarities, titles)
#' print(radar)
radar_sim <- function(similarities, titles) {
  if (length(similarities) == 0) {
    stop("Empty similarity list. Cannot create radar chart.")
  }

  averages <- sapply(similarities, function(x) x$average)
  df <- data.frame(
    measure = titles,
    similarity = averages,
    angle = (seq_along(titles) - 1) * (360 / length(titles))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$measure, y = .data$similarity)) +
    ggplot2::geom_polygon(fill = "lightblue", alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::coord_polar() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}

#' Aggregate similarity sequence for a single dyad
#'
#' This function aggregates a similarity sequence into a specified number of segments for a single dyad.
#'
#' @param sequence A numeric vector of similarity scores for a single dyad
#' @param num_segments The number of segments to aggregate into
#' @return A numeric vector of aggregated similarity scores
#' @export
#' @name agg_seq
#' @title Aggregate Similarity Sequence
#' @examples
#' seq <- c(0.5, 0.6, 0.7, 0.6, 0.8, 0.7, 0.9, 0.8, 0.7, 0.8)
#' # Aggregate the sequence into 3 segments
#' agg_3 <- agg_seq(seq, 3)
#' print(agg_3)
#'
#' # Aggregate the sequence into 5 segments
#' agg_5 <- agg_seq(seq, 5)
#' print(agg_5)
agg_seq <- function(sequence, num_segments) {
  segment_size <- ceiling(length(sequence) / num_segments)
  aggregated <- numeric(num_segments)

  for (i in 1:num_segments) {
    start_idx <- (i - 1) * segment_size + 1
    end_idx <- min(i * segment_size, length(sequence))
    aggregated[i] <- mean(sequence[start_idx:end_idx], na.rm = TRUE)
  }

  return(aggregated)
}

#' Calculate Correlation Between Similarity Measures for a Single Dyad
#'
#' This function calculates the correlation between different similarity measures for a single dyad.
#'
#' @name cor_sim_seq
#' @title Calculate Correlation Between Similarity Measures for a Single Dyad
#' @param similarities A list of similarity measures for a single dyad
#' @param method The correlation method to use (default is "pearson")
#' @return A correlation matrix
#' @export
#'
#' @examples
#' sim1 <- list(sequence = c(0.8, 0.7, 0.9), average = 0.8)
#' sim2 <- list(sequence = c(0.6, 0.8, 0.7), average = 0.7)
#' cor_sim_seq(list(sim1, sim2))
#' print(plot)
cor_sim_seq <- function(similarities, method = "pearson") {
  sequences <- lapply(similarities, function(x) x$sequence)
  cor_matrix <- cor(do.call(cbind, sequences), use = "pairwise.complete.obs", method = method)
  return(cor_matrix)
}

#' Plot Correlation Heatmap for a Single Dyad
#'
#' This function creates a heatmap of correlations between similarity measures for a single dyad.
#'
#' @name plot_cor_heatmap
#' @title Plot Correlation Heatmap for a Single Dyad
#' @param cor_matrix A correlation matrix for a single dyad
#' @param titles A character vector of titles for each similarity measure
#' @return A ggplot object
#' @export
#'
#' @examples
#' sim1 <- list(sequence = c(0.8, 0.7, 0.9), average = 0.8)
#' sim2 <- list(sequence = c(0.6, 0.8, 0.7), average = 0.7)
#' cor_matrix <- cor_sim_seq(list(sim1, sim2))
#' plot_cor_heatmap(cor_matrix, c("Topic", "Lexical"))
#' print(plot)
plot_cor_heatmap <- function(cor_matrix, titles) {
  # Create a data frame from the correlation matrix
  df <- expand.grid(Measure1 = titles, Measure2 = titles)
  df$Correlation <- as.vector(cor_matrix)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Measure1, y = .data$Measure2, fill = .data$Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    ggplot2::labs(x = "", y = "", fill = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

