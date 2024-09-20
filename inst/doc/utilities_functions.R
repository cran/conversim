## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(conversim)

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
combine_sims <- function(similarities, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, length(similarities))
  } else {
    weights <- unlist(weights)
  }

  weighted_sum <- sum(unlist(similarities) * weights)
  total_weight <- sum(weights)

  return(weighted_sum / total_weight)
}

plot_sims <- function(similarities) {
  df <- data.frame(
    measure = names(similarities),
    score = unlist(similarities)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$measure, y = .data$score)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Similarity Scores", x = "Measure", y = "Score") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(limits = c(0, 1))
}

compare_style <- function(stylistic_result) {
  features <- names(stylistic_result$text1_features)
  speech1_values <- unlist(stylistic_result$text1_features)
  speech2_values <- unlist(stylistic_result$text2_features)

  df <- data.frame(
    feature = rep(features, 2),
    value = c(speech1_values, speech2_values),
    speech = rep(c("speech1", "speech2"), each = length(features))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$feature, y = .data$value, fill = .data$speech)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "Comparison of Stylistic Features", x = "Feature", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

gen_sim_report <- function(speech1, speech2, topic_method = "lda", semantic_method = "tfidf", glove_path = NULL) {
  topic_sim <- topic_similarity(speech1, speech2, method = topic_method)
  lexical_sim <- lexical_similarity(speech1, speech2)
  semantic_sim <- semantic_similarity(speech1, speech2, method = semantic_method, model_path = glove_path)
  structural_sim <- structural_similarity(speech1, speech2)
  stylistic_result <- stylistic_similarity(speech1, speech2)
  sentiment_sim <- sentiment_similarity(speech1, speech2)

  similarities <- list(
    topic = topic_sim,
    lexical = lexical_sim,
    semantic = semantic_sim,
    structural = structural_sim,
    stylistic = stylistic_result$overall_similarity,
    sentiment = sentiment_sim
  )

  combined_sim <- combine_sims(similarities)

  report <- list(
    similarities = similarities,
    combined_similarity = combined_sim,
    similarity_plot = plot_sims(similarities),
    stylistic_plot = compare_style(stylistic_result)
  )

  return(report)
}

print_sim_report <- function(report) {
  cat("Similarity Report\n")
  cat("=================\n\n")

  cat("Individual Similarity Scores:\n")
  for (measure in names(report$similarities)) {
    cat(sprintf("  %s: %.4f\n", measure, report$similarities[[measure]]))
  }

  cat("\nCombined Similarity Score: %.4f\n", report$combined_similarity)

  cat("\nPlots have been generated for overall similarities and stylistic features.\n")
  cat("Use 'report$similarity_plot' and 'report$stylistic_plot' to view them.\n")
}

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

norm_sim <- function(similarities) {
  min_sim <- min(similarities, na.rm = TRUE)
  max_sim <- max(similarities, na.rm = TRUE)

  if (min_sim == max_sim) {
    return(rep(0, length(similarities)))
  }

  return((similarities - min_sim) / (max_sim - min_sim))
}

cor_sim_seq <- function(similarities, method = "pearson") {
  sequences <- lapply(similarities, function(x) x$sequence)
  cor_matrix <- cor(do.call(cbind, sequences), use = "pairwise.complete.obs", method = method)
  return(cor_matrix)
}

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

calc_sim_cor <- function(comparison_df) {
  cor(comparison_df[, -1], use = "pairwise.complete.obs")
}

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

## -----------------------------------------------------------------------------
sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)
combined_score <- combine_sims(sims)
print(combined_score)

# With custom weights
weighted_score <- combine_sims(sims, weights = list(topic = 2, lexical = 1, semantic = 1.5, structural = 1))
print(weighted_score)

## -----------------------------------------------------------------------------
sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)
plot_sims(sims)

## -----------------------------------------------------------------------------
# Simulating the result of stylistic_similarity function
stylistic_result <- list(
  text1_features = list(sentence_length = 15, word_length = 5, unique_words = 100),
  text2_features = list(sentence_length = 12, word_length = 4, unique_words = 80),
  overall_similarity = 0.85
)

compare_style(stylistic_result)

## -----------------------------------------------------------------------------
speech1 <- "This is the first speech. It talks about important topics."
speech2 <- "This is the second speech. It covers similar subjects."

# Note: This function call might not work as-is because it depends on other functions
# that are not defined in the utility files. For demonstration purposes, we'll create
# a mock report.

mock_report <- list(
  similarities = list(
    topic = 0.8,
    lexical = 0.6,
    semantic = 0.7,
    structural = 0.9,
    stylistic = 0.85,
    sentiment = 0.75
  ),
  combined_similarity = 0.75,
  similarity_plot = plot_sims(list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)),
  stylistic_plot = compare_style(stylistic_result)
)

# Print the mock report
print_sim_report(mock_report)

## -----------------------------------------------------------------------------
# Combine similarity measures for a single dyad
sim1 <- list(sequence = c(0.8, 0.7, 0.9), average = 0.8)
sim2 <- list(sequence = c(0.6, 0.8, 0.7), average = 0.7)
combined <- combine_sim_seq(list(sim1, sim2))
print(combined)

# Normalize similarity scores
scores <- c(0.2, 0.5, 0.8, 1.0, 0.3)
normalized <- norm_sim(scores)
print(normalized)

# Aggregate similarity sequence
seq <- c(0.5, 0.6, 0.7, 0.6, 0.8, 0.7, 0.9, 0.8, 0.7, 0.8)
aggregated <- agg_seq(seq, 3)
print(aggregated)

# Calculate correlation between similarity measures
cor_matrix <- cor_sim_seq(list(sim1, sim2))
print(cor_matrix)

## -----------------------------------------------------------------------------
# Create mock data for multiple dyads
similarities <- list(
  "1" = c(0.5, 0.6, 0.7),
  "2" = c(0.4, 0.5, 0.6)
)

# Plot similarity over time for multiple dyads
plot_sim_time(similarities, "Topic Similarity", "Similarity Score")

# Calculate summary statistics
stats <- calc_sum_stats(similarities)
print(stats)

# Plot summary statistics
plot_sum_stats(stats, "Summary Statistics of Similarities")

# Compare multiple similarity measures
topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
comparison_df <- compare_sim_meas(
  list(topic_similarities, lexical_similarities),
  c("Topic", "Lexical")
)
print(head(comparison_df))

# Plot comparison of multiple similarity measures
plot_sim_comp(comparison_df, "Comparison of Similarity Measures")

