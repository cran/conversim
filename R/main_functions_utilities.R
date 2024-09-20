#' Utility and visualization functions for speech similarity analysis
#'
#' This file contains utility functions and visualization tools to complement
#' the main similarity calculation functions for comparing two speeches.

#' Combine multiple similarity measures
#'
#' This function combines multiple similarity measures into a single score.
#'
#' @param similarities A named list of similarity scores
#' @param weights A named list of weights for each similarity measure (optional)
#' @return A single combined similarity score
#' @export
#' @examples
#' sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)
#' combine_sims(sims)
#' combine_sims(sims, weights = list(topic = 2, lexical = 1, semantic = 1.5, structural = 1))
#' print(plot)
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

#' Visualize similarity scores
#'
#' This function creates a bar plot of similarity scores.
#'
#' @param similarities A named list of similarity scores
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)
#' plot_sims(sims)
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

#' Compare stylistic features
#'
#' This function visualizes the comparison of stylistic features between two speeches.
#'
#' @param stylistic_result The result from stylistic_similarity function
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' text1 <- "The quick brown fox jumps over the lazy dog. It's a sunny day."
#' text2 <- "A lazy cat sleeps on the warm windowsill. Birds chirp outside."
#' result <- stylistic_similarity(text1, text2)
#' compare_style(result)
#' print(plot)
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

#' Generate similarity report
#'
#' This function generates a comprehensive report of all similarity measures.
#'
#' @param speech1 A character string representing the first speech
#' @param speech2 A character string representing the second speech
#' @param topic_method Method for topic similarity calculation ("lda" or "lsa")
#' @param semantic_method Method for semantic similarity calculation ("tfidf", "word2vec", or "glove")
#' @param glove_path Path to pre-trained GloVe file (if using "glove" method)
#' @return A list containing all similarity measures and visualizations
#' @export
#'
#' @examples
#' \donttest{
#' speech1 <- "This is the first speech. It talks about important topics."
#' speech2 <- "This is the second speech. It covers similar subjects."
#' report <- gen_sim_report(speech1, speech2)
#' }
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

#' Print similarity report
#'
#' This function prints a formatted summary of the similarity report.
#'
#' @param report A similarity report generated by gen_sim_report function
#' @return NULL (invisibly). This function is called for its side effect of printing to the console.
#' @export
#'
#' @examples
#' \donttest{
#' speech1 <- "This is the first speech. It talks about important topics."
#' speech2 <- "This is the second speech. It covers similar subjects."
#' report <- gen_sim_report(speech1, speech2)
#' print_sim_report(report)
#' }
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
