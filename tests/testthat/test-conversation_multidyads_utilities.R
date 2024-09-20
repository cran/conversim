library(testthat)
library(ggplot2)
library(conversim)  # Assuming the package is called conversim

# Sample data for testing
sample_similarities <- list(
  "1" = c(0.5, 0.6, 0.7),
  "2" = c(0.4, 0.5, 0.6)
)

test_that("plot_sim_time works correctly", {
  plot <- plot_sim_time(sample_similarities, "Test Plot", "Similarity")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Test Plot")
  expect_equal(plot$labels$y, "Similarity")
  expect_equal(plot$labels$x, "Time")
})

test_that("calc_sum_stats works correctly", {
  stats <- calc_sum_stats(sample_similarities)

  expect_true(is.matrix(stats))
  expect_equal(dim(stats), c(2, 4))
  expect_equal(colnames(stats), c("mean", "sd", "min", "max"))
  expect_equal(rownames(stats), c("1", "2"))
})

test_that("plot_sum_stats works correctly", {
  stats <- calc_sum_stats(sample_similarities)
  plot <- plot_sum_stats(stats, "Summary Stats")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Summary Stats")
  expect_equal(plot$labels$x, "Dyad")
  expect_equal(plot$labels$y, "Value")
})

test_that("compare_sim_meas works correctly", {
  topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
  lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))

  comparison <- compare_sim_meas(
    list(topic_similarities, lexical_similarities),
    c("Topic", "Lexical")
  )

  expect_s3_class(comparison, "data.frame")
  expect_equal(ncol(comparison), 3)
  expect_equal(nrow(comparison), 6)
  expect_equal(names(comparison), c("dyad", "Topic", "Lexical"))
})

test_that("plot_sim_comp works correctly", {
  topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
  lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
  comparison_df <- compare_sim_meas(
    list(topic_similarities, lexical_similarities),
    c("Topic", "Lexical")
  )

  plot <- plot_sim_comp(comparison_df, "Comparison Plot")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Comparison Plot")
  expect_equal(plot$labels$x, "Dyad")
  expect_equal(plot$labels$y, "Similarity")
})



test_that("plot_sim_cor_heatmap works correctly", {
  topic_similarities <- list("1" = c(0.5, 0.6, 0.7), "2" = c(0.4, 0.5, 0.6))
  lexical_similarities <- list("1" = c(0.6, 0.7, 0.8), "2" = c(0.5, 0.6, 0.7))
  comparison_df <- compare_sim_meas(
    list(topic_similarities, lexical_similarities),
    c("Topic", "Lexical")
  )
  cor_matrix <- calc_sim_cor(comparison_df)

  plot <- plot_sim_cor_heatmap(cor_matrix, "Correlation Heatmap")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Correlation Heatmap")
})

test_that("functions handle edge cases correctly", {
  # Empty list
  expect_error(calc_sum_stats(list()), "No data to calculate summary statistics")

  # Single dyad
  single_dyad <- list("1" = c(0.5, 0.6, 0.7))
  single_result <- calc_sum_stats(single_dyad)
  expect_true(is.matrix(single_result))
  expect_equal(dim(single_result), c(1, 4))

  # Mismatched similarity lists and measure names
  expect_error(compare_sim_meas(
    list(sample_similarities, sample_similarities),
    c("Topic")
  ), "The number of similarity lists must match the number of measure names.")

  # NA values
  na_similarities <- list("1" = c(0.5, NA, 0.7), "2" = c(0.4, 0.5, NA))
  expect_warning(calc_sum_stats(na_similarities), "NAs present in the data")
})
