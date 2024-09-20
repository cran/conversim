library(testthat)
library(ggplot2)

# Test data
sim1 <- list(sequence = c(0.8, 0.7, 0.9), average = 0.8)
sim2 <- list(sequence = c(0.6, 0.8, 0.7), average = 0.7)

test_that("combine_sim_seq works correctly", {
  result <- combine_sim_seq(list(sim1, sim2))
  expect_equal(length(result$sequence), 3)
  expect_equal(result$average, 0.75)

  # Test with custom weights
  weighted_result <- combine_sim_seq(list(sim1, sim2), weights = c(0.7, 0.3))
  expect_equal(weighted_result$average, 0.8 * 0.7 + 0.7 * 0.3)
})

test_that("norm_sim works correctly", {
  scores <- c(0.2, 0.5, 0.8, 1.2, 1.5)
  normalized <- norm_sim(scores)
  expect_equal(min(normalized), 0)
  expect_equal(max(normalized), 1)

  # Test when all scores are the same
  expect_equal(norm_sim(rep(1, 5)), rep(0, 5))
})

test_that("plot_sim_seq returns a ggplot object", {
  plot <- plot_sim_seq(sim1, "Test Plot")
  expect_s3_class(plot, "ggplot")
})

test_that("plot_sim_multi returns a ggplot object", {
  plot <- plot_sim_multi(list(sim1, sim2), c("Topic", "Lexical"))
  expect_s3_class(plot, "ggplot")
})

test_that("heatmap_sim returns a ggplot object", {
  plot <- heatmap_sim(list(sim1, sim2), c("Topic", "Lexical"))
  expect_s3_class(plot, "ggplot")
})

test_that("radar_sim returns a ggplot object", {
  plot <- radar_sim(list(sim1, sim2), c("Topic", "Lexical"))
  expect_s3_class(plot, "ggplot")
})

test_that("agg_seq works correctly", {
  seq <- c(0.8, 0.7, 0.9, 0.6, 0.8, 0.7)
  aggregated <- agg_seq(seq, 3)
  expect_equal(length(aggregated), 3)
  expect_equal(aggregated[1], mean(c(0.8, 0.7)))
})

test_that("cor_sim_seq returns a correlation matrix", {
  cor_matrix <- cor_sim_seq(list(sim1, sim2))
  expect_true(is.matrix(cor_matrix))
  expect_equal(dim(cor_matrix), c(2, 2))
})

test_that("plot_cor_heatmap returns a ggplot object", {
  cor_matrix <- cor_sim_seq(list(sim1, sim2))
  plot <- plot_cor_heatmap(cor_matrix, c("Topic", "Lexical"))
  expect_s3_class(plot, "ggplot")
})
