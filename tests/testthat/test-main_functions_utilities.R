library(testthat)
library(ggplot2)

test_that("combine_sims works correctly", {
  sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)

  # Test with default weights
  result <- combine_sims(sims)
  expect_equal(result, mean(unlist(sims)))

  # Test with custom weights
  weights <- list(topic = 2, lexical = 1, semantic = 1.5, structural = 1)
  result <- combine_sims(sims, weights)
  expected <- sum(unlist(sims) * unlist(weights)) / sum(unlist(weights))
  expect_equal(result, expected)
})

test_that("plot_sims returns a ggplot object", {
  sims <- list(topic = 0.8, lexical = 0.6, semantic = 0.7, structural = 0.9)
  plot <- plot_sims(sims)
  expect_s3_class(plot, "ggplot")
})

test_that("compare_style returns a ggplot object", {
  text1 <- "The quick brown fox jumps over the lazy dog. It's a sunny day."
  text2 <- "A lazy cat sleeps on the warm windowsill. Birds chirp outside."
  result <- stylistic_similarity(text1, text2)
  plot <- compare_style(result)
  expect_s3_class(plot, "ggplot")
})

test_that("gen_sim_report generates a valid report", {
  speech1 <- "This is the first speech. It talks about important topics."
  speech2 <- "This is the second speech. It covers similar subjects."

  report <- gen_sim_report(speech1, speech2)

  expect_type(report, "list")
  expect_named(report, c("similarities", "combined_similarity", "similarity_plot", "stylistic_plot"))

  expect_type(report$similarities, "list")
  expect_named(report$similarities, c("topic", "lexical", "semantic", "structural", "stylistic", "sentiment"))

  expect_type(report$combined_similarity, "double")
  expect_true(report$combined_similarity >= 0 && report$combined_similarity <= 1)

  expect_s3_class(report$similarity_plot, "ggplot")
  expect_s3_class(report$stylistic_plot, "ggplot")
})

test_that("print_sim_report works without errors", {
  speech1 <- "This is the first speech. It talks about important topics."
  speech2 <- "This is the second speech. It covers similar subjects."
  report <- gen_sim_report(speech1, speech2)

  expect_output(print_sim_report(report), "Similarity Report")
  expect_output(print_sim_report(report), "Individual Similarity Scores:")
  expect_output(print_sim_report(report), "Combined Similarity Score:")
})

# Helper function to capture output
capture_output <- function(x) {
  out <- capture.output(x)
  paste(out, collapse = "\n")
}

test_that("print_sim_report prints all similarity scores", {
  speech1 <- "This is the first speech. It talks about important topics."
  speech2 <- "This is the second speech. It covers similar subjects."
  report <- gen_sim_report(speech1, speech2)

  output <- capture_output(print_sim_report(report))

  expect_match(output, "topic:")
  expect_match(output, "lexical:")
  expect_match(output, "semantic:")
  expect_match(output, "structural:")
  expect_match(output, "stylistic:")
  expect_match(output, "sentiment:")
})
