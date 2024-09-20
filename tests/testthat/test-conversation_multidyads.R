library(testthat)
library(conversim)

# Sample data for testing
sample_conversations <- data.frame(
  dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
  speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
  text = c("Hello!", "Hi there!", "How are you?", "I'm fine, thanks!",
           "Nice weather", "Indeed it is", "Do you like hiking?", "Yes, I love it!")
)

test_that("preprocess_dyads works correctly", {
  result <- preprocess_dyads(sample_conversations)

  expect_equal(ncol(result), 4)
  expect_true("processed_text" %in% colnames(result))
  expect_equal(nrow(result), 8)
  expect_true(all(nchar(result$processed_text) > 0))
})

test_that("topic_sim_dyads works correctly", {
  preprocessed <- preprocess_dyads(sample_conversations)
  result <- topic_sim_dyads(preprocessed, method = "lda", num_topics = 2, window_size = 2)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("lexical_sim_dyads works correctly", {
  preprocessed <- preprocess_dyads(sample_conversations)
  result <- lexical_sim_dyads(preprocessed, window_size = 2)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("semantic_sim_dyads works correctly", {
  preprocessed <- preprocess_dyads(sample_conversations)
  result <- semantic_sim_dyads(preprocessed, method = "tfidf", window_size = 2)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("structural_sim_dyads works correctly", {
  result <- structural_sim_dyads(sample_conversations)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("stylistic_sim_dyads works correctly", {
  preprocessed <- preprocess_dyads(sample_conversations)
  result <- stylistic_sim_dyads(preprocessed, window_size = 2)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("sentiment_sim_dyads works correctly", {
  preprocessed <- preprocess_dyads(sample_conversations)
  result <- sentiment_sim_dyads(preprocessed, window_size = 2)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("participant_sim_dyads works correctly", {
  result <- participant_sim_dyads(sample_conversations)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})

test_that("timing_sim_dyads works correctly", {
  result <- timing_sim_dyads(sample_conversations)

  expect_type(result, "list")
  expect_true("similarities_by_dyad" %in% names(result))
  expect_true("overall_average" %in% names(result))
  expect_length(result$similarities_by_dyad, 2)
  expect_type(result$overall_average, "double")
})
