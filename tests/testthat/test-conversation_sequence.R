library(testthat)
library(slam)
library(topicmodels)
library(tm)

# Create a sample dataset for testing
sample_conversation <- data.frame(
  speaker = c("A", "B", "A", "B", "A", "B"),
  processed_text = c(
    "hello how are you",
    "im fine thank you how about you",
    "im doing well thanks for asking",
    "thats great to hear lovely weather today",
    "yes it is perfect for a walk outside",
    "absolutely lets go for a stroll later"
  )
)

test_that("create_windows works correctly", {
  windows <- create_windows(sample_conversation, window_size = 3)

  expect_type(windows, "list")
  expect_length(windows, 4)  # 6 rows - 3 + 1 = 4 windows
  expect_equal(length(windows[[1]]), 3)
  expect_equal(length(windows[[4]]), 3)
})

test_that("topic_sim_seq works correctly", {
  result <- topic_sim_seq(sample_conversation, method = "lda", num_topics = 2, window_size = 3)

  expect_type(result, "list")
  expect_true(all(c("sequence", "average") %in% names(result)))
  expect_length(result$sequence, 3)  # Aggregated into 3 segments
  expect_type(result$average, "double")
  expect_true(all(result$sequence >= 0 & result$sequence <= 1))
  expect_true(result$average >= 0 & result$average <= 1)
})

test_that("lex_sim_seq works correctly", {
  result <- lex_sim_seq(sample_conversation, window_size = 3)

  expect_type(result, "list")
  expect_true(all(c("sequence", "average") %in% names(result)))
  expect_length(result$sequence, 3)  # 6 rows - 3 = 3 similarity scores
  expect_type(result$average, "double")
  expect_true(all(result$sequence >= 0 & result$sequence <= 1))
  expect_true(result$average >= 0 & result$average <= 1)
})

test_that("sem_sim_seq works correctly", {
  result <- sem_sim_seq(sample_conversation, method = "tfidf", window_size = 3)

  expect_type(result, "list")
  expect_true(all(c("sequence", "average") %in% names(result)))
  expect_length(result$sequence, 3)  # 6 rows - 3 = 3 similarity scores
  expect_type(result$average, "double")
  expect_true(all(result$sequence >= 0 & result$sequence <= 1))
  expect_true(result$average >= 0 & result$average <= 1)
})

test_that("style_sim_seq works correctly", {
  result <- style_sim_seq(sample_conversation, window_size = 3)

  expect_type(result, "list")
  expect_true(all(c("sequence", "average") %in% names(result)))
  expect_length(result$sequence, 3)  # 6 rows - 3 = 3 similarity scores
  expect_type(result$average, "double")
  expect_true(all(result$sequence >= 0 & result$sequence <= 1))
  expect_true(result$average >= 0 & result$average <= 1)
})

test_that("sent_sim_seq works correctly", {
  result <- sent_sim_seq(sample_conversation, window_size = 3)

  expect_type(result, "list")
  expect_true(all(c("sequence", "average") %in% names(result)))
  expect_length(result$sequence, 3)  # 6 rows - 3 = 3 similarity scores
  expect_type(result$average, "double")
  expect_true(all(result$sequence >= 0 & result$sequence <= 1))
  expect_true(result$average >= 0 & result$average <= 1)
})

test_that("functions handle short conversations correctly", {
  short_conversation <- sample_conversation[1:2, ]

  topic_result <- topic_sim_seq(short_conversation, window_size = 3)
  expect_equal(length(topic_result$sequence), 0)
  expect_true(is.na(topic_result$average))

  lex_result <- lex_sim_seq(short_conversation, window_size = 3)
  expect_equal(length(lex_result$sequence), 0)
  expect_true(is.na(lex_result$average))

  sem_result <- sem_sim_seq(short_conversation, window_size = 3)
  expect_equal(length(sem_result$sequence), 0)
  expect_true(is.na(sem_result$average))

  style_result <- style_sim_seq(short_conversation, window_size = 3)
  expect_equal(length(style_result$sequence), 0)
  expect_true(is.na(style_result$average))

  sent_result <- sent_sim_seq(short_conversation, window_size = 3)
  expect_equal(length(sent_result$sequence), 0)
  expect_true(is.na(sent_result$average))
})
