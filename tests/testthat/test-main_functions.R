library(testthat)
library(tm)
library(topicmodels)
library(lsa)
library(word2vec)
library(sentimentr)

test_that("preprocess_text works correctly", {
  input <- "Hello, World! This is an example text (with 123 numbers)."
  expected <- "hello world this is an example text with numbers"
  expect_equal(preprocess_text(input), expected)
})

test_that("topic_similarity works with LDA", {
  conv1 <- c("I love pizza", "Pizza is my favorite food")
  conv2 <- c("I prefer pasta", "Pasta is delicious")
  similarity <- topic_similarity(conv1, conv2, method = "lda", num_topics = 2)
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("topic_similarity works with LSA", {
  conv1 <- c("I love pizza", "Pizza is my favorite food")
  conv2 <- c("I prefer pasta", "Pasta is delicious")
  similarity <- topic_similarity(conv1, conv2, method = "lsa", num_topics = 2)
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("lexical_similarity works correctly", {
  conv1 <- "The quick brown fox jumps over the lazy dog"
  conv2 <- "The lazy dog sleeps under the quick brown fox"
  similarity <- lexical_similarity(conv1, conv2)
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("semantic_similarity works with TF-IDF", {
  conv1 <- "The quick brown fox jumps over the lazy dog"
  conv2 <- "The lazy dog sleeps under the quick brown fox"
  similarity <- semantic_similarity(conv1, conv2, method = "tfidf")
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("semantic_similarity works with Word2Vec", {
  conv1 <- "The quick brown fox jumps over the lazy dog"
  conv2 <- "The lazy dog sleeps under the quick brown fox"
  similarity <- semantic_similarity(conv1, conv2, method = "word2vec")
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("semantic_similarity throws error for GloVe without model path", {
  conv1 <- "The quick brown fox jumps over the lazy dog"
  conv2 <- "The lazy dog sleeps under the quick brown fox"
  expect_error(semantic_similarity(conv1, conv2, method = "glove"))
})

test_that("structural_similarity works correctly", {
  conv1 <- c("Hello", "Hi there", "How are you?", "I'm fine, thanks")
  conv2 <- c("Good morning", "Hello", "Nice day, isn't it?", "Yes, indeed")
  similarity <- structural_similarity(conv1, conv2)
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})

test_that("stylistic_similarity works correctly", {
  text1 <- "The quick brown fox jumps over the lazy dog. It's a sunny day."
  text2 <- "A lazy cat sleeps on the warm windowsill. Birds chirp outside."
  result <- stylistic_similarity(text1, text2)
  expect_true(is.list(result))
  expect_true(all(c("text1_features", "text2_features", "feature_differences", "overall_similarity", "cosine_similarity") %in% names(result)))
  expect_true(is.numeric(result$overall_similarity))
  expect_true(result$overall_similarity >= 0 && result$overall_similarity <= 1)
})

test_that("sentiment_similarity works correctly", {
  conv1 <- "I love this product! It's amazing and works great."
  conv2 <- "This item is okay. It does the job but could be better."
  similarity <- sentiment_similarity(conv1, conv2)
  expect_true(is.numeric(similarity))
  expect_true(similarity >= 0 && similarity <= 1)
})
