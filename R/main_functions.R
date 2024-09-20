#' This file contains core similarity calculation functions such as topic similarity, lexical similarity, semantic similarity,
#' structural similarity, stylistic similarity, sentiment similarity, participant similarity, and timing similarity.
#'
#'
#' Preprocess text for analysis
#'
#' This function preprocesses the input text by converting to lowercase,
#' removing punctuation and digits, and trimming whitespace.
#'
#' @param text A character string to be preprocessed
#' @return A preprocessed character string
#' @export
#'
#' @examples
#' text <- "Hello, World! This is an example text (with 123 numbers)."
#' preprocess_text(text)
#' @name preprocess_text
preprocess_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("[[:digit:]]", "", text)
  text <- gsub("\\s+", " ", trimws(text))
  return(text)
}

#' Calculate topic similarity between two conversations
#'
#' This function calculates the topic similarity between two conversations using either
#' Latent Dirichlet Allocation (LDA) or Latent Semantic Analysis (LSA).
#'
#' @param conv1 A character vector representing the first conversation
#' @param conv2 A character vector representing the second conversation
#' @param method A character string specifying the method to use: "lda" or "lsa"
#' @param num_topics An integer specifying the number of topics to use in the model
#' @return A numeric value representing the topic similarity
#' @export
#'
#' @importFrom tm Corpus VectorSource DocumentTermMatrix
#' @importFrom topicmodels LDA posterior
#' @importFrom lsa lsa cosine
#'
#' @examples
#' conv1 <- c("I love pizza", "Pizza is my favorite food")
#' conv2 <- c("I prefer pasta", "Pasta is delicious")
#' topic_similarity(conv1, conv2, method = "lda", num_topics = 2)
#' topic_similarity(conv1, conv2, method = "lsa", num_topics = 2)
#' @name topic_similarity
topic_similarity <- function(conv1, conv2, method = "lda", num_topics = 2) {
  corpus <- c(conv1, conv2)
  dtm <- DocumentTermMatrix(Corpus(VectorSource(corpus)))
  dtm_matrix <- as.matrix(dtm)

  if (method == "lda") {
    lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
    topic_dist <- posterior(lda_model)$topics

    js_divergence <- function(p, q) {
      m <- 0.5 * (p + q)
      0.5 * sum(p * log(p / m)) + 0.5 * sum(q * log(q / m))
    }

    similarity <- 1 - sqrt(js_divergence(topic_dist[1,], topic_dist[2,]))

  } else if (method == "lsa") {
    if (nrow(dtm_matrix) < num_topics) {
      num_topics <- nrow(dtm_matrix)
    }
    lsa_space <- lsa(dtm_matrix, dims = num_topics)
    doc_lsa <- lsa_space$dk

    similarity <- cosine(doc_lsa[1,], doc_lsa[2,])
    similarity <- (similarity + 1) / 2

  } else {
    stop("Invalid method. Choose 'lda' or 'lsa'.")
  }

  return(similarity)
}

#' Calculate lexical similarity between two conversations
#'
#' This function calculates the lexical similarity between two conversations
#' based on the overlap of unique words.
#'
#' @param conv1 A character string representing the first conversation
#' @param conv2 A character string representing the second conversation
#' @return A numeric value representing the lexical similarity
#' @export
#'
#' @examples
#' conv1 <- "The quick brown fox jumps over the lazy dog"
#' conv2 <- "The lazy dog sleeps under the quick brown fox"
#' lexical_similarity(conv1, conv2)
#' @name lexical_similarity
lexical_similarity <- function(conv1, conv2) {
  words1 <- unique(unlist(strsplit(conv1, " ")))
  words2 <- unique(unlist(strsplit(conv2, " ")))

  intersection <- length(intersect(words1, words2))
  union <- length(union(words1, words2))

  return(intersection / union)
}

#' Calculate semantic similarity between two conversations
#'
#' This function calculates the semantic similarity between two conversations
#' using either TF-IDF, Word2Vec, or GloVe embeddings approach.
#'
#' @param conversation1 A character string representing the first conversation
#' @param conversation2 A character string representing the second conversation
#' @param method A character string specifying the method to use: "tfidf", "word2vec", or "glove"
#' @param model_path A character string specifying the path to pre-trained GloVe file (required for "glove" method)
#' @param dim An integer specifying the dimensionality for Word2Vec embeddings (default: 100)
#' @param window An integer specifying the window size for Word2Vec (default: 5)
#' @param iter An integer specifying the number of iterations for Word2Vec (default: 5)
#' @return A numeric value representing the semantic similarity (between 0 and 1)
#' @export
#'
#' @importFrom tm Corpus VectorSource DocumentTermMatrix weightTfIdf
#' @importFrom word2vec word2vec
#' @name semantic_similarity
#' @examples
#' conv1 <- "The quick brown fox jumps over the lazy dog"
#' conv2 <- "A fast auburn canine leaps above an idle hound"
#' semantic_similarity(conv1, conv2, method = "tfidf")
semantic_similarity <- function(conversation1, conversation2, method = "tfidf", model_path = NULL, dim = 100, window = 5, iter = 5) {
  # Internal function to calculate cosine similarity
  cosine_similarity <- function(a, b) {
    if (length(a) == 0 || length(b) == 0) return(0)
    sim <- sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
    # Ensure the result is between 0 and 1
    return((sim + 1) / 2)
  }

  # Internal function to load pre-trained GloVe embeddings
  load_glove <- function(file_path) {
    tryCatch({
      conn <- file(file_path, "r")
      lines <- readLines(conn)
      close(conn)
      split_lines <- strsplit(lines, " ")
      words <- sapply(split_lines, `[`, 1)
      vectors <- t(sapply(split_lines, function(x) as.numeric(x[-1])))
      rownames(vectors) <- words
      return(vectors)
    }, error = function(e) {
      stop(paste("Error loading GloVe file:", e$message))
    })
  }

  # Internal function to calculate sentence embedding
  sentence_embedding <- function(sentence, word_vectors) {
    tokens <- unlist(strsplit(sentence, "\\s+"))
    valid_tokens <- tokens[tokens %in% rownames(word_vectors)]
    if (length(valid_tokens) == 0) {
      return(rep(0, ncol(word_vectors)))
    }
    embeddings <- word_vectors[valid_tokens, , drop = FALSE]
    if (nrow(embeddings) == 0) return(rep(0, ncol(word_vectors)))
    return(colMeans(embeddings))
  }

  if (method == "tfidf") {
    # TF-IDF approach
    corpus <- c(conversation1, conversation2)
    dtm <- DocumentTermMatrix(Corpus(VectorSource(corpus)))
    tfidf <- weightTfIdf(dtm)
    m <- as.matrix(tfidf)

    # Issue a warning for short conversations or little vocabulary overlap
    if (nchar(conversation1) < 50 || nchar(conversation2) < 50 || ncol(m) < 5) {
      warning("The 'tfidf' method may not provide highly meaningful results for short conversations or those with little vocabulary overlap. Consider using 'word2vec' or 'glove' methods for more robust results.")
    }

    # If the conversations are identical, return 1
    if (identical(conversation1, conversation2)) {
      return(1)
    }
    # Ensure we have at least one term in common
    if (ncol(m) == 0) {
      return(0)
    }
    # Calculate cosine similarity
    similarity <- cosine_similarity(m[1,], m[2,])

  } else if (method == "word2vec" || method == "glove") {
    # Word2Vec or GloVe approach
    if (method == "word2vec") {
      # Train Word2Vec model
      all_text <- c(conversation1, conversation2)
      model <- word2vec(x = all_text, dim = dim, iter = iter, window = window, min_count = 1)
      word_vectors <- as.matrix(model)
    } else { # method == "glove"
      if (is.null(model_path)) {
        stop("Please provide a path to the pre-trained GloVe file.")
      }
      # Load pre-trained GloVe vectors
      word_vectors <- load_glove(model_path)
    }

    # Calculate embeddings for each conversation
    embedding1 <- sentence_embedding(conversation1, word_vectors)
    embedding2 <- sentence_embedding(conversation2, word_vectors)

    # Calculate cosine similarity
    similarity <- cosine_similarity(embedding1, embedding2)
  } else {
    stop("Invalid method. Choose 'tfidf', 'word2vec', or 'glove'.")
  }

  return(similarity)
}

#' Calculate structural similarity between two conversations
#'
#' This function calculates the structural similarity between two conversations
#' based on their length and average turn length.
#'
#' @param conv1 A character vector representing the first conversation
#' @param conv2 A character vector representing the second conversation
#' @return A numeric value representing the structural similarity
#' @export
#'
#' @examples
#' conv1 <- c("Hello", "Hi there", "How are you?", "I'm fine, thanks")
#' conv2 <- c("Good morning", "Hello", "Nice day, isn't it?", "Yes, indeed")
#' structural_similarity(conv1, conv2)
#' @name structural_similarity
structural_similarity <- function(conv1, conv2) {
  length_sim <- 1 - abs(length(conv1) - length(conv2)) / max(length(conv1), length(conv2))

  avg_turn_length1 <- mean(nchar(conv1))
  avg_turn_length2 <- mean(nchar(conv2))
  turn_length_sim <- 1 - abs(avg_turn_length1 - avg_turn_length2) / max(avg_turn_length1, avg_turn_length2)

  return(mean(c(length_sim, turn_length_sim)))
}

#' Calculate stylistic similarity between two conversations
#'
#' This function calculates various stylistic features and their similarity
#' between two conversations.
#'
#' @param text1 A character string representing the first conversation
#' @param text2 A character string representing the second conversation
#' @return A list containing stylistic features and similarity measures
#' @export
#'
#' @importFrom stats sd
#'
#' @examples
#' text1 <- "The quick brown fox jumps over the lazy dog. It's a sunny day."
#' text2 <- "A lazy cat sleeps on the warm windowsill. Birds chirp outside."
#' stylistic_similarity(text1, text2)
#' @name stylistic_similarity
stylistic_similarity <- function(text1, text2) {
  # Helper function to calculate features for a single text
  calculate_features <- function(text) {
    words <- strsplit(text, " ")[[1]]
    sentences <- strsplit(text, "\\. ")[[1]]

    ttr <- length(unique(words)) / length(words)
    avg_sentence_length <- mean(sapply(sentences, function(s) length(strsplit(s, " ")[[1]])))
    syllables <- sum(sapply(words, function(w) max(1, nchar(gsub("[^aeiouAEIOU]", "", w)))))
    fk_grade <- 0.39 * (length(words) / length(sentences)) + 11.8 * (syllables / length(words)) - 15.59

    c(ttr = ttr, avg_sentence_length = avg_sentence_length, fk_grade = fk_grade)
  }

  features1 <- calculate_features(text1)
  features2 <- calculate_features(text2)
  feature_diff <- abs(features1 - features2)
  overall_similarity <- 1 - mean(feature_diff / pmax(features1, features2))

  normalized1 <- (features1 - mean(features1)) / sd(features1)
  normalized2 <- (features2 - mean(features2)) / sd(features2)
  cosine_similarity <- sum(normalized1 * normalized2) /
    (sqrt(sum(normalized1^2)) * sqrt(sum(normalized2^2)))

  list(
    text1_features = features1,
    text2_features = features2,
    feature_differences = feature_diff,
    overall_similarity = overall_similarity,
    cosine_similarity = cosine_similarity
  )
}

#' Calculate sentiment similarity between two conversations
#'
#' This function calculates the sentiment similarity between two conversations
#' using the sentimentr package.
#'
#' @param conv1 A character string representing the first conversation
#' @param conv2 A character string representing the second conversation
#' @return A numeric value representing the sentiment similarity
#' @export
#'
#' @importFrom sentimentr sentiment_by
#'
#' @examples
#' conv1 <- "I love this product! It's amazing and works great."
#' conv2 <- "This item is okay. It does the job but could be better."
#' sentiment_similarity(conv1, conv2)
#' @name sentiment_similarity
sentiment_similarity <- function(conv1, conv2) {
  sent1 <- sentiment_by(conv1)$ave_sentiment
  sent2 <- sentiment_by(conv2)$ave_sentiment

  return(1 - abs(sent1 - sent2) / 2)
}
