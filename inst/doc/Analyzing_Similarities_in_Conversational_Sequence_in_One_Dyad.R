## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(ggplot2)
library(tm)
library(conversim)

## ----sample_data--------------------------------------------------------------
set.seed(123)
conversation <- data.frame(
  speaker = rep(c("A", "B"), 10),
  processed_text = c(
    "Hello how are you today",
    "I'm doing well thanks for asking",
    "That's great to hear what are your plans",
    "I'm planning to go for a walk later",
    "Sounds nice the weather is beautiful",
    "Yes it's perfect for outdoor activities",
    "Do you often go for walks",
    "Yes I try to walk every day for exercise",
    "That's a great habit to have",
    "Thanks I find it helps me stay healthy",
    "Have you tried other forms of exercise",
    "I also enjoy swimming and yoga",
    "Those are excellent choices too",
    "What about you what exercise do you prefer",
    "I like running and playing tennis",
    "Tennis is fun do you play often",
    "I try to play at least once a week",
    "That's a good frequency to maintain",
    "Yes it keeps me active and social",
    "Social aspects of exercise are important too"
  )
)

## ----echo=FALSE---------------------------------------------------------------
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

create_windows <- function(conversation, window_size) {
  total_windows <- max(1, nrow(conversation) - window_size + 1)
  windows <- vector("list", total_windows)

  for (i in 1:total_windows) {
    windows[[i]] <- conversation$processed_text[i:min(i + window_size - 1, nrow(conversation))]
  }

  return(windows)
}

topic_sim_seq <- function(conversation, method = "lda", num_topics = 2, window_size = 3) {
  windows <- create_windows(conversation, window_size)
  total_windows <- length(windows)
  similarities <- numeric(max(1, total_windows - 1))

  if (total_windows <= 1) {
    return(list(sequence = numeric(0), average = NA))
  }

  for (i in 1:(total_windows - 1)) {
    window1 <- windows[[i]]
    window2 <- windows[[i + 1]]

    # Create document-term matrices
    corpus1 <- tm::Corpus(tm::VectorSource(window1))
    corpus2 <- tm::Corpus(tm::VectorSource(window2))
    dtm1 <- tm::DocumentTermMatrix(corpus1)
    dtm2 <- tm::DocumentTermMatrix(corpus2)

    # Check if the DTMs are empty or have any empty documents
    if (sum(slam::col_sums(dtm1) > 0) == 0 || sum(slam::col_sums(dtm2) > 0) == 0) {
      similarities[i] <- NA
      next
    }

    # Remove empty documents
    dtm1 <- dtm1[slam::row_sums(dtm1) > 0, ]
    dtm2 <- dtm2[slam::row_sums(dtm2) > 0, ]

    if (method == "lda") {
      tryCatch({
        lda_model1 <- topicmodels::LDA(dtm1, k = min(num_topics, ncol(dtm1)), control = list(seed = 1234))
        lda_model2 <- topicmodels::LDA(dtm2, k = min(num_topics, ncol(dtm2)), control = list(seed = 1234))
        topics1 <- topicmodels::topics(lda_model1)
        topics2 <- topicmodels::topics(lda_model2)
        similarities[i] <- sum(topics1 == topics2) / length(topics1)
      }, error = function(e) {
        similarities[i] <- NA
      })
    } else {
      stop("Unsupported method. Only 'lda' is currently implemented.")
    }
  }

  # Calculate the average similarity
  average_similarity <- mean(similarities, na.rm = TRUE)

  # Aggregate similarities into three segments
  segment_size <- ceiling(length(similarities) / 3)
  aggregated_similarities <- c(
    mean(similarities[1:segment_size], na.rm = TRUE),
    mean(similarities[(segment_size + 1):(2 * segment_size)], na.rm = TRUE),
    mean(similarities[(2 * segment_size + 1):length(similarities)], na.rm = TRUE)
  )

  return(list(
    sequence = aggregated_similarities,
    average = average_similarity
  ))
}

calc_sim_seq <- function(conversation, window_size, similarity_func) {
  windows <- create_windows(conversation, window_size)
  total_windows <- length(windows)

  if (total_windows <= 1) {
    return(list(sequence = numeric(0), average = NA))
  }

  similarities <- numeric(total_windows - 1)

  for (i in 1:(total_windows - 1)) {
    window1 <- paste(windows[[i]], collapse = " ")
    window2 <- paste(windows[[i + 1]], collapse = " ")
    similarities[i] <- similarity_func(window1, window2)
  }

  return(list(sequence = similarities, average = mean(similarities, na.rm = TRUE)))
}

lex_sim_seq <- function(conversation, window_size = 3) {
  calc_sim_seq(conversation, window_size, conversim::lexical_similarity)
}

sem_sim_seq <- function(conversation, method = "tfidf", window_size = 3, ...) {
  similarity_func <- function(text1, text2) {
    semantic_similarity(text1, text2, method, ...)
  }
  calc_sim_seq(conversation, window_size, similarity_func)
}

style_sim_seq <- function(conversation, window_size = 3) {
  similarity_func <- function(text1, text2) {
    stylistic_similarity(text1, text2)$overall_similarity
  }
  calc_sim_seq(conversation, window_size, similarity_func)
}

sent_sim_seq <- function(conversation, window_size = 3) {
  calc_sim_seq(conversation, window_size, conversim::sentiment_similarity)
}

## ----topic_similarity---------------------------------------------------------
topic_sim <- topic_sim_seq(conversation, method = "lda", num_topics = 2, window_size = 3)

# Plot the topic similarity sequence
ggplot(data.frame(Segment = 1:3, Similarity = topic_sim$sequence), aes(x = Segment, y = Similarity)) +
  geom_line() +
  geom_point() +
  labs(title = "Topic Similarity Sequence", x = "Conversation Segment", y = "Similarity Score") +
  theme_minimal()

# Print the average topic similarity
cat("Average topic similarity:", round(topic_sim$average, 3))

## ----lexical_similarity-------------------------------------------------------
lexical_sim <- lex_sim_seq(conversation, window_size = 3)

# Plot the lexical similarity sequence
ggplot(data.frame(Exchange = 1:length(lexical_sim$sequence), Similarity = lexical_sim$sequence), 
       aes(x = Exchange, y = Similarity)) +
  geom_line() +
  geom_point() +
  labs(title = "Lexical Similarity Sequence", x = "Conversation Exchange", y = "Similarity Score") +
  theme_minimal()

# Print the average lexical similarity
cat("Average lexical similarity:", round(lexical_sim$average, 3))

## ----semantic_similarity------------------------------------------------------
semantic_sim <- sem_sim_seq(conversation, method = "tfidf", window_size = 3)

# Plot the semantic similarity sequence
ggplot(data.frame(Exchange = 1:length(semantic_sim$sequence), Similarity = semantic_sim$sequence), 
       aes(x = Exchange, y = Similarity)) +
  geom_line() +
  geom_point() +
  labs(title = "Semantic Similarity Sequence", x = "Conversation Exchange", y = "Similarity Score") +
  theme_minimal()

# Print the average semantic similarity
cat("Average semantic similarity:", round(semantic_sim$average, 3))

## ----stylistic_similarity-----------------------------------------------------
stylistic_sim <- style_sim_seq(conversation, window_size = 3)

# Plot the stylistic similarity sequence
ggplot(data.frame(Exchange = 1:length(stylistic_sim$sequence), Similarity = stylistic_sim$sequence), 
       aes(x = Exchange, y = Similarity)) +
  geom_line() +
  geom_point() +
  labs(title = "Stylistic Similarity Sequence", x = "Conversation Exchange", y = "Similarity Score") +
  theme_minimal()

# Print the average stylistic similarity
cat("Average stylistic similarity:", round(stylistic_sim$average, 3))

## ----sentiment_similarity-----------------------------------------------------
sentiment_sim <- sent_sim_seq(conversation, window_size = 3)

# Plot the sentiment similarity sequence
ggplot(data.frame(Exchange = 1:length(sentiment_sim$sequence), Similarity = sentiment_sim$sequence), 
       aes(x = Exchange, y = Similarity)) +
  geom_line() +
  geom_point() +
  labs(title = "Sentiment Similarity Sequence", x = "Conversation Exchange", y = "Similarity Score") +
  theme_minimal()

# Print the average sentiment similarity
cat("Average sentiment similarity:", round(sentiment_sim$average, 3))

