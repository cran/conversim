#' Create windows from a conversation
#'
#' This function creates a list of windows from a conversation dataframe.
#'
#' @param conversation A dataframe containing the conversation, with a column named 'processed_text'.
#' @param window_size An integer specifying the size of each window.
#' @export
#' @return A list of character vectors, where each vector represents a window of text.
#' @examples
#' conversation <- data.frame(processed_text = c("hello", "world", "how", "are", "you"))
#' windows <- create_windows(conversation, 3)
create_windows <- function(conversation, window_size) {
  total_windows <- max(1, nrow(conversation) - window_size + 1)
  windows <- vector("list", total_windows)
  for (i in 1:total_windows) {
    windows[[i]] <- conversation$processed_text[i:min(i + window_size - 1, nrow(conversation))]
  }
  return(windows)
}

#' Calculate topic similarity sequence for a single dyad
#'
#' This function calculates topic similarity over a sequence of conversation exchanges within a single dyad.
#'
#' @param conversation A data frame representing the conversation
#' @param method A character string specifying the method to use: "lda" or "lsa"
#' @param num_topics An integer specifying the number of topics to use in the model
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities and the average similarity
#' @import slam
#' @import topicmodels
#' @import tm
#' @export
#' @examples
#' conversation <- data.frame(
#'   processed_text = c("The cat sat on the mat", "The dog chased the cat",
#'                      "The mat was comfortable", "The cat liked the mat")
#' )
#' result <- topic_sim_seq(conversation, method = "lda", num_topics = 2, window_size = 2)
#' print(result)
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

#' Calculate similarity sequence
#'
#' This function calculates a sequence of similarities between consecutive windows in a conversation.
#'
#' @param conversation A dataframe containing the conversation, with a column named 'processed_text'.
#' @param window_size An integer specifying the size of each window.
#' @param similarity_func A function that calculates similarity between two text strings.
#' @export
#' @return A list containing two elements:
#'   \item{sequence}{A numeric vector of similarity scores between consecutive windows}
#'   \item{average}{The mean of the similarity scores}
#' @examples
#' conversation <- data.frame(processed_text = c("hello", "world", "how", "are", "you"))
#' result <- calc_sim_seq(conversation, 2, function(x, y) sum(x == y) / max(length(x), length(y)))
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

#' Calculate lexical similarity sequence for a single dyad
#'
#' This function calculates lexical similarity over a sequence of conversation exchanges within a single dyad.
#'
#' @param conversation A data frame representing the conversation
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities and the average similarity
#' @export
#' @importFrom conversim lexical_similarity
#' @examples
#' conversation <- data.frame(
#'   processed_text = c("Hello world", "World of programming",
#'                      "Programming is fun", "Fun world of coding")
#' )
#' result <- lex_sim_seq(conversation, window_size = 2)
#' print(result)
lex_sim_seq <- function(conversation, window_size = 3) {
  calc_sim_seq(conversation, window_size, conversim::lexical_similarity)
}

#' Calculate semantic similarity sequence for a single dyad
#'
#' This function calculates semantic similarity over a sequence of conversation exchanges within a single dyad.
#'
#' @param conversation A data frame representing the conversation
#' @param method A character string specifying the method to use: "tfidf", "word2vec", or "glove"
#' @param window_size An integer specifying the size of the sliding window
#' @param ... Additional arguments passed to semantic_similarity
#' @return A list containing the sequence of similarities and the average similarity
#' @export
#' @importFrom conversim semantic_similarity
#' @examples
#' conversation <- data.frame(
#'   processed_text = c("The weather is nice", "It's a beautiful day",
#'                      "The sun is shining", "Perfect day for a picnic")
#' )
#' result <- sem_sim_seq(conversation, method = "tfidf", window_size = 2)
#' print(result)
sem_sim_seq <- function(conversation, method = "tfidf", window_size = 3, ...) {
  similarity_func <- function(text1, text2) {
    conversim::semantic_similarity(text1, text2, method, ...)
  }
  calc_sim_seq(conversation, window_size, similarity_func)
}

#' Calculate stylistic similarity sequence for a single dyad
#'
#' This function calculates stylistic similarity over a sequence of conversation exchanges within a single dyad.
#'
#' @param conversation A data frame representing the conversation
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities and the average similarity
#' @export
#' @importFrom conversim stylistic_similarity
#' @examples
#' conversation <- data.frame(
#'   processed_text = c("How are you doing?", "I'm doing great, thanks!",
#'                      "That's wonderful to hear.", "I'm glad you're doing well.")
#' )
#' result <- style_sim_seq(conversation, window_size = 2)
#' print(result)
style_sim_seq <- function(conversation, window_size = 3) {
  similarity_func <- function(text1, text2) {
    conversim::stylistic_similarity(text1, text2)$overall_similarity
  }
  calc_sim_seq(conversation, window_size, similarity_func)
}

#' Calculate sentiment similarity sequence for a single dyad
#'
#' This function calculates sentiment similarity over a sequence of conversation exchanges within a single dyad.
#'
#' @param conversation A data frame representing the conversation
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities and the average similarity
#' @export
#' @importFrom conversim sentiment_similarity
#' @examples
#' conversation <- data.frame(
#'   processed_text = c("I love this movie!", "It's really amazing.",
#'                      "The acting is superb.", "I couldn't agree more.")
#' )
#' result <- sent_sim_seq(conversation, window_size = 2)
#' print(result)
sent_sim_seq <- function(conversation, window_size = 3) {
  calc_sim_seq(conversation, window_size, conversim::sentiment_similarity)
}
