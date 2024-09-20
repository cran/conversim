#' Preprocess multiple dyad conversations
#'
#' This function preprocesses conversations from multiple dyads by applying text cleaning to each utterance.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'text'
#' @return A data frame with an additional 'processed_text' column, removing any rows with empty processed text
#' @export
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 2, 2),
#'   speaker = c("A", "B", "C", "D"),
#'   text = c("Hello!", "Hi there!", "How are you?", "I'm fine, thanks!")
#' )
#' preprocess_dyads(convs)
#' @name preprocess_dyads
preprocess_dyads <- function(conversations) {
  conversations$processed_text <- sapply(conversations$text, function(text) {
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("[[:digit:]]", "", text)
    text <- gsub("\\s+", " ", trimws(text))
    return(text)
  })

  # Remove empty processed texts
  conversations <- conversations[nchar(conversations$processed_text) > 0, ]

  return(conversations)
}

#' Calculate topic similarity for multiple dyads
#'
#' This function calculates topic similarity over a sequence of conversation exchanges for multiple dyads.
#' It uses the Latent Dirichlet Allocation (LDA) method for topic modeling and the "slam" package for
#' efficient handling of sparse matrices.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @param method A character string specifying the method to use: currently only "lda" is supported
#' @param num_topics An integer specifying the number of topics to use in the LDA model
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities for each dyad and the overall average similarity
#' @export
#' @import lme4
#' @import topicmodels
#' @import tm
#' @import slam
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' topic_sim_dyads(convs, method = "lda", num_topics = 2, window_size = 2)
#' @name topic_sim_dyads
topic_sim_dyads <- function(conversations, method = "lda", num_topics = 2, window_size = 3) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for this function. Please install it.")
  }

  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    stop("Package 'topicmodels' is required for this function. Please install it.")
  }

  if (!requireNamespace("tm", quietly = TRUE)) {
    stop("Package 'tm' is required for this function. Please install it.")
  }

  if (!requireNamespace("slam", quietly = TRUE)) {
    stop("Package 'slam' is required for this function. Please install it.")
  }

  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    similarities <- c()
    for (i in 1:(nrow(dyad_conv) - window_size + 1)) {
      window <- dyad_conv$processed_text[i:(i+window_size-1)]

      # Create a document-term matrix
      corpus <- tm::Corpus(tm::VectorSource(window))
      dtm <- tm::DocumentTermMatrix(corpus)

      # Check if the DTM is empty or has any empty documents using slam
      if (sum(slam::col_sums(dtm) > 0) == 0) {
        similarities <- c(similarities, NA)
        next
      }

      # Remove empty documents using slam
      dtm <- dtm[slam::row_sums(dtm) > 0, ]

      if (method == "lda") {
        tryCatch({
          lda_model <- topicmodels::LDA(dtm, k = num_topics, control = list(seed = 1234))
          topics <- topicmodels::topics(lda_model)
          sim <- sum(topics[1:(window_size/2)] == topics[(window_size/2+1):window_size]) / (window_size/2)
        }, error = function(e) {
          sim <- NA
        })
      } else {
        stop("Unsupported method. Only 'lda' is currently implemented.")
      }
      similarities <- c(similarities, sim)
    }
    all_similarities[[as.character(dyad)]] <- similarities
  }

  # Prepare data for multilevel modeling
  model_data <- data.frame(
    dyad_id = rep(dyads, sapply(all_similarities, length)),
    similarity = unlist(all_similarities)
  )

  # Remove NA values
  model_data <- model_data[!is.na(model_data$similarity), ]

  # Fit multilevel model
  model <- lme4::lmer(similarity ~ 1 + (1|dyad_id), data = model_data)

  # Extract overall average similarity accounting for dyad-level variation
  overall_average <- lme4::fixef(model)[1]

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate lexical similarity for multiple dyads
#'
#' This function calculates lexical similarity over a sequence of conversation exchanges for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities for each dyad and the overall average similarity
#' @import lme4
#' @importFrom conversim lexical_similarity
#' @export
#'
#' @examples
#' library(lme4)
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' lexical_sim_dyads(convs, window_size = 2)
#' @name lexical_sim_dyads
lexical_sim_dyads <- function(conversations, window_size = 3) {

  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    similarities <- c()
    for (i in 1:(nrow(dyad_conv) - window_size + 1)) {
      window1 <- paste(dyad_conv$processed_text[i:(i+window_size/2-1)], collapse = " ")
      window2 <- paste(dyad_conv$processed_text[(i+window_size/2):(i+window_size-1)], collapse = " ")
      sim <- conversim::lexical_similarity(window1, window2)
      similarities <- c(similarities, sim)
    }
    all_similarities[[as.character(dyad)]] <- similarities
  }

  # Prepare data for multilevel modeling
  model_data <- data.frame(
    dyad_id = rep(dyads, sapply(all_similarities, length)),
    similarity = unlist(all_similarities)
  )

  # Fit multilevel model
  model <- lme4::lmer(similarity ~ 1 + (1|dyad_id), data = model_data)

  # Extract overall average similarity accounting for dyad-level variation
  overall_average <- fixef(model)[1]

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate semantic similarity for multiple dyads
#'
#' This function calculates semantic similarity over a sequence of conversation exchanges for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @param method A character string specifying the method to use: "tfidf", "word2vec", or "glove"
#' @param window_size An integer specifying the size of the sliding window
#' @param ... Additional arguments passed to semantic_similarity
#' @return A list containing the sequence of similarities for each dyad and the overall average similarity
#' @import lme4
#' @importFrom conversim semantic_similarity
#' @export
#'
#' @examples
#' library(lme4)
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' semantic_sim_dyads(convs, method = "tfidf", window_size = 2)
#' @name semantic_sim_dyads
semantic_sim_dyads <- function(conversations, method = "tfidf", window_size = 3, ...) {

  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    similarities <- c()
    for (i in 1:(nrow(dyad_conv) - window_size + 1)) {
      window1 <- paste(dyad_conv$processed_text[i:(i+window_size/2-1)], collapse = " ")
      window2 <- paste(dyad_conv$processed_text[(i+window_size/2):(i+window_size-1)], collapse = " ")
      sim <- conversim::semantic_similarity(window1, window2, method, ...)
      similarities <- c(similarities, sim)
    }
    all_similarities[[as.character(dyad)]] <- similarities
  }

  # Prepare data for multilevel modeling
  model_data <- data.frame(
    dyad_id = rep(dyads, sapply(all_similarities, length)),
    similarity = unlist(all_similarities)
  )

  # Fit multilevel model
  model <- lme4::lmer(similarity ~ 1 + (1|dyad_id), data = model_data)

  # Extract overall average similarity accounting for dyad-level variation
  overall_average <- fixef(model)[1]

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate structural similarity for multiple dyads
#'
#' This function calculates an extended measure of structural similarity for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @return A list containing structural similarity for each dyad and the overall average similarity
#' @export
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' structural_sim_dyads(convs)
#' @name structural_sim_dyads
structural_sim_dyads <- function(conversations) {
  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]

    length_sim <- 1
    turn_lengths <- nchar(dyad_conv$processed_text)
    turn_length_sim <- 1 - sd(turn_lengths) / mean(turn_lengths)

    speaker_changes <- sum(dyad_conv$speaker[-1] != dyad_conv$speaker[-nrow(dyad_conv)])
    speaker_change_sim <- 1 - abs(speaker_changes - (nrow(dyad_conv) / 2)) / (nrow(dyad_conv) / 2)

    similarity <- mean(c(length_sim, turn_length_sim, speaker_change_sim))
    all_similarities[[as.character(dyad)]] <- similarity
  }

  # Calculate overall average using simple mean
  overall_average <- mean(unlist(all_similarities))

  # Print warning about not using multilevel modeling
  warning("Only one observation per dyad. Using simple mean for overall average instead of multilevel modeling.")

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate stylistic similarity for multiple dyads
#'
#' This function calculates stylistic similarity over a sequence of conversation exchanges for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities for each dyad and the overall average similarity
#' @import lme4
#' @importFrom conversim stylistic_similarity
#' @export
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' stylistic_sim_dyads(convs, window_size = 2)
#' @name stylistic_sim_dyads
stylistic_sim_dyads <- function(conversations, window_size = 3) {

  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    similarities <- c()
    for (i in 1:(nrow(dyad_conv) - window_size + 1)) {
      window1 <- paste(dyad_conv$processed_text[i:(i+window_size/2-1)], collapse = " ")
      window2 <- paste(dyad_conv$processed_text[(i+window_size/2):(i+window_size-1)], collapse = " ")
      sim <- conversim::stylistic_similarity(window1, window2)$overall_similarity
      similarities <- c(similarities, sim)
    }
    all_similarities[[as.character(dyad)]] <- similarities
  }

  # Prepare data for multilevel modeling
  model_data <- data.frame(
    dyad_id = rep(dyads, sapply(all_similarities, length)),
    similarity = unlist(all_similarities)
  )

  # Fit multilevel model
  model <- lme4::lmer(similarity ~ 1 + (1|dyad_id), data = model_data)

  # Extract overall average similarity accounting for dyad-level variation
  overall_average <- fixef(model)[1]

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate sentiment similarity for multiple dyads
#'
#' This function calculates sentiment similarity over a sequence of conversation exchanges for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @param window_size An integer specifying the size of the sliding window
#' @return A list containing the sequence of similarities for each dyad and the overall average similarity
#' @export
#' @import lme4
#' @importFrom conversim sentiment_similarity
#'
#' @examples
#' library(lme4)
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' sentiment_sim_dyads(convs, window_size = 2)
#' @name sentiment_sim_dyads
sentiment_sim_dyads <- function(conversations, window_size = 3) {

  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    similarities <- c()
    for (i in 1:(nrow(dyad_conv) - window_size + 1)) {
      window1 <- paste(dyad_conv$processed_text[i:(i+window_size/2-1)], collapse = " ")
      window2 <- paste(dyad_conv$processed_text[(i+window_size/2):(i+window_size-1)], collapse = " ")
      sim <- conversim::sentiment_similarity(window1, window2)
      similarities <- c(similarities, sim)
    }
    all_similarities[[as.character(dyad)]] <- similarities
  }

  # Prepare data for multilevel modeling
  model_data <- data.frame(
    dyad_id = rep(dyads, sapply(all_similarities, length)),
    similarity = unlist(all_similarities)
  )

  # Fit multilevel model
  model <- lme4::lmer(similarity ~ 1 + (1|dyad_id), data = model_data)

  # Extract overall average similarity accounting for dyad-level variation
  overall_average <- fixef(model)[1]

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate participant similarity for multiple dyads
#'
#' This function calculates an extended measure of participant similarity for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @return A list containing participant similarity for each dyad and the overall average similarity
#' @export
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' participant_sim_dyads(convs)
#' @name participant_sim_dyads
participant_sim_dyads <- function(conversations) {
  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    speakers <- table(dyad_conv$speaker) / nrow(dyad_conv)

    # Calculate entropy as a measure of speaker balance
    entropy <- -sum(speakers * log(speakers))
    max_entropy <- -log(1/length(speakers))

    # Normalize entropy to [0, 1] range
    similarity <- entropy / max_entropy

    all_similarities[[as.character(dyad)]] <- similarity
  }

  # Calculate overall average using simple mean
  overall_average <- mean(unlist(all_similarities))

  # Print warning about not using multilevel modeling
  warning("Only one observation per dyad. Using simple mean for overall average instead of multilevel modeling.")

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}

#' Calculate timing similarity for multiple dyads
#'
#' This function calculates an extended measure of timing similarity for multiple dyads.
#'
#' @param conversations A data frame with columns 'dyad_id', 'speaker', and 'processed_text'
#' @return A list containing timing similarity for each dyad and the overall average similarity
#' @export
#'
#' @examples
#' convs <- data.frame(
#'   dyad_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   speaker = c("A", "B", "A", "B", "C", "D", "C", "D"),
#'   processed_text = c("i love pizza", "me too favorite food",
#'                      "whats your favorite topping", "enjoy pepperoni mushrooms",
#'                      "i prefer pasta", "pasta delicious like spaghetti carbonara",
#'                      "ever tried making home", "yes quite easy make")
#' )
#' timing_sim_dyads(convs)
#' @name timing_sim_dyads
timing_sim_dyads <- function(conversations) {
  dyads <- unique(conversations$dyad_id)
  all_similarities <- list()

  for (dyad in dyads) {
    dyad_conv <- conversations[conversations$dyad_id == dyad, ]
    turn_lengths <- nchar(dyad_conv$processed_text)

    length_sim <- 1 - stats::sd(turn_lengths) / mean(turn_lengths)

    # Calculate rhythm similarity based on turn length differences
    rhythm_diffs <- diff(turn_lengths)
    rhythm_sim <- 1 - stats::sd(rhythm_diffs) / mean(abs(rhythm_diffs))

    similarity <- mean(c(length_sim, rhythm_sim))
    all_similarities[[as.character(dyad)]] <- similarity
  }

  # Calculate overall average using simple mean
  overall_average <- mean(unlist(all_similarities))

  # Print warning about not using multilevel modeling
  warning("Only one observation per dyad. Using simple mean for overall average instead of multilevel modeling.")

  return(list(similarities_by_dyad = all_similarities, overall_average = overall_average))
}
