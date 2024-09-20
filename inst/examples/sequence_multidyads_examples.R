library(dplyr)

# Load data
data_path <- system.file("extdata", "dyad_example_data.Rdata", package = "conversim")
load(data_path)

# Now, let's test functions from each file

# 1. Testing functions from conversation_multidyads.R

# Preprocess the conversations
preprocessed_data <- preprocess_dyads(dyad_example_data)

# Calculate topic similarity for multiple dyads
topic_sim_results <- topic_sim_dyads(preprocessed_data, method = "lda", num_topics = 3, window_size = 2)

# Calculate lexical similarity for multiple dyads
lexical_sim_results <- lexical_sim_dyads(preprocessed_data, window_size = 2)

# Calculate semantic similarity for multiple dyads
semantic_sim_results <- semantic_sim_dyads(preprocessed_data, method = "tfidf", window_size = 2)

# Calculate structural similarity for multiple dyads
structural_sim_results <- structural_sim_dyads(preprocessed_data)

# Calculate stylistic similarity for multiple dyads
stylistic_sim_results <- stylistic_sim_dyads(preprocessed_data)

# Calculate sentiment similarity for multiple dyads
sentiment_sim_results <- sentiment_sim_dyads(preprocessed_data)

# Calculate participant similarity for multiple dyads
participant_sim_results <- participant_sim_dyads(preprocessed_data)

# Calculate timing similarity for multiple dyads
timing_sim_results <- timing_sim_dyads(preprocessed_data)

# Print results
cat("Topic Similarity in dyads:", topic_sim_results$overall_average, "\n")
cat("Lexical Similarity in dyads:", lexical_sim_results$overall_average, "\n")
cat("Semantic Similarity in dyads:", semantic_sim_results$overall_average, "\n")
cat("Structural Similarity in dyads:", structural_sim_results$overall_average, "\n")
cat("Stylistic Similarity in dyads:", stylistic_sim_results$overall_average, "\n")
cat("Sentiment Similarity in dyads:", sentiment_sim_results$overall_average, "\n")
cat("Participant Similarity in dyads:", participant_sim_results$overall_average, "\n")
cat("Timing Similarity in dyads:", timing_sim_results$overall_average, "\n")

# 2. Testing functions from conversation_sequence.R

# Select two conversations for comparison
conversation <- preprocessed_data %>% filter(dyad_id == 1) %>% select(speaker_id, processed_text)

# Calculate topic similarity sequence
topic_sim <- topic_sim_seq(conversation, method = "lda", num_topics = 2, window_size = 3)

## Lexical Similarity Sequence
lexical_sim <- lex_sim_seq(conversation, window_size = 3)

## Semantic Similarity Sequence
semantic_sim <- sem_sim_seq(conversation, method = "tfidf", window_size = 3)

## Stylistic Similarity Sequence
stylistic_sim <- style_sim_seq(conversation, window_size = 3)

## Sentiment Similarity Sequence
sentiment_sim <- sent_sim_seq(conversation, window_size = 3)

# Print results
cat("Topic Similarity:", topic_sim$sequence, "\n")
cat("Lexical Similarity:", lexical_sim$sequence, "\n")
cat("Semantic Similarity:", semantic_sim$sequence, "\n")
cat("Stylistic Similarity:", stylistic_sim$sequence, "\n")
cat("Sentiment Similarity:", sentiment_sim$sequence, "\n")

