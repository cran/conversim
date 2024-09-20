# Load required libraries
library(tm)
library(topicmodels)
library(lsa)
library(word2vec)
library(sentimentr)

# Load the speeches_data dataset
load(system.file("extdata", "speeches_data.RData", package = "conversim"))

# Test preprocess_text function
cat("Testing preprocess_text function:\n")
preprocessed_A <- preprocess_text(speeches_data$text[1])
preprocessed_B <- preprocess_text(speeches_data$text[2])
cat("First 100 characters of preprocessed speech A:", substr(preprocessed_A, 1, 100), "\n")
cat("First 100 characters of preprocessed speech B:", substr(preprocessed_B, 1, 100), "\n\n")

# Test topic_similarity function
cat("Testing topic_similarity function:\n")
lda_similarity <- topic_similarity(speeches_data$text[1], speeches_data$text[2], method = "lda", num_topics = 5)
lsa_similarity <- topic_similarity(speeches_data$text[1], speeches_data$text[2], method = "lsa", num_topics = 5)
cat("LDA topic similarity:", lda_similarity, "\n")
cat("LSA topic similarity:", lsa_similarity, "\n\n")

# Test lexical_similarity function
cat("Testing lexical_similarity function:\n")
lex_similarity <- lexical_similarity(preprocessed_A, preprocessed_B)
cat("Lexical similarity:", lex_similarity, "\n\n")

# Test calculate_semantic_similarity function
cat("Testing calculate_semantic_similarity function:\n")
tfidf_similarity <- semantic_similarity(speeches_data$text[1], speeches_data$text[2], method = "tfidf")
word2vec_similarity <- semantic_similarity(speeches_data$text[1], speeches_data$text[2], method = "word2vec")
cat("TF-IDF semantic similarity:", tfidf_similarity, "\n")
cat("Word2Vec semantic similarity:", word2vec_similarity, "\n\n")

# Test structural_similarity function
cat("Testing structural_similarity function:\n")
struct_similarity <- structural_similarity(strsplit(speeches_data$text[1], "\n")[[1]], strsplit(speeches_data$text[2], "\n")[[1]])
cat("Structural similarity:", struct_similarity, "\n\n")

# Test calculate_stylistic_similarity function
cat("Testing calculate_stylistic_similarity function:\n")
style_similarity <- stylistic_similarity(speeches_data$text[1], speeches_data$text[2])
print(style_similarity)
cat("\n")

# Test sentiment_similarity function
cat("Testing sentiment_similarity function:\n")
sent_similarity <- sentiment_similarity(speeches_data$text[1], speeches_data$text[2])
cat("Sentiment similarity:", sent_similarity, "\n\n")
