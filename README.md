
# conversim <img src="man/figures/conversim_logo.png" align="right" height="139" alt="logo" style="float:right; height:139px;"/>

The `conversim` package provides tools for analyzing similarity between
conversations, with a focus on calculating topic, lexical, semantic,
stylistic, and sentiment similarities. This package can handle
comparisons between two long speeches, a sequence of conversations in
one or multiple dyads. Some utility functions are also provided that
allow researchers to explore and visualize conversational patterns.

## Installation

You can install `conversim` on CRAN:

``` r
install.packages("conversim")
```

## Load the package and example datasets

``` r
library(conversim)
load(system.file("extdata", "dyad_example_data.Rdata", package = "conversim"))
load(system.file("extdata", "speeches_data.RData", package = "conversim"))
```

## Example usage

Below are examples of how to use the main functions in the `conversim`
package.

#### Analyzing Similarities between Two Long Speeches

``` r
# preprocess_text function
preprocessed_A <- preprocess_text(speeches_data$text[1])
preprocessed_B <- preprocess_text(speeches_data$text[2])

# topic_similarity function
lda_similarity <- topic_similarity(speeches_data$text[1], speeches_data$text[2], method = "lda", num_topics = 5)
lsa_similarity <- topic_similarity(speeches_data$text[1], speeches_data$text[2], method = "lsa", num_topics = 5)

# lexical_similarity function
lex_similarity <- lexical_similarity(preprocessed_A, preprocessed_B)

# semantic_similarity function
tfidf_similarity <- semantic_similarity(speeches_data$text[1], speeches_data$text[2], method = "tfidf")
word2vec_similarity <- semantic_similarity(speeches_data$text[1], speeches_data$text[2], method = "word2vec")

# structural_similarity function
struct_similarity <- structural_similarity(strsplit(speeches_data$text[1], "\n")[[1]], strsplit(speeches_data$text[2], "\n")[[1]])

# stylistic_similarity function
style_similarity <- stylistic_similarity(speeches_data$text[1], speeches_data$text[2])

# sentiment_similarity function
sent_similarity <- sentiment_similarity(speeches_data$text[1], speeches_data$text[2])
```

#### Analyzing Similarities over a Sequence of Conversations in a Single Dyad

``` r
# Preprocess the conversations from multiple dyads
preprocessed_data <- preprocess_dyads(dyad_example_data)

# Select one dyad for comparison
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
```

#### Analyzing Similarities over a Sequence of Conversations across Multiple Dyads

``` r
# Preprocess the conversations from multiple dyads
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
```

For more tutorials, please visit
[liu-chao.site/conversim](https://liu-chao.site/conversim/)
