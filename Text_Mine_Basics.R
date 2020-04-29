##### Text Mining - 2019 ####
### Author: Allen Baumgardner-Zuzik ###
### Date: 5/2019 - ###

### Guide: Julia Silge - tidy text mining ###
# Check Github: JuliaSilge
# bit.ly/silge-sdss-1
# bit.ly/silge-sdss-2
# Text Mining Book: tidytextmining.com 

### Perliminaries ###

# Install packages
# install.packages(c("tidytext", "gutenbergr"))

# Install development tools
# library(devtools)
# install.github("Juliasilge/tidytext)

# Load libraries
library(tidyverse)
library(tidytext)
library(gutenbergr)

### Introduction ###
# This code shows the basic way unnest_tokens works

# We want one line per row
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# Put text in a data frame, one line per row
text_df <- data_frame(line = 1:4, text = text)
text_df

# what we want is one word per row. unnest_tokens breaks out words
text_df %>%
  unnest_tokens(word, text)

### Text Mining - Part 1 ###
# Load text. Use Les Miserables, by Victor Hugo - gutenberg library code 135
full_text <- gutenberg_download(135)

tidy_book <- full_text %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text)

tidy_book

# What are the most common words?
tidy_book %>%
  count(word, sort = TRUE)

# These words are not terribly useful for a typical analysis. These are called stop words and we 
# want to get rid of them.

# Stop words, default uses the snowball lexicon. Can try out using different languages (`language`)
# and different lexicons (`source`)
get_stopwords()

# Another try at most common words, use the 'smart' lexicon

tidy_book %>%
  anti_join(get_stopwords(source = "smart")) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip() # flips into a bar chart

# Word frequencies and counts alone can yield a lot of interesting information

### Sentiment Analysis ###
## Explore some Lexicons

# These libraries are built by computational linguists and are very credible. 'afinn' is scored from
# -5 to +5, increasing with positivity. 'bing' is scored in a binary fashion, 'nrc' has both negative
# and poistive words and can also be labeled in terms of affect, 'loughran' is a database built for
# use in a financial context. 
get_sentiments("afinn")

## Implement sentiment analysis using an `inner_join()`.

tidy_book %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE)

# What words contribute the most to sentiment scores for the chosen book?

tidy_book %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, n),
             n, 
             fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free")

# Notice that there are some words that are falsly identified as positive or negative

## What is a document about?

# Make a collection (corpus) of four books. The original four numbers are for four of Jane Austin's 
# novels: 1342, 158, 161, 141
# But I want to build my own collection 
#   98 - A tale of two cities; 
#   4300 - Ulysses, James Joyce; 
#   844 - The importnace of being earnest, Oscar Wilde
#   135 - Les Mis, Victor Hugo
#full_collection <- gutenberg_download(c(1342, 158, 161, 141),
#                                      meta_fields = "title")

full_collection <- gutenberg_download(c(98, 4300, 844, 135),
                                      meta_fields = "title")

# count the number of words per book
full_collection %>% count(title)

# Count the word frequencies in your collection, by title.
book_words <- full_collection %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

book_words

## Calculate tf-idf: term frequency - inverse document frequency. the weight is a statstical measure
## used to evaluate how important a word is to a document in a collection. The importance increases
## proportionally to the number of times a word appears in a document but is offset by the frequency
## of the word in the collection (corpus). 

## Calculate tf-idf.
book_tfidf <- book_words %>%
  bind_tf_idf(word, title, n)

book_tfidf

# Find *high* tf-idf words.
book_tfidf %>%
  arrange(-tf_idf)

# How can we visualize this? Let's go step-by-step.

book_tfidf %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, tf_idf),
             tf_idf, 
             fill = title)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~title, scales = "free")

## N-grams... and beyond!

# This introduces a bigram where n = 2. For trigrams replace n = 3
tidy_ngram <- full_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

tidy_ngram

# What are the most common bigrams?
tidy_ngram %>%
  count(bigram, sort = TRUE)

# Let's use `separate()` from tidyr to remove stop words
tidy_ngram %>%
  separate(bigram, c("word1", "word2", sep = " ")) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

# Note: you can use n-grams in a network analysis. This could be very interesting!

