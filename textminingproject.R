####################################################################################################
### Text Modeling - 2019
### Author: Allen Baumgardner-Zuzik
### Date: 2019/5
####################################################################################################

### Guide: Julia Silge - tidy text mining ###
# Check Github: JuliaSilge
# bit.ly/silge-sdss-1
# bit.ly/silge-sdss-2
# Text Mining Book: tidytextmining.com 

### Perliminaries ###

# Install necessary Packages
# install.packages(c("stm", "glmnet", "yardstick", "broom))

## Load Global Libraries
library(tidyverse)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(janeaustenr)

### Basics of Text Mining - Using the Tidytext Package ###

## This code shows the basic way unnest_tokens works

# Title: Excerpt from If- 
# Author: Rudyard Kipling
# Input the poem into the `text` variable
text <- c("If you can dream – and not make dreams your master;",
          "If you can think – and not make thoughts your aim;",
          "If you can meet with Triumph and Disaster",
          "And treat those two impostors just the same;",
          "If you can bear to hear the truth you’ve spoken",
          "Twisted by knaves to make a trap for fools,",
          "Or watch the things you gave your life to, broken,",
          "And stoop and build ’em up with worn-out tools:")
text


# Put text in a data frame, one line per row
text_df <- data_frame(line = 1:8, text = text)
text_df


# We want one word per row. The unnest_tokens function breaks out words
text_df %>%
  unnest_tokens(word, text)


### Sentiment Analyses ###


## What are a texts most common words?
# Load text. Use Les Miserables, by Victor Hugo - gutenberg library code 135
full_text <- gutenberg_download(135)

tidy_book <- full_text %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text)

tidy_book

# What are the most common words?
tidy_book %>%
  count(word, sort = TRUE) %>%
#  summarise(sum(n))
  top_n(20) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# This isn't terribly useful. Filter using stopword lexicons to get a better analysis
# stop_words combines stopwords from three lexicons or
# use get_stopwords() to filter by a specific lexicon
get_stopwords()
view(stop_words)
# Another try at most common words, use the 'smart' lexicon

tidy_book %>%
#  anti_join(get_stopwords(source = "smart")) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# Tidytext containts three general purpose sentiment lexicons - based on unigrams.
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

## tidy the data
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

## What are the most commonly used words in jane Austen's Novels"
#
# Total Words
#total_words <- tidy_books %>%
#  group_by(book) %>%
#  count(book, word, sort = TRUE) %>%
#  summarize(total = sum(n)) %>%
#  ungroup()
#
# Filtered Total Words
#book_words <- tidy_books %>%
#  anti_join(stop_words) %>%
#  count(book, word, sort = TRUE)
#
#total_words <- book_words %>%
#  group_by(book) %>%
#  summarize(total = sum(n))
#
#book_words <- left_join(book_words, total_words)
#
#ggplot(book_words, words, aes(n/total, fill = book)) +
#  geom_histogram(show.legend = FALSE) +
#  xlim(NA, 0.0009) +
#  facet_wrap(~book, ncol = 2, scales = "free_y")


## Code that finds colors with the given number of graphs!!
#library(scales)
#show_col(hue_pal()(4))

## build a word count using the "joy" sentiment
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col(fill = "#00BFC4") +
  expand_limits(y = c(0,400)) +
  coord_flip()

## build a word count using the "trust" sentiment
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col(fill = "#00BFC4") +
  expand_limits(y = c(0,400)) +
  coord_flip()


## Build a positive and negative word count for the book "Emma" 
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, n), n, fill = sentiment)) +
  geom_col() +
  expand_limits(y = c(0,600)) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free")

# "miss" is a misleading word. It is not necessarily negative but instead a title for an 
# unamrried woman. Let's add it as a custom stopword and recreate the count.
custom_stop_words <- tibble(word = c("miss"), lexicon = c("custom"))

# Now recount and replot
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, n), n, fill = sentiment)) +
  geom_col() +
  expand_limits(y = c(0,400)) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free")

## build a sentiment score

# Upload and create a new corpus of books
# Gutenebrg ID: 1400 - Great Expectations
# Gutenebrg ID: 98 - A Tale of Two Cities
# Gutenebrg ID: 1023 - Bleak House
# Gutenebrg ID: 24022 - A Christmas Carol
# Gutenebrg ID: 766 - David Copperfield
# Gutenebrg ID: 730 - Oliver Twist

charles_dickens_collection <- gutenberg_download(c(1400, 98, 1023, 24022, 766, 730),
                                      meta_fields = "title")
charles_dickens_collection %>% count(title)

# Tidy the corpus
tidy_dickens <- charles_dickens_collection %>%
  group_by(title) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

charles_dickens_sentiment <- tidy_dickens %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(charles_dickens_sentiment, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

## Which sentiment lexicon works best? The one that is most appropriate for your purpose!

# AFINN assigns words with a score that runs between -5 and 5, with negative scores indicating 
# negative sentiment and positive scores indicating positive sentiment
# bing categorizes words in a binary fashion into positive and negative categories
# nrc categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, 
# anger, anticipation, disgust, fear, joy, sadness, surprise, and trust

# Compare Sentiment Scores of " A Tale of Two Cities" using AFINN, bing, and nrc 
two_cities <- tidy_dickens %>%
  filter(title == "A Tale of Two Cities")

two_cities

# NOTE: AFINN lexicon measures sentiment with a numeric score between -5 and 5, while the other two 
# lexicons categorize words in a binary fashion, either positive or negative. To find a sentiment 
# score in chunks of text throughout the novel, we will need to use a different pattern for the 
# AFINN lexicon than for the other two.

afinn <- two_cities %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN") 

bing_and_nrc <- bind_rows(two_cities %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."), two_cities %>%
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Calculate the tf-idf for the Charles Dickens Corpus
tidy_dickens
unique(tidy_dickens$title)

book_words <- tidy_dickens %>%
    count(title, word, sort = TRUE)

book_words

## Calculate tf-idf: term frequency - inverse document frequency. the weight is a statstical measure
## used to evaluate how important a word is to a document in a collection. The importance increases
## proportionally to the number of times a word appears in a document but is offset by the frequency
## of the word in the collection (corpus). 

## Calculate tf-idf and arrange by "high" tf-idf words.
book_tfidf <- book_words %>%
  bind_tf_idf(word, title, n) %>%
  arrange(-tf_idf)

book_tfidf

# visulaize tf-dif by book.

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


## Build a bigram to find the most famous streets in Jane Austen's corpus

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

# filter out stop words and perform another count
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Perform a count
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# most common "streets" in each book
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

## Sex perceptions in Jane Austen's Corpus

austen_bigrams %>%
  count(bigram, sort = TRUE)
  
austen_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

austen_she <- austen_separated %>%
  filter(word1 == "she") %>%
  count(book, word2) %>%
  rename(word = word2, she_n = n) %>%
  select(-book) %>%
  group_by(word) %>%
  summarize(she_n = sum(she_n)) %>%
  filter(she_n > 1) %>%
  arrange(desc(she_n))

view(austen_she)

austen_he <- austen_separated %>%
  filter(word1 == "he") %>%
  count(book, word2) %>%
  rename(word = word2, he_n = n) %>%
  select(-book) %>%
  group_by(word) %>%
  summarize(he_n = sum(he_n)) %>%
  filter(he_n > 1) %>%
  arrange(desc(he_n))

view(austen_he)

# Computes log2 ratios, better computation but thinking in logs is less intuitive

#gender_combine <- inner_join(austen_she, austen_he) %>%
#  anti_join(stop_words)  %>%
#  mutate(verb_ratio = log2(she_n / he_n)) %>%
#  arrange(desc(verb_ratio))

# computes using ratios, works but feels hinky
gender_combine <- inner_join(austen_she, austen_he) %>%
  anti_join(stop_words)  %>%
  mutate(verb_ratio_she = she_n / he_n, verb_ratio_he = -1*(he_n / she_n)) %>%
  mutate(verb_ratio_she = replace(verb_ratio_she, verb_ratio_she < 1, 0)) %>%
  mutate(verb_ratio_he = replace(verb_ratio_he, verb_ratio_he > -1, 0)) %>%
  mutate(verb_ratio = verb_ratio_she + verb_ratio_he) %>%
  arrange(desc(verb_ratio))

view(gender_combine)

# combine the top 15 and the lower 15 gendered verbs into a single dataframe
gender_perception <- rbind(head(gender_combine, 15), tail(gender_combine, 15))

view(gender_perception)

gender_perception %>%
  ggplot(aes(fct_reorder(word, verb_ratio), verb_ratio)) +
    geom_col(show.legend =FALSE) +
    coord_flip()

## Sex perceptions in Charles Dickens' Corpus

charles_dickens_collection <- gutenberg_download(c(1400, 98, 1023, 24022, 766, 730),
                                                 meta_fields = "title")
charles_dickens_collection %>% count(title)

# Tidy the corpus
dickens_bigrams <- charles_dickens_collection %>%
  group_by(title) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(-gutenberg_id, -linenumber, -chapter)%>%
  rename(book = title)

dickens_bigrams %>%
  count(bigram, sort = TRUE)

dickens_separated <- dickens_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dickens_she <- dickens_separated %>%
  filter(word1 == "she") %>%
  count(book, word2) %>%
  rename(word = word2, she_n = n) %>%
  select(-book) %>%
  group_by(word) %>%
  summarize(she_n = sum(she_n)) %>%
  filter(she_n > 1) %>%
  arrange(desc(she_n))

view(dickens_she)

dickens_he <- dickens_separated %>%
  filter(word1 == "he") %>%
  count(book, word2) %>%
  rename(word = word2, he_n = n) %>%
  select(-book) %>%
  group_by(word) %>%
  summarize(he_n = sum(he_n)) %>%
  filter(he_n > 1) %>%
  arrange(desc(he_n))

view(dickens_he)

# Computes log2 ratios, better computation but thinking in logs is less intuitive
#gender_combine <- inner_join(austen_she, austen_he) %>%
#  anti_join(stop_words)  %>%
#  mutate(verb_ratio = log2(she_n / he_n)) %>%
#  arrange(desc(verb_ratio))

# computes using ratios, works but feels hinky
gender_combine <- inner_join(dickens_she, dickens_he) %>%
  anti_join(stop_words)  %>%
  mutate(verb_ratio_she = she_n / he_n, verb_ratio_he = -1*(he_n / she_n)) %>%
  mutate(verb_ratio_she = replace(verb_ratio_she, verb_ratio_she < 1, 0)) %>%
  mutate(verb_ratio_he = replace(verb_ratio_he, verb_ratio_he > -1, 0)) %>%
  mutate(verb_ratio = verb_ratio_she + verb_ratio_he) %>%
  arrange(desc(verb_ratio))

view(gender_combine)

# combine the top 15 and the lower 15 gendered verbs into a single dataframe
gender_perception <- rbind(head(gender_combine, 15), tail(gender_combine, 15))

view(gender_perception)

gender_perception %>%
  ggplot(aes(fct_reorder(word, verb_ratio), verb_ratio)) +
  geom_col(show.legend =FALSE) +
  coord_flip()







### Topic Modeling ###
# Topic modeling is unsupervised macine learning
# we have a pile of documents in front of us, and we believe that there are latent topics of 
# interest in them. The UML will sift through and discover these topics. Each topic is a mixture 
# of words. Shreds the documents line by line and compiles into one large database. Then the machine
# sifts through each line and assigns a probability that theat line belongs to a discovered topic
###

## Load Libraries
library(tidyverse)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(stm)
library(glmnet)
library(yardstick)
library(broom)


## We want to tear apart and put the books back togethor using an unsupervised Machine Learning
## Algorithm.

## load titles by name, rather than ID number.
#titles <- c("A Tale of Two Cities", 
#            "The War of the Worlds",
#            "Alice's Adventures in Wonderland", 
#            "Great Expectations")
#The Picture of Dorian Gray

# Only books with chapter formatting work with this code. The following books work:
# "The Adventures of Tom Sawyer"
# "Great Expectations"
# "The War of the Worlds"
# "The Hound of the Baskervilles"

titles <- c("Emma",                              # Jane Austen
            "The Picture of Dorian Gray",        # Oscar Wilde
            "Alice's Adventures in Wonderland",  # Lewis Carroll
            "The War of the Worlds",             # H. G. Wells
            "Jane Eyre: An Autobiography",       # Charlotte Bronte
            "The Hound of the Baskervilles"      # Authur Conan Doyle
)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

books
unique(books$title)

# I've TORN THE BOOKS APART!!!

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, 
                                     regex("^chapter ", 
                                           ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter
unique(by_chapter$document)

# Let's use topic modeling to see if we can put books back together
# As a first step, let's tokenize and tidy these chapters.

word_counts <- by_chapter %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart")) %>%
  count(document, word, sort = TRUE) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

word_counts

# Next, let's cast to a sparse matrix.

words_sparse <- word_counts %>%
  cast_sparse(document, word, n)

class(words_sparse)

# Train a topic model: written in C++, main function is stm and is Silge's favorite package. STM is 
# an unsupervised machine learning model. K identifies K topics within the corpus.

topic_model <- stm(words_sparse, K = 6, 
                   init.type = "Spectral")

summary(topic_model)

## Explore the output of topic modeling

# The word-topic probabilities are called the "beta" matrix. This creates the probability that a 
# given word appears in a topic.

chapter_topics <- tidy(topic_model, matrix = "beta")

head(chapter_topics,18)

# What are the highest probability words in each topic?

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#top_terms
#view(top_terms)

# Let's build a visualization.

top_terms %>%
  mutate(term = fct_reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free")


# The document-topic probabilities are called "gamma". Looks at every document and assigns a 
# probability that a given topic was generated from a specific document

chapters_gamma <- tidy(topic_model, matrix = "gamma",
                       document_names = rownames(words_sparse))

unique(chapters_gamma$document)

## How well did we do in putting our books back together into the 4 topics?

chapters_parsed <- chapters_gamma %>%
  separate(document, c("title", "chapter"), 
           sep = "_", convert = TRUE)

chapters_parsed

# Visualize the results. This is a graphical representation showing that the algorithm 
# learned what book generates what topic

chapters_parsed %>%
  mutate(title = fct_reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

