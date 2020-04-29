##### Text Modeling - 2019 ####
### Author: Allen Baumgardner-Zuzik ###
### Date: 5/2019 - ###

### Guide: Julia Silge - tidy text mining ###
# Check Github: JuliaSilge
# bit.ly/silge-sdss-1
# bit.ly/silge-sdss-2
# Text Mining Book: tidytextmining.com 

### Perliminaries ###

# Install necessary Packages
# install.packages(c("stm", "glmnet", "yardstick", "broom))

# Load Global Libraries
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(stm)
library(glmnet)
library(yardstick)
library(broom)

### Topic Modeling ###
# Topic modeling is unsupervised macine learning
# we have a pile of documents in front of us, and we believe that there are latent topics of 
# interest in them. The UML will sift through and discover these topics. Each topic is a mixture 
# of words. Shreds the documents line by line and compiles into one large database. Then the machine
# sifts through each line and assigns a probability that theat line belongs to a discovered topic
###

## We want to tear apart and put the books back togethor using an unsupervised Machine Learning
## Algorithm.

## load titles by name, rather than ID number.
titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice", 
            "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

books

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

# Let's use topic modeling to see if we can put books back together
# As a first step, let's tokenize and tidy these chapters.

word_counts <- by_chapter %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart")) %>%
  count(document, word, sort = TRUE) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup

word_counts

# Next, let's **cast** to a sparse matrix.

words_sparse <- word_counts %>%
  cast_sparse(document, word, n)

class(words_sparse)

# Train a topic model: written in C++, main function is stm and is Silge's favorite package. STM is 
# an unsupervised machine learning model. K identifies K topics within the corpus.

topic_model <- stm(words_sparse, K = 4, 
                   init.type = "Spectral")

summary(topic_model)

## Explore the output of topic modeling

# The word-topic probabilities are called the "beta" matrix. This creates the probability that a 
# given word appears in a topic.

chapter_topics <- tidy(topic_model, matrix = "beta")

chapter_topics

# What are the highest probability words in each topic?

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
view(top_terms)

# Let's build a visualization.

top_terms %>%
  mutate(term = fct_reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# The document-topic probabilities are called "gamma". Looks at every document and assigns a 
# probability that a given topic was generated from a specific document

chapters_gamma <- tidy(topic_model, matrix = "gamma",
                       document_names = rownames(words_sparse))

chapters_gamma

## How well did we do in putting our books back together into the 4 topics?

chapters_parsed <- chapters_gamma %>%
  separate(document, c("title", "chapter"), 
           sep = "_", convert = TRUE)

chapters_parsed

# Let's visualization the results. This is  agraphical representation showing is that the algorithm 
# learned what book generates what topic

chapters_parsed %>%
  mutate(title = fct_reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

### Text Classification - Save for another time ###
# This is supervised machine learning
###

## Build a dataset for modeling

# Let's get two texts and build a model to distinguish between them. For this example, I chose "The
# War of the Worlds" and "A Princess of Mars"

titles <- c("The War of the Worlds",
            "A Princess of Mars")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title") %>%
  mutate(document = row_number())

books %>% count(title)

## By making the `document` column and using that as our modeling unit, we are splitting each book 
## up until its individual lines, as given to us by Project Gutenberg. Next, let's make a tidy, 
## tokenized dataset.

tidy_books <- books %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup

tidy_books

# And build:
#   - a sparse matrix with the features to use in modeling
#   - a dataframe with the **response** variable (i.e. title)

sparse_words <- tidy_books %>%
  count(document, word, sort = TRUE) %>%
  cast_sparse(document, word, n)

books_joined <- tibble(document = as.integer(rownames(sparse_words))) %>%
  left_join(books %>%
              select(document, title))

## Train a regularized regression model

is_mars <- books_joined$title == "A Princess of Mars"
model <- cv.glmnet(sparse_words, is_mars, 
                   family = "binomial", 
                   keep = TRUE)

# You can also check out the built-in `plot(model)` results from glmnet.

## Understand and evaluate the model, how does the glmnet model classify each document?









