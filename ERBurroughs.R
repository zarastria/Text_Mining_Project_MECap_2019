# load relavent packages

library(tidyverse)
library(tidytext)
library(gutenbergr)
library(stm)


# tokenize a robert Frost poem

text <- c("O hushed October morning mild,",
          "Thy leaves have ripened to the fall,",
          "To-morrow's wind, if it be wild,",	
          "Should waste them all.",
          "The crows above the forest call;",	        
          "To-morrow they may form and go.",
          "O hushed October morning mild,",
          "Begin the hours of this day slow,",	
          "Make the day seem to us less brief.",
          "Hearts not averse to being beguiled,",        
          "Beguile us in the way you know;",
          "Release one leaf at break of day;",
          "At noon release another leaf;",
          "One from our trees, one far away;",	
          "Retard the sun with gentle mist;",     
          "Enchant the land with amethyst.",
          "Slow, slow!",
          "For the grapes' sake, if they were all,",
          "Whose leaves already are burnt with frost,",
          "Whose clustered fruit must else be lost —",     
          "For the grapes' sake along the wall.")
text

text_df <- tibble(line = 1:21, text = text)
text_df

text_df %>%
  unnest_tokens(word, text)

##  Books by William Rice Burroughs - 
# "Warlord of Mars" (68), "The Beasts of Tarzan" (85), "The Son of Tarzan" (90), 
# "A Princess of Mars" (62), "The Return of Tarzan" (81), "Thuvia, Mid of Mars" (72), 
# "Tarzan and the Jewels of Opar" (92), "Tarzan of the Apes" (78), "The Gods of Mars" (64),
# "The Monster Men" (96), "The Moon Maid" (59752), "Jungle Tales of Tarzan" (106), 
# "The Mucker" (331), "The Oakdale Affair" (363), "Pellucidar" (605), "The Lost Continent" (149),
# "The Mad King" (364), "The Land That Time Forgot" (551), "Out of Time's Abyss" (553), 
# "At the Earth's Core" (123), "The People that Time Forgot" (552), "The outlaw of Torn" (369),
# "The Chessmen of Mars" (1153), "The Gods of Mars" (29405), "Tarzan the Terrible" (2020), 
# "Tarzan the Untamed" (1401), "Tarzan and the Golden Lion" (58874) "The efficiency Expert" (3475)


# Perform a basic count

wrburroughs <- gutenberg_download(c(68, 85, 90, 62, 81, 72, 92, 78, 64, 96, 59752, 106, 331, 363, 
                                    605, 149, 364, 551, 553, 123, 552, 369, 1153, 29405, 2020, 1401, 
                                    58874, 3475))

wrburroughs_short <- gutenberg_download(c(62, 78, 363, 3475))

tidy_wrb <- wrburroughs_short %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_wrb %>%
  count(word, sort = TRUE)

# Run an 

titles <- c("A Princess of Mars",
            "Tarzan of the Apes",
            "The Oakdale Affair",
            "The efficiency Expert")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

unique(books$title)

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, 
                                     regex("^chapter ", 
                                           ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter

# tokenize the book

word_counts <- by_chapter %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart")) %>%
  count(document, word, sort = TRUE) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup

word_counts

words_sparse <- word_counts %>%
  cast_sparse(document, word, n)

class(words_sparse)

topic_model <- stm(words_sparse, K = 4, 
                   init.type = "Spectral")

summary(topic_model)

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