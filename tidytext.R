# https://www.tidytextmining.com/tidytext.html
library(tidytext)

tidyBookSummaries <- bookSummaries %>% filter(!is.na(nameMaleness)) %>% unnest_tokens(word, plotSummary)

data("stop_words")

tidyBookSummaries %>% anti_join(stop_words)

tidyBookSummaries <- tidyBookSummaries %>% anti_join(stop_words)

tidyBookSummaries %>% count(word, sort = TRUE) 

library(ggplot2)

# words most used by writers with high Maleness
tidyBookSummaries %>% filter(nameMaleness >= 0.50) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# words most used by writers with low Maleness
tidyBookSummaries %>% filter(nameMaleness < 0.50) %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# Chapter 2
install.packages("textdata")
library(textdata)

get_sentiments("nrc")

nrc <- get_sentiments("nrc")

tidyBookSummarySentiments <- left_join(tidyBookSummaries,nrc, by="word") # this expands the dataset (multiple sentiments for one word)

names(tidyBookSummarySentiments) <- c("wikipediaArticleId","freebaseId","bookTitle","author","publicationDate",  
 "bookGenres","authorFirst","nameMaleness","summaryWordCount","word","nrcSentiment")

tidyBookSummarySentiments %>% select(wikipediaArticleId,nameMaleness,nrcSentiment) %>% unique()

sentimentsByAuthor <- tidyBookSummarySentiments %>% select(wikipediaArticleId,nameMaleness,nrcSentiment) %>% unique()

