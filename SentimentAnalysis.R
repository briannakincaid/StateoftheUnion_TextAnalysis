library(knitr)
library(tidytext)
library(dplyr)
library(stringr)
library(readtext)
library(wordcloud)
library(ggplot2)
library(reshape2)

get_sentiments("bing")

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

####### TRUMP ########

trump <- readtext("trump first state of the union.txt")
trump <- trump %>% unnest_tokens(word,text)

trump_anger <- trump %>% 
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

trump_anticipation <- trump %>% 
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

trump_joy <- trump %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

trump_bing <- trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

trump_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

## TRUMP'S WORD CLOUD ##

trump %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

###### NIXON #########

nixon <- readtext("nixon first state of the union.txt")
nixon <- nixon %>% unnest_tokens(word,text)

nixon_anger <- nixon %>% 
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

nixon_anticipation <- nixon %>% 
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

nixon_joy <- nixon %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

nixon_bing <- nixon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nixon_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

## NIXON'S WORD CLOUD ##

nixon %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

nixon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
