library(knitr)
library(tidytext)
library(dplyr)
library(stringr)
library(readtext)
library(wordcloud)

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

## WORD CLOUD ##

trump_joy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

trump_anger %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

trump_anticipation %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


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

## WORD CLOUD ##

nixon_joy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

nixon_anger %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

nixon_anticipation %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

