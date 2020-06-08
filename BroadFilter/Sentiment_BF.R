#Sentiment Broad Filter
#Feb 27 2020
#Rob Wells, Ph.D., and Austin Wilkins
#University of Arkansas School of Journalism


rm(list=ls())

library(tidyverse)
library(tidytext)
library(XML)
library(janitor)
library(kableExtra)
library(lubridate)

#Import
BF <- rio::import("./BroadFilter_Jan 2020/BroadFilter2020.csv")

#---------------------------------------------------------------------------
# Sentiments
#---------------------------------------------------------------------------
#Tokenize the TEXT table)


reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
TEXT_token <- BF %>%
  filter(!str_detect(Text, '^"')) %>%
  #mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, Text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#Clean out ' character in word columm of TEXT_token
#example: 'growing
TEXT_token$word <- 
  gsub("'","", TEXT_token$word)

#remove spaces
TEXT_token$word <- 
  gsub(" ","", TEXT_token$word)


BF_YEAR <- TEXT_token

#save
write.csv(BF_YEAR, "./BroadFilter_Jan 2020/BF_YEAR_TOKENS.csv")


#TEXT_token <- select (TEXT_token, c(headline, article_nmbr, linenumber, word2))
#colnames(TEXT_token)[4] <- "word"

#Table of Common Words 
CommonTEXTWords <- TEXT_token %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  arrange(desc(n))


#save
write.csv(CommonTEXTWords, "./BroadFilter_Jan 2020/BF_Words_Total.csv")

#Table of Common Words by Publication
CommonTEXTWords2 <- TEXT_token %>%
  group_by(Pub) %>% 
  count(word) %>%
  filter(sum(n) >= 5) %>%
  arrange(desc(n))


#Table of Common Words
CommonTEXTWords <- TEXT_token %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup()
#------------------------
#------------------------

#install.packages("sentimentr")
#install.packages("tidytext")
#install.packages("textdata")
library(textdata)
library(sentimentr)
library(tidytext)

afinn <-get_sentiments("afinn")
bing <- get_sentiments("bing")

#------------------------
#Have to load NRC separately
#http://sentiment.nrc.ca/lexicons-for-research/
# nrc <- rio::import("/Users/robwells/Dropbox/Current_Projects/China Notes  Background/NRC-Sentiment-Emotion-Lexicons/NRC-Emotion-Lexicon-v0.92/Older Versions/NRC-Emotion-Lexicon-v0.92-InManyLanguages.xlsx")
# nrc <- janitor::clean_names(nrc)
# nrc <- nrc %>% 
#   select(english_word, positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust)
# colnames(nrc)[1] <- "word"
# write.csv(nrc, "nrc.csv")
# 
# nrc %>% 
#   count(positive, negative)
# 
# nrc1 <- rio::import("/Users/robwells/Dropbox/Current_Projects/China Notes  Background/NRC-Sentiment-Emotion-Lexicons/NRC-Emotion-Lexicon-v0.92/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")
# colnames(nrc1)[1:3] <- c("word", "sentiment", "score")
# write.csv(nrc1, "nrc1.csv")
# #
nrc1 <- rio::import("nrc1.csv")


nrc2 <- nrc1 %>% 
  filter(score >=1)

TEXT_sentiment <- CommonTEXTWords %>%
  inner_join(nrc1, by = "word") %>% 
  select(word, sentiment, score, n) %>% 
  arrange(desc(score))

TEXT_sentiment <- CommonTEXTWords %>%
  inner_join(nrc1, by = "word") %>% 
  select(word, sentiment, score, n) %>% 
  mutate(total_score = score * n) %>% 
  filter(score >=1)

write.csv(TEXT_sentiment, "./BroadFilter_Jan 2020/BF_sentiment_score.csv")


#create totalled sentiment: NRC
total_TEXTsentiment <- CommonTEXTWords %>%
  inner_join(nrc2, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  group_by(sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  adorn_totals("row")

colnames(total_TEXTsentiment)[1:2] <- c("Sentiment", "Total")

#Make html table with results
total_TEXTsentiment %>% 
  kable() %>%
  kable_styling("striped")
#Export


#create totalled sentiment: NRC w/ Publications
total_PUB_TEXTsentiment <- CommonTEXTWords2 %>%
  select(Pub, word, n) %>% 
  inner_join(nrc2, by = "word") %>%
  count(sentiment) %>%
  group_by(Pub, sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  adorn_totals("row")

#create totalled sentiment: bing
bing_TEXTsentiment <- CommonTEXTWords %>%
  inner_join(bing, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  group_by(sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  adorn_totals("row")

write.csv(total_TEXTsentiment, "./BroadFilter_Jan 2020/BF_total_text_sentiment_score.csv")


#create totalled sentiment by pub: bing
pub_bing_total_TEXTsent <- CommonTEXTWords2 %>%
  inner_join(bing, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  group_by(Pub, sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup()
write.csv(pub_total_TEXTsentiment, "./BroadFilter_Jan 2020/BF_pub_total_text_sentiment_score.csv")




#create totalled sentiment: NRC w/ Publications
total_PUB_TEXTsentiment <- CommonTEXTWords2 %>%
  select(Pub, word, n) %>% 
  inner_join(nrc2, by = "word") %>%
  count(sentiment) %>%
  group_by(Pub, sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  adorn_totals("row")
