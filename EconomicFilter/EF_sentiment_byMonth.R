#Sentiment Broad Filter
#Feb 27 2020
#Rob Wells, Ph.D., and Austin Wilkins
#University of Arkansas School of Journalism

# Sentiment by Month
# June 9, 2020
# This script is mostly a copy of EF_Sentiment.R, with edits to add montly analysis

rm(list=ls())

setwd("/home/austin/Documents/Projects/ChinaFDI")

library(tidyverse)
library(tidytext)
library(XML)
library(janitor)
library(kableExtra)
library(lubridate)
library(bit64)

#Import
EF <- rio::import("./EconomicFilter/Economicfilter2020.csv") %>% 
  separate(Date, c("Year","Month","Day"),"-",convert=TRUE) %>% 
  unite("Year-Month",Year:Month,remove=FALSE)



#---------------------------------------------------------------------------
# Sentiments
#---------------------------------------------------------------------------
#Tokenize the TEXT table)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
TEXT_token <- EF %>%
  filter(!str_detect(Text, '^"')) %>%
  #mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, Text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#remove apostrophe
TEXT_token$word <- 
  gsub("'","", TEXT_token$word)

#remove spaces
TEXT_token$word <- 
  gsub(" ","", TEXT_token$word)


#nest dataframe
by_month <- TEXT_token %>% 
  group_by(`Year-Month`) %>% 
  nest()


#create token lists and pub_token lists
#Table of Common Words by Publication
by_month <- by_month %>%
  mutate(pub_tokens = map(by_month$data,function(x) x %>%
                          group_by(Pub) %>% 
                          count(word) %>%
                          filter(sum(n) >= 5) %>%
                          arrange(desc(n))))

#Table of Common Words
by_month <- by_month %>%
  mutate(tokens = map(by_month$data,function(x) x %>% 
                        count(word) %>%
                        filter(sum(n) >= 5) %>%
                        ungroup()))
  

# names(by_month)
# by_month$pub_tokens[[1]]


#------------------------
#------------------------
library(textdata)
library(sentimentr)
library(tidytext)

nrc1 <- rio::import("nrc1.csv")


nrc2 <- nrc1 %>% 
  filter(score >=1)

by_month <- by_month %>% 
  mutate(monthly_sent = map(by_month$tokens,function(x) x %>%
        inner_join(nrc2, by = "word") %>%
        count(sentiment) %>%
        ungroup() %>%
        group_by(sentiment, n) %>%
        arrange(desc(n)) %>% 
        ungroup() %>% 
        adorn_totals("row")))


monthly_sent <- by_month %>% 
  unnest(monthly_sent)


monthly_sent <- monthly_sent %>% filter(sentiment == "positive" | sentiment == "negative")

write.csv(monthly_sent, "./EconomicFilter/EF_sentiment_score_byMonth.csv")




