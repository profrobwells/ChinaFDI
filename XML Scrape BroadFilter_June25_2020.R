#BroadFilter Tables Feb 16 2020
#Austin edit of XML scraper

library(tidyverse)
library(tidytext)
library(XML)
library(janitor)


#This function works with a single file
myxmlscrape <- function(file){
  file <- xmlParse(file)
  a <- setNames(xmlToDataFrame(node=getNodeSet(file,"//Obj/NumericDate")),"Date")
  b <- setNames(xmlToDataFrame(node=getNodeSet(file,"//Obj/TitleAtt/Title")),"Title")
  c <- setNames(xmlToDataFrame(node=getNodeSet(file,"//GOID")),"UniqueID")
  d <- setNames(xmlToDataFrame(node=getNodeSet(file,"//DFS/PubFrosting/Title")),"Pub")
  e <- setNames(xmlToDataFrame(node=getNodeSet(file,"//Obj/ObjectTypes/mstar")),"Type")
  #f <- setNames(xmlToDataFrame(node=getNodeSet(file,"//Obj/Contributors/Contributor/Author/NormalizedDisplayForm")),"Author")
  text <- setNames(xmlToDataFrame(node=getNodeSet(file,"//TextInfo/Text")),"Text")
  #Combine in df
  articles <- cbind(a,b,c,d,e,text)
}

#-----------------------------------
#Ran this on the full BroadFilter
all_texts <- list.files("./EconomicFilter/EconomicFilter",full.names = TRUE)
test <- lapply(all_texts,myxmlscrape)
test2 <- list()
for (i in test){
  test2 <- bind_rows(test2,i)
}


#-------
#Examine a specific cell
test2[1, 6]

BF[1, 6]

#Clean out html tags
test2$Text <- gsub("<html>", "",test2$Text)
test2$Text <- gsub("<head", "",test2$Text)
test2$Text <- gsub("<title>", "",test2$Text)
test2$Text <- gsub("<body>", "",test2$Text)
test2$Text <- gsub("</head", "",test2$Text)
test2$Text <- gsub("<title/>", "",test2$Text)
test2$Text <- gsub("</body>", "",test2$Text)
test2$Text <- gsub("</html>", "",test2$Text)


#remove the URL and meta name
test2$Text <- 
  gsub("<meta name=\"ValidationSchema\" content=\"http[^[:space:]]*", "",test2$Text)
test2$Text <- 
  gsub("<meta name='ValidationSchema' content=\'http[^[:space:]]*", "",test2$Text)

#Delete Columns
#test2 <- test2[ -c(6) ]

#Put cleaned df into a csv. file
write.csv(test2,"./BroadFilter_Jan 2020/BroadFilter6_25_2020.csv")
#
BF <- test2

#Import
BF <- rio::import("./Data/BroadFilter2020.csv")

#save
write.csv(CommonTEXTWords, "./Data/TidyTexts/BroadFilterTotal.csv")
#write.csv(CommonTEXTWords,paste0("TidyTexts/CommonWords_",textfile,".csv"))

#--------------------------------
# Frequency Analysis
#--------------------------------


#Table of articles by publication
total <- BF %>%
  group_by(Pub) %>% 
  mutate(total_articles = n()) %>% 
  ungroup() %>% 
  distinct(Pub, total_articles) %>% 
  arrange(desc(total_articles)) %>% 
  adorn_totals("row")

#library(janitor)
#total %>%
#  adorn_totals("row")

colnames(total)[1:2] <- c("Publication", "Total Articles")

install.packages("kableExtra")
library(kableExtra)
# This makes kables
total %>% 
  kable() %>%
  kable_styling("striped")
#Export


#------------------
BF2 <- BF
library(lubridate)

BF$datex <- BF$Date

BF <- BF %>%
  mutate(datex = ymd(Date)) %>%
  mutate(yearmon= format(datex, "%Y-%m")) %>% 
  mutate(year= format(datex, "%Y"))

#Count articles by Year
BroadFilter_year <- BF %>%
  select(Pub, datex, year) %>%   
  group_by(Pub) %>%
  count(year)
#double check
sum(BroadFilter_year$n)

#Stacked Bar Chart
ggplot(BroadFilter_year, aes(x = year, y = n, fill=Pub))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(fill = "Publication Name") +
  labs(title = "U.S.-China Broad Filter News - Articles by Year", 
       caption = "Source: ProQuest - BroadFilter Search. 5,204 articles
       Graphic by Rob Wells. 2-16-2020",
       x="Year",
       y="Number of Articles") 




#Created BroadFilter ArticlesYr 2-16-2020

#------------------------------
#sentiments
#------------------------------
#----------------------------------------------------------------
#Tokenize the TEXT table)
#----------------------------------------------------------------

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

#TEXT_token <- select (TEXT_token, c(headline, article_nmbr, linenumber, word2))
#colnames(TEXT_token)[4] <- "word"

#Table of Common Words 
CommonTEXTWords <- TEXT_token %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  arrange(desc(n))

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

write.csv(TEXT_sentiment, "./Sentiment Analysis/BF_text_sentiment_score.csv")


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

#install.packages("kableExtra")
library(kableExtra)
# This makes kables
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

write.csv(total_TEXTsentiment, "./Sentiment Analysis/BF_total_text_sentiment_score.csv")


#create totalled sentiment by pub: bing
pub_bing_total_TEXTsent <- CommonTEXTWords2 %>%
  inner_join(bing, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  group_by(Pub, sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup()
write.csv(pub_total_TEXTsentiment, "./Sentiment Analysis/BF_pub_total_text_sentiment_score.csv")




#create totalled sentiment: NRC w/ Publications
total_PUB_TEXTsentiment <- CommonTEXTWords2 %>%
  select(Pub, word, n) %>% 
  inner_join(nrc2, by = "word") %>%
  count(sentiment) %>%
  group_by(Pub, sentiment, n) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  adorn_totals("row")

#--------------------
#stopped Feb 15 2020
#--------------------




#--------------------------------

#Narrative Analysis - Newspapers 
#March 25 2019#
#AW
#Wells edited 7-11-19

rm(list=ls())


library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(kableExtra)
library(knitr)
install.packages("rio")

#------------------------------------------------------------------#
# STEP #1: Build Single Table With All Newspapers and Select MetaData
#   For EconomicFilter
#------------------------------------------------------------------#
#
#EconomicFilter MetaData
#
#Title
#pubdate
#pubtitle
#pages
#documentType
#companies
#


#Process Tweets Lubidate - New YearMon Column
library(lubridate)

EconomicFilter <- EconomicFilter %>%
  mutate(pubdate = mdy (pubdate)) %>%
  mutate(yearmon= format(pubdate, "%Y-%m")) %>% 
  mutate(year= format(pubdate, "%Y"))

#fixes ChinaFDI-NYT data
EconomicFilter <- EconomicFilter %>%
  filter(year > 1999)



#Count articles by Year
EconomicFilter_year <- EconomicFilter %>%
  filter(year!=2019) %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(year)

#Count articles by Publication
EconomicFilter_pub <- EconomicFilter %>%
  filter(year!=2019) %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(pubtitle)


#SAVE WORK
write.csv(EconomicFilter,"EconomicFilter.csv")
write.csv(EconomicFilter_pages,"EconomicFilter-pages.csv")
write.csv(EconomicFilter_year,"EconomicFilter-year.csv")
write.csv(EconomicFilter_pub,"EconomicFilter_pub.csv")
#------------------------------------------------------------------#
# STEP #3: Graphics of Articles
#------------------------------------------------------------------#
library(ggplot2)

#Stacked Bar Chart
ggplot(EconomicFilter_year, aes(x = year, y = n, fill=pubtitle))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(fill = "Publication Name") +
  labs(title = "U.S.-China Economic News - Articles by Year", 
       caption = "Source: ProQuest - Economic Filter Search. 4,249 articles
       Graphic by Rob Wells. 7-11-19",
       x="Year",
       y="Number of Articles") 

ggsave("./Output Data-Images/Bar_ChinaFDI.pdf",device = "pdf",width=9,height=6)

#Line Chart
ggplot(EconomicFilter_year, aes(x = year, y = n, group = pubtitle, color = pubtitle))  +
  geom_line() +
  labs(title = "China Foreign Direct Investment - Articles by Year", 
       subtitle = "subtitle",
       caption = "Source: ProQuest - China Foreign Direct Investment Search
       Graphic by Rob Wells",
       x="Year",
       y="Number of Articles") 
ggsave("./Output Data-Images/Line_ChinaFDI.pdf",device = "pdf",width=9,height=6)


#------------------------------------------------------------------#
#   Repeat Steps 1-3 for: 
#   ChinaFDI
#   Economic Filter
#   
#------------------------------------------------------------------#
# ChinaFDI
#------------------------------------------------------------------#
# STEP #1: Build Single Table With All Newspapers and Select MetaData
#   For ChinaFDI
#------------------------------------------------------------------#
#
#Load the Data

ChinaFDINYT <- rio::import("./Metadata/ChinaFDI_NYT.xls")
ChinaFDIWP <- rio::import("./Metadata/ChinaFDI_WP.xls")
ChinaFDIWSJ <- rio::import("./Metadata/ChinaFDI_WSJ.xls")
ChinaFDILAT <- rio::import("./Metadata/ChinaFDI_LAT.xls")
ChinaFDIIUT <- rio::import("./Metadata/ChinaFDI_IUT.xls")
#
#Append and combine tables
#
library(dplyr)
#Make all tables consistent
ChinaFDINYT <- select(ChinaFDINYT, Title, pubdate, pubtitle, pages,
                            documentType, companies)

ChinaFDIWP <- select(ChinaFDIWP, Title, pubdate, pubtitle, pages,
                           documentType, companies)

ChinaFDIWSJ <- select(ChinaFDIWSJ, Title, pubdate, pubtitle, pages,
                            documentType, companies)

ChinaFDILAT <- select(ChinaFDILAT, Title, pubdate, pubtitle, pages,
                            documentType, companies)

#pages not found for IUT and IUCT - 
#won't combine in first cut but will redo the larger table without pages

ChinaFDIIUT <- select(ChinaFDIIUT, Title, pubdate, pubtitle,
                            documentType, companies)


#Combine dataframes - without IUT and IUCT - using rbind
ChinaFDI_pages <- rbind(ChinaFDINYT, ChinaFDIWP, 
                              ChinaFDIWSJ, ChinaFDILAT)

#Combine with IUT and IUCT - no pages
ChinaFDI <- ChinaFDI_pages[ -c(4) ]
ChinaFDI <- rbind(ChinaFDIIUT, ChinaFDI)


#------------------------------------------------------------------#
# STEP #2: Process and Analyze Articles
#------------------------------------------------------------------#


#Process Tweets Lubidate - New YearMon Column
library(lubridate)

ChinaFDI <- ChinaFDI %>%
  mutate(pubdate = mdy (pubdate)) %>%
  mutate(yearmon= format(pubdate, "%Y-%m")) %>% 
  mutate(year= format(pubdate, "%Y"))

#fixes ChinaFDI-NYT data
ChinaFDI <- ChinaFDI %>%
  filter(year > 1999)



#Count articles by Year
ChinaFDI_year <- ChinaFDI %>%
  filter(year!=2019) %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(year)

#Count articles by Publication
ChinaFDI_pub <- ChinaFDI %>%
  filter(year!=2019) %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(pubtitle)


#SAVE WORK
write.csv(ChinaFDI,"ChinaFDI.csv")
write.csv(ChinaFDI_pages,"ChinaFDI-pages.csv")
write.csv(ChinaFDI_year,"ChinaFDI-year.csv")
write.csv(ChinaFDI_pub,"ChinaFDI-pub.csv")
#------------------------------------------------------------------#
# STEP #3: Graphics of Articles ChinaFDI
#------------------------------------------------------------------#
library(ggplot2)

#Stacked Bar Chart
ggplot(ChinaFDI_year, aes(x = year, y = n, fill=pubtitle))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(fill = "Publication Name") +
  labs(title = "U.S.-China FDI News - Articles by Year", 
       caption = "Source: ProQuest - ChinaFDI Search. 375 articles
       Graphic by Rob Wells. 7-11-19",
       x="Year",
       y="Number of Articles") 

ggsave("./Output Data-Images/Bar_ChinaFDI.pdf",device = "pdf",width=9,height=6)

#Line Chart
ggplot(EconomicFilter_year, aes(x = year, y = n, group = pubtitle, color = pubtitle))  +
  geom_line() +
  labs(title = "China Foreign Direct Investment - Articles by Year", 
       subtitle = "subtitle",
       caption = "Source: ProQuest - China Foreign Direct Investment Search
       Graphic by Rob Wells",
       x="Year",
       y="Number of Articles") 
ggsave("./Output Data-Images/Line_ChinaFDI.pdf",device = "pdf",width=9,height=6)

#------------------------------------------------------------------#
# BroadFilter
#Awaiting fixed metadata from IUT - 7-11-19. Existing sheet missing 900+ records
#------------------------------------------------------------------#
# STEP #1: Build Single Table With All Newspapers and Select MetaData
#   For BroadFilter
#------------------------------------------------------------------#
#
#Load the Data - Updated 7-27-19

BroadFilterNYT <- rio::import("./Metadata/BroadFilter-NYT.csv")
BroadFilterWP <- rio::import("./Metadata/BroadFilter-WP.csv")
BroadFilterWSJ <- rio::import("./Metadata/BroadFilter-WSJ.csv")
BroadFilterLAT <- rio::import("./Metadata/BroadFilter-LAT.csv")
BroadFilterIUT <- rio::import("./Metadata/BroadFilter-IUT.csv")
#
#Append and combine tables
#
library(dplyr)
#Make all tables consistent
BroadFilterNYT <- select(BroadFilterNYT, Title, pubdate, pubtitle, pages,
                      documentType, companies)

#BroadFilterNYT$pubdate <- as.character(BroadFilterNYT$pubdate)

BroadFilterWP <- select(BroadFilterWP, Title, pubdate, pubtitle, pages,
                     documentType, companies)

BroadFilterWSJ <- select(BroadFilterWSJ, Title, pubdate, pubtitle, pages,
                      documentType, companies)

BroadFilterLAT <- select(BroadFilterLAT, Title, pubdate, pubtitle, pages,
                      documentType, companies)

#pages not found for IUT and IUCT - 
#won't combine in first cut but will redo the larger table without pages

BroadFilterIUT <- select(BroadFilterIUT, Title, pubdate, pubtitle,
                      documentType, companies)



#Combine dataframes - without IUT - using rbind
BroadFilter_pages <- rbind(BroadFilterNYT, BroadFilterWP, BroadFilterWSJ, BroadFilterLAT)

#Combine with IUT and IUCT - no pages
BroadFilter <- BroadFilter_pages[ -c(4) ]
BroadFilter <- rbind(BroadFilterIUT, BroadFilter)


#------------------------------------------------------------------#
# STEP #2: Process and Analyze Articles
#------------------------------------------------------------------#


#Process Tweets Lubidate - New YearMon Column
library(lubridate)

#fix the mixed bag of date formats
#https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time

#parse_date_time(x = df$date,
 #               orders = c("d m y", "d B Y", "m/d/y"),
  #              locale = "eng")


BroadFilter$date <- parse_date_time(BroadFilter$pubdate,
                  orders = c("d m y", "d B Y", "m/d/y", "y m d", "y-m-d"))

BroadFilter1 <- BroadFilter %>%
  mutate(year = format(date, "%Y"))

BroadFilter <- BroadFilter1

#fixes BroadFilter-NYT data
#BroadFilter <- BroadFilter %>%
#  filter(year > 1999)

#Count articles by Year
BroadFilter_year <- BroadFilter %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(year)

#Count articles by Publication
BroadFilter_pub <- BroadFilter %>%
  select(pubtitle, pubdate, year) %>%   
  group_by(pubtitle) %>%
  count(pubtitle)


#SAVE WORK
write.csv(BroadFilter,"BroadFilter.csv")
write.csv(BroadFilter_pages,"BroadFilter-pages.csv")
write.csv(BroadFilter_year,"BroadFilter-year.csv")
write.csv(BroadFilter_pub,"BroadFilter-pub.csv")
#------------------------------------------------------------------#
# STEP #3: Graphics of Articles BroadFilter
#------------------------------------------------------------------#
library(ggplot2)

#Stacked Bar Chart
ggplot(BroadFilter_year, aes(x = year, y = n, fill=pubtitle))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(fill = "Publication Name") +
  labs(title = "U.S.-China Broad Filter News - Articles by Year", 
       caption = "Source: ProQuest - BroadFilter Search. 2,424 articles
       Graphic by Rob Wells. 7-27-19",
       x="Year",
       y="Number of Articles") 
#Created BroadFilter ArticlesYr 7-27-19

ggsave("./Output Data-Images/Bar_BroadFilter.pdf",device = "pdf",width=9,height=6)

#Line Chart
ggplot(EconomicFilter_year, aes(x = year, y = n, group = pubtitle, color = pubtitle))  +
  geom_line() +
  labs(title = "China Foreign Direct Investment - Articles by Year", 
       subtitle = "subtitle",
       caption = "Source: ProQuest - China Foreign Direct Investment Search
       Graphic by Rob Wells",
       x="Year",
       y="Number of Articles") 
ggsave("./Output Data-Images/Line_BroadFilter.pdf",device = "pdf",width=9,height=6)
