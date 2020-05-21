#Aug 24, Building Table with % change
source("Functions.R")
#table comes from SentimentsScoring6-30-19.R
EF_type2 <- rio::import("./Sentiment Analysis/EconomicFilter_type copy.csv")

#Update 5-15-2020
EF_type2 <- rio::import("/Users/robwells/Dropbox/Current_Projects/China Notes  Background/Storage/China FDI pre-Jan 2020 code-data/Sentiment Analysis/EconomicFilter_type copy.csv")
#
#pct change by prior year in a stack
#https://stackoverflow.com/questions/48196552/calculate-percentage-change-in-r-using-dplyr
# z %>%
#  group_by(VERTICAL) %>% 
#  arrange(YEAR, .by_group = TRUE) %>%
#  mutate(pct_change = (Profit/lag(Profit) - 1) * 100)


#Mean Bing Score by Year & Pct Chg
Mean_Bing <- EF_type2 %>%
  filter(year!=2019) %>%
  group_by(year) %>% 
  summarize(mean = mean(score)) %>% 
  mutate(adj_mean = (mean)+10) %>% 
  mutate(pct_change = (adj_mean/lag(adj_mean)-1) * 100) 
write.csv(Mean_Bing, "Mean_Bing_8-24-19.csv")


  EF_type2 %>%
  filter(year!=2019) %>%
  group_by(year) %>% 
  summarize(mean = mean(score)) %>% 
  mutate(adj_mean = (mean)+10) %>% 
  mutate(pct_change = (adj_mean/lag(adj_mean)-1) * 100) %>% 
  ggplot(aes(x = year, y = pct_change, fill=pct_change, color=pct_change)) +
  geom_col(show.legend = FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Sentiment Percent Change: China Economic News", 
       subtitle = "Avg Bing Sentiment / Year: Economic Filter, 2000-2018.",
       caption = "4,046 articles. Source: ProQuest. Graphic by Rob Wells, 8-24-19",
       x="Year",
       y="Sentiment, Pct Change from Prior Year")

#Created Mean_Bing_Pct_Change 8-24-19.jpg
  
#Aug 25: Part 2 Normalize the % of change of negative discourse terms
  #using custom list of 169 trade terms curated by Wells and Zeng
  #full list is in Box, Negative-Postiive Key Terms on Trade 4-18-19.xlsx
  #trade_terms <- rio::import("./Sentiment Analysis/Sentiment analysis output/trade_sentiment_5-25.csv")
  #trade_terms columns are word and score
  
  
  EF_Custom_Sent_Score <- rio::import("./Sentiment Analysis/EF_Custom_Sent_Score.csv")
  
EF_Custom_Sent_Score <- rio::import("/Users/robwells/Dropbox/Current_Projects/China Notes  Background/Storage/China FDI pre-Jan 2020 code-data/Sentiment Analysis/EF_Custom_Sent_Score.csv")
  
  
#Mean Custom Terms Score by Year & Pct Chg
  Mean_Custom_Terms <- EF_Custom_Sent_Score %>%
    filter(year!=2019) %>%
    group_by(year) %>% 
    summarize(mean = mean(sentiment)) %>% 
    mutate(adj_mean = (mean)+10) %>% 
    mutate(pct_change = (adj_mean/lag(adj_mean)-1) * 100) 
  
write.csv(Mean_Custom_Terms, "Mean_Custom_Terms_8-25-19.csv")


EF_Custom_Sent_Score %>%
  filter(year!=2019) %>%
  group_by(year) %>% 
  summarize(mean = mean(sentiment)) %>% 
  mutate(adj_mean = (mean)+10) %>% 
  mutate(pct_change = (adj_mean/lag(adj_mean)-1) * 100) %>% 
ggplot(aes(x = year, y = pct_change, fill=pct_change, color=pct_change)) +
  geom_col(show.legend = FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Custom Sentiment, Pct Change: China Economic News", 
       subtitle = "Custom Sentiment, Pct Chg / Year: Economic Filter, 2000-2018.",
       caption = "4,046 articles. Source: ProQuest. Graphic by Rob Wells, 8-25-19",
       x="Year",
       y="Custom Sentiment, Pct Change from Prior Year")
#Created Custom_Sent_Pct_Change-8-25-19.jpg

#Blend the Bing and Custom Scores
Mean_Bing$type <- "Bing"
Mean_Custom_Terms$type <- "Custom"

Combined_Sentiment <- rbind(Mean_Bing, Mean_Custom_Terms)
write.csv(Combined_Sentiment, "Combined_Sentiment_8-25-19.csv")

Combined_Sentiment %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  ggplot(aes(x= year, y= pct_change, fill = type)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  #scale_y_reverse() +
  #coord_flip()  +
  labs(title = "Percent Change Per Year in Average Sentiment - China Economic News", 
       subtitle = "Bing, Custom Sentiment: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 8-25-19",
       x="Year",
       y="Pct Change in Average Sentiment Score / Year")  
#Created Pct_Chg_Sentiment_Bing_Custom-8-25-19.jpg


#-------------------------------------------------------------------------#
#Custom Sentiment list 7-5-19.R
#July 5, adapting script from May 25
#Aug 16, updating links
#
bing <- get_sentiments("bing")
#bing columns are word and sentiment
afinn <- get_sentiments("afinn")
#afinn columns are word and score
#importing custom list of 169 trade terms curated by Wells and Zeng
#full list is in Box, Negative-Postiive Key Terms on Trade 4-18-19.xlsx
trade_terms <- rio::import("./Sentiment Analysis/Sentiment analysis output/trade_sentiment_5-25.csv")
#trade_terms columns are word and score
#
library(sentimentr)
#
#Revised table, 7-6-19, following sentiment_score
#WSJ
#Create table with article index, by sentence, date field
WSJ_EF <- headline_date("./ExtractedTexts/EconomicFilter-WSJ.txt")
#colnames(WSJ_EF): [1] "headline"     "article_nmbr" "linenumber"   "text" "date"  
#Tokenize text by word. Score by sentiment  
custom_trade_score <- WSJ_EF %>% 
  make_token() %>% 
  inner_join(trade_terms) %>% 
  group_by(article_nmbr, date) %>% 
  summarise(sentiment = sum(rating)) %>% 
  arrange(sentiment) 

#Identifier column
WSJ_custom_trade_score <- custom_trade_score 
WSJ_custom_trade_score$pub <- "WSJ"
#NYT
#Create table with article index, by sentence, date field
NYT_EF <- headline_date("./ExtractedTexts/EconomicFilter-NYT.txt")
#colnames(WSJ_EF): [1] "headline"     "article_nmbr" "linenumber"   "text" "date"  
#Tokenize text by word. Score by sentiment  
custom_trade_score <- NYT_EF %>% 
  make_token() %>% 
  inner_join(trade_terms) %>% 
  group_by(article_nmbr, date) %>% 
  summarise(sentiment = sum(rating)) %>% 
  arrange(sentiment) 
#Identifier column
NYT_custom_trade_score <- custom_trade_score 
NYT_custom_trade_score$pub <- "NYT"
#LAT
#Create table with article index, by sentence, date field
LAT_EF <- headline_date("./ExtractedTexts/EconomicFilter-LAT.txt")
#colnames(WSJ_EF): [1] "headline"     "article_nmbr" "linenumber"   "text" "date"  
#Tokenize text by word. Score by sentiment  
custom_trade_score <- LAT_EF %>% 
  make_token() %>% 
  inner_join(trade_terms) %>% 
  group_by(article_nmbr, date) %>% 
  summarise(sentiment = sum(rating)) %>% 
  arrange(sentiment) 
#Identifier column
LAT_custom_trade_score <- custom_trade_score 
LAT_custom_trade_score$pub <- "LAT"
#WP
#Create table with article index, by sentence, date field
WP_EF <- headline_date("./ExtractedTexts/EconomicFilter-WP.txt")
#colnames(WSJ_EF): [1] "headline"     "article_nmbr" "linenumber"   "text" "date"  
#Tokenize text by word. Score by sentiment  
custom_trade_score <- WP_EF %>% 
  make_token() %>% 
  inner_join(trade_terms) %>% 
  group_by(article_nmbr, date) %>% 
  summarise(sentiment = sum(rating)) %>% 
  arrange(sentiment) 
#Identifier column
WP_custom_trade_score <- custom_trade_score 
WP_custom_trade_score$pub <- "WP"
#IUT
#Create table with article index, by sentence, date field
IUT_EF <- headline_date("./ExtractedTexts/EconomicFilter-IUT.txt")
#colnames(WSJ_EF): [1] "headline"     "article_nmbr" "linenumber"   "text" "date"  

#Tokenize text by word. Score by sentiment  
custom_trade_score <- IUT_EF %>% 
  make_token() %>% 
  inner_join(trade_terms) %>% 
  group_by(article_nmbr, date) %>% 
  summarise(sentiment = sum(rating)) %>% 
  arrange(sentiment) 

#Identifier column
IUT_custom_trade_score <- custom_trade_score 
IUT_custom_trade_score$pub <- "IUT"

#Make one bigass table
EF_Custom_Sent_Score <- rbind(WP_custom_trade_score, WSJ_custom_trade_score, NYT_custom_trade_score, LAT_custom_trade_score, IUT_custom_trade_score)
#Format for year: 
EF_Custom_Sent_Score$date <- ymd(EF_Custom_Sent_Score$date)
EF_Custom_Sent_Score$year  <- year(EF_Custom_Sent_Score$date)
#filter out 2019 - partial results
EF_Custom_Sent_Score <- EF_Custom_Sent_Score%>%filter(year <=2018)
#
colnames(EF_Custom_Sent_Score)[4] <- "Pub"
#Cement that sucker into a .csv
write.csv(EF_Custom_Sent_Score, "EF_Custom_Sent_Score.csv")
#--------------------------------------------------------------
#--------------------------------------------------------------------------------#
EF_Custom_Sent_Score <- rio::import("./Sentiment Analysis/EF_Custom_Sent_Score.csv")

#Joining to EF_type table
#EF_type comes from Extract News from Opinion, 7-3-19.R
EF_type <- rio::import("./Sentiment Analysis/EconomicFilter_type.csv")

#table is the product of all five publications with a specific News or Commentary attached to it
#this table is grouped by article and joined to EF_Sentiment
#
#Joining the opinion to the sentiment score 
EF_type3 <- EF_type %>% 
  select(article_nmbr, date, type, type_detail, date, Pub, headline) %>% 
  group_by(article_nmbr) %>% 
  distinct()
#Format date for EF_type3$date
EF_type3$date <- ymd(EF_type3$date)

#Creates df with 4,039 records vs 4,041. 
EF_new_type <- EF_Custom_Sent_Score %>% 
  inner_join(EF_type3, by=c("article_nmbr", "Pub", "date"))
#
EF_custom_type <- EF_new_type
write_csv(EF_custom_type, "EconomicFilter_custom_type.csv")
#--------------------------------------------------------------------------------#
#  Analysis  7-6-19
#
EF_custom_type <- rio::import("./Sentiment Analysis/EconomicFilter_custom_type.csv")

#--------------------------------------------------------------------------------#
#
ggplot(EF_custom_type)+
  aes(x = year, y = sentiment, fill = type) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Sentiment in News vs. Opinion: China Economic News", 
       subtitle = "Custom Sentiment Score: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment: Negative -> Positive")
#Created Overall EF News-Opin Sent 7-6-19
#
#plot sentiment over time
ggplot(EF_custom_type)+
  aes(x = year, y = sentiment, fill = Pub)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Special Trade Terms and Sentiment", 
       subtitle = "Postive, Negative Sentiment in Five Major News Orgs, 2000-2018",
       caption = "Source: ProQuest: WNYT, WSJ, WP, LAT, IUT. Sentiment = Custom List 169 Terms 
         Graphic by Rob Wells",
       x="Year",
       y="Sentiment: Negative -> Positive ")

#Created Overall Sentiment by Pub, Custom Terms, 7-6-19

#--------------------------------------------------------------------------------#
#Average Scores
#--------------------------------------------------------------------------------#
EF_custom_type %>%
  filter(year!=2019) %>%
  group_by(year, type, sentiment) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x = year, y = mean, fill=type, color=type)) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Average Sentiment in News vs. Opinion: China Economic News", 
       subtitle = "Custom Sentiment Score: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment: Negative -> Positive")

#Created Lines EF Custom News-Opin Sent 7-6-19
#
#Opinion only chart  
EF_custom_type %>%
  filter(type=="Opinion") %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x = year, y = mean, fill = mean)) +
  geom_col(show.legend = FALSE) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Averaged Sentiment Opinion Articles: China Economic News", 
       subtitle = "Custom Sentiment, Avg.: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment score")
#Created Sentiment-Opinion-Only Years 7-6-19

#Opinion only line chart  
EF_custom_type %>%
  filter(type=="Opinion") %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x = year, y = mean, fill = mean)) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Averaged Sentiment Opinion Articles: China Economic News", 
       subtitle = "Custom Sentiment, Avg.: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment score")
#Created Sentiment-Line Opinion-Only Years 7-6-19

#News only chart  
EF_custom_type %>%
  filter(type=="News") %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x = year, y = mean, fill = mean)) +
  geom_col(show.legend = FALSE) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Averaged Sentiment News Articles: China Economic News", 
       subtitle = "Custom Sentiment, Avg.: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment score")
#Created Sentiment-News-Only Years 7-6-19

#News only line chart  
EF_custom_type %>%
  filter(type=="News") %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x = year, y = mean, fill = mean)) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Averaged Sentiment News Articles: China Economic News", 
       subtitle = "Custom Sentiment, Avg.: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Sentiment score")
#Created Sentiment-Line-News-Only Years 7-6-19


# Mean score per year
EF_custom_type %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(sentiment))  %>% 
  ggplot(aes(x= year, y= mean, fill = type)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  #scale_y_reverse() +
  #coord_flip()  +
  labs(title = "Sentiment Editorial/Opinion Articles- China Economic News", 
       subtitle = "Custom Sentiment Score: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Year",
       y="Average Sentiment Score / Year")  
#Created EF News-Opin Avg Sent By Year 7-6-19

#Average score by publication, news
#aes(x = reorder(Category, -Count), y = Count)) +

EF_custom_type %>% 
  group_by(Pub, type) %>%
  filter(type=="News") %>% 
  summarize(mean = mean(sentiment)) %>% 
  ggplot(aes(x= reorder(Pub, mean), y= mean, fill= Pub)) +
  geom_col(show.legend = FALSE) +
  labs(title = "News, Sentiment by Publication: China Economic News", 
       subtitle = "Avg Sentiment in News: NYT, WSJ, LAT, WP, IUT",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Publication",
       y="Average Sentiment Score")  
#Created News Custom Sentiment by Pub 7-6-19


#Average score by publication, opinion
EF_custom_type %>% 
  group_by(Pub, type) %>%
  filter(type=="Opinion") %>% 
  summarize(mean = mean(sentiment)) %>% 
  ggplot(aes(x= reorder(Pub, mean), y= mean, fill= Pub)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Opinion, Sentiment by Publication: China Economic News", 
       subtitle = "Avg Sentiment in Opinion: NYT, WSJ, LAT, WP, IUT",
       caption = "4,040 articles. Source: ProQuest. Sentiment = Custom List 169 Terms. Graphic by Rob Wells, 7-6-19",
       x="Publication",
       y="Average Sentiment Score")  
#Created Opinion Custom Sentiment by Pub 7-6-19

