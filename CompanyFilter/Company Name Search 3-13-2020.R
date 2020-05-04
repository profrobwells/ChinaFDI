#SEARCHING FOR COMPANIES Using Quanteda
#Rob Wells, Ph.D.
#Austin Wilkins
#University of Arkansas
#March 19, 2020 
#(updates July 18, 2019 - Based on Word String Search 6-14-19.R, Peter Navarro search)

library(tidyverse)
library(quanteda)
library(lubridate)
source("Functions.R")

#-----------------------------------------------------------------------------#
#Constructed table of negative terms, extracted table
#-----------------------------------------------------------------------------#
#simplify the names
companies1 <- c("HNA Group",	
               "Dalian Wanda",	
               "Anbang Insurance",	
               "China Investment",	
               "WH Group",	
               "Lenovo Group",	
               "Qingdao Haier",	
               "Shanghai Greenland",	
               "China Life Insurance",	
               "Fosun International",	
               "China Petrochemical",
               "Sinopec",	
               "Apex Technology",
               "Legend Capital", 
               "PAG Asia",
               "CNOOC",
               "SMI USA",	
               "Hangzhou Liaison",	
               "SAFE",	
               "China Oceanwide",	
               "Hainan Traffic Control",	
               "Shandong Tralin Paper")

#-----------------------------------------------------------------------------#
#Import data scraped from XML files
#-----------------------------------------------------------------------------#

cos <- rio::import("https://raw.githubusercontent.com/profrobwells/ChinaFDI/master/CompanyFilter/Companyfilter_FINAL_2020.csv")

#-----------------------------------------------------------------------------#
# Quanteda for phrase searching
#-----------------------------------------------------------------------------#

#install.packages("quanteda")
#library(quanteda)
#
text <- cos$Text  
text <- gsub("'s", "", text)
text <- as.character(text)    
toks <- tokens(text)
results <- (kwic(toks, pattern = phrase(companies1))) 
results1 <- as.data.frame(results) 
#
#Clean docname
results1$V1 <- gsub("text", "", results1$docname)
results1$V1 <- as.numeric(results1$V1)
#
#Rejoin with main document

token_cos <- cos %>% 
  inner_join(results1, by="V1")


#Table with each individual hit of a negative phrase by year
companies_toks <- token_cos %>% 
  select(UniqueID, Date, pattern, Pub, Title, pre, post)
#dates
library(lubridate)
companies_toks$Date <- ymd(companies_toks$Date)
#Create year field
companies_toks$year <- year(companies_toks$Date)


#Export output this file to a CSV 
write.csv(companies_toks,"./CompanyFilter/companies_toks.csv")

#------------------------------------------------------#
# Analysis of company work / phrase frequency 
#------------------------------------------------------#
#
companies_sum <- companies_toks %>% 
  select(year, UniqueID) %>% 
  group_by(year) %>% 
  count(UniqueID) %>% 
  summarise(total = sum(n))

#------------------------------------------------------#
#Chart companies over time
#------------------------------------------------------#

ggplot(companies_sum)+
  aes(x = year, y = total, fill = total)+
  geom_col(position = "dodge", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "News of Top Chinese Companies in U.S., 2000-2019",
       subtitle = "Count of Mentions / Year: NYT, WP, WSJ, NYT, Inside US Trade",
       caption = "353 Articles. n=1,527 Mentions. Source: ProQuest. Graphic by Rob Wells & Austin Wilkins. 3-17-2020",
       x="year",
       y="Count of 23 Chinese Cos Mentions In News")

ggsave("./CompanyFilter/ChineseCosTotalMentions_3_17_2020.png",device = "png",width=9,height=6)

#Created Chinese Cos Total Mentions 3-17-2020


cos_pattern_sum <- companies_toks %>%
  select(UniqueID, pattern, year) %>% 
  group_by(UniqueID) %>% 
  count(pattern) %>% 
  arrange(desc(n))


#total mentions of top companies in five news organizations
cos_sum2 <- cos_pattern_sum %>% 
  select(pattern, n) %>% 
  group_by(pattern) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

colnames(Total_Calls_Master)[1:2] <- c("Complaints", "Number")
colnames(cos_sum2)[(1:2)] <- c("Company", "Total")

library(kableExtra)
cos_sum2 %>% 
  kable() %>%
  kable_styling("striped")


# Summarize by publication
companies_toks %>% 
  select(UniqueID, Pub) %>% 
  group_by(Pub) %>% 
  count(Pub) %>% 
  ggplot(aes(x = reorder(Pub, -n), y=n, fill=Pub))+
  geom_col(show.legend = FALSE) +
 geom_text(aes(label = n), vjust = -.2, size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Articles, Top Chinese Cos. Investing in U.S.", 
       subtitle = "Count of Mentions: NYT, WSJ, LAT, WP, IUT",
       caption = "Cos. $2 bln U.S. investment. 353 Articles Source: ProQuest. \n Graphic by Rob Wells & Austin Wilkins. 3-19-2020",
       y="Count of Mentions in Articles",
       x="Publications")  
ggsave("./CompanyFilter/Pubs_ChinaCos_Sum_3_19_2020.png",device = "png",width=9,height=6)
#Created Pubs China Cos_Sum2 Coverage 7-19-19

#Total Mentions of Companies
companies_toks %>% 
  select(UniqueID, pattern) %>% 
  group_by(pattern) %>% 
  count(pattern) %>%  
  ggplot(aes(x = reorder(pattern, n), y=n, fill=pattern))+
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -.2, size = 3) +
  coord_flip()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits=c(0, 1250)) +
  labs(title = "Total Articles, Top Chinese Cos. Investing in U.S.", 
       subtitle = "Count of Articles: NYT, WSJ, LAT, WP, IUT",
       caption = "353 Articles. n=1,527 Mentions. Source: ProQuest \n Graphic by Rob Wells & Austin Wilkins. 3-17-2020",
       y="Count of Mentions",
       x="") 
#Created China Cos Articles 7-19-19
ggsave("./CompanyFilter/ChinaCos_Sum2Coverage_3_17_2020.png",device = "png",width=9,height=6)


#Count the number of articles mentioning the company
companies_toks %>% 
  select(UniqueID, pattern) %>% 
  group_by(pattern) %>% 
  distinct() %>% 
  count(pattern) %>%  
  ggplot(aes(x = reorder(pattern, n), y=n, fill=pattern))+
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -.2, size = 3) +
  coord_flip()  +
  scale_y_continuous(limits=c(0, 165)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of Articles Mentioning Top Chinese Companies", 
       subtitle = "Articles 2000-2019: NYT, WSJ, LAT, WP, IUT",
       caption = "Cos. $2 bln U.S. investment. 353 Articles Source: ProQuest. \n Graphic by Rob Wells & Austin Wilkins. 3-19-2020",
       y="Count of Articles",
       x="") 
ggsave("./CompanyFilter/Count_Companies_Articles_3-19_2020.png",device = "png",width=9,height=6)
#Created Count_Companies_Articles_3-19_2020

#-------------------------
# Stopped March 19 2020
#-------------------------

#Two or more mentions of company per article
#Companies by articles
#Table of pubs, articles
cos_sum5 <- Chinese_Cos7_19 %>% 
  select(article_nmbr, pattern) %>% 
  group_by(pattern, article_nmbr) %>% 
  count(pattern) %>% 
  filter(n>1)

cos_sum7 <- cos_sum5 %>% 
  select(article_nmbr, pattern) %>% 
  group_by(pattern) %>% 
  count(pattern) 

cos_sum5 %>% 
  ggplot(aes(x = reorder(pattern, n), y=n, fill=pattern))+
  geom_col(show.legend = FALSE) +
  coord_flip()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "In-Depth Articles, Top Chinese Cos. Investing in U.S.", 
       subtitle = "Count of Articles: NYT, WSJ, LAT, WP, IUT",
       caption = "Cos. $2 bln U.S. investment. 344 Articles. Graphic by Rob Wells & Austin Wilkins, 7-19-19",
       y="Articles with 2 or More Mentions of a Company",
       x="") 
#Created In-Depth Coverage Chinese Cos 7-19-19

#Cut out CNOOC
cos_sum5 %>% 
  filter(!"CNOOC" %in% pattern) %>% 
  ggplot(aes(x = reorder(pattern, n), y=n, fill=pattern))+
  geom_col(show.legend = FALSE) +
  coord_flip()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "In-Depth Articles, Top Chinese Cos. Investing in U.S.", 
       subtitle = "Count of Articles: NYT, WSJ, LAT, WP, IUT",
       caption = "Cos. $2 bln U.S. investment. 344 Articles. Graphic by Rob Wells & Austin Wilkins, 7-19-19",
       y="Articles with 2 or More Mentions of a Company. No CNOOC",
       x="") 
#Created In-Depth Coverage Chinese Cos No CNOOC 7-19-19

#CNOOC Pattern 
  Chinese_Cos7_19 %>%
  filter(pattern =="CNOOC") %>% 
  select(article_nmbr, pattern, year) %>% 
  group_by(article_nmbr, year) %>% 
  count(pattern) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = year, y=n, fill=year))+
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Media Mentions of CNOOC, 2000-2018", 
       subtitle = "Count of Mentions: NYT, WSJ, LAT, WP, IUT",
       caption = "344 Articles. Graphic by Rob Wells & Austin Wilkins, 7-19-19",
       y="Mentions of CNOOC",
       x="") 
#Created CNOOC coverage by year, 7-19-19

#CNOOC Coverage Summary Table
cnooc <- Chinese_Cos7_19 %>%
    filter(pattern =="CNOOC") %>% 
    select(article_nmbr, year, pub, headline, pattern) %>% 
    group_by(article_nmbr) %>% 
    distinct()
#Link to corpus
corpus <- headline_date("./ExtractedTexts/cos_newsearch7_18.txt")

cnooc_articles <- corpus %>% 
  inner_join(cnooc, by=("article_nmbr")) %>%
               select(text, article_nmbr, year, pub.x)             

write_tsv(cnooc_articles, "cnooc_articles.txt")
#http://www.sthda.com/english/wiki/fast-writing-of-data-from-r-to-txt-csv-files-readr-package

library(knitr)
library(rmarkdown)
R -e rmarkdown::render(cnooc_articles)

cnooc_articles$text

#not available for osx
#install.packages("R2wd")

library(kableExtra)

cnooc_articles %>% 
  kable() %>%
  kable_styling("striped", full_width = F)

xy <- pander(cnooc_articles)

x <- pandoc.table(cnooc_articles$text, split.cell = 80)



#doesn't return a table. waits too long
  kable(cnooc_articles) %>%
  kable_styling(full_width = F) %>% 
    column_spec(1, bold = T, border_right = T) %>%
    column_spec(2, bold = T, border_right = T) %>%
    column_spec(3, bold = T, border_right = T) %>%
  column_spec(4, width = "30em")
  
  #return a table. waits too long
  kable(cnooc_articles) %>%
    kable_styling(full_width = F) %>% 
    column_spec(1, width = "10em")
  
cnooc_articles %>% 
  kable() %>% 
  kable_styling()
  

kable(cnooc_articles) %>%
  kable_styling() %>%
  column_spec(1, width = "30em") 


Yes %>% 
  kable() %>%
  kable_styling("striped")

TweetText_Table %>% 
  kable() %>%
  kable_styling("striped")



#------------------------------------------------------------
# #With headline_date function
# WSJ1_char <- WSJ1$text    
# WSJ1_char <- as.character(WSJ1_char)    
# toks1 <- tokens(WSJ1_char)
# results2 <- (kwic(toks1, pattern = phrase(Nav))) 
# results2 <- as.data.frame(results2) 
# #
# #Format for year
# WSJ1$date <- ymd(WSJ1$date)
# #Create year field
# WSJ1$year <- year(WSJ1$date)

#------------------------------------------------------------
