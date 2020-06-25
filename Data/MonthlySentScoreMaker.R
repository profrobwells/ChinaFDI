
setwd("Documents/Projects/ChinaFDI")

library(tidyverse)


BF <- read.csv("BroadFilter/BF_sentiment_score_byMonth.csv")
EF <- read.csv("EconomicFilter/EF_sentiment_score_byMonth.csv")
Comb <- read.csv("Data/combined_sentiment_byMonth.csv")


#make negative values actually negative

neg <- function(df){

  for (i in 1:nrow(df)){
  if (df$sentiment[i] == "negative"){
    df$n[i] <- df$n[i] - (df$n[i] * 2)
  }
  }
  return(df)
}

BF <- neg(BF)
EF <- neg(EF)
Comb <- neg(Comb)


BF$X <- NULL
EF$X <- NULL
Comb$X <- NULL


BF <- BF %>% spread(sentiment,n)
EF <- EF %>% spread(sentiment,n)
Comb <- Comb %>% spread(sentiment,n)


colnames(BF)[3] <- "BF_positive"
colnames(BF)[2] <- "BF_negative"
colnames(EF)[3] <- "EF_positive"
colnames(EF)[2] <- "EF_negative"
colnames(Comb)[3] <- "combined_positive"
colnames(Comb)[2] <- "combined_negative"



MonthlySentScore <- full_join(Comb,BF,by="Year.Month")
MonthlySentScore <- full_join(MonthlySentScore,EF,by="Year.Month")
MonthlySentScore

write.csv(MonthlySentScore,"Data/MonthlySentScore.csv")
