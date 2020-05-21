#IMF_CDIS data Processing
#Importing Multiple Years of Data, Creating a Filtered Dataframe for Time Series Analysis

library(tidyverse)
library(rio)
FDI2018 <- rio::import("/Users/rswells/Dropbox/Current_Projects/China Notes  Background/IMF_CDIS data/CDIS_Table_3_Direct_Investment_Posi_2018.xlsx", which="Inward", skip=5)
FDI2018 <- janitor::clean_names(FDI2018)

colnames(FDI2018)[1:7] <- c("country", "di_in", "di_out", "equity_in", "equity_out", "debt_in", "debt_out")
FDI2018[2:7] <- lapply(FDI2018[2:7], as.numeric)

FDI2018$country <- gsub("[[:space:]]", "", FDI2018$country)
FDI2018$country <- gsub("[[:punct:]]", "", FDI2018$country)

x2018 <- FDI2018 %>% filter(country == "ChinaPRMainland" | country =="ChinaPRHongKong" | country =="World")

x2018$year <- "2018"

#2017
#2016
#2015
#2014
#2013
#2012
#2011
#2010
#2009
df <- rio::import("/Users/rswells/Dropbox/Current_Projects/China Notes  Background/IMF_CDIS data/CDIS_Table_3_Direct_Investment_Posi_2009.xlsx", which="Inward", skip=5)
df <- janitor::clean_names(df)

colnames(df)[1:7] <- c("country", "di_in", "di_out", "equity_in", "equity_out", "debt_in", "debt_out")
df[2:7] <- lapply(df[2:7], as.numeric)

df$country <- gsub("[[:space:]]", "", df$country)
df$country <- gsub("[[:punct:]]", "", df$country)

x2009 <- df %>% filter(country == "ChinaPRMainland" | country =="ChinaPRHongKong" | country =="World")

x2009$year <- "2009"


#rbind
FDI2018_09 <- rbind(x2018,x2017, x2016, x2015, x2014, x2013, x2012, x2011, x2010, x2009)

write.csv(FDI2018_09, "FDI2018_09.csv")
