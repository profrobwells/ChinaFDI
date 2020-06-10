rm(list=ls())

setwd("/home/austin/Documents/Projects/ChinaFDI")

library(tidyverse)


#Import
EF <- rio::import("./EconomicFilter/Economicfilter2020.csv") %>% 
  separate(Date, c("Year","Month","Day"),"-",convert=TRUE) %>% 
  unite("Year-Month",Year:Month,remove=FALSE) %>% 
  mutate(V1 = as.character("V1"))


#Import
BF <- rio::import("./BroadFilter/BroadFilter2020.csv") %>% 
  separate(Date, c("Year","Month","Day"),"-",convert=TRUE) %>% 
  unite("Year-Month",Year:Month,remove=FALSE) %>% 
  mutate(V1 = as.character("V1"))


full <- union(EF,BF,by = "Title")

write.csv(full,"Data/combined_BF_EF.csv")







