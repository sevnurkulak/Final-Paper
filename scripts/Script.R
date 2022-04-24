rawData <-read.csv("13100802.csv")
interData <- rawData[,c('GEO','Age.group','Sex','Indicators')]

library(tidyverse)
library(tibble)
library(janitor)
library(dplyr)

#which(interData$GEO == 'Saskatchewan')
#interData$GEO[which(interData$GEO == 'Saskatchewan')] 


interData$Indicators <- gsub("Main source of stress in day-to-day life, ", '',interData$Indicators)
