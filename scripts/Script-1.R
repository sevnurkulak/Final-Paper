library(tidyverse)
library(tibble)
library(janitor)
library(dplyr)
library(plyr)
library(ggplot2)
workingDir <- "/Users/sevnur/Desktop/finalPaper/Final-Paper/inputs"
setwd(workingDir)
set.seed(85993)

# Reading Data and cleaning summary statistics
rawData <-read.csv("13100802.csv")

rawData <- rawData[which(rawData$Characteristics == "Number of persons"),]
rawData <- rawData[which(rawData$GEO != "Canada (excluding territories)"),]
rawData <- rawData[which(rawData$Age.group != "Total, 12 years and over"),]
rawData <- rawData[which(rawData$Sex != "Both sexes"),]
interData <- rawData[,c('GEO','Age.group','Sex','Indicators','VALUE')]


# Renaming variables
interData$Indicators <- gsub("Main source of stress in day-to-day life, ", '',interData$Indicators)
interData$Indicators <- gsub("none", 'Unknown',interData$Indicators)

# Cleaning End

# Response Characteristics
with(interData, tapply(VALUE, GEO, sum, na.rm=TRUE)) * 100 / sum(interData$VALUE, na.rm= TRUE)
with(interData, tapply(VALUE, Age.group, sum, na.rm=TRUE)) * 100/ sum(interData$VALUE, na.rm= TRUE)


# Creating Table 1 ===================================
# Mental illness percentages by gender

maleIdx <- which(interData$Sex == "Males")

maleTable <- interData[maleIdx,]

femaleIdx <- which(interData$Sex == "Females")

femaleTable <- interData[femaleIdx,]


nonmentalHealth <- sum(interData$VALUE[which(interData$Indicators == "Ability to handle the day-to-day demands in life, good or excellent")]) + sum(interData$VALUE[which(interData$Indicators == "Ability to handle unexpected and difficult problems, good or excellent")])
 
total <- sum(interData$VALUE, na.rm = TRUE)

mentalHealth <- total - nonmentalHealth



nonmentalHealthMale <- sum(maleTable$VALUE[which(maleTable$Indicators == "Ability to handle the day-to-day demands in life, good or excellent")]) + sum(maleTable$VALUE[which(maleTable$Indicators == "Ability to handle unexpected and difficult problems, good or excellent")])

totalMale <- sum(maleTable$VALUE, na.rm = TRUE)

mentalHealthMale <- totalMale - nonmentalHealthMale


nonmentalHealthFemale <- sum(femaleTable$VALUE[which(femaleTable$Indicators == "Ability to handle the day-to-day demands in life, good or excellent")]) + sum(femaleTable$VALUE[which(femaleTable$Indicators == "Ability to handle unexpected and difficult problems, good or excellent")])

totalFemale <- sum(femaleTable$VALUE, na.rm = TRUE)

mentalHealthFemale <- totalFemale - nonmentalHealthFemale


tb1 <- matrix(c(mentalHealthMale / totalMale,  nonmentalHealthMale / totalMale, mentalHealthFemale / totalFemale,  nonmentalHealthFemale / totalFemale),ncol=2,byrow=TRUE)
colnames(tb1) <- c("Mental Health","No Mental Health")
rownames(tb1) <- c("Male","Female")
tb1 <- as.table(tb1)
tb1

# Creating Table 2 ===================================
# Number of Respondents for each subgroups by age and response

Table12_17 <- interData[which(interData$Age.group == "12 to 17 years"),]

Table18_34 <- interData[which(interData$Age.group == "18 to 34 years"),]

Table35_49 <- interData[which(interData$Age.group == "35 to 49 years"),]

Table50_64 <- interData[which(interData$Age.group == "50 to 64 years"),]

Table65_ <- interData[which(interData$Age.group == "65 years and over"),]

Table12_17_agg <-  with(Table12_17, tapply(VALUE, Indicators, sum, na.rm=TRUE))
Table18_34_agg <- with(Table18_34, tapply(VALUE, Indicators, sum, na.rm=TRUE))
Table35_49_agg <- with(Table35_49, tapply(VALUE, Indicators, sum, na.rm=TRUE))
Table50_64_agg <- with(Table50_64, tapply(VALUE, Indicators, sum, na.rm=TRUE))
Table65_agg <- with(Table65_, tapply(VALUE, Indicators, sum, na.rm=TRUE))


tb2 <- matrix(c(c(Table12_17_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table12_17_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table12_17_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
,
c(Table18_34_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table18_34_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table18_34_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
,
c(Table35_49_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table35_49_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table35_49_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
,
c(Table50_64_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table50_64_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table50_64_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
,
c(Table65_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table65_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table65_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
), ncol=9, byrow=TRUE)

colnames(tb2) <- c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown", "No Issue")
rownames(tb2) <- c("12-17","18-34","35-49","50-64","65+")
tb2 <- as.table(tb2)

# Creating Table 3 =========================================================
# Percentages of Respondents in population for each subgroups by age and response

row1 <- c(Table12_17_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table12_17_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table12_17_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(Table12_17$VALUE, na.rm = TRUE)
row2 <- c(Table18_34_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table18_34_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table18_34_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(Table18_34$VALUE, na.rm = TRUE)
row3 <- c(Table35_49_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table35_49_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table35_49_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(Table35_49$VALUE, na.rm = TRUE)
row4 <- c(Table50_64_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table50_64_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table50_64_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(Table50_64$VALUE, na.rm = TRUE)
row5 <- c(Table65_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (Table65_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + Table65_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(Table65_$VALUE, na.rm = TRUE)

tb3 <- matrix(c(row1,row2,row3,row4,row5), ncol=9, byrow=TRUE)

colnames(tb3) <- c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown", "No Issue")
rownames(tb3) <- c("12-17","18-34","35-49","50-64","65+")
tb3 <- as.table(tb3)
tb3

# Creating Table 4 ===========================================================
# Percentages of mental issues by age group

mentallyIssues <- interData[which(interData$Indicators != "Ability to handle the day-to-day demands in life, good or excellent" & interData$Indicators != "Ability to handle unexpected and difficult problems, good or excellent"), ]


tb4 <- with(mentallyIssues, tapply(VALUE, Age.group, sum, na.rm=TRUE)) * 100 / sum(with(mentallyIssues, tapply(VALUE, Age.group, sum, na.rm=TRUE)), na.rm = TRUE)

tb4

# Creating Table 5 ===========================================================
# Number of respondents by province

al <- interData[which(interData$GEO == "Alberta"),]

bc <- interData[which(interData$GEO == "British Columbia"),]

mn <- interData[which(interData$GEO == "Manitoba"),]

nb <- interData[which(interData$GEO == "New Brunswick"),]

nf <- interData[which(interData$GEO == "Newfoundland and Labrador"),]

ns <- interData[which(interData$GEO == "Nova Scotia"),]

on <- interData[which(interData$GEO == "Ontario"),]

pei <- interData[which(interData$GEO == "Prince Edward Island"),]

qc <- interData[which(interData$GEO == "Quebec"),]

sk <- interData[which(interData$GEO == "Saskatchewan"),]


al_agg <-  with(al, tapply(VALUE, Indicators, sum, na.rm=TRUE))

bc_agg <-  with(bc, tapply(VALUE, Indicators, sum, na.rm=TRUE))

mn_agg <-  with(mn, tapply(VALUE, Indicators, sum, na.rm=TRUE))

nb_agg <-  with(nb, tapply(VALUE, Indicators, sum, na.rm=TRUE))

nf_agg <-  with(nf, tapply(VALUE, Indicators, sum, na.rm=TRUE))

ns_agg <-  with(ns, tapply(VALUE, Indicators, sum, na.rm=TRUE))

on_agg <-  with(on, tapply(VALUE, Indicators, sum, na.rm=TRUE))

qc_agg <-  with(qc, tapply(VALUE, Indicators, sum, na.rm=TRUE))

sk_agg <-  with(sk, tapply(VALUE, Indicators, sum, na.rm=TRUE))

pei_agg <-  with(pei, tapply(VALUE, Indicators, sum, na.rm=TRUE))

row1 <- c(al_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (al_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + al_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row2 <- c(bc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (bc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + bc_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row3 <- c(mn_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (mn_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + mn_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row4 <- c(nb_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (nb_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nb_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row5 <- c(nf_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (nf_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nf_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row6 <- c(ns_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (ns_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + ns_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row7 <- c(on_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (on_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + on_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row8 <- c(qc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (qc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + qc_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row9 <- c(sk_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (sk_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + sk_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row10 <- c(pei_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (pei_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + pei_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))


tb5 <- matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10), ncol=9, byrow=TRUE)

colnames(tb5) <- c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown", "No Issue")
rownames(tb5) <- c("AL","BC", "MN", "NB", "NF", "NS", "ON", "QC", "SK", "PEI")
tb5 <- as.table(tb5)



# Creating Table 6 =================================================================
# Percentages of respondents by indicator and province

row1 <- c(al_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (al_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + al_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(al$VALUE, na.rm = TRUE)
row2 <- c(bc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (bc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + bc_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(bc$VALUE, na.rm = TRUE)
row3 <- c(mn_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (mn_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + mn_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(mn$VALUE, na.rm = TRUE)
row4 <- c(nb_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (nb_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nb_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(nb$VALUE, na.rm = TRUE)
row5 <- c(nf_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (nf_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nf_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(nf$VALUE, na.rm = TRUE)
row6 <- c(ns_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (ns_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + ns_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(ns$VALUE, na.rm = TRUE)
row7 <- c(on_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (on_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + on_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(on$VALUE, na.rm = TRUE)
row8 <- c(qc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (qc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + qc_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(qc$VALUE, na.rm = TRUE)
row9 <- c(sk_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (sk_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + sk_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(sk$VALUE, na.rm = TRUE)
row10 <- c(pei_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")], (pei_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + pei_agg[("Ability to handle unexpected and difficult problems, good or excellent")])) * 100 / sum(pei$VALUE, na.rm = TRUE)


tb6 <- matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10), ncol=9, byrow=TRUE)

colnames(tb6) <- c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown", "No Issue")
rownames(tb6) <- c("AL","BC", "MN", "NB", "NF", "NS", "ON", "QC", "SK", "PEI")
tb6 <- as.table(tb6)
tb6



# Creating Table 7 & 8 =================================================================


# Percentage of mental problems by province
tb7 <- with(mentallyIssues, tapply(VALUE, GEO, sum, na.rm=TRUE)) * 100 / sum(with(mentallyIssues, tapply(VALUE, GEO, sum, na.rm=TRUE)), na.rm = TRUE)

# For each province percentage of population with mental problem
tb8 <- with(mentallyIssues, tapply(VALUE, GEO, sum, na.rm=TRUE))/with(interData, tapply(VALUE, GEO, sum, na.rm=TRUE)) 

tb7
tb8

# Creating Graph 1 ==========================================================

# Bar plots for number of people experience mental issues for each province by indicators and gender
p <- with(al[which(al$Sex == "Males"),], tapply(VALUE, Indicators, sum, na.rm=TRUE))

p2 <- with(al[which(al$Sex == "Females"),], tapply(VALUE, Indicators, sum, na.rm=TRUE))


ggplot(al[which(substr(al$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity" )  +
  ggtitle("Number of People Experiencing Mental Issues in Alberta by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(bc[which(substr(bc$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in British Columbia by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(mn[which(substr(mn$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Manitoba by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nb[which(substr(nb$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in New Bruinswick by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nf[which(substr(nf$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in New Foundland & Labrador by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(ns[which(substr(ns$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Nova Scotia by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(on[which(substr(on$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Ontario by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(pei[which(substr(pei$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Prince Edward Island by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(qc[which(substr(qc$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Quebec by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sk[which(substr(sk$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(aes(fill = Sex),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues in Saskatchewan by Indicators and Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating Table 2 ==========================================

# Bar plots for number of people experience mental issues for each province by indicators and age group
ggplot(al[which(substr(al$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Alberta by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(bc[which(substr(bc$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in British Columbia by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(mn[which(substr(mn$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Manitoba by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nb[which(substr(nb$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in New Brunswick by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nf[which(substr(nf$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in New Foundland & Labrador by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(ns[which(substr(ns$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Nova Scotia by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(on[which(substr(on$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Ontario by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(pei[which(substr(pei$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Prince Edward Island by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(qc[which(substr(qc$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Quebec by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sk[which(substr(sk$Indicators, 1,4) != 'Abil'),], aes(x = Indicators, y = VALUE, fill = Age.group, label = VALUE)) +
  geom_bar(aes(fill = Age.group),stat = "identity") +
  ggtitle("Number of People Experiencing Mental Issues in Saskatchewan by Indicators and Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating Table 3 ===============================================

ggplot(mentallyIssues, aes(x = GEO, y = VALUE, fill = Indicators, label = VALUE)) +
  geom_bar(aes(fill = Indicators),stat = "identity")  +
  ggtitle("Number of People Experiencing Mental Issues by Province and Indicators") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating Table 4 ================================================

# For each indicator percentages by province
row1 <- c(al_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(al$VALUE, na.rm = TRUE) - (al_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + al_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row2 <- c(bc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(bc$VALUE, na.rm = TRUE) - (bc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + bc_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row3 <- c(mn_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(mn$VALUE, na.rm = TRUE) - (mn_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + mn_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row4 <- c(nb_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(nb$VALUE, na.rm = TRUE) - (nb_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nb_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row5 <- c(nf_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(nf$VALUE, na.rm = TRUE) - (nf_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + nf_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row6 <- c(ns_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(ns$VALUE, na.rm = TRUE) - (ns_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + ns_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row7 <- c(on_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(on$VALUE, na.rm = TRUE) - (on_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + on_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row8 <- c(qc_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(qc$VALUE, na.rm = TRUE) - (qc_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + qc_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row9 <- c(sk_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(sk$VALUE, na.rm = TRUE) - (sk_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + sk_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))
row10 <- c(pei_agg[c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")]) * 100 / (sum(pei$VALUE, na.rm = TRUE) - (pei_agg[("Ability to handle the day-to-day demands in life, good or excellent")] + pei_agg[("Ability to handle unexpected and difficult problems, good or excellent")]))


tb13 <- matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10), ncol=8, byrow=TRUE)

colnames(tb13) <- c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown")
rownames(tb13) <- c("AL","BC", "MN", "NB", "NF", "NS", "ON", "QC", "SK", "PEI")
tb13 <- as.table(tb13)
tb13
GEO      <- c(rep(c("AL","BC", "MN", "NB", "NF", "NS", "ON", "QC", "SK", "PEI"), each = 8))
Indicators  <- c(rep(c("family","financial concerns", "health", "school work (12 to 34 years old)","time pressures/ not enough time", "work", "other", "Unknown"), times = 10))
Percentage <- c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10)
tb13_df      <- data.frame(GEO, Indicators, Percentage)

ggplot(tb13_df, aes(x = GEO, y = Percentage, fill = Indicators, label = Percentage)) +
  geom_bar(aes(fill = Indicators),stat = "identity")  +
  ggtitle("Percentage of Indicators in People Experiencing Mental Issues by Province")








