library(tidyverse)
library(modelsummary)
set.seed(85993)
workingDir <- "/Users/sevnur/Desktop/finalPaper/Final-Paper/inputs"
setwd(workingDir)

# Creating population using simulation
size_of_sample_for_canadian_mh_survey <- 2000

sample_for_can_mh_survey <-
  tibble(Age.group = 
           sample(x = c(0:4), 
                  size = size_of_sample_for_canadian_mh_survey,
                  replace = TRUE
           ),
         Sex = 
           sample(x = c(0:1),
                  size = size_of_sample_for_canadian_mh_survey,
                  replace = TRUE
           ),
         GEO = 
           sample(x = c(1:10),
                  size = size_of_sample_for_canadian_mh_survey,
                  replace = TRUE
           ),
         noise = rnorm(size_of_sample_for_canadian_mh_survey, mean = 0, sd = 1), 
         Indicators = 1 + 0.5 * Age.group + 0.5 * Sex + 0.01 * GEO + noise
  ) 


# Setting variables 
sample_for_can_mh_survey <- 
  sample_for_can_mh_survey |> 
  mutate(Indicators = 
           if_else(Indicators > 3, 
                   'mental problem exist', 
                   'good or excellent')
  )


sample_for_can_mh_survey <- 
  sample_for_can_mh_survey |> 
  mutate(
    Age.group = case_when(
      Age.group == 0 ~ '12 to 17 years',
      Age.group == 1 ~ '18 to 34 years',
      Age.group == 2 ~ '35 to 49 years',
      Age.group == 3 ~ '50 to 64 years',
      Age.group == 4 ~ '65 years and over',
      TRUE ~ 'Problem'
    ),
    Sex = case_when(
      Sex == 0 ~ 'Males',
      Sex == 1 ~ 'Females',
      TRUE ~ 'Problem'
    ),
    GEO = case_when(
      GEO == 1 ~ 'Alberta',
      GEO == 2 ~ 'British Columbia',
      GEO == 3 ~ 'Manitoba',
      GEO == 4 ~ 'New Brunswick',
      GEO == 5 ~ 'Newfoundland and Labrador',
      GEO == 6 ~ 'Nova Scotia',
      GEO == 7 ~ 'Ontario',
      GEO == 8 ~ 'Prince Edward Island',
      GEO == 9 ~ 'Quebec',
      GEO == 10 ~ 'Saskatchewan',
      TRUE ~ 'Problem'
    ),
    
  ) |> 
  select(-noise)

# Make classes as factor so we can fit a model.
sample_for_can_mh_survey <- 
  sample_for_can_mh_survey |> 
  mutate(across(c(Age.group, Sex, GEO, Indicators), as_factor))


sample_for_can_mh_survey |>   
  head()

# Summary of simualted data
sample_for_can_mh_survey |> 
  datasummary_skim(type = "categorical")

# Removing 300 males so we can have a bias in our dataset
sample_for_can_mh_survey <- 
  sample_for_can_mh_survey |> 
  arrange(Sex) |> 
  slice(1:1700)

# Summary of biased dataset with oversampled females
sample_for_can_mh_survey |> 
  datasummary_skim(type = "categorical")

# Fitting model
model <- 
  glm(Indicators ~ Sex + Age.group + GEO, 
      data = sample_for_can_mh_survey,
      family = "binomial"
  )

# Logistic regression model summary
model |> 
  modelsummary(fmt = 2, exponentiate = TRUE)


# Post strata Real Data===================================

# Read real data
rawData <-read.csv("13100802.csv")

# Removing summary statstics in dataset
rawData <- rawData[which(rawData$Characteristics == "Number of persons"),]
rawData <- rawData[which(rawData$GEO != "Canada (excluding territories)"),]
rawData <- rawData[which(rawData$Age.group != "Total, 12 years and over"),]
rawData <- rawData[which(rawData$Sex != "Both sexes"),]

# Renaming variables
interData <- rawData[,c('GEO','Age.group','Sex','Indicators','VALUE')]
interData$Indicators <- gsub("Main source of stress in day-to-day life, ", '',interData$Indicators)
interData$Indicators <- gsub("Ability to handle the day-to-day demands in life, ", '',interData$Indicators)
interData$Indicators <- gsub("Ability to handle unexpected and difficult problems, ", '',interData$Indicators)
interData$Indicators <- gsub("none", 'Unknown',interData$Indicators)
# Converting indicators of the real dataset to binary indicator so we can predict using logistic regression model.
interData <- 
  interData |> 
  mutate(Indicators = 
           if_else(Indicators == "good or excellent", 
                   'good or excellent',
                   'mental problem exist', 
                   )
  )
# Make variables as factor so we can fit a model
interData <- 
interData |> 
mutate(across(c(Age.group, Sex, GEO, Indicators), as_factor))


# Preparing dataset and aggregating responders by subgroups
interDataAgg <- 
  interData %>% 
    group_by(GEO, Age.group, Sex, Indicators) %>% 
  summarise(VALUE = sum(VALUE, na.rm = TRUE))

interDataAgg$VALUE <- interDataAgg$VALUE * 100 / sum(interDataAgg$VALUE)

# Post Stratification 

# Predict percentages using logistic regression model.
post_strat_interData <- 
  model |> 
  predict(newdata = interDataAgg, type = 'response', se.fit = TRUE) |> 
  as_tibble() |> 
  cbind(interDataAgg)


predictedResults <- post_strat_interData |> 
  mutate(VALUE = fit*VALUE) |> 
  group_by(GEO) |> 
  summarise(results = sum(VALUE))

actualResults <- interDataAgg |> 
  group_by(GEO) |> 
  summarise(results = sum(VALUE))

predictedResults

actualResults


