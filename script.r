rm(list = ls())

#utilities
library(dplyr)
source("./utils.r")

data <- read.csv("./europe-champions-league/2015-16/champs.csv",header = TRUE)

dataset <- data.frame(data)

dataset = dataset %>% mutate(score_Team.1=(extract_results(FT, 1)) , score_Team.2=(extract_results(FT, 2)))

spain.matches <- filter(dataset, grepl("ESP", Team.1) | grepl("ESP", Team.2))
german.matches <- filter(dataset, grepl("GER", Team.1) | grepl("GER", Team.2))
confrontation.matches <- filter(dataset, (grepl("ESP", Team.1) & grepl("GER", Team.2)) | (grepl("GER", Team.1) & grepl("ESP", Team.2)))

#Now that we have filtered the data of interest, we'll process the data



