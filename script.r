rm(list = ls())

library(dplyr)
source("./utils.r")

data <- read.csv("./europe-champions-league/2015-16/champs.csv",header = TRUE)

dataset <- data.frame(data)

elems <- filter(dataset, (grepl("ESP", Team.1) & grepl("GER", Team.2)) | (grepl("GER", Team.1) & grepl("ESP", Team.2)))

