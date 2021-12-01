rm(list = ls())

library(dplyr)

data <- read.csv("./europe-champions-league/2015-16/champs.csv",header = TRUE)

dataset <- data.frame(data)

elems <- filter(dataset, Stage == "Group")

