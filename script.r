rm(list = ls())

#utilities
library(dplyr)
library(stringr)
source("./utils.r")

data <- read.csv("./europe-champions-league/2015-16/champs.csv",header = TRUE)

dataset <- data.frame(data)

dataset = dataset %>% mutate(FT = str_trim(str_remove(dataset$FT, "\\(\\*\\)")), # Quita (*) y espacio en blanco
       score_Team.1 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 1]),
       score_Team.2 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 2]))





spain.matches <- filter(dataset, 
                        grepl("ESP", Team.1) | grepl("ESP", Team.2))
germany.matches <- filter(dataset, 
                        grepl("GER", Team.1) | grepl("GER", Team.2))
confrontation.matches <- filter(dataset, 
                        (grepl("ESP", Team.1) & grepl("GER", Team.2)) | (grepl("GER", Team.1) & grepl("ESP", Team.2)))


## (b)
#long <- nchar(x)
#long
#country <- substr(x, long - 6, long -4)
#country



#interesting.data = rbind(interesting.data, confrontation.matches)
#Now that we have filtered the data of interest, we'll process the data
data.spain = spain.matches %>% transmute( 
          Stage = Stage,
          Country = "ESP",
          Score = case_when(
            str_detect(Team.1, "ESP") ~ score_Team.1,
            str_detect(Team.2, "ESP") ~ score_Team.2
          ),
          Enemy.score = case_when(
            str_detect(Team.1, "ESP") ~ score_Team.2,
            str_detect(Team.2, "ESP") ~ score_Team.1
          ),
          Enemy.country = case_when(
            str_detect(Team.1, "ESP") ~ substr(str_trim(str_split(Team.2, "›", simplify = TRUE)[, 2]), 1, 3),
            str_detect(Team.2, "ESP") ~ substr(str_trim(str_split(Team.1, "›", simplify = TRUE)[, 2]), 1, 3)
          )
          )

data.germany = germany.matches %>% transmute( 
  Stage = Stage,
  Country = "GER",
  Score = case_when(
    str_detect(Team.1, "GER") ~ score_Team.1,
    str_detect(Team.2, "GER") ~ score_Team.2
  ),
  Enemy.score = case_when(
    str_detect(Team.1, "GER") ~ score_Team.2,
    str_detect(Team.2, "GER") ~ score_Team.1
  ),
  Enemy.country = case_when(
    str_detect(Team.1, "GER") ~ substr(str_trim(str_split(Team.2, "›", simplify = TRUE)[, 2]), 1, 3),
    str_detect(Team.2, "GER") ~ substr(str_trim(str_split(Team.1, "›", simplify = TRUE)[, 2]), 1, 3)
  )
)

#Spain
mean(data.spain$Score)
sd(data.spain$Score)
#Germany
mean(data.germany$Score)
sd(data.germany$Score)