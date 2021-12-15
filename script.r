rm(list = ls())

#utilities
library(dplyr)
library(stringr)
#library(ggplot2)
library(scales)
source("./utils.r")


#data = read.csv("./europe-",header = TRUE)


file_list <- list.files("./matches_of_interest", pattern=".csv", full.names=T)
dataset <- do.call("rbind", lapply(file_list, read.csv))

#dataset <- data.frame(data)

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
all = rbind(data.spain, data.germany)
#Spain
mean(data.spain$Score)
sd(data.spain$Score)

#Germany
mean(data.germany$Score)
sd(data.germany$Score)



#graphics

#SPAIN
x11()
hist(data.spain$Score, col="skyblue", border=T, lwd=0.5,
     main="Histograma de Goles en la Champions League equipos españoles", 
     xlab="Goles por partido", 
     ylab="Frecuencia",
     labels=T)
abline(v=mean(data.spain$Score),
       col="dodgerblue3",
       lty=2,
       lwd=2)
box()

x11()
hist(data.spain$Score, col="skyblue", border=T, lwd=0.5, freq=F,
     main="Histograma de densidad de probabilidad de Goles en la Champions League equipos españoles", 
     xlab="Goles por partido", 
     ylab="Densidad",
     labels=T)
lines(density(data.spain$Score))
polygon(density(data.spain$Score),
        col=scales::alpha('skyblue',.35))


#GERMANY

x11()
hist(data.germany$Score, col=scales::alpha('red',.35), border=T, lwd=0.5,
     main="Histograma de Goles en la Champions League equipos alemanes", 
     xlab="Goles por partido", 
     ylab="Frecuencia",
     labels=T)
abline(v=mean(data.germany$Score),
       col="red",
       lty=2,
       lwd=2)
box()

x11()
hist(data.germany$Score, col=scales::alpha('red',.35), border=T, lwd=0.5, freq=F,
     main="Histograma de densidad de probabilidad de Goles en la Champions League equipos alemanes", 
     xlab="Goles por partido", 
     ylab="Densidad",
     labels=T)
lines(density(data.germany$Score))
polygon(density(data.germany$Score),
        col=scales::alpha('red',.35))



#Altogether

x11()
hist(data.spain$Score, col=scales::alpha('skyblue',0.4),border=T, lwd=0.5, 
     main="Histograma comparativo de Goles en la Champions League equipos alemanes vs españoles", 
     xlab="Goles por partido", 
     ylab="Frecuencia",
     labels=T)
hist(data.germany$Score, col=scales::alpha('red',0.4), border=T, lwd=0.5, labels=T, add=T)
#box()
legend("topright", c("España", "Alemania"), lwd=2, col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

x11()
hist(data.germany$Score, col=scales::alpha('red',0.4),border=T, lwd=0.5, freq=F,
     main="Histograma comparativo de probabilidades Goles en la Champions League equipos alemanes vs españoles", 
     xlab="Goles por partido", 
     ylab="Frecuencia",
     labels=T)
hist(data.spain$Score, col=scales::alpha('skyblue',0.4), border=T, lwd=0.5, labels=T, add=T, freq=F)
#box()
legend("topright", c("España", "Alemania"), lwd=2, col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


#sp <- hist(data.germany$Score, plot = FALSE)
#de <- hist(data.spain$Score, plot=F)
#sp
#de

#altura <- max(sp$density, de$density)
#altura
