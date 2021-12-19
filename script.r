rm(list = ls())

#utilities
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
source("./utils.r")

file_list <- list.files("./matches_of_interest", pattern=".csv", full.names=T)
df_list <- lapply(file_list, read.csv)
processed_data <- lapply(df_list, process_year)

for (i in 1:length(processed_data))
{
  processed_data[[i]] = mutate(processed_data[[i]], Year = substr(file_list[i], 23, 31))
}

data_per_year = do.call("rbind", processed_data)


#write.csv(x=data_per_year, file="data_per_year.csv")
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
