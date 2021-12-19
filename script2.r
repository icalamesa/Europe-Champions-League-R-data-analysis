rm(list = ls())

#utilities
library(dplyr)
library(stringr)

library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggforce)

library(scales)
source("./utils.r")

file_list <- list.files("./matches_of_interest", pattern=".csv", full.names=T)
df_list <- lapply(file_list, read.csv)

#Get list of filtered out and processed data
processed_data <- lapply(df_list, process_year)

#Add year to each dataframe
for (i in 1:length(processed_data))
{
  processed_data[[i]] = mutate(processed_data[[i]], Year = substr(file_list[i], 23, 31))
  i
}

#Stacking dataframes
data_per_year = do.call("rbind", processed_data)

#Separating data
spain.data = filter(data_per_year, grepl("ESP", data_per_year$Country))
germany.data = filter(data_per_year, grepl("GER", data_per_year$Country))

#Spanish group metrics
spain.means_col = setNames(aggregate(spain.data[, 3], by=list(spain.data$Year), mean), c("Year", "Mean"))
spain.sd_col = setNames(aggregate(spain.data[, 3], by=list(spain.data$Year), sd), c("Year", "Stdev"))
spain_group.data = merge(spain.means_col, spain.sd_col, by="Year")

#German group metrics
germany.means_col = setNames(aggregate(germany.data[, 3], by=list(germany.data$Year), mean), c("Year", "Mean"))
germany.sd_col = setNames(aggregate(germany.data[, 3], by=list(germany.data$Year), sd), c("Year", "Stdev"))
germany_group.data = merge(germany.means_col, germany.sd_col, by="Year")

x11()
ggplot(data = data_per_year,aes(y = Score, x = Year, color=Country )) +
  geom_point(size=3)+
  geom_hline(yintercept=mean(spain.data$Score), color=scales::alpha('red',.35), size=0.5)+
  geom_hline(yintercept=mean(germany.data$Score), color="skyblue", size=0.5)
  

#graphics

#SPAIN
#x11()
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
