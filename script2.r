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

#All in one
dataset <- do.call("rbind", lapply(file_list, read.csv))

#General processing
all = general_dataset_processing(dataset)
data.spain = filter(all, grepl("ESP", Country))
data.germany = filter(all, grepl("GER", Country))

#Group based processing
#Add year to each dataframe
for (i in 1:length(processed_data))
{
  processed_data[[i]] = mutate(processed_data[[i]], Year = substr(file_list[i], 23, 31))
  i
}

#Stacking dataframes
data_per_year = do.call("rbind", processed_data)
puntos = c(10,
           5,
           6, 
           8 ,
           10,
           9, 
           13, 
           13, 
           5, 
           13, 
           13, 
           12, 
           15, 
           6, 
           8, 
           12, 
           13, 
           11, 
           14, 
           13, 
           12, 
           7, 
           9, 
           6, 
           8, 
           10, 
           8, 
           13, 
           13, 
           7, 
           11, 
           6, 
           10, 
           15, 
           9, 
           13, 
           12, 
           14, 
           8, 
           6, 
           12, 
           18, 
           9, 
           16, 
           9, 
           2, 
           10, 
           9, 
           10, 
           14, 
           9, 
           9, 
           12, 
           2, 
           10, 
           11, 
           7, 
           11, 
           10, 
           13, 
           16, 
           7, 
           10, 
           10, 
           13, 
           8, 
           7, 
           11, 
           11, 
           13, 
           12, 
           3, 
           10, 
           14, 
           11, 
           15, 
           5,
           8, 
           3, 
           6, 
           12, 
           13, 
           12, 
           9, 
           14, 
           7, 
           3, 
           11, 
           13, 
           13, 
           10, 
           9, 
           7, 
           14, 
           16, 
           11, 
           15, 
           13, 
           5, 
           13, 
           12, 
           11, 
           13, 
           13, 
           14, 
           12, 
           16, 
           13, 
           16, 
           1, 
           10, 
           15, 
           12, 
           10, 
           7, 
           13, 
           15, 
           18,
           10,
           15,
           13,
           8,
           13,
           14,
           16,
           6,
           6,
           6,
           15,
           5,
           12)

data_per_year$Points = puntos

#Separating data
spain.data = filter(data_per_year, grepl("ESP", data_per_year$Country))
germany.data = filter(data_per_year, grepl("GER", data_per_year$Country))

#Spanish group metrics
spain.means_col = setNames(aggregate(spain.data$Score, by=list(spain.data$Year), mean), c("Year", "Mean"))
spain.sd_col = setNames(aggregate(spain.data$Score, by=list(spain.data$Year), sd), c("Year", "Stdev"))
spain.points_means = setNames(aggregate(spain.data$Points, by=list(spain.data$Year), mean), c("Year", "Points"))
spain_group.data = merge(spain.means_col, spain.sd_col, by="Year")
spain_group.data = merge(spain_group.data, spain.points_means, by="Year")

#German group metrics
germany.means_col = setNames(aggregate(germany.data$Score, by=list(germany.data$Year), mean), c("Year", "Mean"))
germany.sd_col = setNames(aggregate(germany.data$Score, by=list(germany.data$Year), sd), c("Year", "Stdev"))
germany.points_means = setNames(aggregate(germany.data$Points, by=list(germany.data$Year), mean), c("Year", "Points"))
germany_group.data = merge(germany.means_col, germany.sd_col, by="Year")
germany_group.data = merge(germany_group.data, germany.points_means, by="Year")


#x11()
#ggplot(data=data_per_year, aes(x=Year, y=Score, color=Country)) +
#  geom_histogram(fill="white", position="dodge")+
#  theme(legend.position="top")
# Add mean lines
#ggplot(df, aes(x=weight, color=sex)) +
#  geom_histogram(fill="white", position="dodge")+
#  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
#             linetype="dashed")+
#  theme(legend.position="top")

x11()
ggplot(data = data_per_year,aes(y = Score, x = Year, color=Country)) +
  geom_point(size=3)+
  geom_hline(yintercept=mean(spain.data$Score), color=scales::alpha('red',.35), size=1)+
  geom_hline(yintercept=mean(germany.data$Score), color="skyblue", size=1) +
  ggtitle("Goles por equipos agrupados por país distribuido por años.") +
  labs(x="Año", y="Goles")
ggsave("Goals per Country.png", path="./plots")

x11()
# linear trend + confidence interval
ggplot(data=data_per_year, aes(x=Score, y=Points, color=Country)) +
ggtitle("Relación entre goles y puntos obtenidos por equipo, agrupados por país") +
geom_point(size = 4) +
geom_smooth(method=lm ,  se=T)+
  labs(x="Goles", y="Puntos")
ggsave("Relation Score and total Points per team.png", path="./plots")

x11()
# linear trend + confidence interval
ggplot(data=data_per_year, aes(x=Score, y=Points, color=Country)) +
  geom_point(size = 4) +
  ggtitle("Relación global entre goles y puntos obtenidos por equipo, agrupados por país") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=T)
ggsave("Relation Score and total Points per team global.png", path="./plots")


x11()
ggplot(all, aes(Score, group = Country)) + 
  geom_bar(aes(y = ..prop.., fill = Country)) +
  ggtitle("Distribución de goles por partido, separado por país/liga de origen") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frecuencia relativa") +
  xlab("Goles")+
  facet_grid(~Country)
ggsave("Goals distribution per match.png", path="./plots")

#datos

#por partido
t.test(spain.data$Score, germany.data$Score, var.equal=TRUE)
#por liguilla
t.test(spain_group.data$Mean, germany_group.data$Mean, var.equal=TRUE)

#Correlación global entre goles y puntos
cor(data_per_year$Score, data_per_year$Points)

#Correlación entre goles y puntos España
cor(spain.data$Score, spain.data$Points)

#Correlación entre goles y puntos Alemania
cor(germany.data$Score, germany.data$Points)

rm(spain.sd_col, spain.points_means, spain.means_col, germany.means_col, germany.points_means, germany.sd_col)
#graphics
