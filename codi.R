rm(list=ls())
library(readxl)
data <- read_excel("../nightingale-competition/datos_florence.xlsx", skip=1)
library(dplyr)
L <- nrow(data)
time_period <- seq(0,(L-1))
data$time_period <- time_period
data <- as.data.frame(data)

#Cols 3-5: deaths
#Cols 6-8: annual mortality rate (per 1000) ??
colnames(data)[1] <- "month"
colnames(data)[2] <- "avg_size_army"
colnames(data)[3] <- "zymotic"
colnames(data)[4] <- "injuries"
colnames(data)[5] <- "other"
colnames(data)[6] <- "zymotic_rate"
colnames(data)[7] <- "injuries_rate"
colnames(data)[8] <- "other_rate"

#Plots
library(dygraphs)
library(xts)
#Fer servir time_period és recomanable però transformant la data amb xts() queda l'eix X amb dates
data_plot_1 <- data.frame(
  time=data$time_period, 
  Zymotic=data$zymotic, 
  Injuries=data$injuries,
  Other=data$other)
dygraph(data_plot_1, main="Death causes")

data_plot_2 <- data.frame(
  time=data$time_period,
  Zymotic=data$zymotic_rate, 
  Injuries=data$injuries_rate,
  Other=data$other_rate)
dygraph(data_plot_2, main="Death causes (rates)")

#Mateix gràfic of course. Explicar per què (super basic) i escollir quin deixar, depèn de si comencem treballant amb nombres. Gràfic també amb avg_size_army per posar en perspectiva

#Aggregated deaths
deaths <- vector() 
for(i in 1:L){
  deaths[i] <- data$zymotic[i] + data$injuries[i] + data$other[i]
}
data$total_deaths <- deaths 

#Cumulative deaths
cum_deaths <- vector()
cum_deaths <- cumsum(deaths)
data_plot_3 <- data.frame(
  time=data$time_period,
  Accumulated_deaths=cum_deaths) #No m'acaba d'agradar el nom de la variable top-right. Millor data amb xts()?
dygraph(data_plot_3, main="Accumulated number of deaths")

#Podríem mirar d'informar-nos sobre què va fer realment ella i mirar de replicar-ho (senzill)
#Examinar relació avg_size_army amb les altres variables