rm(list=ls())
library(readxl)
data <- read_excel("../nightingale-competition/datos_florence.xlsx", skip=1)
str(data)
#Cols 3-5: deaths
#Cols 6-8: annual mortality rate (per 1000) 
library(dplyr)
L <- nrow(data)
time_period <- seq(0,(L-1)) #Podríem passar-ho de 1 a L
data$time_period <- time_period
data <- as.data.frame(data)

#Making everything more readable
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
library(xts) #Millor fer servir time_period però transformant la data amb xts() queda l'eix x amb dates al gràfic dinàmic
library(ggplot2)
data_plot_1 <- data.frame(
  time=data$time_period, 
  Zymotic=data$zymotic, 
  Injuries=data$injuries,
  Other=data$other)
dygraph(data_plot_1, main="Death causes (absolute values)")

data_plot_2 <- data.frame(
  time=data$time_period,
  Zymotic=data$zymotic_rate, 
  Injuries=data$injuries_rate,
  Other=data$other_rate)
dygraph(data_plot_2, main="Death causes (rates)")

data_plot_3 <- data.frame(
  time=data$time_period,
  Army=data$avg_size_army)
dygraph(data_plot_3, main="Average size of the army")

#Aggregated deaths
deaths <- vector() 
for(i in 1:L){
  deaths[i] <- data$zymotic[i] + data$injuries[i] + data$other[i]
}
data$total_deaths <- deaths #Morts totals en cada mes

#Cumulative deaths
cum_deaths <- vector()
cum_deaths <- cumsum(deaths)
data$cum_deaths <- cum_deaths #Cumulative deaths
data_plot_4 <- data.frame(
  time=data$time_period,
  Accumulated_deaths=cum_deaths) #No m'acaba d'agradar el nom de la variable top-right. Millor data amb xts()?
dygraph(data_plot_4, main="Accumulated number of deaths")

#Correlacions
corr1 <- cor(data$avg_size_army, data$zymotic) #Poden comparar-se/relacionar-se amb resultats de regressions 
corr2 <- cor(data$avg_size_army, data$injuries)
corr3 <- cor(data$avg_size_army, data$other)
corr4 <- cor(data$avg_size_army, data$total_deaths)

#Regressions
library(lmtest)
tra_death <- data.frame(y=data$total_deaths[2:L], lag1=data$total_deaths[1:(L-1)]) 
reg1 <- lm(y ~ lag1, data=tra_death) #Regressió morts avui en funció de les morts d'ahir
summary(reg1)

reg2 <- lm(data$total_deaths ~ data$time_period, data=data)
summary(reg2) #No significació del time period sobre total deaths

#Accuracy de reg1 per fer prediccions (he provat de fer la del número de morts basant-me només en un lag d'aquesta mateixa variable per provar de fer-ne alguna)
train_data <- subset(data, time_period<=round(0.7*L)) #70% training data
test_data <- subset(data, time_period>round(0.7*L)) #30% test/validation data
training1 <- data.frame(y=train_data$total_deaths, lag1=train_data$total_deaths) 
model1 <- lm(y ~ lag1, data=training1)
y.hat1 <- predict(model1, data=test_data)
error1 <- abs(y.hat1 - test_data$total_deaths)
rmse1 <- sqrt((sum(error1)**2)/nrow(test_data)) 
#Fatal performance, tampoc sé si té gaire sentit predir el número de morts futures a partir de les del dia anterior, ens caldria algo amb més chicha

#Prediccions