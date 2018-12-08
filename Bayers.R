
library(jsonlite)
library(kernlab)
library(randomForest)

data <- data
data$Satisfaction = as.character(data$Satisfaction)
kl <- data[data$Satisfaction > 3.5,]

data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)

data_clean <- data[,c("Airline.Status", "Gender", "Type.of.Travel", "Class", "Airline.Code", "Airline.Name", "Flight.cancelled", "Arrival.Delay.greater.5.Mins", "Overall_Satisfaction")]
