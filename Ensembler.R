
data <- data
library('caret')
library(e1071)
library(dplyr)
library(kernlab)
library(randomForest)

data$Satisfaction <- as.character(data$Satisfaction)
data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)

dim(data)
rand_index <- sample(1:dim(data)[1])
summary(rand_index)
length(rand_index)

head(rand_index)

cut_point_2_by_3 <- floor(2 * dim(data)[1] / 3)

trainData <- data[rand_index[1: cut_point_2_by_3],]
testData <- data[rand_index[(cut_point_2_by_3 + 1) : dim(data)[1]], ]
outcomeName<-'Overall_Satisfaction'

predictors<-c("Airline.Status", "Age", "Gender", "Type.of.Travel", 
              "No.of.Flights.p.a.", "Arrival.Delay.greater.5.Mins", "Arrival.Delay.in.Minutes", "Departure.Delay.in.Minutes", 'Flight.time.in.minutes', 'No..of.other.Loyalty.Cards')

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)
