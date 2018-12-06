
library(jsonlite)

library(dplyr)
library(kernlab)

data <- data
kl <- data[data$Satisfaction > 3.5,]

data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)
str(data)

data_clean <- data[, c("Overall_Satisfaction", "Airline.Status", "Gender", "Price.Sensitivity", "Type.of.Travel",
                       "Class", "Airline.Name", "Destination.City", "Flight.cancelled", "Arrival.Delay.greater.5.Mins")]

table(data_clean$Overall_Satisfaction)

dim(data_clean)
rand_index <- sample(1:dim(data_clean)[1])
summary(rand_index)
length(rand_index)

head(rand_index)

cut_point_2_by_3 <- floor(2 * dim(data_clean)[1] / 3)

trainData <- data_clean[rand_index[1: cut_point_2_by_3],]
testData <- data_clean[rand_index[(cut_point_2_by_3 + 1) : dim(data_clean)[1]], ]


str(trainData)
str(testData)

#kernfit <- ksvm(Overall_Satisfaction ~., data = trainData, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
#kernfit
