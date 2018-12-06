
data <- data
library(dplyr)
library(kernlab)
library(randomForest)

data$Satisfaction <- as.character(data$Satisfaction)
data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)

