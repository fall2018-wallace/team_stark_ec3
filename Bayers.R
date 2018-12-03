
library(jsonlite)
library(kernlab)
library(randomForest)
library(mlr)

data <- data
data$Satisfaction = as.character(data$Satisfaction)
kl <- data[data$Satisfaction > 3.5,]

data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)

data_clean <- data[,c("Airline.Status", "Gender", "Type.of.Travel", "Class", "Airline.Code", "Airline.Name", "Flight.cancelled", "Arrival.Delay.greater.5.Mins", "Overall_Satisfaction")]

repeating_sequence=rep.int(seq_len(nrow(data_clean)), data_clean$Overall_Satisfaction)

Clean_dataset=data_clean[repeating_sequence,]
Naive_Bayes_Model=naiveBayes(Overall_Satisfaction ~., data=Clean_dataset)
Naive_Bayes_Model


NB_Predictions=predict(Naive_Bayes_Model,Clean_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Clean_dataset$Overall_Satisfaction)


