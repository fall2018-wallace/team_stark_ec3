
library(jsonlite)
library(dplyr)
library(kernlab)
library(randomForest)

data <- data
data$Satisfaction = as.character(data$Satisfaction)
kl <- data[data$Satisfaction > 3.5,]

data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)

str(data)Gender
data_clean <- data[,c("Airline.Status", "Gender", "Type.of.Travel", "Class", "Airline.Code", "Airline.Name", "Flight.cancelled", "Arrival.Delay.greater.5.Mins", "Overall_Satisfaction")]

repeating_sequence=rep.int(seq_len(nrow(data_clean)), data_clean$Overall_Satisfaction)

Clean_dataset=data_clean[repeating_sequence,]
Naive_Bayes_Model=naiveBayes(Overall_Satisfaction ~., data=Clean_dataset)
Naive_Bayes_Model


NB_Predictions=predict(Naive_Bayes_Model,Clean_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Clean_dataset$Overall_Satisfaction)



#Getting started with Naive Bayes in mlr
#Install the package
#install.packages(“mlr”)
#Loading the library
library(mlr)

#Create a classification task for learning on Titanic Dataset and specify the target feature
task = makeClassifTask(data = Clean_dataset, target = "Overall_Satisfaction")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model


#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Clean_dataset[,1:8]))

##Confusion matrix to check accuracy
table(predictions_mlr[,1],Clean_dataset$Overall_Satisfaction)
  
  
  
  
## Random Forest
Rf_fit<-randomForest(formula=Overall_Satisfaction~., data=data_clean)

print(Rf_fit)
importance(Rf_fit)
