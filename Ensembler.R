

library('e1071')
library(tidyverse)
library(dplyr)
library(kernlab)
library(randomForest)

data <- data
summary(data$Flight.time.in.minutes)

abc <- c('Flight.date', 'Orgin.City', 'Origin.State', 'Destination.City', 'Destination.State')
data <- data[, -c(17, 18, 19, 20  )]
data$Satisfaction <- as.character(data$Satisfaction)
data[data$Satisfaction >= 3.5, "Overall_Satisfaction"] <- "Satisfied"
data[data$Satisfaction < 3.5, "Overall_Satisfaction"] <- "Unsatisfied"

data$Overall_Satisfaction <- as.factor(data$Overall_Satisfaction)
str(data)

unique(data$Price.Sensitivity)

data[data$Price.Sensitivity > 3, "Price_Sensitivity"] <- "High"
data[data$Price.Sensitivity <= 3, "Price_Sensitivity"] <- "Low"

data$Price_Sensitivity <- as.factor(data$Price_Sensitivity)

data[data$Flight.time.in.minutes >= 141, "Flight_time_in_minutes"] <- "High"
data[data$Flight.time.in.minutes < 141, "Flight_time_in_minutes"] <- "Low"
data$Flight_time_in_minutes <- as.factor(data$Flight_time_in_minutes)

#data_clean <- data[, c("Overall_Satisfaction", "Airline.Status", "Gender", "Price_Sensitivity", "Type.of.Travel",
         #              "Class", "Airline.Name", "Flight.cancelled", "Arrival.Delay.greater.5.Mins", "Flight_time_in_minutes" )]


data_clean <- data[, c("Overall_Satisfaction", "Airline.Status", "Gender", "Price_Sensitivity", "Type.of.Travel",
                       "Class", "Airline.Name", "Flight.cancelled", "Arrival.Delay.greater.5.Mins", "Flight_time_in_minutes" )]

library(dplyr)
test_data <- data_clean %>% group_by(Overall_Satisfaction) %>% filter(Type.of.Travel=="Business travel", Flight.cancelled =="No", Price_Sensitivity=="Low", Flight_time_in_minutes == "High")         #using library dplyr use groupby function to group by columns INJURy and filter on INJURY =YES values and count this subset of data using count()       
injury_incidents_dplyr

str(data)

Rf_fit<-randomForest(formula=Overall_Satisfaction~., data=data)
print(Rf_fit)
importance(Rf_fit)


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

model_rf<-train(trainData[,predictors],trainData[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

testData$pred_rf<-predict(object = model_rf,testData[,predictors])

confusionMatrix(testData$Overall_Satisfaction,testData$pred_rf)



############################################################
#KNN Model
trainknn <- trainData 
trainknn <- trainknn[,-1]
testknn <- testData
testknn <- testknn[,-1]
trainknn$Airline.Status <- as.integer(factor(trainknn$Airline.Status))
trainknn$Gender <- as.integer(factor(trainknn$Gender))
trainknn$Type.of.Travel <- as.integer(factor(trainknn$Type.of.Travel))
trainknn$Class <- as.integer(factor(trainknn$Class))
trainknn$Airline.Code <- as.integer(factor(trainknn$Airline.Code))
trainknn$Airline.Name <- as.integer(factor(trainknn$Airline.Name))
trainknn$Flight.cancelled <- as.integer(factor(trainknn$Flight.cancelled))
trainknn$Arrival.Delay.greater.5.Mins <- as.integer(factor(trainknn$Arrival.Delay.greater.5.Mins))


testknn$Airline.Status <- as.integer(factor(testknn$Airline.Status))
testknn$Gender <- as.integer(factor(testknn$Gender))
testknn$Type.of.Travel <- as.integer(factor(testknn$Type.of.Travel))
testknn$Class <- as.integer(factor(testknn$Class))
testknn$Airline.Code <- as.integer(factor(testknn$Airline.Code))
testknn$Airline.Name <- as.integer(factor(testknn$Airline.Name))
testknn$Flight.cancelled <- as.integer(factor(testknn$Flight.cancelled))
testknn$Arrival.Delay.greater.5.Mins <- as.integer(factor(testknn$Arrival.Delay.greater.5.Mins))

model_knn<-train(trainknn[,predictors],trainknn[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testknn$pred_knn<-predict(object = model_knn,testknn[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testknn$Overall_Satisfaction,testknn$pred_knn)




############################################################
#Logistic Regression

model_lr<-train(trainData[,predictors],trainData[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testData$pred_lr<-predict(object = model_lr,testData[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testData$Overall_Satisfaction,testData$pred_lr)



# ###########################################################
# #Averaging

testData$pred_rf_prob<-predict(object = model_rf,testData[,predictors],type='prob')
testData$pred_knn_prob<-predict(object = model_knn,testknn[,predictors],type='prob')
testData$pred_lr_prob<-predict(object = model_lr,testData[,predictors],type='prob')

testData$pred_avg<-(testData$pred_rf_prob$Satisfied+testData$pred_knn_prob$Satisfied+testData$pred_lr_prob$Satisfied)/3

#Splitting into binary classes at 0.5
testData$pred_avg<-as.factor(ifelse(testData$pred_avg>0.5,'Yes','No'))
matrix <- as.matrix(table(testData$Overall_Satisfaction,testData$pred_avg))
predication_power <- (matrix[1,2] + matrix[2,1]) / sum(matrix) 
predication_power
# 
# ###########################################################
# #Averaging
# testData$pred_rf_prob<-predict(object = model_rf,testData[,predictors],type='prob')
# testknn$pred_knn_prob<-predict(object = model_knn,testknn[,predictors],type='prob')
# testData$pred_lr_prob<-predict(object = model_lr,testData[,predictors],type='prob')
# 
# #Taking average of predictions
# testData$pred_avg<-(testData$pred_rf_prob+testknn$pred_knn_prob+testData$pred_lr_prob)/3
# 
# #Splitting into binary classes at 0.5
# testData$pred_avg<-as.factor(ifelse(testData$pred_avg>0.5,'Y','N'))
# 
# confusionMatrix(testSet$Loan_Status,testSet$pred_avg)
# 
# 
# 
# ############################################################
# #Majority Voting
# testData$pred_majority<-as.factor(ifelse(testData$pred_rf=='Y' & testknn$pred_knn=='Y','Y',ifelse(testData$pred_rf=='Y' & testData$pred_lr=='Y','Y',ifelse(testknn$pred_knn=='Y' & testData$pred_lr=='Y','Y','N'))))
# 
# confusionMatrix(testData$Overall_Satisfaction,testData$pred_majority)
# 
# 
# ###########################################################
# #Weighted Average
# testSet$pred_weighted_avg<-(testSet$pred_rf_prob$Y*0.25)+(testSet$pred_knn_prob$Y*0.25)+(testSet$pred_lr_prob$Y*0.5)
# 
# #Splitting into binary classes at 0.5
# testSet$pred_weighted_avg<-as.factor(ifelse(testSet$pred_weighted_avg>0.5,'Y','N'))
# 
# confusionMatrix(testSet$Loan_Status,testSet$pred_weighted_avg)
# 
# 


#######################################################################################################
#Stacking

#Step 1: Train the individual base layer models on training data
#Step 2: Predict using each base layer model for training data and test data

#Predicting the out of fold prediction probabilities for training data
trainData$OOF_pred_rf<-model_rf$pred$Satisfied[order(model_rf$pred$rowIndex)]
trainData$OOF_pred_knn<-model_knn$pred$Satisfied[order(model_knn$pred$rowIndex)]
trainData$OOF_pred_lr<-model_lr$pred$Satisfied[order(model_lr$pred$rowIndex)]


testData$OOF_pred_rf<-predict(model_rf,testData[predictors],type='prob')$Satisfied
testData$OOF_pred_knn<-predict(model_knn,testknn[predictors],type='prob')$Satisfied
testData$OOF_pred_lr<-predict(model_lr,testData[predictors],type='prob')$Satisfied


#Step 3: Now train the top layer model again on the predictions of the bottom layer models that has been made on the training data
#Predictors for top layer models 

predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_lr')
model_gbm<- 
  train(trainData[,predictors_top],trainData[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
testData$gbm_stacked<-predict(model_gbm,testData[,predictors_top])
confusionMatrix(testData$Overall_Satisfaction,testData$gbm_stacked)






###########################################################################################
#Sample Data testing

library(dplyr)
test_data_sample <- data %>% group_by(Overall_Satisfaction) %>% filter(Type.of.Travel=="Business travel", Flight.cancelled =="No", Price.Sensitivity== 1, Flight.time.in.minutes == 140)      

test_data_sample <- test_data_sample[1,]

test_data_knn <- test_data_sample
test_data_knn <- test_data_knn[, -which( colnames(test_data_knn)=="Overall_Satisfaction" )]
test_data_knn$Airline.Status <- as.integer(factor(test_data_knn$Airline.Status))
test_data_knn$Gender <- as.integer(factor(test_data_knn$Gender))
test_data_knn$Type.of.Travel <- as.integer(factor(test_data_knn$Type.of.Travel))
test_data_knn$Class <- as.integer(factor(test_data_knn$Class))
test_data_knn$Airline.Code <- as.integer(factor(test_data_knn$Airline.Code))
test_data_knn$Airline.Name <- as.integer(factor(test_data_knn$Airline.Name))
test_data_knn$Flight.cancelled <- as.integer(factor(test_data_knn$Flight.cancelled))
test_data_knn$Arrival.Delay.greater.5.Mins <- as.integer(factor(test_data_knn$Arrival.Delay.greater.5.Mins))

test_data_sample$OOF_pred_rf <- predict(model_rf,test_data_sample[predictors],type='prob')$Satisfied
test_data_sample$OOF_pred_knn<- predict(model_knn,test_data_knn[predictors],type='prob')$Satisfied
test_data_sample$OOF_pred_lr <- predict(model_lr,test_data_sample[predictors],type='prob')$Satisfied

test_data_sample$gbm_stacked <- predict(model_gbm,test_data_sample[,predictors_top])

confusionMatrix(test_data_sample$Overall_Satisfaction,test_data_sample$gbm_stacked)
