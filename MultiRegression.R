

data <- data
str(data)  

library(ggplot2)
Status <- as.factor(data$Airline.Status)                                                # type safing it so that state is taken as factors for conversion
Airline_Status <- unclass(Status)



Status <- as.factor(data$Satisfaction)                                                # type safing it so that state is taken as factors for conversion
Categorical_Satisfaction <- unclass(Status)
data$Categorical_Satisfaction <- Categorical_Satisfaction

plot <- ggplot(data,aes(jitter(Airline_Status), Satisfaction))+ geom_point()  # ggplot function is called with 2 paraments the data source and the aes aesthetic mapping of hotelsize and overalcustomer satisfaction. These go into respective x and y axis.
plot  

data$Categorical_airline_status <- Airline_Status

str(data)
options(scipen = 999)                             # options(scipen =999) is used to eliminate the scintific notation representaiton and to get floating point representation.
predictor_model2 <- lm(data$Categorical_Satisfaction~data$Categorical_airline_status, data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model2)  

