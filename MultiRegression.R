


library(ggplot2)
Status <- as.factor(data$Airline.Status)                                                # type safing it so that state is taken as factors for conversion
data$Airline.Status <- unclass(Status)



Status <- as.factor(data$Satisfaction)                                                # type safing it so that state is taken as factors for conversion
data$Satisfaction <- unclass(Status)

travel_type <- as.factor(data$Type.of.Travel)
data$Type.of.Travel <- unclass(travel_type)

gender <- as.factor(data$Gender)
data$Gender <- unclass(gender)


class <- as.factor(data$Class)
data$Class <- unclass(class)





origin_city <- as.factor(data$Orgin.City)
data$Orgin.City <- unclass(origin_city)


origin_state <- as.factor(data$Origin.State)
data$Origin.State <- unclass(origin_state)



destination_city <- as.factor(data$Destination.City)
data$Destination.City <- unclass(destination_city)



destination_state <- as.factor(data$Destination.State)
data$Destination.State <- unclass(destination_state)


airline_name <- as.factor(data$Airline.Name)
data$Airline.Name <- unclass(airline_name)




cancelled <- as.factor(data$Flight.cancelled)
data$Flight.cancelled <- unclass(cancelled)


delay_five <- as.factor(data$Arrival.Delay.greater.5.Mins)
data$Arrival.Delay.greater.5.Mins <- unclass(delay_five)



str(data)
options(scipen = 999)                             # options(scipen =999) is used to eliminate the scintific notation representaiton and to get floating point representation.
predictor_model_airline_status <- lm(data$Satisfaction~data$Airline.Status , data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model_airline_status)

plot <- ggplot(data,aes(jitter(Airline.Status), Satisfaction))+ geom_point()  # ggplot function is called with 2 paraments the data source and the aes aesthetic mapping of hotelsize and overalcustomer satisfaction. These go into respective x and y axis.
plot


predictor_model_type_of_travel <- lm(data$Satisfaction~data$Type.of.Travel , data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model_type_of_travel)

plot1 <- ggplot(data,aes(jitter(Type.of.Travel), Satisfaction))+ geom_point()  # ggplot function is called with 2 paraments the data source and the aes aesthetic mapping of hotelsize and overalcustomer satisfaction. These go into respective x and y axis.
plot1


predictor_model_age <- lm(data$Satisfaction~data$Age , data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model_age)

plot2 <- ggplot(data,aes(jitter(Age), Satisfaction))+ geom_point()  # ggplot function is called with 2 paraments the data source and the aes aesthetic mapping of hotelsize and overalcustomer satisfaction. These go into respective x and y axis.
plot2

predictor_model_gender <- lm(data$Satisfaction~data$Gender , data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model_gender)

plot2 <- ggplot(data,aes(jitter(Age), Satisfaction))+ geom_point()  # ggplot function is called with 2 paraments the data source and the aes aesthetic mapping of hotelsize and overalcustomer satisfaction. These go into respective x and y axis.
plot2



predictor_model <- lm(data$Satisfaction~data$Airline.Status+data$Age+data$Gender+data$Type.of.Travel , data = data)
# the dendent variable ~ and list of independent variables appended by + with each variable
# the data source which is data frame data
summary(predictor_model)
