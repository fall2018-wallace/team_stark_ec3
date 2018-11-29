
d<- clean_data
head(d)
#library(dplyr)
#x<- group_by(d, d$Gender)
#x1<- summarise(x,cou= n())
#o<- pie(x1$cou)

d$Satisfaction_b<- replicate(length(d$Satisfaction), "Average")
d$Satisfaction_b[(d$Satisfaction)<3 ]<- "Low"
d$Satisfaction_b[(d$Satisfaction)>3.5]<- "High"
d$Satisfaction_b<- as.factor(d$Satisfaction_b)

q1 <- quantile(d$Age, c(0.4, 0.6))## to determine the values of the 40th and 60 the percentile
d$Age_b <- replicate(length(d$Age), "Average") ##A new colum is created replicating average upto the length of the vector
d$Age_b[d$Age<= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$Age_b[d$Age > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$Age_b<- as.factor(d$Age_b) ## the new column is conevrted in factor class

d$Price.Sensitivity_b<- replicate(length(d$Price.Sensitivity), "Average")
d$Price.Sensitivity_b[as.numeric(d$Price.Sensitivity)<3 ]<- "Low"
d$Price.Sensitivity_b[as.numeric(d$Price.Sensitivity)>3.5]<- "High"
d$Price.Sensitivity_b<- as.factor(d$Price.Sensitivity_b)

d$Year.of.First.Flight_b<- as.factor(d$Year.of.First.Flight) ## the new column is conevrted in factor class

q1 <- quantile(d$No.of.Flights.p.a. , c(0.4, 0.6))## to determine the values of the 40th and 60 the percentile
d$No.of.Flights.p.a._b <- replicate(length(d$No.of.Flights.p.a.), "Average") ##A new colum is created replicating average upto the length of the vector
d$No.of.Flights.p.a._b[d$No.of.Flights.p.a.<= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$No.of.Flights.p.a._b[d$No.of.Flights.p.a. > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$No.of.Flights.p.a._b<- as.factor(d$No.of.Flights.p.a._b) ## the new column is conevrted in factor class


q1 <- quantile(d$X..of.Flight.with.other.Airlines , c(0.4, 0.6))## to determine the values of the 40th and 60 the percentile
d$X..of.Flight.with.other.Airlines_b <- replicate(length(d$X..of.Flight.with.other.Airlines), "Average") ##A new colum is created replicating average upto the length of the vector
d$X..of.Flight.with.other.Airlines_b[d$X..of.Flight.with.other.Airlines<= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$X..of.Flight.with.other.Airlines_b[d$X..of.Flight.with.other.Airlines > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$X..of.Flight.with.other.Airlines_b<- as.factor(d$X..of.Flight.with.other.Airlines_b) ## the new column is conevrted in factor class

d$Departure.Delay.in.Minutes_b[as.numeric(d$Departure.Delay.in.Minutes)<= 60]<- "Less than an hour"
d$Departure.Delay.in.Minutes_b[as.numeric(d$Departure.Delay.in.Minutes)> 60]<- "More than an hour"
d$Departure.Delay.in.Minutes_b<- as.factor(d$Departure.Delay.in.Minutes)

d$Arrival.Delay.in.Minutes_b[as.numeric(d$Arrival.Delay.in.Minutes)<= 60]<- "Less than an hour"
d$Arrival.Delay.in.Minutes_b[as.numeric(d$Arrival.Delay.in.Minutes)> 60]<- "More than an hour"
d$Arrival.Delay.in.Minutes_b<- as.factor(d$Arrival.Delay.in.Minutes)

d$Flight.time.in.minutes_b<- replicate(length(d$Shopping.Amount.at.Airport), "Zero")
q1 <- quantile(d$Flight.time.in.minutes , c(0.4, 0.6))
d$Flight.time.in.minutes_b[d$Flight.time.in.minutes <= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$Flight.time.in.minutes_b[d$Flight.time.in.minutes > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$Flight.time.in.minutes_b<- as.factor(d$Flight.time.in.minutes_b) ## the new column is conevrted in factor class


str(d)

ageSat<-plot(d$Satisfaction_b, d$Age)
