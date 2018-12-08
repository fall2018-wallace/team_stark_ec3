
d<- data
str(d)

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


d$No..of.other.Loyalty.Cards_b<- replicate(length(d$No..of.other.Loyalty.Cards), "Average")
d$No..of.other.Loyalty.Cards_b[as.numeric(d$No..of.other.Loyalty.Cards)<5 ]<- "Low"
d$No..of.other.Loyalty.Cards_b[as.numeric(d$No..of.other.Loyalty.Cards)>8]<- "High"
d$No..of.other.Loyalty.Cards_b<- as.factor(d$No..of.other.Loyalty.Cards_b)

d$Shopping.Amount.at.Airport_b<- replicate(length(d$Shopping.Amount.at.Airport), "Zero")
d$Shopping.Amount.at.Airport1<- d$Shopping.Amount.at.Airport!= 0
q1 <- quantile(d$Shopping.Amount.at.Airport1 , c(0.4, 0.6))
d$Shopping.Amount.at.Airport_b[d$Shopping.Amount.at.Airport <= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$Shopping.Amount.at.Airport_b[d$Shopping.Amount.at.Airport > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$Shopping.Amount.at.Airport_b<- as.factor(d$Shopping.Amount.at.Airport_b) ## the new column is conevrted in factor class

d$Eating.and.Drinking.at.Airport_b<- replicate(length(d$Shopping.Amount.at.Airport), "Zero")
d$Eating.and.Drinking.at.Airport1<- d$Shopping.Amount.at.Airport!= 0
q1 <- quantile(d$Eating.and.Drinking.at.Airport1 , c(0.4, 0.6))
d$Eating.and.Drinking.at.Airport_b[d$Eating.and.Drinking.at.Airport <= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$Eating.and.Drinking.at.Airport_b[d$Eating.and.Drinking.at.Airport > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$Eating.and.Drinking.at.Airport_b<- as.factor(d$Eating.and.Drinking.at.Airport_b) ## the new column is conevrted in factor class
head(d,1)

d$Day.of.Month_b<- replicate(length(d$Day.of.Month), "Mid")
d$Day.of.Month_b[as.numeric(d$Day.of.Month)<= 10 ]<- "Start"
d$Day.of.Month_b[as.numeric(d$Day.of.Month)>= 20]<- "End"
d$Day.of.Month_b<- as.factor(d$Day.of.Month_b)

d$Scheduled.Departure.Hour_b[(d$Scheduled.Departure.Hour)<= 4 ]<- "After Midnight"
d$Scheduled.Departure.Hour_b[(d$Scheduled.Departure.Hour)>=  5 & (d$Scheduled.Departure.Hour)<=11]<- "Morning"
d$Scheduled.Departure.Hour_b[(d$Scheduled.Departure.Hour)>=  12 & (d$Scheduled.Departure.Hour)<=15]<- "Day"
d$Scheduled.Departure.Hour_b[(d$Scheduled.Departure.Hour)>=  16 & (d$Scheduled.Departure.Hour)<= 20]<- "Evening"
d$Scheduled.Departure.Hour_b[(d$Scheduled.Departure.Hour)>=  21 & (d$Scheduled.Departure.Hour)<= 24]<- "Night"
d$Scheduled.Departure.Hour_b<- as.factor(d$Scheduled.Departure.Hour_b)

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

d$Flight.Distance_b<- replicate(length(d$Flight.Distance), "Zero")
q1 <- quantile(d$Flight.Distance , c(0.4, 0.6))
d$Flight.Distance_b[d$Flight.Distance <= q1[1]] <- "Low" ## vector having value lesser than 40th percentile are labelled as low
d$Flight.Distance_b[d$Flight.Distance > q1[2]] <- "High" ## vector having value greater than 60th percentile are labelled as high
d$Flight.Distance_b<- as.factor(d$Flight.Distance_b) ## the new column is conevrted in factor class

str(d)

d_factors<- d[, c(1,2,4,9,13,15,16,17,18,19,20,21,25,28,29,30,31,32,33,34,36,38,40,41,42,43,44,45)]
str(d_factors)

library(arules)
library(arulesViz)
library(methods)

hotelSurveyX <- as(d_factors,"transactions") ##data frame is converted into a sparse transactions matrix using as().

rules <- apriori(hotelSurveyX1,parameter = list(sup = 0.5, conf = 0.9), appearance = list(default ="lhs", rhs= ("Satisfaction_b=High"))) ##support value is approximately the proportion of the satisfied customers

inspect(rules) ## to view the ruleset

plot(rules) ## gives the scatter plot for the support and confidence of the 8 rules

goodrules <- rules[quality(rules)$support >0.55 & quality(rules)$confidence > .9312] ## to get the rules with highest confidence
inspect(goodrules) ## to view the ruleset
