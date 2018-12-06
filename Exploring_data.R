
d<- clean_data
#head(d)
#library(dplyr)
#x<- group_by(d, d$Gender)
#x1<- summarise(x,cou= n())
#o<- pie(x1$cou)

# Need this line in our actual code to produce the correct output for Satifcation 
# categories 
d$Satisfaction<-as.numeric(as.character(d$Satisfaction))

d$Satisfaction_b<- replicate(length(d$Satisfaction), "Average")
d$Satisfaction_b[(d$Satisfaction)<3 ]<- "Low"
d$Satisfaction_b[(d$Satisfaction)>3.5]<- "High"
d$Satisfaction_b<- as.factor(d$Satisfaction_b)


q1 <- quantile(d$Age, c(0.4, 0.6))## to determine the values of the 40th and 60 the percentile
d$Age_b <- replicate(length(d$Age), "41-50") ##A new colum is created replicating average upto the length of the vector
d$Age_b[d$Age<= q1[1]] <- "15-40" ## vector having value lesser than 40th percentile are labelled as low
d$Age_b[d$Age > q1[2]] <- "51-85" ## vector having value greater than 60th percentile are labelled as high
d$Age_b<- as.factor(d$Age_b) ## the new column is conevrted in factor class

#d$Age_b<-round(d$Age_b)

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

# Stacked Bar Plot with Colors and Legend ## Hotel Satisfaction by Age Group
counts <- table(d$Age_b, d$Satisfaction)
barplot(counts, main="Overall Cust Satisfaction by Age Group",
        ylab = "Count by Age",
        xlab="Satisfaction Score", col=c("darkblue","red", "yellow"),
        legend = rownames(counts))

# Grouped Bar Plot ## Overall customer satisfaction by airline name (bar)
counts1 <- table(d$Satisfaction_b, d$Airline.Name)
op <- par(mar = c(10,4,4,2) + 0.1)
barplot(counts1, main="Overall Cust Satisfaction by Airline",
        xlab="Airlines", col=topo.colors(3),
        ylab = "Count of Satisfaction",
        beside=TRUE, las=2,cex.names=0.5,cex=0.5)
legend("topright", inset=.02, title="Satisfaction Group",
      c("Average","High","Low"),fill=topo.colors(3), horiz=TRUE, cex=0.5)
par(op)
op

# Pie Chart with Percentages
slices <- c(20.469, 79.53,0.001) 
lbls <- c("Low(<3)", "High(>3)", "Average(=3)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
p<-pie(slices,labels = lbls,main="Pie Chart of Customer satisfaction level")

