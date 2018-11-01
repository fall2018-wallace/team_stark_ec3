
f<- lm(formula= as.numeric(d$Satisfaction)~ as.numeric(d$Age) + as.numeric(d$Price.Sensitivity )+ as.numeric(d$No.of.Flights.p.a.) + as.numeric(d$X..of.Flight.with.other.Airlines) + as.numeric(d$No..of.other.Loyalty.Cards)+ as.numeric(d$Shopping.Amount.at.Airport) + as.numeric(d$Day.of.Month) + as.numeric(d$Departure.Delay.in.Minutes) + as.numeric(d$Arrival.Delay.in.Minutes) + as.numeric(d$Flight.time.in.minutes) + as.numeric(d$Flight.Distance), data= d)
summary(f)
