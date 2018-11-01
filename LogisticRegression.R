
satSuv<-data

satSuv$satlevel[satSuv$Satisfaction <2.5] <- 'low'
satSuv$satlevel[satSuv$Satisfaction >2 & satSuv$Satisfaction <4] <- 'median'
satSuv$satlevel[satSuv$Satisfaction >3.5] <- 'high'
head(satSuv)

satSuv<-satSuv[satSuv$satlevel != 'median',]
GLM1 <- glm(satlevel ~ Airline.Status +Age+ Gender +Price.Sensitivity+ 
    Year.of.First.Flight+ No.of.Flights.p.a. +X..of.Flight.with.other.Airlines
    + Type.of.Travel+No..of.other.Loyalty.Cards +Shopping.Amount.at.Airport
    +  Eating.and.Drinking.at.Airport  +  Class Day.of.Month+ Flight.date Airline.Code
    +Airline.Name+Scheduled.Departure.Hour+ Departure.Delay.in.Minutes+
    Arrival.Delay.in.Minutes +Flight.cancelled Flight.time.in.minutes
    +Flight.Distance +Arrival.Delay.greater.5.Mins ,
    data = -satSuv, family = "binomial")
summary(GLM1)
