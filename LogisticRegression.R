
satSuv<-data
# make satisfaction score categorical variable
satSuv$satlevel[satSuv$Satisfaction <3] <- 'low'
satSuv$satlevel[satSuv$Satisfaction ==3] <- 'median'
satSuv$satlevel[satSuv$Satisfaction >3] <- 'high'
satSuv<-satSuv[satSuv$satlevel != 'median',]
# transfer the categorical variables into factors.
satSuv$Airline.Status<-as.factor(satSuv$Airline.Status)
satSuv$Gender<-as.factor(satSuv$Gender)
satSuv$Type.of.Travel<-as.factor(satSuv$Type.of.Travel)
satSuv$Class <-as.factor(satSuv$Class)
satSuv$Airline.Code <-as.factor(satSuv$Airline.Code)
satSuv$Airline.Name<-as.factor(satSuv$Airline.Name)
satSuv$Flight.cancelled<-as.factor(satSuv$Flight.cancelled)
satSuv$Arrival.Delay.greater.5.Mins<-as.factor(satSuv$Arrival.Delay.greater.5.Mins)
satSuv$satlevel <-as.factor(satSuv$satlevel)

str(satSuv)

GLM1 <- glm(satlevel ~ Airline.Status +Age+ Gender +Price.Sensitivity+ 
    Year.of.First.Flight+ No.of.Flights.p.a. +X..of.Flight.with.other.Airlines+
     Type.of.Travel+No..of.other.Loyalty.Cards +Shopping.Amount.at.Airport+
     Eating.and.Drinking.at.Airport  +  Class +Day.of.Month+ Airline.Code+
    Scheduled.Departure.Hour+ Departure.Delay.in.Minutes+
    Arrival.Delay.in.Minutes +Flight.cancelled +Flight.time.in.minutes+
    Flight.Distance +Arrival.Delay.greater.5.Mins ,
    data = satSuv, family = binomial(link="logit"))
summary(GLM1)

GLM2 <- glm(satlevel ~ Airline.Status +Age+ Gender +Price.Sensitivity+ 
    No.of.Flights.p.a. +X..of.Flight.with.other.Airlines+
     Type.of.Travel+No..of.other.Loyalty.Cards +Shopping.Amount.at.Airport+
     Eating.and.Drinking.at.Airport  +  Class + 
    Scheduled.Departure.Hour+ 
    Flight.cancelled 
    +Arrival.Delay.greater.5.Mins ,
    data = satSuv, family = binomial(link="logit"))
summary(GLM2)

Residual_Deviance <- 43235
Null_Deviance <- 111244
pseudo_R <- 1 - (Residual_Deviance/Null_Deviance)
pseudo_R




