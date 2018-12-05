
satSuv<-data

satSuv$satlevel[satSuv$Satisfaction <3] <- 'low'
satSuv$satlevel[satSuv$Satisfaction ==3] <- 'average'
satSuv$satlevel[satSuv$Satisfaction >3] <- 'high'
head(satSuv)
library(MASS)
tb1<-table(satSuv$Airline.Status,satSuv$satlevel)
tb1
chisq.test(tb1) 
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the airliene status.
# They are depended with each other.
tb2<-table(satSuv$Gender,satSuv$satlevel)
tb2
chisq.test(tb2) 
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the gender.
# They are depended with each other.
tb3<-table(satSuv$Type.of.Travel,satSuv$satlevel)
tb3
chisq.test(tb3)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the type of travel.
# They are depended with each other.
tb4<-table(satSuv$Class,satSuv$satlevel)
tb4
chisq.test(tb4)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the class.
# They are depended with each other.
tb5<-table(satSuv$Airline.Code,satSuv$satlevel)
tb5
chisq.test(tb5)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the airline code.
# They are depended with each other.
tb6<-table(satSuv$Airline.Name,satSuv$satlevel)
tb6
chisq.test(tb6)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the airline name.
# They are depended with each other.
tb7<-table(satSuv$Flight.cancelled,satSuv$satlevel)
tb7
chisq.test(tb7)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the cancelation of flight.
# They are depended with each other.
tb8<-table(satSuv$Arrival.Delay.greater.5.Mins,satSuv$satlevel)
tb8
chisq.test(tb8)
# P<0.01,reject the null hypothesis that the satisfaction level is independent of the Arrival Delay greater 5 Mins.
# They are depended with each other.

