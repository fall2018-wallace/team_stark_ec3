
satSurvey <- clean_data
#satSurvey[!complete.cases(satSurvey),]
colSums(is.na(satSurvey))
satSurvey<-na.omit(satSurvey)
colSums(is.na(satSurvey))
str(satSurvey)
hist(satSurvey$Shopping.Amount.at.Airport)#noticed that choosing outliers 
# is not an effective method.

hist(satSurvey$Eating.and.Drinking.at.Airport, bins=5, xlim = range(200), ylim=NULL)
# #noticed that choosing outliers is not an effective method.


hist(satSurvey$Day.of.Month)


### How to understand the data better
summary(satSurvey$Eating.and.Drinking.at.Airport)
#be able to see the 5 number summary of what amount of people are 
# speding  at a certain dollar value

sd(satSurvey$Eating.and.Drinking.at.Airport)
#standard deviation =  52.22347
quantile(satSurvey$Eating.and.Drinking.at.Airport, probs = c(.05,0.25,.50,.75,.95))
# 5% 25% 50% 75% 95% 
# 5  30  60  90 165 

summary(satSurvey$Class)
#There are 3 cataegories for classes. You can see how much of the data is in each
#category
summary(satSurvey$Day.of.Month)
# not useful, but the histogram is useful

summary(satSurvey$Flight.date)
#not useful
summary(satSurvey$Eating.and.Drinking.at.Airport)
# summary(satSurvey$Eating.and.Drinking.at.Airport)
#This is useful information
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   30.00   60.00   68.23   90.00  895.00 


summary(satSurvey$Airline.Code)
#you can see the total number of flights with each airline code

summary(satSurvey$Airline.Name)
#you can look at the total number of flights per airline

summary(satSurvey$Orgin.City)
#you can see what cities the total number of flights are coming out

summary(satSurvey$Origin.State)
#you can see what states the total number of flights are coming out

summary(satSurvey$Destination.City)
#you can see what cities the total number of flights are going to 
