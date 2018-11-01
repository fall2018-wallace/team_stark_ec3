
satSuv<-data

satSuv$satlevel[satSuv$Satisfaction <2.5] <- 'low'
satSuv$satlevel[satSuv$Satisfaction >2 & satSuv$Satisfaction <4] <- 'median'
satSuv$satlevel[satSuv$Satisfaction >3.5] <- 'high'
head(satSuv)

satSuv<-satSuv[satSuv$satlevel != 'median',]
GLM1 <- glm(satlevel ~ Airline.Status +Age+ Gender +Price.Sensitivity+ Year.of.First.Flight, data = mydata, family = "binomial")
