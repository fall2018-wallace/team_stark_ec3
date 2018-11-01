
satSuv<-data

satSuv$satlevel[satSuv$Satisfaction <2.5] <- 'low'
satSuv$satlevel[satSuv$Satisfaction >2 & satSuv$Satisfaction <4] <- 'median'
satSuv$satlevel[satSuv$Satisfaction >3.5] <- 'high'
head(satSuv)

satSuv<-satSuv[satSuv$satlevel != 'median',]
head(satSuv)
tail(satSuv)
