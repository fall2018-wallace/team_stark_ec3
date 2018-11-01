
dataframe1 <- data

dataframe1$satisfaction_numeric <- as.numeric(as.character(dataframe1$Satisfaction))

options(scipen = 999)
predictor_model <- lm(dataframe1$satisfaction_numeric  ~dataframe1$Type.of.Travel + dataframe1$Age + dataframe1$No.of.Flights.p.a., data = dataframe1)
summary(predictor_model)

graph <- plot(predictor_model)
graph <- abline(predictor_model)
