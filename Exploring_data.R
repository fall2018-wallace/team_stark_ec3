
d<- clean_data
head(d)
library(dplyr)
x<- group_by(d, d$Gender)
x1<- summarise(x,cou= n())
o<- pie(x1$cou)
