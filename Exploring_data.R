
clean_data$Age
str(clean_data)
summary(clean_data)

#importing library for ggplot2
library("ggplot2")
#using ggplot() to generate a histogram 
# using geom_histogram() function with binwidth attribute to group the data 
# using 5 as binwidth 
my_Plot <- ggplot(clean_data, aes(x=Age))
my_Plot <- my_Plot + geom_histogram(binwidth = 5)
my_Plot <- my_Plot + ggtitle("Histogram of age")
my_Plot

# Histogram for the satisfaction
 
bar2<- ggplot(clean_data, aes(x=Class, y=Satisfaction))
bar2<- bar2 + geom_col()
bar2 <- bar2 + ggtitle("Bar chart for Class vs satisfaction")
bar2




#generating a barchart for age and satisfaction


bar1 <- ggplot(clean_data, aes(x=Age, y=Satisfaction))
bar1 <- bar1 + geom_col()
bar1 <- bar1 + ggtitle("Bar chart for age vs satisfaction")
bar1
