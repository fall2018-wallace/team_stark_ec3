
clean_data
str(clean_data)

#importing library for ggplot2
library("ggplot2")
#using ggplot() to generate a histogram 
# using geom_histogram() function with binwidth attribute to group the data 
# Since the age is below 100  we are using 10 as binwidth 
my_Plot <- ggplot(clean_data, aes(x=Age))
my_Plot <- my_Plot + geom_histogram(binwidth = 100)
my_Plot <- my_Plot + ggtitle("Histogram of age")
my_Plot
