
head(raw_data)
sum(is.na(raw_data))
sum(is.null(raw_data))
clean_data<- na.omit(raw_data,stringsAsFactors = TRUE)
sum(is.na(clean_data))
clean_data
str(clean_data)
