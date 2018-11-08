
head(raw_data)
str(raw_data)
sum(is.na(raw_data))
sum(is.null(raw_data))
clean_data<- raw_data[is.na(raw_data)]<-0
sum(is.na(clean_data))
length(raw_data)
