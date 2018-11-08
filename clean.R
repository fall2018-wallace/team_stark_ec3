
head(raw_data)
sum(is.na(raw_data))
sum(is.null(raw_data))
clean_data<- data[is.na(data)]<-0
sum(is.na(clean_data))
length(raw_data)
