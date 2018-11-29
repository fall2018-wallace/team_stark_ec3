
head(raw_data)
sum(is.na(raw_data))
sum(is.null(raw_data))
raw_data[is.na(raw_data)]<-0
clean_data<- raw_data
sum(is.na(clean_data))
length(raw_data)

