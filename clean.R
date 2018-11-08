
sum(is.na(raw_data))
sum(is.null(raw_data))
colnames(raw_data)[colSums(is.na(raw_data)) > 0]
clean_data<- na.omit(raw_data)
sum(is.na(clean_data))


