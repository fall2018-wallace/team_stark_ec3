
head(raw_data)
sum(is.na(raw_data))
sum(is.null(raw_data))
raw_data[is.na(raw_data)]<-0
clean_data<- raw_data
clean_data<- clean_data[-38898:-38900,]
rownames(clean_data)<-NULL
sum(is.na(clean_data))
length(clean_data)

