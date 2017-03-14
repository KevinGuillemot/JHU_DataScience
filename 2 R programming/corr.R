directory<-"rprog%2Fdata%2Fspecdata/specdata/"
threshold<-0
corr<-function(directory,threshold=0){
  correlations<-numeric()
  for (file in list.files(directory)){
    data<-read.csv(paste(directory,file,sep=""))
    if (sum(complete.cases(data))>threshold){
      clean<-data[complete.cases(data),]
      correlations<-c(correlations,cor(clean$sulfate,clean$nitrate))
    }
  }
  return(correlations)
}

cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))