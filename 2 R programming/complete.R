directory<-"rprog%2Fdata%2Fspecdata/specdata/"
id<-c(1)
complete<-function(directory, id=1:332){
  completes<-data.frame(id=numeric(),nobs=numeric())
  for (i in id){
    namefile<-paste(directory,sprintf("%03d", i),".csv",sep = "")
    data<-read.csv(namefile)
    nobs<-sum(complete.cases(data))
    completes[nrow(completes)+1,]=c(i,nobs)
  }
  return(completes)
}