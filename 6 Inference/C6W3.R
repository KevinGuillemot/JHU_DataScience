#1
nSamples<-9
avg<-1100
sd<-30
avg+sd/sqrt(nSamples)*qt(p = 0.975,df = nSamples-1)

#2
nSamples<-9
avg<- -2
2*sqrt(nSamples)/qt(p = 0.975,df = nSamples-1)

#4
newMean<-3
newVar<-0.6
oldMean<-5
oldVar<-0.68
n<-20
diffMean<-newMean-oldMean
diffSqrt<-sqrt((9*newVar+9*oldVar)/(20-2))
diffMean+diffSqrt*qt(p=0.975,df=n-2)*sqrt(1/10+1/10)

#6
n<-200
newMean<-4
newSd<-0.5
oldMean<-6
oldSd<-2
diffMean<-newMean-oldMean
diffSqrt<-sqrt((99*newSd^2+99*oldSd^2)/(200-2))
diffMean+diffSqrt*qt(p=0.975,df=n-2)*sqrt(1/100+1/100)

#7
n<-18
nPlacebo<-9
nTreatment<-9
meanPlacebo<-1
meanTreatment<--3
sdPlacebo<-1.8
sdTreatment<-1.5
diffMean<- meanTreatment-meanPlacebo
diffSd<-sqrt(((nPlacebo-1)*sdPlacebo^2+(nTreatment-1)*sdTreatment^2)/(n-2))
diffMean+qt(p = 0.95,df=n-2)*diffSd*sqrt(1/nPlacebo+1/nTreatment)