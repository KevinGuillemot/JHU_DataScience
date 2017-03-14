fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
fileName<-basename(fileUrl)
download.file(fileUrl, destfile=fileName, method="auto")
rawData<-read.csv(fileName)
sum(rawData$VAL[complete.cases(rawData$VAL)]==24)

library(xlsx)
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
fileName<-basename(fileUrl)
download.file(fileUrl, destfile=fileName, method="auto", mode='wb')
rawData<-read.xlsx(fileName,sheetIndex=1,rowIndex=2:400,colIndex=1:5,header=TRUE)
dat<-read.xlsx(fileName,sheetIndex=1,rowIndex=8:23,colIndex=7:15,header=TRUE)

library(XML)
fileUrl<-"http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc<- xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
data<-xpathSApply(rootNode,"//zipcode",xmlValue) 
sum(data==21231)

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
fileName<-basename(fileUrl)
download.file(fileUrl, destfile=fileName, method="auto")
DT<-fread(fileName)

ptm <- proc.time()
invisible(replicate(1000,DT[,mean(pwgtp15),by=SEX]))
proc.time() - ptm

ptm <- proc.time()
invisible(replicate(1000,mean(DT[DT$SEX==1,]$pwgtp15)))
invisible(replicate(1000,mean(DT[DT$SEX==2,]$pwgtp15)))
proc.time() - ptm

ptm <- proc.time()
invisible(replicate(1000,sapply(split(DT$pwgtp15,DT$SEX),mean)))
proc.time() - ptm

ptm <- proc.time()
rowMeans(DT)[DT$SEX==1]
rowMeans(DT)[DT$SEX==2]
proc.time() - ptm

ptm <- proc.time()
invisible(replicate(1000,mean(DT$pwgtp15,by=DT$SEX)))
proc.time() - ptm

ptm <- proc.time()
invisible(replicate(1000,tapply(DT$pwgtp15,DT$SEX,mean)))
proc.time() - ptm


53
tidy
365
127 for 21231
