# ########################################################################################
# Machine learning quizz 3
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# ########################################################################################
# Libraries
# ########################################################################################

library(AppliedPredictiveModeling)
library(caret)
library(pgmm)
library(ElemStatLearn)
library(dplyr)
library(rattle)

###########################################################################################
# Data
###########################################################################################
#Q1
data(segmentationOriginal)
training<-filter(segmentationOriginal,Case=="Train")
testing<-filter(segmentationOriginal,Case=="Test")
set.seed(125)
modelfit<-train(Class~.,method="rpart",data=training)
test1<-data.frame(TotalIntench2=23000,FiberWidthCh1 = 10, PerimStatusCh1=2)
test2<-data.frame(TotalIntench2 = 50000, FiberWidthCh1 = 10,VarIntenCh4 = 100)
test3<-data.frame(TotalIntench2 = 57000, FiberWidthCh1 = 8,VarIntenCh4 = 100)
test4<-data.frame(FiberWidthCh1 = 8,VarIntenCh4 = 100, PerimStatusCh1=2)
predict(modelfit,test2)
fancyRpartPlot(modelfit$finalModel)

#Q3
data(olive)
training = olive[,-1]
testing = as.data.frame(t(colMeans(olive)))

modFit<-train(Area~., method="rpart",data=training)
print(modFit$finalModel)

predict(modFit,newdata=testing)

#Q4
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit<-train(chd~age+alcohol+obesity+typea+tobacco+ldl,
              method="glm",
              family="binomial",
              data=trainSA)
trainPrecition <- predict(modFit,newdata=trainSA)
testPrecition <- predict(modFit,newdata=testSA)

missClass = function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}
missClass(trainSA$chd,trainPrecition)
missClass(testSA$chd,testPrecition)

#Q5
library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test)
rawTrain<-vowel.train
rawTest<-vowel.test
rawTrain$y<-factor(rawTrain$y)
rawTest$y<-factor(rawTest$y)
set.seed(33833)
rforsetFit<-randomForest(y~.,data=rawTrain,importance = FALSE)
order(varImp(rforsetFit), decreasing=T)

