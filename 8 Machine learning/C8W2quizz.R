
# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)

#2
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

trainingCut<-training
ggplot(data = training,aes(y=CompressiveStrength,x=1:nrow(training)) ) +
    geom_point(aes(color=cut2(trainingCut$FlyAsh,g=5)))

#3
ggplot(data=training,aes(Superplasticizer))+
  geom_histogram()

#4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


selectFeaturesIndices <- grep("^IL",names(training),value = FALSE)
trainingSelect <- training[,selectFeaturesIndices]
selectFeaturesNames <- grep("^IL",names(training),value = TRUE)
names(trainingSelect) <- selectFeaturesNames

preProc<-preProcess(trainingSelect,method = "pca",thresh = 0.8)
trainPC<-predict(preProc,trainingSelect)

#5
New_training <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
New_testing <- data.frame(testing[,grep('^IL',names(testing))],testing$diagnosis)

NotPCFit <- train(training$diagnosis ~.,data = New_training, method="glm")
NotPCTestPredict <- predict(NotPCFit, New_testing[, -13])
confusionMatrix(New_testing$testing.diagnosis, NotPCTestPredict)




rm(list = ls())
#Close all graphs
graphics.off()

library(caret)
library(AppliedPredictiveModeling)

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# get the data with column names started with IL
New_training <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
New_testing <- data.frame(testing[,grep('^IL',names(testing))],testing$diagnosis)

# non-PCA
NotPCFit <- train(training.diagnosis ~.,data = New_training, method="glm")
NotPCTestPredict <- predict(NotPCFit, New_testing[, -13])
confusionMatrix(New_testing$testing.diagnosis, NotPCTestPredict)

# PCA
preProc <- preProcess(New_training[, -13],method="pca",thresh=.8)
trainPC <- predict(preProc, New_training[, -13])
testPC <- predict(preProc, New_testing[, -13])
# add the diagnosis into the trainPC data
trainPC <- data.frame(trainPC, training$diagnosis)

PCFit <- train(training.diagnosis ~.,data= trainPC, method="glm")
PCTestPredict <- predict(PCFit, testPC)
confusionMatrix(New_testing$testing.diagnosis, PCTestPredict)