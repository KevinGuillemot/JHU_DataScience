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

###########################################################################################
# Data
###########################################################################################

data(olive)
training = olive[,-1]

testing = as.data.frame(t(colMeans(olive)))

# Training
modFit<-train(Area~., method="rpart",data=training)
print(modFit$finalModel)

#Prediction
predict(modFit,newdata=testing)