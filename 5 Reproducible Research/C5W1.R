# ########################################################################################
# JHU Data Science - Course 5 Week 1
# Reproducible Research
# 
# ########################################################################################


# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()


# ########################################################################################
# Libraries
# ########################################################################################

library(ggplot2)
library(kernlab)
library(boot)


###########################################################################################
# Question framing
###########################################################################################

#General question - What is the goal of the analysis?
# Q: Can I automatically detect spam emails?

#Data question - Question reformulted in data analysis vocabulary
# Q: Can I use quantitative characteristics of the emails to classify them as spam?

#Type of analysis - Descriptive/Exploratory/Inferential/Predictive
# A: Predictive. Given email, predict if it is spam


###########################################################################################
# Raw Data
###########################################################################################

#Type of Analysis - Ideal Dataset 
# Descriptive - Entire population
# Exploratory - Random sample with many variables
# Inferential - Appropriate population and appropriate sampling
# Predictive - Trainging set and test set from the same appropriate population

#Dataset chosen
# Dataset: spam.html
# Dataset desc: UCI machine learning training set
# Data date: 1999-07-01
# Source: https://archive.ics.uci.edu/ml/datasets/Spambase
# Dataset Date: 2017-03-15
# Sampling method: UCI personnal emails
# Preprocessing: yes, but NA

#Import data
data(spam)
rawData <- spam

###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawData)
# type : factor classifiying spam and non spam
# others : frequency in email of the word stored in the variable name

#Data type normalization
# Desc: none

#Missing Data
# Desc: leave
sum(complete.cases(rawData))

#Subsampling
# Desc: Training set and Test set randomly split
trainIndicator <- rbinom(nrow(rawData), size = 1, prob = 0.5)
rawTrain <- rawData[trainIndicator == 1,]
rawTest <- rawData[trainIndicator == 0,]


###########################################################################################
# EDA
###########################################################################################

#Factors count
# Desc: validate subsampling
table(rawData$type)
table(rawTrain$type)
table(rawTest$type)

#Single variable vs factor
ggplot(rawTrain, aes(x=type, y=capitalAve)) + geom_boxplot()
#Skewed, log tranform
ggplot(rawTrain, aes(x=type, y=log10(1+capitalAve))) + geom_boxplot()
#Difference in mean

#Relationship between variables
plot(log10(rawTrain[,1:4]+1))

#Hierarchical clustering
hCluster<-hclust(dist(t(log10(rawTrain[,1:57]+1))))
plot(hCluster)
#Few groups

# Is data good enough to answer the initial question?
# A: Yes


###########################################################################################
# Statistical Analysis
###########################################################################################

# Training

#Source of uncertainty:


#Choice of model:
# regression

#Normalize variables
rawTrain$numType <- as.numeric(rawTrain$type)-1
costFunction <- function(x,y){ sum( x != (y>0.5)) }
cvError <- rep(NA, 55)

#Try different variables
for (i in 1:55){
  lmFormula = reformulate(names(rawTrain)[i],response="numType")
  glmFit = glm(lmFormula, family="binomial", data=rawTrain)
  cvError[i] = cv.glm(rawTrain, glmFit, costFunction, 2)$delta[2]
}
names(rawTrain)[which.min(cvError)]

#Use best model from the group
predictionModel = glm(numType~charDollar,family = "binomial", data=rawTrain)




# Test
predictedSpam=rep(NA,dim(rawTest)[1])

predictionTest=predict(predictionModel,rawTest)
predictedSpam[predictionModel$fitted > 0.5] = "spam"
predictedSpam[predictionModel$fitted <= 0.5] = "nonspam"

#Results

#Classification table
table(predictedSpam,rawTest$type)

#Key statistics



###########################################################################################
# Interpretation
###########################################################################################

#Type of analysis - Interpretation
#Describes
#Correlates/associated with
#Leads to
#Predicts

#Explanation
# Desc: Fraction of char that are $ sign can be used to predict if email is spam

#Interpret coefficients
# Desc: More $ signs means more Spam under our prediction
# Desc: Any email with more than 6.6% dollar signs is classified as spam


#Interpret measures of uncertainty
# Error rate was 22%


###########################################################################################
# Challenge results
###########################################################################################

#Challenge steps:
#Question
#Data source
#Processing
#Analysis
  #Challenge choice of terms to include in model
  #Challenge measures of uncertainty
  #Potential alternatives
#Interpretation


###########################################################################################
# Synthetize results
###########################################################################################

#Question

#Summarize analyses (only if needed and address a challenge)

#Order analyses according to story rather than chronogically

#Produce figures