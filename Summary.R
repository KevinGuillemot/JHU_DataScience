# ########################################################################################
# JHU Data Science - Summary
# Kevin Guillemot
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()


# ########################################################################################
# Libraries
# ########################################################################################

library(data.table)
library(reshape2)
library(dplyr)
library(impute)
library(ggplot2)
library(RColorBrewer)
library(knitr)

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
data("spam")
rawData <- mtcars


###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawData)
# Observations in rows 
# Feature in columns with descriptive names (not duplicated, no special characters except . and _)
# One table per type of variable (and per file)
# If multiple tables commum columns to perform joins
# Char should be made into factors when appropriate and should be descriptive (instead of "0" and "1")

#Data type normalization
# Desc: char to factor or build factor
rawData$cyl<-factor(rawData$cyl)
cutpoints<- quantile(rawData$mpg, seq(0,1,length=4),na.rm = TRUE)
rawData$mpgFactor<-cut(rawData$mpg,cutpoints)

#Missing Data
# Desc: leave/impute
sum(!complete.cases(rawData))
rawData<-impute.knn(rawData)$data

#Subsampling
# Desc: None
trainIndicator <- rbinom(nrow(rawData), size = 1, prob = 0.5)
rawTrain <- rawData[trainIndicator == 1,]
rawTest <- rawData[trainIndicator == 0,]


###########################################################################################
# EDA
###########################################################################################
#GGplot options
#Scatter: +geom_point()
#Scatter w color/factor: geom_point(aes(color=cyl))
#Lines: +geom_smooth()
#Regression total: +geom_smooth(method = "lm")
#Regression/factor: +geom_smooth(aes(color=cyl),method = "lm")
#Pannels/factor: +facet_grid(.~drv)
#Theme: +theme_bw()
#Title/Axis: +labs(title="hwy")+labs(x="displ",y="hwy")
#Points attributes: geom_point(color="steelblue",size=2,alpha=0.5)


#Factors count
# Desc: validate subsampling
table(rawData$cyl)
table(rawTrain$cyl)
table(rawTest$cyl)

#Numerical summaries
summary(rawData)
tapply(rawData$mpg,rawData$cyl,mean)

#Relationship between variables
pairs(rawData)

#Numeric vs factor - boxplot
ggplot(rawData, aes(x=cyl, y=mpg)) + geom_boxplot() +labs(title="mpg vs cyl")
#Difference in mean? also check if variable needs to be scaled (log or so on)

#Hierarchical clustering
hCluster<-hclust(dist(rawData[,1:2]))
plot(hCluster)
#Few groups

#Heatmap
heatmap(as.matrix(rawData))

#Kmeans
kmeansObj<-kmeans(dataFrame, centers = 3)
kmeansObj$cluster
kmeansObj$centers
plot(x,y,col=kmeansObj$cluster,pch=19)
points(kmeansObj$centers,col=1:3,pch=3)



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