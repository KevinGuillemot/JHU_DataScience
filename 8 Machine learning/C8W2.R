# ########################################################################################
# JHU Data Science - Summary
# Kevin Guillemot
# 
# ########################################################################################

# ########################################################################################
# R settings
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# Profiler
#system.time() for a single expression
#Rprof() keeps track of function call stack every 0.02 seconds and print it
#SummaryRprof() organizes. by.self option

# Debugger
#debug(lm) will enter debug mode next time function is called. n for next instruction


# ########################################################################################
# Libraries
# ########################################################################################
library(caret)
library(kernlab)
library(splines)

library(data.table)
library(reshape2)
library(dplyr)
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

#Enough memory to load data?
#1,000,000 rows and 100 columns of numeric data (8 bytes/numeric) =8.10^8 bytes = 800 MB

#Import data
data("spam")
rawData<-spam

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


#Missing Data
# Desc: leave/impute


#Subsampling
# Random
trainIndicator <- createDataPartition(y=rawData$type,p=0.75,list = FALSE)
rawTrain <- rawData[trainIndicator,]
rawTest <- rawData[-trainIndicator,]

#k folds
folds <- createFolds(y=rawData$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds,length)

#Resampling
folds<-createResample(y=rawData$type,times=10,list=TRUE)

#Time slices
tme<-1:1000
folds<-createTimeSlices(y=tme,initialWindow = 20,horizon = 10)

###########################################################################################
# EDA
###########################################################################################
#Show comparisons: a hypothesis is always relative to another
#Show multivariate data: world is multivariate

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

# Color options: 
# display.brewer.all()
# pal <- colorRamPalette(brewer.pal(3, "BuGn"))
# col=pal(10)

#Factors count
# Desc: validate subsampling

#Variability in features
nsv<-nearZeroVar(rawTrain,saveMetrics = TRUE)

#Distribution of a variable
ggplot(data=rawTrain,aes(capitalAve))+
  geom_histogram()
#Data is very skewed, standardize is
trainCapAve<-rawTrain$capitalAve
trainCapAve<-(trainCapAve-mean(trainCapAve))/sd(trainCapAve)
#!! modify the testing set with parameters estimated in the training only
testCapAve<-rawTest$capitalAve
trainCapAve<-(testCapAve-mean(trainCapAve))/sd(trainCapAve)
#can be done with package
preObj<-preProcess(rawTrain[,-58],method = c("center","scale"))

#Relationship between variables
featurePlot(x=rawTrain[,c("charExclamation","chaDollar")],
            y=rawTrain$wage,
            plot = "pairs")

#Create spline features
# creates variables with different degree polynomials to fit
bsBasis<-bs(rawTrain$charDollar,df=3)

#Find features with high correlation with themselves
correlationMatrix<-abs(cor(rawTrain[,-58]))
diag(correlationMatrix)<-0
which(correlationMatrix>0.8,arr.ind=T)

#Use pca to pick features
prcomp<-prcomp(rawTrain[,-58])
#or
preProc<-preProcess(rawTrain[,-58],method = "pca",pcaComp = 2)
trainPC<-predict(preProc,rawTrain[,-58])
#When testing use same
testPC<-predict(preProc,rawTest[,-58])
confusionMatrix(rawTest$type,predict(modelFit,testPC))


# Is data good enough to answer the initial question?
# Confounding: did we miss any important extranal variable
# A: Yes

# Modeling suggested by data
# Desc:

###########################################################################################
# Statistical Analysis
###########################################################################################

# Training

#Source of uncertainty:



#Choice of model:
# regression

modelFit<-train(type ~.,data=rawTrain, method="glm")
modelFit



# Test
predictedSpam=predict(modelFit,newdata=rawTest)


#Results

confusionMatrix(predictedSpam,rawTest$type)

#Residuals
#plot residuals by color of different features (ones not used in model)
#Residual by index to see if trend and identify in model
#actual vs predicted



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