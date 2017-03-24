# ########################################################################################
# JHU Data Science - Course 5 Week2
# Reproducible research
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
# Q: Not defined in the assignment

#Data question - Question reformulted in data analysis vocabulary
# Q: Can I use step data to find a pattern in the sample we are being presented

#Type of analysis - Descriptive/Exploratory/Inferential/Predictive
# A: Exploratory. Absence of info over sampling rules out any relevant descriptive or inferential analysis


###########################################################################################
# Raw Data
###########################################################################################

#Type of Analysis - Ideal Dataset 
# Exploratory - Random sample with many variables
# Desc: sample seem to be far from random, group of enthusiasts who love sport and tech


#Dataset chosen
# Dataset: activity.csv
# Dataset desc: activity from quantified self movement devices
# Data date: Oct Nov 2012
# Source: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
# Dataset Date: 2017-03-24
# Sampling method: people interesting in sport and measuring their movements
# Preprocessing: Unkown

#Import data
fileLocation<-"repdata_data_activity/activity.csv"
rawActivity <- read.csv(fileLocation)



###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawActivity)

#Data type normalization
# Desc: num to factorfor interval
rawActivity$interval<-factor(rawActivity$interval)

#Missing values
noNaActivity<-rawActivity[complete.cases(rawActivity),]
imputedActivity<-impute.knn(data = rawActivity,k = 5,)


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



# Mean total number of steps taken per day

# Total number of steps per day
totalStepsPerDay<-group_by(noNaActivity,date) %>% summarise(totalSteps=sum(steps))

# Histogram of total number of steps taken each day
h <- ggplot(totalStepsPerDay,aes(x=totalSteps))
h + geom_histogram(bins = 50)

# Average and median of the total number of steps per day
avgTotalSteps <- mean(totalStepsPerDay$totalSteps)
medTotalSteps <- median(totalStepsPerDay$totalSteps)


# Average daily activity pattern

# Average number of steps by interval
avgStepsPerInterval<-group_by(noNaActivity,interval) %>% summarise(avgSteps=mean(steps))

# Time serie plot of average number of steps per interval
t <- ggplot(avgStepsPerInterval,aes(x=interval,y=avgSteps))
t + geom_line()

# Average with max number of steps
maxStepInterval <- avgStepsPerInterval$interval[which.max(avgStepsPerInterval$avgSteps)]









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