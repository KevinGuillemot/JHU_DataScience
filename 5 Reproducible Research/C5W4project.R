# ########################################################################################
# JHU Data Science - C5W4 project
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

library(reshape2)
library(dplyr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(knitr)


###########################################################################################
# Question framing
###########################################################################################

#General question - What is the goal of the analysis?
# Q: Can we measure the impact of severe weather events on health and economy

#Data question - Question reformulted in data analysis vocabulary
# Q: Which events are the most harmul in terms of health and economy

#Type of analysis - Descriptive/Exploratory/Inferential/Predictive
# A: Descriptive - look at all weather events


###########################################################################################
# Raw Data
###########################################################################################

#Type of Analysis - Ideal Dataset 
# Descriptive - Entire population
# NOAA storm database records all major events in the US and has the metrics we are concerned with
# Good dataset. Might have needed an updated one since many severe invents between 2012 and 2017

#Dataset chosen
# Dataset: spam.html
# Dataset desc: NOAA storm database
# Data date: 1950 - 2011
# Source: http://www.nws.noaa.gov/
# Dataset Date: 2017-04-06
# Sampling method: All events that cause significant health or economic damage by NWS standards
# Preprocessing: yes, but NA (Storm Data software program run by NWS)

#Enough memory to load data?
# 420MB

#Import data
rawData <- read.csv("repdata_data_StormData.csv.bz2")


###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawData)

#Data selection
selectData<-select(rawData,REFNUM,EVTYPE,BGN_DATE,STATE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>%
  filter(PROPDMGEXP %in% c("","K","M","B")) %>%
  filter(CROPDMGEXP %in% c("","K","M","B"))

#Modify damage unit in unit into $
cleanData<-mutate(selectData,PROPDMGDOL=PROPDMG*ifelse(PROPDMGEXP=="B",10^9,ifelse(PROPDMGEXP=="M",10^6,ifelse(PROPDMGEXP=="K",10^3,1)))) %>%
  mutate(CROPDMGDOL=CROPDMG*ifelse(CROPDMGEXP=="B",10^9,ifelse(CROPDMGEXP=="M",10^6,ifelse(CROPDMGEXP=="K",10^3,1)))) %>%
  mutate(TOTALDMGDOL=PROPDMGDOL+CROPDMGDOL) %>%
  select(REFNUM,EVTYPE,BGN_DATE,STATE,FATALITIES,INJURIES,TOTALDMGDOL)

#Missing Data
# No missing data after cleaning process
sum(!complete.cases(cleanData))

#Split dataset to study events with health vs material damages
healthData<-filter(cleanData, INJURIES!=0 | FATALITIES!=0)
economyData<-filter(cleanData, TOTALDMGDOL!=0)


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

# Injuries distribution
summary(healthData$INJURIES)
ggplot(data=healthData,aes(INJURIES))+
  geom_histogram()+
  labs(title="Injuries histogram")
#Distribution exhibits a high skew

# Fatalities distribution
summary(healthData$FATALITIES)
ggplot(data=healthData,aes(FATALITIES))+
  geom_histogram()+
  labs(title="Fatalities histogram")

# Damage distribution
summary(economyData$TOTALDMGDOL)
ggplot(data=economyData,aes(TOTALDMGDOL))+
  geom_density()+
  labs(title="Damage histogram")






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