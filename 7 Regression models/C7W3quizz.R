# ########################################################################################
# C7W3 multiple regression quizz
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
library(GGally)
library(datasets)



#Q1

data(mtcars)
rawData<-mtcars
rawData$cyl=factor(rawData$cyl)
fitCars<-lm(mpg~cyl+wt,data = rawData)

#Q2
fitCars2<-lm(mpg~cyl,data = rawData)

#3
fitCars3<-lm(mpg~cyl*wt,data = rawData)

#4
fitCars4<-lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
f<-lm(y~x)
hatvalues(f)

#6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
f<-lm(y~x)
dfbetas(f)

#7