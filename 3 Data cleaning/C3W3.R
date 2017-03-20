# ########################################################################################
# JHU Data Science - Course 2 Week 3 project
# R Programming
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
library(reshape2)
library(dplyr)
library(jpeg)

###########################################################################################
# Data
###########################################################################################

#Get built in R dataset
data("mtcars")
head(mtcars)

#Reshape dataset from many values in different columns to one value column and others defining id
mtcars$carname<-rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"), measure.vars=c("mpg","hp"))
head(carMelt)

#Summarize data from melte dataset
cylData<-dcast(carMelt,cyl~variable)
cylData<-dcast(carMelt,cyl~variable,mean)
tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg,mtcars$cyl),mean)
ddply(mtcars,.(cyl),summarize, mean=mean(mpg))

#Quiz
#1
df<-read.csv("getdata_data_ss06hid.csv")
str(df)
agricultureLogical<-df$ACR==3 & df$AGS==6
head(which(agricultureLogical))

#2
img<-readJPEG("getdata_jeff.jpg",native = TRUE)
quantile(img,0.3)

#3
gdp<-read.csv("getdata_data_GDP.csv")
statsdf<-read.csv("getdata_data_EDSTATS_Country.csv")
str(gdp)
str(statsdf)
gdp<-mutate(gdp,gdpRank=as.numeric(as.character(Gross.domestic.product.2012)))
gdp<-filter(gdp,!is.na(gdpRank))

length(intersect(gdp$X,statsdf$CountryCode))

m<-merge(gdp,statsdf,by.x = "X",by.y = "CountryCode",all = FALSE)
head(arrange(m,desc(gdpRank)),15)

#4
g<-group_by(m,Income.Group)
summarize(g,mean=mean(gdpRank))

#5
n<- mutate(m,quantile=ntile(gdpRank,5))