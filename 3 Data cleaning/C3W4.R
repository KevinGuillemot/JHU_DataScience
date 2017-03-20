# ########################################################################################
# JHU Data Science - Course 3 Week 4
# Daat cleaning
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


###########################################################################################
# Data
###########################################################################################

#1
housing <- read.csv("getdata_data_ss06hid.csv")
str(housing)
strsplit(names(housing),"wgtp")[123]

#2
gdp<-read.csv("getdata_data_GDP.csv")
cleangdp<-gdp
cleangdp$X.3<-as.numeric(gsub(",","",as.character(cleangdp$X.3)))
cleangdp$Gross.domestic.product.2012<-as.numeric(as.character(cleangdp$Gross.domestic.product.2012))
mean(cleangdp$X.3[complete.cases(cleangdp$X.3,cleangdp$Gross.domestic.product.2012)])

