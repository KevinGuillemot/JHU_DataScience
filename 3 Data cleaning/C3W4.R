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
gdpmil<-as.character(gdp$X.3)
nocomma<-gsub(",","",gdpmil)
tonum<-as.numeric(nocomma)
clean<-tonum[complete.cases(tonum)]
mean(clean)
