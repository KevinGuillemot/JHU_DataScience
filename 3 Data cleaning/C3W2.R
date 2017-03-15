# ########################################################################################
# JHU Data Science - Course 3 Week 2 Quizz
# Data cleaning
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# ########################################################################################
# Libraries
# ########################################################################################

library(sqldf)
library(XML)
library(httr)

###########################################################################################
# Data import and cleaning
###########################################################################################

#Read raw file
fileName<-"getdata%2Fdata%2Fss06pid.csv"
acs<-read.csv(fileName)

#SQL select
sqldf("select pwgtp1 from acs where AGEP < 50")


#Read html
htmlConnect = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(htmlConnect)
close(htmlConnect)

nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

fileName<-"getdata%2Fwksst8110.for"
df<-read.fwf(fileName,widths = c(10,4,5,4,4,5,5,5,5,5),skip=4,header = FALSE)
sum(df$V6)