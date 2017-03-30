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
library(ggplot2)
library(gridExtra)
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
fileLocation<-"repdata%2Fdata%2Factivity/activity.csv"
rawActivity <- read.csv(fileLocation)



###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawActivity)

#Missing values are ignored
noNaActivity<-rawActivity[complete.cases(rawActivity),]


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
totalStepsPerDayNoNA<-group_by(noNaActivity,date) %>% summarise(totalSteps=sum(steps))

# Histogram of total number of steps taken each day
h <- ggplot(totalStepsPerDayNoNA,aes(x=totalSteps))
  + geom_histogram(bins = 50)



# Average and median of the total number of steps per day
avgTotalStepsNoNA <- mean(totalStepsPerDayNoNA$totalSteps)
medTotalStepsNoNA <- median(totalStepsPerDayNoNA$totalSteps)




# Average daily activity pattern

# Average number of steps by interval
avgStepsPerInterval<-group_by(noNaActivity,interval) %>% summarise(avgSteps=mean(steps))

# Time serie plot of average number of steps per interval
t <- ggplot(avgStepsPerInterval,aes(x=interval,y=avgSteps))
t + geom_line()

# Average with max number of steps
maxStepInterval <- avgStepsPerInterval$interval[which.max(avgStepsPerInterval$avgSteps)]




# Imputing missing values

# Total number of missing values
nbNAs<-sum(!complete.cases(rawActivity))

# Missing values are replaced by average on the 5 min interval
imputedActivity <- inner_join(rawActivity,avgStepsPerInterval)
imputedActivity$steps[!complete.cases(imputedActivity)] = imputedActivity$avgSteps[!complete.cases(imputedActivity)]
imputedActivity <- select(imputedActivity,steps,date,interval)

# Total number of steps per day
totalStepsPerDayImp<-group_by(imputedActivity,date) %>% summarise(totalSteps=sum(steps))

# Histogram of total number of steps taken each day
h <- ggplot(totalStepsPerDayImp,aes(x=totalSteps))
h + geom_histogram(bins = 50)

# Average and median of the total number of steps per day
avgTotalStepsImp <- mean(totalStepsPerDayImp$totalSteps)
medTotalStepsImp <- median(totalStepsPerDayImp$totalSteps)




#differences in activity patterns between weekdays and weekends?

# Add weekday variable to dataset
imputedActivity <- mutate(imputedActivity, weekday = weekdays(as.Date(date)) ) 

# Compute average steps on weekdays and on weekend
weekendActivity <- filter(imputedActivity,weekday %in% c("Saturday","Sunday")) %>%
                    select(steps,date,interval) %>%
                    group_by(interval) %>% 
                    summarise(avgSteps=mean(steps))
weekdaysActivity <- filter(imputedActivity,weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
                    select(steps,date,interval) %>%
                    group_by(interval) %>% 
                    summarise(avgSteps=mean(steps))

# Plot weekdays and weeks activity side by side
weekendPlot <- ggplot(weekendActivity,aes(x=interval,y=avgSteps)) +
                geom_line() +
                labs(title="Weekend", x="",y="Avg Number of steps") +
                theme(plot.title = element_text(hjust = 0.5))
weekdaysPlot <- ggplot(weekdaysActivity,aes(x=interval,y=avgSteps)) +
                geom_line() +
                labs(title="Weekday", x="Interval",y="Avg Number of steps") +
                theme(plot.title = element_text(hjust = 0.5))
grid.arrange(weekendPlot,weekdaysPlot,ncol=1)
       
