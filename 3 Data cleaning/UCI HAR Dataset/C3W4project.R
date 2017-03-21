# ########################################################################################
# JHU Data Science - Course 3 Week 4 project
# Data Cleaning
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

library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)


###########################################################################################
# Raw Data
###########################################################################################

#Training set
rawTrainingSet <- fread("train/X_train.txt")
rawTrainingLabels <- fread("train/y_train.txt")       # Identifies activity for rows of rawTrainingSet
trainingSubjects <- fread("train/subject_train.txt")  # Identifies subject for rows of rawTrainingSet

#Testing set
rawTestingSet <- fread("test/X_test.txt")
rawTestingLabels <- fread("test/y_test.txt")          # Identifies activity for rows of rawTestingSet
testingSubjects <- fread("test/subject_test.txt")     # Identifies subject for rows of rawTestingSet

#Features of Testing/Training sets
features <- fread("features.txt")                     # Identifies feature measured for columns of rawTrainingSet and rawTestingSet 

#Activity labels
activityLabels <- fread("activity_labels.txt")        # Maps activity ID to human readable


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
featuresNames <- gsub("-","_",features$V2)
featuresNames <- gsub(",","_",featuresNames)
featuresNames <- gsub("\\(|\\)",".",featuresNames)


# 1 Merge training and testing sets
rawCombinedSet <- rbind(rawTrainingSet,rawTestingSet)

# 2 Extract only measurments on the mean and standard deviation
selectFeaturesNames <- grep("mean\\.|std\\.",featuresNames,value = TRUE)
selectFeaturesIndices <- grep("mean\\.|std\\.",featuresNames,value = FALSE)
rawMeanStdSet <- rawCombinedSet[,selectFeaturesIndices,with=FALSE]
names(rawMeanStdSet) <- selectFeaturesNames

# 3 
trainingLabels <- sapply(rawTrainingLabels,function(x){activityLabels$V2[x]})
testingLabels <- sapply(rawTestingLabels,function(x){activityLabels$V2[x]})
labels <- rbind(trainingLabels,testingLabels)

subjects <- rbind(trainingSubjects,testingSubjects)

cleanSet <- data.frame(subject=subjects,label=labels,as.data.frame(rawMeanStdSet))
names(cleanSet)[1]="subject"
names(cleanSet)[2]="activity"

#5
summaryMean <- group_by(cleanSet,subject,activity) %>% summarise_each(funs(mean))


#Data type normalization
# Desc: none

#Missing Data
# Desc: leave
sum(complete.cases(rawData))

#Subsampling
# Desc: None

