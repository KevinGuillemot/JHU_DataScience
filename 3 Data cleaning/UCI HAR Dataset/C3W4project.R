# ########################################################################################
# JHU Data Science - Course 3 Week 4 project
# Data Cleaning
# Kevin Guillemot
# 
# ########################################################################################

# This script needs to be run in a folder containing UCI HAR Dataset as of 3/24/2017 at the following address
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

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
featuresNames <- features$V2

#Activity labels
activityLabels <- fread("activity_labels.txt")        # Maps activity ID to human readable


###########################################################################################
# Data cleaning
###########################################################################################


# 1 Merge training and testing sets
rawCombinedSet <- rbind(rawTrainingSet,rawTestingSet)



# 2 Extract only measurments on the mean and standard deviation

# Extract mean and std variables for data
selectFeaturesIndices <- grep("mean\\(|std\\(",featuresNames,value = FALSE)
rawMeanStdSet <- rawCombinedSet[,selectFeaturesIndices,with=FALSE]

# Extract mean and std variables names 
selectFeaturesNames <- grep("mean\\(|std\\(",featuresNames,value = TRUE)
names(rawMeanStdSet) <- selectFeaturesNames



# 3 Uses descriptive activity names to name the activities in the data set

# Map activity ID to its human readable description for testing and training sets
trainingLabels <- sapply(rawTrainingLabels,function(x){activityLabels$V2[x]})
testingLabels <- sapply(rawTestingLabels,function(x){activityLabels$V2[x]})
labels <- rbind(trainingLabels,testingLabels)

# Combine subjects for testing and training sets
subjects <- rbind(trainingSubjects,testingSubjects)

# Include human readable activity to the dataset
cleanSet <- data.frame(subject=subjects,label=labels,as.data.frame(rawMeanStdSet))
names(cleanSet)[1]="subject"
names(cleanSet)[2]="activity"



#4 Appropriately labels the data set with descriptive variable names
names(cleanSet) <- gsub("\\.","",names(cleanSet))
names(cleanSet) <- gsub("mean","Mean",names(cleanSet))
names(cleanSet) <- gsub("std","Std",names(cleanSet))
names(cleanSet) <- gsub("^(t)","time",names(cleanSet))
names(cleanSet) <- gsub("^(f)","freq",names(cleanSet))


#5 creates a second, independent tidy data set with the average of each variable for each activity and each subject
summaryMean <- group_by(cleanSet,subject,activity) %>% summarise_each(funs(mean))

write.table(cleanSet, './cleanSet.txt',row.names=TRUE)


