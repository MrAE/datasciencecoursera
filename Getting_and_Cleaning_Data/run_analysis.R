###
### R script for Coursera Data Science 
### Getting and Cleaning Data 
###
### Jesse Leigh Patsolic 
### 2015 <StudioJLP@gmial.com> 
### S.D.G 
#
#! /usr/bin/R

###### INSTRUCTIONS ######
# You should create one R script called run_analysis.R 
#    that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard 
#    deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy 
#    data set with the average of each variable for each activity and 
#    each subject.



## This script assumes you are in the directory "UCI HAR Dataset/"


## READ IN THE DATA

Activity <- read.table("activity_labels.txt")[,2]

## Fix "Laying" to "Lying"
Activity <- gsub("LAYING", "LYING", Activity)


CNames <- as.vector(read.table("features.txt", row.names=1,stringsAsFactors=F)[,1])  

## Clean the names to be more easily readable
## using regex (poorly, but it works)
CNames <- gsub("(\\()","_", CNames,perl=T)
CNames <- gsub("(\\))", "", CNames,perl=T)
CNames <- gsub("(-+)", "_", CNames, perl=T)
CNames <- gsub(",(.)", "_\\U\\1", CNames, perl=T)
CNames <- gsub("(__)", "_", CNames, perl=T)
CNames <- gsub("_$", "", CNames, perl=T)

CNames <- c("SubjectNum", "ActivityNum", CNames)



Xtest <- read.table("test/X_test.txt")
testSubject<-read.table("test/subject_test.txt")
TestActivity <- read.table("test/y_test.txt")

## Merging the test data with subject and activity 
testData <- cbind(testSubject, TestActivity, Xtest)

Xtrain <- read.table("train/X_train.txt")
TrainActivity <- read.table("train/y_train.txt")
trainSubject <-
    read.table("train/subject_train.txt") 

## Merging the training data with subject and activity 
trainData <- cbind(trainSubject, TrainActivity, Xtrain)


## Selecting columns with mean and sd variables only
MeanCols <- CNames[grep("mean", CNames, ignore.case=T)]
sdCols <- CNames[grep("std", CNames, ignore.case=T, perl=T)]


## Merging training and test sets and label 
## columns appropriately
tdf <- rbind(trainData, testData); colnames(tdf) <- CNames;


## Substituting descriptive names for activities 
tdf$ActivityNum <- gsub("(1)", "WALKING", tdf$Activity, perl=T)
tdf$ActivityNum <- gsub("(2)", "WALKING_UPSTAIRS", tdf$Activity, perl=T)
tdf$ActivityNum <- gsub("(3)", "WALKING_DOWNSTAIRS", tdf$Activity, perl=T)
tdf$ActivityNum <- gsub("(4)", "SITTING", tdf$Activity, perl=T) 
tdf$ActivityNum <- gsub("(5)", "STANDING", tdf$Activity, perl=T) 
tdf$ActivityNum <- gsub("(6)", "LYING", tdf$Activity, perl=T)


### The dataframe 'tdf' now holds
### the test and training sets with 
### descriptive column names and 
### activity labels.  It may be written 
### out if the folloing line is uncommented.

#write.table(tdf, file="MergedData.csv", sep=",")


## Subsetting the data set to include 
## only the columns with 'mean' and 'sd'
tdf <- tdf[,c("SubjectNum", "ActivityNum", MeanCols,sdCols)]

## Making a char vector to label the
## activity for each subject appropriately
ActivityNums <- rep(Activity, 30) 

## Building the rows of the tidy data set 
## by subsetting 'tdf' by subject and activity.
L <- data.frame()
for(i in 1:30){
    for(j in 1:length(Activity)){
        L <-    rbind(L,
                   as.data.frame(lapply(tdf[tdf$Subj == i
                          & tdf$Ac == Activity[j],-2],
                         mean),
                   deparse.level=0))
        }
    }


## Creating the tidy data set
tds <- data.frame(Subject = L[,1],Activity = ActivityNums,L[,2:87])

colnames(tds) <- c( 
                "Subject", 
                "Activity", 
                paste(colnames(tds[,3:88]),"OverallMean", sep="_"))

write.table(tds, file="TidyDataSet.csv", row.names=FALSE, sep=",")


#   Time: 9 hours
##  Working status: Check
### Comments: NONE
####Soli Deo Gloria
