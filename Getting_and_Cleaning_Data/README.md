# README 

This is the course project README for the Coursera 
[Data Science Specialization](https://www.coursera.org/specialization/jhudatascience/1?utm_medium=listingPage) 
Getting and Cleaning Data course.  The explanations of the variables
and experiment that generated the data are given in the CodeBook.md 

The data used for this project are located here: 

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

#### The instructions are as follows:

You should create one R script called run_analysis.R that does the
following.
 
1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for
each measurement. 
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names. 
5.  From the data set in step 4, creates a second, independent tidy data set
with the average of each variable for each activity and each subject.


### To Run 
To work, the script should be in the 'UCI HAR Dataset' directory.
To run, just execute the script from terminal via 'Rscript
run_analysis.r' or run it from RStudio.


### Output

A csv file named "TidyDataSet.csv" that merges the test and training
sets taking only the columns of variables of means and standard
deviations. 

