# Getting and Cleaning Data, Course Project

## About
This repo contains the files necessary for completing the course project for the Coursera course in Getting and Cleaning Data. This involves analysing the data from the [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](UCI HAR Dataset) and generating a tidy output dataset based on the raw data.

## Project Description
The description of the task required for successful completion of this project is as follows:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Files
The repo contains two primary files:
- run_analysis.R, the R script used to process the raw data and generate an output data file
- CodeBook.RMS, a description of the variables in the output dataset generated by run_analysis.R

## Main Logic 
The run_analysis.R script conducts the following steps in order to produce the output data file:
- Loads relevant libraries required for the analysis (dplyr, tidyr)
- Reads the test data files into memory and combines their columns
- Reads the training data files into memory and combines their columns
- Combines the rows of the test and training data sets into a joint dataset
- Assigns appropriate names to the columns based on the features described in the source
- Downselects the columns to only use those related to mean or std
- Maps activity numbers to meaningful activitynames
- Summarises the mean and std of the relevant columns by subject and activity
- Writes the ouput file

## Usage
To use this script put the file run_analysis.R in your working directory along with the raw data files in a subfolder named "UCI HAR Dataset".

Then run source("run_analysis.R") this will generate the output datafile named "output.txt" in the working directory.