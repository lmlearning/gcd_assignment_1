## load dplyr and tidyr for data manipulation
library(dplyr)
library(tidyr)

## read test data
testx <- read.table("UCI HAR Dataset/test/X_test.txt")
testy <- read.table("UCI HAR Dataset/test/Y_test.txt",col.names=c("Y"))
testsub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("SUB"))

## combine test data columns
combinedTest <- bind_cols(testsub,testx,testy)

## read training data
trainx <- read.table("UCI HAR Dataset/train/X_train.txt")
trainy <- read.table("UCI HAR Dataset/train/y_train.txt",col.names=c("Y"))
trainsub <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("SUB"))

## combine training data columns
combinedTrain <- bind_cols(trainsub,trainx,trainy)

## combine test and training data rows
combinedHAR <- bind_rows(combinedTest,combinedTrain)

## read feature names
features <- read.table("UCI HAR Dataset/features.txt")

## assign names to combined data set
names(combinedHAR) <- make.names(c("subject", as.character(features$V2), "activity"), unique=TRUE)

## filter columns so only mean and std columns are included
combinedHAR %>% select(subject, activity,  contains("mean"), contains("std")) -> meanAndStdHAR

## assign activity names
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
assignActivityName <- function(activityCode){ 
                            return(as.character(activity_labels[activity_labels$V1 == activityCode, ]$V2))  
                       }
meanAndStdHAR$activityName <- sapply(meanAndStdHAR$activity, assignActivityName)

## clean up names a bit
names(meanAndStdHAR) <- gsub("..", "", names(meanAndStdHAR), fixed=TRUE)

## create summary output
meanAndStdHAR %>% group_by(subject, activityName) %>% summarise_each(funs(mean)) %>% gather(measureName, measureMean, -(c(subject, activity, activityName)) ) %>% select(-activity) -> outputDs

## write output file
write.table(outputDs,file="output.txt", row.name = FALSE)
