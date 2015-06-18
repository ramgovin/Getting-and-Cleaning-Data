## Getting and Cleaning Data
## Project - 2


## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.

## Libraries 
library('dplyr')

## Read general data 
## Features file
features = read.table('./features.txt',header=FALSE); 
## Activity_labels file
activityType = read.table('./activity_labels.txt',header=FALSE); 

## Read Train data 
## Subject_Train file 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
## x_Train file 
xTrain = read.table('./train/x_train.txt',header=FALSE); 
## y_Train file
yTrain = read.table('./train/y_train.txt',header=FALSE);


## Add readable column names for the train dataset
colnames(activityType) = c("activityId", "activityName")
colnames(subjectTrain) = c("subjectId")
colnames(yTrain) = c("activityId")

## Get all the feature names and map them as colnames for xTrain 
colnames(xTrain) = features[,2]

## Create the train data set by combining individual train data using cbind()
trainData<-cbind(yTrain, subjectTrain, xTrain)


## Read test data 
## Subject_Test file 
subjectTest = read.table('./test/subject_test.txt',header=FALSE); 
## x_Train file 
xTest = read.table('./test/x_test.txt',header=FALSE); 
## y_Train file
yTest = read.table('./test/y_test.txt',header=FALSE);

## Add readable column names for the test dataset
colnames(subjectTest) = c("subjectId")
colnames(yTest) = c("activityId")

## Get all the feature names and map them as colnames for xTest 
colnames(xTest) = features[,2]

## Create the test data set by combining individual test data using cbind()
testData<-cbind(yTest, subjectTest, xTest)

## Combine train and test data into single data set
combinedData <- rbind(trainData, testData)

## Get list of columns and store for filtering required columns for analysis
combinedDataColumns <- colnames(combinedData)

## Extract the columns that are needed for the analysis
logicalVector = (grepl("activity..",combinedDataColumns) | grepl("subject..",combinedDataColumns) | 
                 grepl("-mean..",combinedDataColumns) & !grepl("-meanFreq..",combinedDataColumns) & !grepl("mean..-",combinedDataColumns) | 
                 grepl("-std..",combinedDataColumns) & !grepl("-std()..-",combinedDataColumns))


# Subset combinedData table based on the logicalVector to keep only desired columns
finalData = combinedData[logicalVector==TRUE]

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Create finalDataColumns vector to include the new column names
finalDataColumns  = colnames(finalData)

# renmae the variable names with user-friendly names
names(finalData) <- gsub("^(t)", "time", names(finalData))
names(finalData) <- gsub("^(f)", "frequency", names(finalData))
names(finalData) <- gsub("Acc", "Accelerometer", names(finalData))
names(finalData) <- gsub("Gyro", "Gyroscope", names(finalData))
names(finalData) <- gsub("Mag", "Magnitude", names(finalData))
names(finalData) <- gsub("BodyBody", "Body", names(finalData))
names(finalData) <- gsub("mean", "Mean", names(finalData))
names(finalData) <- gsub("std", "StdDev", names(finalData))
names(finalData) <- gsub("\\()", "", names(finalData))


# Generate tidyData using aggregate() 
tidyData<-aggregate(. ~subjectId + activityId, finalData, mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Order tidyData 
tidyData<-tidyData[order(tidyData$subjectId,tidyData$activityId),]

# Export the tidyData set 
# row.names set to TRUE to see how many records are extracted 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')



