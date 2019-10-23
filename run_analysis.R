### Peer graded assignment course 3 data science specialization

### Review criteria

#The submitted data set is tidy. 
#The Github repo contains the required scripts.
#GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
#The README that explains the analysis files is clear and understandable.
#The work submitted for this project is the work of the student who submitted it.

#############

# First download the data set from the following url:

# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Set working directory:
setwd("C:/Users/areddington/Documents/R_dir_DS_specialization/Course3/Assignment4/UCI HAR Dataset")

# Merge the training and the test sets to create one data set

library(data.table)

feature_names <- read.table("./features.txt")
activityLabels <- read.table("./activity_labels.txt", header = FALSE)

subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./train/X_train.txt", header = FALSE)

subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
activityTest <- read.table("./test/y_test.txt", header = FALSE)
featuresTest <- read.table("./test/X_test.txt", header = FALSE)

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

library(dplyr)

# Name the columns

colnames(features) <- t(feature_names[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

# Merging

data_complete <- cbind(features,activity,subject)

# Extract only the measurements on the mean and standard deviation for each measurement:

columns_mu_std <- grep(".*Mean.*|.*Std.*", names(data_complete), ignore.case=TRUE)

# Use descriptive activity names to name the activities in the data set:

columns_act <- c(columns_mu_std, 562, 563)
dim(data_complete)

extractedData <- data_complete[,columns_act]
#dim(extractedData) # to show size

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

# Appropriately label the data set with descriptive variable names:

#names(extractedData) # to view names

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#names(extractedData) # to view names

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject:

tidy_dataset <- aggregate(. ~Subject + Activity, extractedData, mean)
tidy_dataset <- tidy_dataset[order(tidy_dataset$Subject,tidy_dataset$Activity),]
write.table(tidy_dataset, file = "Tidy_data.txt", row.names = FALSE)










