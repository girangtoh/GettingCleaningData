## Load libraries required for this script

if (!require("data.table")) {
        install.packages("data.table")
        library(data.table)
}

if(!require("reshape2")) {
        install.packages("reshape2")
        library(reshape2)
}

## 1. Merge the test and train sets into one
## Load the files in first
activity_Labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Label both columns in each file
names(subject_test) = c("subject")
names(subject_train) = c("subject")


Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

## Read in the feature file, this contains the column names for 'X' files
feature_List <- read.table("./UCI HAR Dataset/features.txt")

## Set column names(measurements) for the data sets 
names(X_test) = feature_List$V2
names(X_train) = feature_List$V2

# Label both the Activity columns in each file
names(Y_test) = c("activity")
names(Y_train) = c("activity")

## Combine test and train data by combining the rows
Y_set <- rbind(Y_test, Y_train)

X_set <- rbind(X_test, X_train)

subject_set <- rbind(subject_test, subject_train)

## 2.  Extract only the mean and standard deviation measurements 

## Only extract out measurements that have mean() or std() in their names
reqIndex <- grep("mean\\(|std\\(", feature_List$V2)

## Recreate the merged set with only the required measurements
reqXSet <- X_set[, reqIndex]

## Combine all columns together
reqMergedSet <- cbind(subject_set, Y_set, reqXSet)


## 3. Use decsriptive activity names to name the activity in the data set
## 1) WALKING 2) WALKING_UPSTAIRS 3) WALKING_DOWNSTAIRS
## 4) SITTING 5) STANDING 6) LAYING

## Use the labels in activity_labels.txt to name the activity

activityIndex <- as.character(1:6)
acts <- factor(activity_Labels[, 2])

reqMergedSet$activity <- acts[match(reqMergedSet$activity, activityIndex)]
        
## 4.Appropriately labels the data set with descriptive variable names.
## I use camel labelling because it is easier to read
## Remove '()' in column names because it is considered illegal in R
## std is standard deviation, I replace with SD because it is easier to read IMO
names(reqMergedSet) <- gsub("-","", names(reqMergedSet))
names(reqMergedSet) <- gsub("mean\\(\\)", "Mean", names(reqMergedSet))
names(reqMergedSet) <- gsub("std\\(\\)", "SD", names(reqMergedSet))
## Somehow there are variable names that have BodyBody in them, I decided that it is redundant
names(reqMergedSet) <- gsub("BodyBody", "Body", names(reqMergedSet))


## 5. From the data set in step 4, creates a second, 
##   independent tidy data set with the average of each variable 
##   for each activity and each subject.

## Melt the data so that Subject and Activity is the Id 
melt_data <- melt(reqMergedSet, id=c("subject", "activity"))

## Create tidy data, calculating the average for each Subject per Activity
tidy_data <- dcast(melt_data, subject + activity ~ variable, mean)

## Write to a text file
write.table(tidy_data, file="./tidy_data.txt", row.name=FALSE)

print("tidy_data.txt is created!")

