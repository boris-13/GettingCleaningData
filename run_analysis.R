# This script called run_analysis.R does the following:
# * Merges the training and the test sets to create one
#   data set.
# * Extracts only the measurements on the mean and standard 
#   deviation for each measurement.
# * Uses descriptive activity names to name the activities
#   in the data set.
# * Appropriately labels the data set with descriptive 
#   variable names.
# * From the data set in step 4, creates a second, 
#  independent tidy data set with the average of each 
#  variable for each activity and each subject.

# load library plyr
library(plyr)

# Downloading dataset
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/harset.zip")

# Unzip data set "harset" to /data directory
unzip(zipfile="./data/harset.zip",exdir="./data")


# 1. Merge train & test sets => create one data set:

# 1.1 Reading files

# 1.1.1 Reading trainings tables:
xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subjecttrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading testing tables:
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subjecttest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# 1.1.4 Reading activity labels:
activitylabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# 1.2 Assigning column names:
colnames(xtrain) <- features[,2] 
colnames(ytrain) <-"activityId"
colnames(subjecttrain) <- "subjectId"

colnames(xtest) <- features[,2] 
colnames(ytest) <- "activityId"
colnames(subjecttest) <- "subjectId"

colnames(activitylabels) <- c('activityId','activityType')

# 1.3 Merging all data in one set "bigset":
mergetrain <- cbind(ytrain, subjecttrain, xtrain)
mergetest <- cbind(ytest, subjecttest, xtest)
bigset <- rbind(mergetrain, mergetest)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 2.1 Reading column names:
columnnames <- colnames(bigset)

# 2.2 Create vector for defining ID, mean and standard deviation:
meanandstd <- (grepl("activityId" , columnnames) | 
                   grepl("subjectId" , columnnames) | 
                   grepl("mean.." , columnnames) | 
                   grepl("std.." , columnnames) 
)

# 2.3 Making nessesary subset from bigset:
setmeanstd <- bigset[ , meanandstd == TRUE]

# 3. Using descriptive activity names to name the activities in the data set:
setactivitynames <- merge(setmeanstd, activitylabels,
                              by='activityId',
                              all.x=TRUE)

# 4. Appropriately labeling the data set with descriptive variable names.
# This step was made in previos steps =) See 1.3, 2.2, 2.3.

# 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject:

# 5.1 Making second tidy data set 
secTidySet <- aggregate(. ~subjectId + activityId, setactivitynames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

# 5.2 Writing second tidy data set in txt file
write.table(secTidySet, "tidydata.txt", row.name=FALSE)