################################################################################
#
# Getting and Cleaning Data Course Projectless
#
################################################################################

# 1. Merges the training and the test sets to create one data set:.

#### 1.1 Clean up workspace and get dir/packages:
rm(list = ls())
dir()
library(reshape2)

#### 1.2 Download and unzip the dataset:
filename = "getdata-projectfiles-UCI HAR Dataset.zip"
if (!file.exists(filename)){
        fileURL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename)
}
if (!file.exists("UCI HAR Dataset")) {
        unzip(filename)
}

#### 1.3 Load the datasets:
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt")
table(subject_train)
dim(subject_train) # 7352x1
group = as.character(rep.int("train", 7352))
y_train = read.table("UCI HAR Dataset/train/Y_train.txt")
dim(y_train) # 7352x1
X_train = read.table("UCI HAR Dataset/train/X_train.txt")
dim(X_train) # 7352x561
train = cbind(group = group, subject_train, y_train, X_train)
dim(train) # 7352x564
rm(filename, fileURL, group, subject_train, y_train, X_train) # cleaning

subject_test = read.table("UCI HAR Dataset/test/subject_test.txt")
table(subject_test)
dim(subject_test) # 2947x1
group = as.character(rep.int("test", 2947))
y_test = read.table("UCI HAR Dataset/test/Y_test.txt")
dim(y_test) # 2947x1
X_test = read.table("UCI HAR Dataset/test/X_test.txt")
dim(X_test) # 2947x561
test = cbind(group = group, subject_test, y_test, X_test)
dim(test) # 2947x564
rm(group, subject_test, y_test, X_test) # cleaning

#### 1.4 Load activity labels and features:
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt",
                             colClasses = c("numeric", "character"))
dim(activity_labels) # 6x2
features = read.table("UCI HAR Dataset/features.txt",
                      colClasses = c("numeric", "character"))
dim(features) # 561X2

#### 1.5 merge datasets and add labels:
data = rbind(train, test)
dim(data) # 10299x564
colnames(data) = c("group", "subject", "activity", features[, 2])
length(colnames(data)) # 564
rm(train, test) # cleaning

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

meanstandarindex = grep(".*mean.*|.*std.*", colnames(data))
meanstandarindex
length(meanstandarindex) # 79
meanstandar.names = names(data[ , meanstandarindex])
meanstandar.names
meanstandar.names = gsub('-mean', 'Mean', meanstandar.names)
meanstandar.names = gsub('-std', 'Std', meanstandar.names)
meanstandar.names = gsub('[-()]', '', meanstandar.names)
meanstandar.names
data = data[ , c(1:3, meanstandarindex)]
names(data) = c("group", "subject", "activity", meanstandar.names)

# 3. Uses descriptive activity names to name the activities in the data set:

data$group = as.factor(data$group)
data$activity = factor(data$activity,
                       levels = activity_labels[,1],
                       labels = activity_labels[,2])
data$subject = as.factor(data$subject)
rm(activity_labels, features) # cleaning

# 4. Appropriately labels the data set with descriptive variable names:
data1 = melt(data, id = c("subject", "activity"), measure.vars = meanstandar.names)
head(data1)
tail(data1)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data2 = dcast(data1, subject + activity ~ variable, mean)
head(data2)
tail(data2)
write.table(data2, "tidy.txt", row.names = FALSE, quote = FALSE)

rm(list = ls())





