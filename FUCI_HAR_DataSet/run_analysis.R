library(tidyverse)
# Import data
x_train <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/train/X_train.txt")
y_train <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/train/y_train.txt")
subject_train <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/train/subject_train.txt")
x_test <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/test/X_test.txt")
y_test <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/test/y_test.txt")
subject_test <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/test/subject_test.txt")
activity <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/activity_labels.txt")
features <- read.table("~/200_level_midterm/FUCI_HAR_DataSet/features.txt") 

glimpse(x_train)
glimpse(y_train)
glimpse(activity)
glimpse(features)
glimpse(subject_train)
glimpse(total)
length(unique(subject_test$V1))

# rename the columns
colnames(x_train) <- features$V2
colnames(x_test) <- features$V2
colnames(activity) <- c("key", "activity")
colnames(subject_train) <- "subject_num"
colnames(subject_test) <- "subject_num"

# Merges the training and the test sets to create one data set.
xy_train <- cbind(x_train, y_train, subject_train)
xy_test <- cbind(x_test, y_test, subject_test)
total <- bind_rows(xy_train, xy_test)

# Use descriptive activity names to name the activities in the data set
total_joined <- left_join(total, activity, by = c("V1"="key"))
 glimpse(total_joined)

# Extract only the measurements on the mean and standard deviation for each measurement and keeps activity name.
mean_sd <- select(total_joined,
    contains ("mean"),
    contains("std"),
    activity,
    subject_num
    )

# Appropriately labels the data set with descriptive variable names.
 names(mean_sd) <-  sub("^t", "Time-", colnames(mean_sd))
 names(mean_sd) <-  sub("^f", "Feature-", colnames(mean_sd)) 
 names(mean_sd) <- sub("Acc", "-Acceleration-", colnames(mean_sd))
 names(mean_sd) <- sub("Freq", "-Frequency", colnames(mean_sd))
 names(mean_sd) <- sub("\\(", "", colnames(mean_sd))
 names(mean_sd) <- sub("\\)", "", colnames(mean_sd))
 names(mean_sd) <- sub("gravity", "Gravity", colnames(mean_sd))
 names(mean_sd) <- sub("mean", "Mean", colnames(mean_sd))
 names(mean_sd) <- sub("Mag", "-Magnitude", colnames(mean_sd))
 names(mean_sd) <- sub("std", "StandardDev", colnames(mean_sd))
 names(mean_sd) <- sub("BodyBody", "Body", colnames(mean_sd))
 names(mean_sd) <- sub("angle", "Angle", colnames(mean_sd))
 
  glimpse(mean_sd)
  
# creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidy_agg_data <-  aggregate(mean_sd[,3:81], by = list(activity = mean_sd$activity, subject = mean_sd$subject),FUN = mean)
glimpse(tidy_agg_data)
write.table(x = tidy_agg_data, file = "~/200_level_midterm/FUCI_HAR_DataSet/tidy_agg_data.txt", row.names = FALSE) 
