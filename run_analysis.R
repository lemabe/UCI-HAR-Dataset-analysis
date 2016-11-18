# Let's Process the "Human Activity Recognition Using Smartphones" dataset


## gather all the usable data to the same directory.
## created from UCI HAR DATASET directory a new directory call Uci har with usable files




# load packages.
library(dplyr)
library(tidyr)

# download the data if not present in the current working directory.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset")) {
    if (!file.exists("data")) {
        dir.create("data")
    }
    download.file(fileUrl, destfile="data/har.zip", method="curl")
    unzip("data/har.zip", exdir="./")
}

# First Step - let's merge both training and the test sets to one data set.

# training data
train_x <- read.table("UCI HAR Dataset//train/X_train.txt", nrows=7352, comment.char="")
train_sub <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names=c("subject"))
train_y <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity"))
train_data <- cbind(train_x, train_sub, train_y)

# test data
test_x <- read.table("UCI HAR Dataset//test/X_test.txt", nrows=2947, comment.char="")
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_y <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
test_data <- cbind(test_x, test_sub, test_y)

# let's merge both train and test data now
data <- rbind(train_data, test_data)

# Second step let's extract only the mean and standard deviation for each measurement.

feature_l <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))
features <- c(as.vector(feature_l[, "name"]), "subject", "activity")

# filter only features that has mean or std in the name
filtered_feat_id <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data = data[, filtered_feat_id]

# third Step let's use descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
    filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
}

# fourth step let's label the data set with descriptive variable names Appropriately.
# make feature names more human readble
filtered_feat_names <- features[filtered_feat_id]
filtered_feat_names <- gsub("\\(\\)", "", filtered_feat_names)
filtered_feat_names <- gsub("Acc", "-acceleration", filtered_feat_names)
filtered_feat_names <- gsub("Mag", "-Magnitude", filtered_feat_names)
filtered_feat_names <- gsub("^t(.*)$", "\\1-time", filtered_feat_names)
filtered_feat_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feat_names)
filtered_feat_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feat_names)
filtered_feat_names <- gsub("BodyBody", "Body", filtered_feat_names)
filtered_feat_names <- tolower(filtered_feat_names)

# assign names to features
names(filtered_data) <- filtered_feat_names

# fifht step - From the data in the fourth step, let's create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- tbl_df(filtered_data) %>%
    group_by('subject', 'activity') %>%
    summarise_each(funs(mean)) %>%
    gather(measurement, mean, -activity, -subject)

# Save  data into the file
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)

# Thanks

