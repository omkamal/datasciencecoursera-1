# Merge training and the test sets to create one data set.
# setwd("~/coursera/cleaning-data/peer_assessment/")

train_data <- read.table("./data/train/X_train.txt")
dim(train_data) 
head(train_data)
train_label <- read.table("./data/train/y_train.txt")
table(train_label)
train_subject <- read.table("./data/train/subject_train.txt")
test_data <- read.table("./data/test/X_test.txt")
dim(test_data) 
test_label <- read.table("./data/test/y_test.txt") 
table(test_label) 
test_subject <- read.table("./data/test/subject_test.txt")
join_data <- rbind(train_data, test_data)
dim(join_data) 
join_label <- rbind(train_label, test_label)
dim(join_label) 
join_subject <- rbind(train_subject, test_subject)
dim(join_subject) 

# extracts measurements on the mean and standard deviation for each measurement. 

features <- read.table("./data/features.txt")
dim(features)  
mean_std_idxs <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(mean_std_idxs) 
join_data <- join_data[, mean_std_idxs]
dim(join_data) 
# clean data: remove (), capitalize M, S, remove "-"
names(join_data) <- gsub("\\(\\)", "", features[mean_std_idxs, 2])
names(join_data) <- gsub("mean", "Mean", names(join_data))
names(join_data) <- gsub("std", "Std", names(join_data))
names(join_data) <- gsub("-", "", names(join_data)) 

# Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity_label <- activity[join_label[, 1], 2]
join_label[, 1] <- activity_label
names(join_label) <- "activity"

# Appropriately labels the data set with descriptive activity names. 
names(join_subject) <- "subject"
clean_data <- cbind(join_subject, join_label, join_data)
dim(clean_data) # 10299*68
write.table(clean_data, "merged_data.txt") # write out the 1st dataset

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subject_length <- length(table(join_subject)) # 30
activity_length <- dim(activity)[1] # 6
col_length <- dim(clean_data)[2]
result <- matrix(NA, nrow=subject_length*activity_length, ncol=col_length) 
result <- as.data.frame(result)
colnames(result) <- colnames(clean_data)
row <- 1
for(i in 1:subject_length) {
    for(j in 1:activity_length) {
        result[row, 1] <- sort(unique(join_subject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        b1 <- i == clean_data$subject
        b2 <- activity[j, 2] == clean_data$activity
        result[row, 3:col_length] <- colMeans(clean_data[b1&b2, 3:col_length])
        row <- row + 1
    }
}
head(result)
write.table(result, "data_with_means.txt") 
