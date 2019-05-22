library(data.table)
url_file = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(url_file,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(subject_train, y_train, x_train)
names(data.train) <- c(c('subject', 'activity'), features)

x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject_test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data.test <-  data.frame(subject_test, y_test, x_test)
names(data.test) <- c(c('subject', 'activity'), features)

##Merges the training and the test sets to create one data set.
data.all <- rbind(data.train, data.test)

##Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std.select <- grep('mean|std', features)
data.sub <- data.all[,c(1,2,mean_std.select + 2)]

##Uses descriptive activity names to name the activities in the data set
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity.labels <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

##Appropriately labels the data set with descriptive variable names.
name.new <- names(data.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "Time_domain_", name.new)
name.new <- gsub("^f", "Frequency_domain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.sub) <- name.new

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
write.table(x = tidy_data, file = "data_tidy.txt", row.names = FALSE)