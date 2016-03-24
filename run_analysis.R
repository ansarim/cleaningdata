library(RCurl)
library(memisc)

if (!file.exists('UCI HAR Dataset')) {
    dataFile <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    dir.create('UCI HAR Dataset')
    download.file(dataFile, 'UCI-HAR-dataset.zip', method='curl')
    unzip('./UCI-HAR-dataset.zip')
}

# First merge the trainig & test dataset.
datax.train <- read.table('./UCI HAR Dataset/train/X_train.txt')
datax.test <- read.table('./UCI HAR Dataset/test/X_test.txt')
combined_xdata <- rbind(datax.train, datax.test)

subj.train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
subj.test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
subj <- rbind(subj.train, subj.test)

datay.train <- read.table('./UCI HAR Dataset/train/y_train.txt')
datay.test <- read.table('./UCI HAR Dataset/test/y_test.txt')
combined_ydata <- rbind(datay.train, datay.test)

# Extract the mean and standard deviation for each measurement. 
features <- read.table('./UCI HAR Dataset/features.txt')
mean.sd <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x.mean.sd <- combined_xdata[, mean.sd]

# Rename activity names to be descriptive
names(x.mean.sd) <- features[mean.sd, 2]
names(x.mean.sd) <- tolower(names(x.mean.sd)) 
names(x.mean.sd) <- gsub("\\(|\\)", "", names(x.mean.sd))

activities <- read.table('./UCI HAR Dataset/activity_labels.txt')
activities[, 2] <- tolower(as.character(activities[, 2]))
activities[, 2] <- gsub("_", "", activities[, 2])

combined_ydata[, 1] = activities[combined_ydata[, 1], 2]
colnames(combined_ydata) <- 'activity'
colnames(subj) <- 'subject'

# Label the data set with descriptive activity names.
data <- cbind(subj, x.mean.sd, combined_ydata)
write.table(data, './data_merged.txt', row.names = F)
cb <- codebook(data)
Write(codebook(data),
      file="codebook.md")

# Second tidy data set for the average of each variable for each activity and each subject. 
average.df <- aggregate(x=data, by=list(activities=data$activity, subj=data$subject), FUN=mean)
average.df <- average.df[, !(colnames(average.df) %in% c("subj", "activity"))]
write.table(average.df, './data_average.txt', row.names = F)
