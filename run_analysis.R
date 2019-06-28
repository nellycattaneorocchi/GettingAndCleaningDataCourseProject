downloadDS <- function() {
    # download data sets
    library(data.table)
    
    myUrl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    if (!file.exists('./UCI HAR Dataset.zip')){
        download.file(myUrl,'./UCI HAR Dataset.zip', mode = 'wb')
        unzip("UCI HAR Dataset.zip", exdir = getwd())
    }
}

extractDS  <- function(data.all, features) {
    mean_std.select <- grep('mean|std', features)
    #print(mean_std.select)
    data.all[,c(1,2,mean_std.select + 2)]
}

setDescrActNames  <- function(data.sub) {
    activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
    activity.labels <- as.character(activity.labels[,2])
    data.sub$activity <- activity.labels[data.sub$activity]
}

setLabels  <- function(data.sub) {
    name.new <- names(data.sub)
    name.new <- gsub("[(][)]", "", name.new)
    name.new <- gsub("^t", "TimeDomain_", name.new)
    name.new <- gsub("^f", "FrequencyDomain_", name.new)
    name.new <- gsub("Acc", "Accelerometer", name.new)
    name.new <- gsub("Gyro", "Gyroscope", name.new)
    name.new <- gsub("Mag", "Magnitude", name.new)
    name.new <- gsub("-mean-", "_Mean_", name.new)
    name.new <- gsub("-std-", "_StandardDeviation_", name.new)
    name.new <- gsub("-", "_", name.new)
    names(data.sub) <- name.new
}

createAverageDS  <- function(data.sub, features) {
    data.tidy <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
    write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)
}

run_analysis  <- function() {
    # This R script, called run_analysis.R, does the following:
    # 1) Merges the training and the test sets to create one data set.
    # 2) Extracts only the measurements on the mean and standard deviation for each measurement.
    # 3) Uses descriptive activity names to name the activities in the data set
    # 4) Appropriately labels the data set with descriptive variable names.
    # 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    # download data sets
    downloadDS()
    
    # Data is read by several file and converted into a single data frame
    features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
    features <- as.character(features[,2])
    
    data.train.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
    data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
    data.train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
    
    data.train <-  data.frame(data.train.subject, data.train.activity, data.train.x)
    names(data.train) <- c(c('subject', 'activity'), features)
    
    data.test.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
    data.test.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
    data.test.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
    
    data.test <-  data.frame(data.test.subject, data.test.activity, data.test.x)
    names(data.test) <- c(c('subject', 'activity'), features)
    
    ##-------------------> process data
    
    # 1) Merge the training and the test sets to create one data set
    data.all <- rbind(data.train, data.test)
    
    # 2) Extract only the measurements on the mean and standard deviation for each measurement
    data.sub <- extractDS(data.all, features)
    
    # 3) Use descriptive activity names to name the activities in the data set
    setDescrActNames(data.sub)
    
    # 4) Appropriately label the data set with descriptive variable names
    setLabels(data.sub)
    
    # 5) From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject
    createAverageDS(data.sub, features)
}