# Tidying the UCI Human Activity Recognition Dataset

## Overview of files used from the data set

1. __*activity_labels.txt*__ - the 6 activities that were measured
1. __*features.txt*__ - the 561 different measurements that were made
1. __*test/subject_test.txt*__ - the sequence of subjects in the test set
1. __*test/X_test.txt*__ - the actual measurements in the test set
1. __*test/y_test.txt*__ - the sequence of activities in the test set
1. __*train/subject_train.txt*__ - the sequence of subjects in the training set
1. __*train/X_train.txt*__ - the actual measurements in the training set
1. __*train/y_train.txt*__ - the sequence of activities in the training set

## Steps taken to tidy the data set

* Open the data set archive

        setwd("~/edu/datasciencecoursera/03 - Getting and Cleaning Data")
        
        unzip("getdata-projectfiles-UCI HAR Dataset.zip")

* Load the features file. This will be used to generate column headers

        feat <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

* Load the measurement data, using the features as column headers

        x.test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feat[, 2], check.names = FALSE)
        x.train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feat[, 2], check.names = FALSE)

* Add ID columns to the measurement tables - these will be used for later merging

        x.test$ID <- seq.int(nrow(x.test))
        x.train$ID = seq.int(nrow(x.train))

* Load the activity data

        y.test <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "activity.ID")
        y.train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "activity.ID")

* Add IDs here too

        y.test$ID <- seq.int(nrow(y.test))
        y.train$ID <- seq.int(nrow(y.train))

* Load the activity labels

        act.labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity.ID", "activity"))

* Load the subject data

        subj.test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subj.ID")
        subj.train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subj.ID")

* These also get IDs to match the sequence of measurements

        subj.test$ID <- seq.int(nrow(subj.test))
        subj.train$ID <- seq.int(nrow(subj.train))

* Add the activity labels to the activity sequence by merging those tables

        y.train.labeled <- merge(y.train, act.labels, by = "activity.ID")
        y.test.labeled <- merge(y.test, act.labels, by = "activity.ID")

* There are duplicate column names in the measurement tables, but they are not among the fields (mean & std) we are looking for. This removes them.

        clean.x.train <- x.train[, !duplicated(colnames(x.train))]
        clean.x.test <- x.test[, !duplicated(colnames(x.test))]

* Merge the measurement tables with the labeled activities

        train <- merge(clean.x.train, y.train.labeled, by = "ID")
        test <- merge(clean.x.test, y.test.labeled, by = "ID")

* Add the subject sequence

        train <- merge(train, subj.train, by = "ID")
        test <- merge(test, subj.test, by = "ID")

* Combine the training and test sets into one dataset

        merged.sets <- rbind(test, train)

* Extract the relevant columns from this set

        merged.sets.extracted <- select(merged.sets, activity, subj.ID, contains("-mean()"), contains("-std()"))

* Aggregate the data, providing the average of each measurement, grouped by activity and subject

        tidy.data <- merged.sets.extracted %>% group_by(activity, subj.ID) %>% summarise_each(funs(mean))

* Write the tidied data to disk

        write.table(tidy.data, "uci_har_tidy.txt")