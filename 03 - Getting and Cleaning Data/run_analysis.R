library(dplyr)

setwd("~/edu/datasciencecoursera/03 - Getting and Cleaning Data")
unzip("getdata-projectfiles-UCI HAR Dataset.zip")

feat <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

x.test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feat[, 2], check.names = FALSE)
x.train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feat[, 2], check.names = FALSE)
x.test$ID <- seq.int(nrow(x.test))
x.train$ID = seq.int(nrow(x.train))

y.test <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "activity.ID")
y.train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "activity.ID")
y.test$ID <- seq.int(nrow(y.test))
y.train$ID <- seq.int(nrow(y.train))

act.labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity.ID", "activity"))

subj.test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subj.ID")
subj.train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subj.ID")
subj.test$ID <- seq.int(nrow(subj.test))
subj.train$ID <- seq.int(nrow(subj.train))

y.train.labeled <- merge(y.train, act.labels, by = "activity.ID")
clean.x.train <- x.train[, !duplicated(colnames(x.train))]
train <- merge(clean.x.train, y.train.labeled, by = "ID")

y.test.labeled <- merge(y.test, act.labels, by = "activity.ID")
clean.x.test <- x.test[, !duplicated(colnames(x.test))]
test <- merge(clean.x.test, y.test.labeled, by = "ID")

train <- merge(train, subj.train, by = "ID")
test <- merge(test, subj.test, by = "ID")

merged.sets <- rbind(test, train)

merged.sets.extracted <- select(merged.sets, activity, subj.ID, contains("-mean()"), contains("-std()"))

tidy.data <- merged.sets.extracted %>% group_by(activity, subj.ID) %>% summarise_each(funs(mean))

write.table(tidy.data, "uci_har_tidy.txt")

