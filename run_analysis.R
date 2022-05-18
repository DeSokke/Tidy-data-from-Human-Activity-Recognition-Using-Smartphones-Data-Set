create_tidy <- function(){
  #meta data
  class_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  features <- read.table("UCI HAR Dataset/features.txt")
  feature_variables <- read.delim("UCI HAR Dataset/features_info.txt")
  x <- c("1","2","3","4","5","6")
  y <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  #data
  test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt",  header = FALSE)
  testx <- read.table("UCI HAR Dataset/test/X_test.txt",  header = FALSE)
  testy <- read.table("UCI HAR Dataset/test/y_test.txt",  header = FALSE)
  train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt",  header = FALSE)
  trainx <- read.table("UCI HAR Dataset/train/X_train.txt",  header = FALSE)
  trainy <- read.table("UCI HAR Dataset/train/y_train.txt",  header = FALSE)
  for (i in x) {
    c <-  as.numeric(i)
    testy$V1 <- replace(testy$V1, testy$V1 == i, y[c])
  }  
  for (i in x) {
    c <-  as.numeric(i)
    trainy$V1 <- replace(trainy$V1, trainy$V1 == i, y[c])
  }
  sub <- rbind(test_sub, train_sub)
  activityy <- rbind(trainy, testy)
  featurex <- rbind(trainx, testx)
  colnames(featurex) <- t(features[2])
  data <- cbind(featurex, activityy,sub)
  #extract mean data
  mean_STD_cols <- grep(".*Mean.*|.*Std.*", names(data), ignore.case=TRUE)
  nrcols <- c(mean_STD_cols, 562, 563)
  mean_STD_data <- data[,nrcols]
  #change activity names
  names(mean_STD_data)<-gsub("Acc", "Accelerometer", names(mean_STD_data))
  names(mean_STD_data)<-gsub("Gyro", "Gyroscope", names(mean_STD_data))
  names(mean_STD_data)<-gsub("BodyBody", "Body", names(mean_STD_data))
  names(mean_STD_data)<-gsub("Mag", "Magnitude", names(mean_STD_data))
  names(mean_STD_data)<-gsub("^t", "Time", names(mean_STD_data))
  names(mean_STD_data)<-gsub("^f", "Frequency", names(mean_STD_data))
  names(mean_STD_data)<-gsub("tBody", "TimeBody", names(mean_STD_data))
  names(mean_STD_data)<-gsub("-mean()", "Mean", names(mean_STD_data), ignore.case = TRUE)
  names(mean_STD_data)<-gsub("-std()", "STD", names(mean_STD_data), ignore.case = TRUE)
  names(mean_STD_data)<-gsub("-freq()", "Frequency", names(mean_STD_data), ignore.case = TRUE)
  names(mean_STD_data)<-gsub("angle", "Angle", names(mean_STD_data))
  names(mean_STD_data)<-gsub("gravity", "Gravity", names(mean_STD_data))
  #making tidy dataset
  library(data.table)
  mean_STD_data$V1.1 <- as.factor(mean_STD_data$V1.1)
  mean_STD_data <- data.table(mean_STD_data)
  tidy_data <- aggregate(. ~V1.1 + V1, mean_STD_data, mean)
  write.table(tidy_data, file = "Tidy_data.txt", row.names = FALSE)
}