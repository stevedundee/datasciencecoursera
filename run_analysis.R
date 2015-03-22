loadData <- function(dataset,filename) {
  dirName = "UCI HAR Dataset"
  read.table(paste(dirName,dataset,filename,sep="/"))
}

run_analysis <- function() {
  
  library(dplyr)
  
  featureNames = loadData("","features.txt")
  activityLabels = loadData("","activity_labels.txt")
  
  # Test Readings
  set<-"test"
  testXTest <- loadData(set,"X_test.txt")
  yTest <- loadData(set,"y_test.txt") #  relate to activityLabels
  subjectTest <- loadData(set,"subject_test.txt")
  testXTest <- cbind(yTest,testXTest)
  testXTest <- cbind(subjectTest,testXTest)

  # Train Readings
  set="train"
  testXTrain = loadData(set,"X_train.txt")
  yTrain = loadData(set,"y_train.txt") #  relate to activityLabels
  subjectTrain = loadData(set,"subject_train.txt")
  testXTrain <- cbind(yTrain,testXTrain)
  testXTrain <- cbind(subjectTrain,testXTrain)
  
  # Append the one to the other
  combined = rbind(testXTest, testXTrain)
  
  # Name the columns (thanks to "Lantana" at http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name
  # for method to get valud, unique names)
  valid_column_names <- make.names(names=as.character(featureNames[,2]), unique=TRUE, allow_ = TRUE)
  names(combined) <- c("Subject","Activity",valid_column_names)
  
  # Get the headings we want
  combined = select(combined,Subject,Activity,contains(".mean."),contains(".std."))
  
  # Human-friendly activity labels
  merged = select(merge(combined,activityLabels,by.x="Activity",by.y="V1"),-Activity)
  merged = rename(merged,Activity=V2)
  
  # Write out the dataset
  write.table(merged,"Merged_Tidy_Dataset.txt",row.name=FALSE)
  
  # Means by Activity and Subject, and write them out
  means = group_by(merged,Activity,Subject)%>%summarise_each(funs(mean))
  write.table(means,"MTD_with_Means.txt",row.name=FALSE)
}