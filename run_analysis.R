# This function cleans up the data from UIC HAR Dataset, extracts variables of
# interest, and summarises those variables for each subject/activity combination.

run_analysis <- function() {
  # Load necessiary packages
  library("dplyr")

  # Read each table into R
  actLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  featTbl <- read.table("./UCI HAR Dataset/features.txt")
  subjTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  subjTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
  xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
  yTest <-read.table("./UCI HAR Dataset/test/y_test.txt")
  yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

  # Add column names from featTbl to xTest and xTrain measurement datasets
  colnames(xTrain) <- featTbl[,2]
  colnames(xTest) <- featTbl[,2]
  
  # Add suffixes to duplicate column names
  xTrain <- xTrain[,]
  xTest <- xTest[,]

  # Add column names for yTest and yTrain datasets
  colnames(yTest) <- c("ActivityNumber")
  colnames(yTrain) <- c("ActivityNumber")
  
  # Add column names for actLables dataset
  colnames(actLabels) <- c("ActivityNumber", "ActivityLabel")

  # Add column names for Subject ID tables
  colnames(subjTest) <- c("SubjectID")
  colnames(subjTrain) <- c("SubjectID")

  # Combine all columns related to Train/Test data, independently
  trainTbl <- cbind(yTrain, subjTrain, xTrain)
  testTbl <- cbind(yTest, subjTest, xTest)

  # Combine the two tables
  fullTbl <- rbind(trainTbl, testTbl)
  fullTbl <- merge(fullTbl, actLabels, by.x="ActivityNumber", by.y="ActivityNumber")
  fullTbl <- fullTbl[,c(1, 2, 564, 3:563)]
  
  # Reformat the field names into cleaner strings
  colNms <- colnames(fullTbl)
  colNms <- gsub("-", "_", colNms)
  colNms <- gsub("\\()", "", colNms)
  colNms <- gsub("BodyBody", "Body", colNms)
  colnames(fullTbl) <- colNms

  # Find the fields containing the words "mean" and "std" using featTbl and
  # combine into one vector
  means <- grep("[Mm]ean", featTbl[,2])
  stds <- grep("[Ss][Tt][Dd]", featTbl[,2])
  origCols<- sort(c(means, stds))

  # Alter the column numbers to account for added columns (Activity Number, 
  # Activity Label, and Subject ID)
  newCols <- origCols + 3

  # Add added column numbers representing (Activity Number, 
  # Activity Label, and Subject ID) to the list
  finalCols <- c(1, 2, 3, newCols) 

  # Use the list of column numbers to extract columns relating to mean and std
  mstTbl <- fullTbl[,finalCols]

  # Group the data by Activity Label and Subject ID and calculate the mean for each group
  avgTbl <- mstTbl %>% group_by(SubjectID, ActivityNumber) %>% summarise_each(funs(mean), -ActivityLabel)
  
  # Add the Activity Labels back to the final dataset and organize the columns
  avgTbl <- merge(avgTbl, actLabels, by="ActivityNumber")
  avgTbl <- arrange(avgTbl[c(2, 1, 89, 3:88)], SubjectID)
  
  write.table(avgTbl, file = "tidy_data.txt", row.names = FALSE)
}

