### Getting and Cleaning Data Project ###

### 1. Merge the training and the test sets to create one data set.

# Read training data
features <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt',header=FALSE)
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE)
xTrain <- read.table('./train/x_train.txt',header=FALSE)
yTrain <- read.table('./train/y_train.txt',header=FALSE)

# Assigin column names
colnames(activityType) <- c('activityId','activityType');
colnames(subjectTrain) <- "subjectId";
colnames(xTrain) <- features[,2]; 
colnames(yTrain) <- "activityId";

# Final training set
trainingData <- cbind(yTrain,subjectTrain,xTrain);

# Read test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names
colnames(subjectTest) <- "subjectId";
colnames(xTest) <- features[,2]; 
colnames(yTest) <- "activityId";

# Final test set
testData <- cbind(yTest,subjectTest,xTest);

# Merge training and test sets
finalData <- rbind(trainingData,testData);


### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colNames <- colnames(finalData); 

# Vector with TRUE values for the ID, mean() & stddev() columns and FALSE for all others
vector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
finalData <- finalData[vector==TRUE];

# 3.Uses descriptive activity names to name the activities in the data set
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE);
colNames  <- colnames(finalData); 

# 4. Appropriately labels the data set with descriptive variable names. 

for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) <- colNames;

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

finalDataNoActivityType <- finalData[,names(finalData) != 'activityType'];
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
