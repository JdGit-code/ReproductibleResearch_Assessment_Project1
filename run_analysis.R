

#----- Run the analysis : creates the tidy data set and saves it under "tidy.txt"

#----- The R code in run_analysis.R proceeds under the assumption that the zip file available at 
#----- https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#----- is downloaded and extracted in the working directory.
#----- We assume the R code is executed in the working directory.

#----- Libraries used

if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("reshape2")) {
  install.packages("reshape2")
}
require("data.table")
require("reshape2")


#----- Read the label of the features and the name of the activities and rename them

activity_label <- as.data.table(read.table("./UCI HAR Dataset/activity_labels.txt", dec="."))
setnames(activity_label,colnames(activity_label),c("ActivityID","ActivityName"))
feature_label <- as.data.table(read.table("./UCI HAR Dataset/features.txt", dec="."))
setnames(feature_label,colnames(feature_label),c("FeatureID","FeatureName"))

#----- Read and store training data

subject_train <- as.data.table(read.table("./UCI HAR Dataset/train/subject_train.txt", dec="."))
activity_train <- as.data.table(read.table("./UCI HAR Dataset/train/y_train.txt",sep="", dec="."))
feature_train <- as.data.table(read.table("./UCI HAR Dataset/train/x_train.txt",sep="", dec=".")) 

#----- Read and store test data

subject_test <- as.data.table(read.table("./UCI HAR Dataset/test/subject_test.txt", dec="."))
activity_test <- as.data.table(read.table("./UCI HAR Dataset/test/y_test.txt", dec="."))
feature_test <- as.data.table(read.table("./UCI HAR Dataset/test/x_test.txt", dec=".")) 

#----- Merge the training and the test sets and rename them

subject <- rbind(subject_train,subject_test)
setnames(subject,colnames(subject),"SubjectID")
activity <- rbind(activity_train,activity_test)
setnames(activity,colnames(activity),"ActivityID")
feature <- rbind(feature_train,feature_test)
setnames(feature,colnames(feature),as.character(feature_label$FeatureName))

#----- Merge the complete data

total_data <- data.table(subject,activity,feature)

#----- Extracts the measurements on the mean and standard deviation for each measurement

required_columns <- c("SubjectID","ActivityID",colnames(total_data)[grep("mean|std",colnames(total_data), ignore.case=TRUE)])
required_columns_ID <- match(required_columns,colnames(total_data))
extracted_data <- total_data[,.SD,.SDcols=required_columns_ID]

#----- Uses descriptive activity names to name the activities in the data set

# add a key in order to match the variables ActivityID and ActivityName 
# the key must be the variable which is in both the activity_label set and the extracted_data set : ActivityID
setkey(activity_label,ActivityID)
setkey(extracted_data,ActivityID)

extracted_data <- extracted_data[activity_label]
# remove the ActivityID column as we add the ActivityName column
extracted_data[,c("ActivityID"):=NULL]


#----- Appropriately labels the data set with descriptive variable names
# Acc can be replaced by Accelerometer
# Gyro can be replaced by Gyroscope
# BodyBody can be replaced by Body
# Mag can be replaced by Magnitude
# Character f can be replaced by Frequency
# Character t can be replaced with Time

names(extracted_data) <-gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data) <-gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data) <-gsub("BodyBody", "Body", names(extracted_data))
names(extracted_data) <-gsub("Mag", "Magnitude", names(extracted_data))
names(extracted_data) <-gsub("^t", "Time", names(extracted_data))
names(extracted_data) <-gsub("^f", "Frequency", names(extracted_data))
names(extracted_data)<-gsub("tBody", "TimeBody", names(extracted_data))
names(extracted_data)<-gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("angle", "Angle", names(extracted_data))
names(extracted_data)<-gsub("gravity", "Gravity", names(extracted_data))


#----- create a second, independent tidy data set with the average of each variable for each activity and each subject

extracted_data_melt <- melt(extracted_data,id.vars=c("SubjectID","ActivityName"),variable.name="FeatureName",value.name="Average")
extracted_data_melt_average <- extracted_data_melt[,.(Average=mean(Average)),by=.(SubjectID,ActivityName,FeatureName)]
tidyData <- as.data.table(dcast(extracted_data_melt_average,formula = SubjectID+ActivityName~FeatureName,value.var="Average"))
tidyData <- tidyData[order(SubjectID, ActivityName)]


#----- creates the tiny data set and saves it under "tidy.txt"
write.table(tidyData, file = "tidy.txt", row.names = FALSE)
