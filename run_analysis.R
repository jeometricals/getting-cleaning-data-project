#Getting and Cleaning Data Project
#Reza Mousavian
#July 2015

#This script will perform the following steps on the UCI HAR Dataset in order to:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#######################################################################################
# 1. Merge the training and the test sets to create one data set.
#######################################################################################
library(reshape2)

filename <- "getdata_dataset.zip"

# Download and unzip the dataset:
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

# Read in the data from files
features = read.table("features.txt", header=FALSE)
activityType = read.table("activity_labels.txt", header=FALSE)

subjectTrain = read.table("./train/subject_train.txt", header=FALSE)
xTrain = read.table("./train/X_train.txt", header=FALSE)
yTrain = read.table("./train/Y_train.txt", header=FALSE)

subjectTest = read.table("./test/subject_test.txt", header=FALSE)
xTest = read.table("./test/X_test.txt", header=FALSE)
yTest = read.table("./test/Y_test.txt", header=FALSE)

#Create column names and final mergerd data set

colnames(activityType) = c("activityId", "activityType")
colnames(subjectTrain) = "subjectId"
colnames(subjectTest) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"
colnames(xTest) = colnames(xTrain)
colnames(yTest) = colnames(yTrain)

train <- cbind(subjectTrain, yTrain, xTrain)
test <- cbind(subjectTest, yTest, xTest)

finalData = rbind(train,test)

###########################################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
###########################################################################################
dataMeanStd <- finalData[,grepl("mean|std|subjectId|activityId", names(finalData))]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################
library(plyr)

dataMeanStd <- join(dataMeanStd, activityType, by = "activityId", match = "first")
dataMeanStd <- dataMeanStd[,-1]

##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################
colNames  = colnames(finalData)
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

melted <- melt(dataMeanStd, id= c("subjectId", "activityType"))
tidy <- dcast(melted, subjectId+activityType ~ variable, mean)
write.csv(tidy, "tidy.csv", row.names=FALSE)

