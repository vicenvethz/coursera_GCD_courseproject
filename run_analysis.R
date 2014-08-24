# This is the code for the programming assignment of the 
# getting and cleaning data coursera class
# 
# The code should:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#
# 
# Author: Victor Garcia, ETH Zurich, August 2014
###############################################################################


## 1. Merges the training and the test sets to create one data set ###########
## It is assumed that the folder "UCI HAR Dataset" is located in the working folder
## i.e., we can access data by /

## Training set
subject_train <- read.table(file = "UCI\ HAR\ Dataset/train/subject_train.txt", header = FALSE, sep = "")
X_train <- read.table(file = "UCI\ HAR\ Dataset/train/X_train.txt", header = FALSE, sep = "")
y_train <- read.table(file = "UCI\ HAR\ Dataset/train/y_train.txt", header = FALSE, sep = "")

### read data in: 
## Test set
subject_test <- read.table(file = "UCI\ HAR\ Dataset/test/subject_test.txt", header = FALSE, sep = "")
X_test <- read.table(file = "UCI\ HAR\ Dataset/test/X_test.txt", header = FALSE, sep = "")
y_test <- read.table(file = "UCI\ HAR\ Dataset/test/y_test.txt", header = FALSE, sep = "")



## X test has the same number of rows than all the other files, 2947, but 561 columns. 
## These columns must be the "561-feature vector with time and frequency domain variables" mentioned in Readme.md
## This organization seems to suggest that these data are organized differently from normal tables.
## They seem to be transposed tables, where the features are rows, and the measurements are columns.

## An in depth investigation of the data suggests that it is only the X-data files which contain 
## the important, processed data. The Intertial Signals folders contain unprocessed, raw data. 
## So in order to merge everything into one data set, we can bind X_train and X_test together, 
## as well as the other files

X_data <- numeric()
y_data <- numeric()
subject_data <- numeric()

X_data <- rbind(X_test, X_train)
y_data <- rbind(y_test, y_train)
subject_data <- rbind(subject_test, subject_train)

##  2. Extracts only the measurements on the mean and standard deviation for each measurement. 

### here we extract the names of the features of all columns of the data set X (X_data)
features <- read.table(file = "UCI\ HAR\ Dataset/features.txt", header = FALSE, sep = "")
feature_names <- features[,2]

## We are only interested in those features which contain a mean() or std() in their name
## with the grep method, we contstruct a vector which indicated whether a particlar entry
## of the feature_names vector contains the word mean()
mean_idx <- grepl("mean\\(\\)", feature_names)

## we do the analogous thing for standard deviations
std_idx <- grepl("std\\(\\)", feature_names)

## We construct a vector of indices which indicate whether "mean()" or "std()" are present in the 
## feature name
mean_std_idx <- mean_idx | std_idx

## we remove all the columns which do not contain either mean or std
X_data <- X_data[,mean_std_idx]

features_data <- feature_names[mean_std_idx]
features_data <- as.character(features_data)

# 3. Uses descriptive activity names to name the activities in the data set
## Optimally, we replace the number labels "1,2,3, etc." from the feature names 
## by descriptive names

## We already have been provided with a label-activity translator
activity <- read.table(file = "UCI\ HAR\ Dataset/activity_labels.txt", header = FALSE, sep = "")[,2]

### redefine activyt as a character
activity <- as.character(activity)

##introduce a new activity feature, which is later going to be added 
## as a column to X_data
activity_feature <- dim(X_data)[1]

for (i in 1:6){
	idx <- y_data == i
	activity_feature[idx] <- rep(activity[i], sum(idx))
}

## add "activity" as new feature to X_data
X_data <- cbind(X_data, activity_feature)

## add "activity" to the feature names as a new feature
features_data <- c(features_data, "activity")

# 4. Appropriately labels the data set with descriptive variable names. 

### replace first letter "t" with "time_"
features_data <- as.character(features_data)

### obtain all vector entries that start with t
t_idx <-regexpr("t(.*)", features_data)

### replace the t's with time_
for(i in 1:length(features_data)){
	if(t_idx[i] == 1){
		features_data[i] <- as.character(sub("[t]", "time_", features_data[i]))
	}
}

### obtain all vector entries that start with f
f_idx <-regexpr("f(.*)", features_data)

### replace the t's with fourier_
for(i in 1:length(features_data)){
	if(f_idx[i] == 1){
		features_data[i] <- as.character(sub("[f]", "fourier_", features_data[i]))
	}
}

## should consider transforming everything to lower data
#features_data <- tolower(features_data)

colnames(X_data) <- features_data

write.table(X_data, file = "X_data.txt", row.name=FALSE)

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

### add the subject ID's as a column
X_data <- cbind(X_data, subject_data)

### Add the new feature name "subject_id"
colnames(X_data)[(dim(X_data)[2])] <- "subject_id"

### there exist pairs of activity and subject ID that need to be collapsed/averaged

unique_act <- unique(activity)
unique_ids <- as.matrix(unique(subject_data))
unique_ids <- as.numeric(unique_ids)

n.rows <- dim(X_data)[1]
n.cols <- dim(X_data)[2]

for(i in 1:length(unique_act)){
	for(j in 1:length(unique_ids)){
		
		n.rows <- dim(X_data)[1]
		idx <- (1:n.rows)[X_data[,"activity"] == unique_act[i] & X_data[,"subject_id"] == unique_ids[j]]
		
		if(length(idx) > 0 ){
			means <- colMeans(X_data[idx,-((n.cols-1):n.cols)])
			
			## remove the non-mean row entries
			X_data <- X_data[-idx[2:length(idx)],]
			## fill in the means
			X_data[idx[1], (1:(n.cols-2))] <-  means
			X_data[idx[1],(n.cols-1)]<-	unique_act[i] 
			X_data[idx[1],n.cols] <- unique_ids[j]
			X_data <- as.data.frame(X_data)
		}
	}
}


write.table(X_data, file = "X_data_independent.txt", row.name=FALSE)

