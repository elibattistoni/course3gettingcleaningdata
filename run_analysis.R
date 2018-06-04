# Final Project Course 3 (Getting and Cleaning Data)
# Dataset: UCI HAR Dataset
# This R script will: 
# - Merge the training and the test sets to create one data set;
# - Extract only the measurements on the mean and standard deviation for each measurement;
# - Use descriptive activity names to name the activities in the data set;
# - Appropriately label the data set with descriptive variable names;
# - From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.

# In the list above are indicated only the most important steps.
# Additional sub-steps to make the data tidier are added.
# Explanatory comments should make the steps clear.

# author: Elisa Battistoni
# June 4th, 2018

####################
## CHECK PACKAGES ##
####################

# Check if the packages that are required in this script are installed:
# if they are, but they are not loaded, then load them (do nothing if they're loaded)
# if they are not, install them and load them.

if ("data.table" %in% rownames(installed.packages())){
    if (!"data.table" %in% (.packages())){library(data.table)}
} else {
    install.packages("data.table"); library(data.table)
}

if ("reshape2" %in% rownames(installed.packages())){
    if (!"reshape2" %in% (.packages())){library(reshape2)}
} else {
    install.packages("reshape2"); library(reshape2)
}

if ("dplyr" %in% rownames(installed.packages())){
    if (!"dplyr" %in% (.packages())){library(dplyr)}
} else {
    install.packages("dplyr"); library(dplyr)
}

###################
## DOWNLOAD DATA ##
###################

# download the data and unzip it in a way that is not-my-computer-specific
url_file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dest_file <- file.path(getwd(),"data.zip")
download.file(url_file, dest_file)
unzip("data.zip")

# now go in the folder with the data
setwd("./UCI HAR Dataset")

#######################
## LOAD DATASET INFO ##
#######################

# load activity information
activity_label <- data.table::fread("activity_labels.txt", col.names = c("ActivityLabel","ActivityName"))

# load measurement information
measure_label <- data.table::fread("features.txt", col.names = c("FeatureLabel","FeatureName"))

# find the indices of the measurements that you are interested in: mean() and std()
idx_measure2keep <- base::grep("(mean|std)\\Q()\\E", measure_label$FeatureName)
# NB: "(mean|std)\\Q()\\E" and "(mean|std)\\(\\)" both match  "mean()" or "std()"

# now select them and assign the values to a new variable called measurement
measurement <- measure_label$FeatureName[idx_measure2keep]
# remove parentheses () to make the measurement labels nicer
measurement <- base::gsub("[()]","",measurement) 

######################################
## LOAD TRAIN & TEST DATA, FIX THEM ## 
######################################

# read into R only the columns (i.e. measurements you're interested in)
# and give each columns the appropriate associated measurement name
train_set <- data.table::fread("./train/X_train.txt", select = idx_measure2keep, col.names = measurement)
test_set <- data.table::fread("./test/X_test.txt", select = idx_measure2keep, col.names = measurement)

# read subjects info
subjects_train <- data.table::fread("./train/subject_train.txt", col.names = "subject")
subjects_test <- data.table::fread("./test/subject_test.txt", col.names = "subject")

# read activities info
activity_train <- data.table::fread("./train/y_train.txt", col.names = "activity")
activity_test <- data.table::fread("./test/y_test.txt", col.names = "activity")

# create the two complete data frames (attach info and data by column)
train_data <- base::cbind(subjects_train, activity_train, train_set)
test_data <- base::cbind(subjects_test, activity_test, test_set)

#######################################################
## MERGE TRAIN & TEST DATA; MAKE THE DATA FRAME TIDY ## 
#######################################################

# now merge them by row
merged_data <- base::rbind(train_data,test_data, fill = T)

# add a column to know what subject is train and what subject is test (as factor)
merged_data$train_vs_test <- as.factor(c(rep("TRAIN",dim(train_set)[1]),rep("TEST",dim(test_set)[1])))

# fix the dataset with descriptive variable names and make them as factors
merged_data$subject <- as.factor(merged_data$subject)
merged_data$activity <- factor(merged_data$activity, levels = activity_label$ActivityLabel,
                               labels = activity_label$ActivityName)

# using the functions of the reshape2 package, melt the data frame to make it more tidy
melted_data <- reshape2::melt(data = merged_data, id = c("subject", "activity","train_vs_test"))

# rename the column "variable" (automatically named by melt) with "measurement"
melted_data <- dplyr::rename(melted_data, measurement = variable) 

# now order the data frame based on subject number
tidy_data <- dplyr::arrange(melted_data, subject)

###############################################
## CREATE A SECOND INDEPENDENT TIDY DATA SET ## 
###############################################
# where the dependent variable is the average of each measurement for each activity and each subject.

# use dplyr chaining to create the new dataset:
# - group the tidy_data by subject x activity x measurement
# - calculate the mean of the measurements in each group
# - assign the result to tidy_data_avg
tidy_data_avg <- tidy_data %>%
    dplyr::group_by(subject,activity,measurement) %>%
    dplyr::summarize(average_value = mean(value))

# since in future analyses you may want to know which subject was test and which was train,
# without going back to the raw files, add a column to the new tidy data set indicating this
# so, first extract the association subject - train vs text from the original tidy data set
subject_train_vs_test <- vector()
for (i in as.numeric(unique(tidy_data$subject))){
    subject_train_vs_test[i] <- as.character(tidy_data$train_vs_test[which(tidy_data$subject == i)[1]])
}
# then, create a vector in which elements ("TRAIN" or "TEST") 
# are based on subject number in tidy_data_2
TRvsTE <- vector(length = dim(tidy_data_2)[1])
for (i in as.numeric(unique(tidy_data_2$subject))){
    TRvsTE[which(tidy_data_2$subject == i)] <- subject_train_vs_test[i]
}
# add the column
tidy_data_2$train_vs_test <- as.factor(TRvsTE)

#####################
## WRITE THE TABLE ## 
#####################
data.table::fwrite(x = tidy_data_2, file = "./tidyData.txt", quote = FALSE)

#############
## THE END ## 
#############
