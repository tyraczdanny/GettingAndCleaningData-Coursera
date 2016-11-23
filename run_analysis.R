# Load the library for reshaping data
library(reshape2)

# Download the data set and place it in a temporary directory
temp = tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

# Read the appropriate data needed
test_data = read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"), header = F)
test_subjects = read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"), header = F)
test_activities = read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"), header = F)
train_data = read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"), header = F)
train_subjects = read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"), header = F)
train_activities = read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"), header = F)

# Read the variable names for the data loaded above
features = read.table(unz(temp, "UCI HAR Dataset/features.txt"), header = F)

# Column-bind the information about subjects and activities with the actual data for the test and training set, respectively
test_data = cbind(test_subjects, test_activities, test_data)
train_data = cbind(train_subjects, train_activities, train_data)

# Set the column names of the test and training data set
names(test_data) = append(c("SubjectID", "ActivityID"), as.character(features[,2]))
names(train_data) = append(c("SubjectID", "ActivityID"), as.character(features[,2]))

# Combine the test and training data sets by row-binding
total_data = rbind(test_data, train_data)

# Set the column of activity labels to be of the factor type and set the levels correctly
total_data$ActivityID = as.factor(total_data$ActivityID)
levels(total_data$ActivityID) = c("Walking", "WalkingUp", "WalkingDown", "Sitting", "Standing", "Laying")

# Search the column names of the data for either "mean" or "std", to subset the data according to specification
mean_or_std = grep("mean|std", names(total_data))

# Extract the data according to specification
extracted_total = total_data[,append(c(1,2),mean_or_std)]

# Melt all columns except "SubjectID" and "ActivityID" and cast the data table in the correct form
extracted_melt = melt(extracted_total, id = names(extracted_total[1:2]), measure.vars = names(extracted_total[3:81]))
extracted_meltdata = dcast(extracted_melt, SubjectID + ActivityID ~ variable , mean)

# Use the CreateCodeBook function in "CreateCodeBook.R" script to generate a table of variables and their features
source("CreateCodeBook.R")
CreateCodeBook(extracted_meltdata)

# Output the tidy data set
write.csv(extracted_meltdata, "tidyData.csv")

# Delete the temporary directory
unlink(temp)