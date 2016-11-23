# Getting and Cleaning Data - project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

# Analysis script requirements
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Analysis script description
The script first loads a reshape2 library, which is needed for the data reshaping part.

    # Load the library for reshaping data
    library(reshape2)
    
Then it downloads the data, puts it in a temporary directory and reads the required files using read.table combined with unz for unzipping. Headers are not read just to ensure that all rows are correctly read.

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
    
Afterwards, the script binds all the required data together and sets the correct column names.

    # Column-bind the information about subjects and activities with the actual data for the test and training set, respectively
    test_data = cbind(test_subjects, test_activities, test_data)
    train_data = cbind(train_subjects, train_activities, train_data)

    # Set the column names of the test and training data set
    names(test_data) = append(c("SubjectID", "ActivityID"), as.character(features[,2]))
    names(train_data) = append(c("SubjectID", "ActivityID"), as.character(features[,2]))

    # Combine the test and training data sets by row-binding
    total_data = rbind(test_data, train_data)
    
Then, the activities IDs are matched with their correct labels. The part where the factor levels are set is hard-coded in order to make the ActivityID column a bit cleaner.

    # Set the column of activity labels to be of the factor type and set the levels correctly
    total_data$ActivityID = as.factor(total_data$ActivityID)
    levels(total_data$ActivityID) = c("Walking", "WalkingUp", "WalkingDown", "Sitting", "Standing", "Laying")
    
Afterwards, the script generates an array of column numbers which include either "mean" or "std" in their names, so that the required columns can be extracted by subsetting, generating "extracted_total" data table. The "extracted_total" is thus the required data set from step 4. of the analysis script requirements.

    # Search the column names of the data for either "mean" or "std", to subset the data according to specification
    mean_or_std = grep("mean|std", names(total_data))

    # Extract the data according to specification
    extracted_total = total_data[,append(c(1,2),mean_or_std)]
    
Then, the script melts all columns representing the measured, dependent variables, leaving the two independent variables being the "SubjectID" and "ActivityID". After that the molten data is reshaped to meet the requirement from step 5. : "independent tidy data set with the average of each variable for each activity and each subject".

    # Melt all columns except "SubjectID" and "ActivityID" and cast the data table in the correct form
    extracted_melt = melt(extracted_total, id = names(extracted_total[1:2]), measure.vars = names(extracted_total[3:81]))
    extracted_meltdata = dcast(extracted_melt, SubjectID + ActivityID ~ variable , mean)

Afterwards, the script outputs the tidy data set in a .csv format and deletes the temporary directory.

    # Output the tidy data set
    write.csv(extracted_meltdata, "tidyData.csv")

    # Delete the temporary directory
    unlink(temp)

Lastly, the script also generates a table of variables and their features used in the codebook.

    # Use the CreateCodeBook function in "CreateCodeBook.R" script to generate a table of variables and their features
    source("CreateCodeBook.R")
    CreateCodeBook(extracted_meltdata)

Taking a look at the CreateCodeBook function, it takes the reshaped molten data from "extracted_meltdata" as an argument to do the following: 
- take the names of the variables in tidy data and put them in "Variable_abbreviated_name" column, 
- read a text file "variable_descriptions.txt" with manually processed information from "features_info.txt" to obtain cleaner descriptions of the variables and put them in "Variable_description" column,
- take the classes of the variables in tidy data and put them in "Variable_class" column
- take the ranges of variables and put them in "Variable_range" column
- take the averages of the appropriate variables and put them in "Variable_mean" column
- write the resulting data frame in a markdown table format to a file

    CreateCodeBook = function(x) {
            result = data.frame(
                    Variable_abbreviated_name = names(x),
                    Variable_description = read.table("variable_descriptions.txt", sep = "\n", stringsAsFactors = F, header = F)[,1],
                    Variable_class = sapply(x, class),
                    Variable_range = sapply(x, function(y) 
                            if (class(y) == "integer" || class(y) == "numeric") {
                                    paste(min(y), max(y), sep = "  -  ")
                            }
                            else if (class(y) == "factor") {
                                    "Walking, WalkingUp, WalkingDown, Sitting, Standing, Laying"
                            }
                    ),
                    Variable_mean = sapply(x, function(y)
                            if (class(y) == "numeric") {
                                    mean(y)
                            }
                            else {
                                    "Not available"
                            }
                    ),
                    row.names = NULL
            )
            write.table(result, "codeBook.md", sep = " | ")
    }
