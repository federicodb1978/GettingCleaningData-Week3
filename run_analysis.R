# Author: Federico Della Bella
# This script accomplishes with the request done by the week 3 of Getting and Cleaning Data
# in the data science specilization of Coursera: 
# https://class.coursera.org/getdata-015/human_grading/view/courses/973502/assessments/3/submissions
# The dataset is here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# The request is the following:
#  You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# My solution is a bit different: in the first part a create a data set with meaningful names 
# for each variable, creating a completed dataset (questions: 1, 3, 4);
# in the second part I extract mean and standard deviation per each variable
# and finally, I create the second independent data set with the mean per each Subject and Activity


run_analysis <-function() {
        
        library(plyr) # to create a summary data frame
        
################################## PART 1 - question 1, 3, and 4 ###############################
#read the datasets, adjust names with meaningful ones, and finally
#create a new dataset

        # read the labels of the 5 types of activitis and the correspondent number
        activity <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
    
        # read the test data set, containing: measured data (x_test), the subject who performed the test (subject_test)
        # and the type of activity performed (y_test)
        
        x_test <- read.table("./UCI_HAR_Dataset/test/x_test.txt") # the test set
        subject_test <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
        y_test<-read.table("./UCI_HAR_Dataset/test/y_test.txt")
        
        # rename the column of subject_test and y_test with something meaningful, thus "Subject" and "Activity
        names(subject_test) <- "Subject"
        names(y_test) <- "Activity"
        
        #transform the column of the subjects and of the activities into factor and rename the levels with something meaningful 
        y_test$Activity <- as.factor(y_test$Activity) # transform the activity into a factor
        levels(y_test$Activity) <- c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying") #replace the type of activity with a meaningful value
        subject_test$Subject <- paste("Tester", as.character(subject_test$Subject), sep="_") #replace the name of tester with a meaningful value
        
        # add some columns to the dataset: the name of the subject, the type population (test or train), and the activity
        x_test <- cbind(subject_test, Population = "Test", y_test, x_test)
        
        #repeat the same with the train set
        
        #read the train dataset, the subjects and the types of activities
        x_train <- read.table("./UCI_HAR_Dataset/train/x_train.txt") # the train set
        subject_train <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")
        y_train <-read.table("./UCI_HAR_Dataset/train/y_train.txt")
        
        # rename the column of subject_train and y_train with something meaningful, thus "Subject" and "Activity
        names(subject_train) <- "Subject"
        names(y_train) <- "Activity"
        
        #transform the column of the subjects and of the activities into factor and rename the levels with something meaningful 
        y_train$Activity <- as.factor(y_train$Activity) # transform the activity into a factor
        levels(y_train$Activity) <- c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying")
        subject_train$Subject <- paste("Trainer", as.character(subject_train$Subject), sep="_") #replace the name of trainer with a meaningful value
        
        # add some columns to the dataset: the name of the subject, the type population (test or train), and the activity
        x_train <- cbind(subject_train, Population = "Train", y_train, x_train)
        
        #now that the 2 dataset are organized with meaningful header and levels, you can create a unique dataset
        #names(x_test) <- names(x_train) # set the same names to each column
        data_complete <- rbind(x_test, x_train)
        
################################## END OF PART 1 ########################################

################################## PART 2: question 2 - mean and standard deviation ########################################

        #extract the mean of eachvalue
        mean_values <- sapply(data_complete[1:nrow(data_complete), 4:ncol(data_complete)], mean)
        standard_values <- sapply(data_complete[1:nrow(data_complete), 4:ncol(data_complete)], sd)
        
################################## END OF PART 2 ########################################


################################## PART 3: question 5 - mean by subject and activty ########################################

        # create a new data framw with the mean for each variable, for each subject and for each activity
        data_complete$Population <- NULL # eliminate population column not used in the following
        
        # create a new data frame, data_summary with the mean per each subject and activity
        data_summary <- aggregate(data_complete, by=list(data_complete$Subject, data_complete$Activity), FUN=mean, na.rm=TRUE)
        data_summary$Subject <- NULL # eliminate the column with NA values (not numeric)
        data_summary$Activity <- NULL #eliminate the column with NA values (not numeric)
        names(data_summary)[1] <-"Subject" # rename the column of the group subject
        names(data_summary)[2] <- "Activity" #rename the column of the group Activity
        
        # order data by subject (tester_10, tester_12, o trainer) and activity(Walkin, Sitting, ...)
        data_summary <- data_summary[order(data_summary$Subject, data_summary$Activity), ]
        
        # write in a new file this new dataset
        write.table(data_summary, file = "data_summary.txt", row.names = FALSE)

        return(data_summary[1:10, 1:5]) # return
}