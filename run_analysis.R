##########
## Load the tidyverse library

library(tidyverse)

##########
## Make a path to the main project directory

project_path = "/home/paul/coursework/UCI HAR Dataset/"

##########
## Make new feature labels
##########

## Get the list of features

features <- read_table(paste0(project_path,"features.txt"), col_names = FALSE)

## Remove leading digits and separating space from first column of features

feature_labels <- gsub("^[0-9]+[0-9]*\ ", "", features$X1)

## Improve feature label names

feature_labels <- gsub("^t", "time", feature_labels)
feature_labels <- gsub("^f", "frequency", feature_labels)
feature_labels <- gsub("Acc", "Accelerometer", feature_labels)
feature_labels <- gsub("Gyro", "Gyroscope", feature_labels)
feature_labels <- gsub("Mag", "Magnitude", feature_labels)
feature_labels <- gsub("\\(\\)", "", feature_labels)

##########
## Handle test set
##########

## Read in the test set and apply feature labels

test <- read_table(paste0(project_path,"test/X_test.txt"), col_names = feature_labels)

## Read in the activities and subjects, and bind them to the test set

activities <- read_table(paste0(project_path,"test/y_test.txt"), col_names = "activity")
subjects <- read_table(paste0(project_path,"test/subject_test.txt"), col_names = "subject")
test <- cbind(subjects, activities, test)

## Make a vector to control the narrowing of the test set

feature_wanted_cols <- grep("mean|std", names(test))

## Narrow the test set

test_narrowed <- select(test, subject, activity, feature_wanted_cols)

##########
## Handle train set
##########

## Read in the train set and apply feature labels

train <- read_table(paste0(project_path,"train/X_train.txt"), col_names = feature_labels)

## Read in the activities and subjects, and bind them to the train set

activities <- read_table(paste0(project_path,"train/y_train.txt"), col_names = "activity")
subjects <- read_table(paste0(project_path,"train/subject_train.txt"), col_names = "subject")
train <- cbind(subjects, activities, train)

## Make a vector to control the narrowing of the train set

feature_wanted_cols <- grep("mean|std", names(train))

## Narrow the train set

train_narrowed <- select(train, subject, activity, feature_wanted_cols)

##########
## Merge the test and train sets

test_train_narrowed_merged <- rbind(test_narrowed, train_narrowed)

## Convert the digits in the activity column into the correct words

activity_labels <- read_table(paste0(project_path,"activity_labels.txt"), col_names = FALSE)
activity_labels <- tolower(activity_labels$X2)

## Apply the new activity labels to the dataset

test_train_narrowed_merged_tidied <- mutate(test_train_narrowed_merged, activity=activity_labels[activity])

##########
## Create a second, independent tidy data set 
## with the average of each variable 
## for each activity and each subject

df2 <- arrange(test_train_narrowed_merged_tidied, activity, subject)

tidy_set <- df2 %>% group_by(activity, subject) %>% summarise_each(funs(mean))

write.table(tidy_set, paste0(project_path,"/tidy_set.txt"), row.names=FALSE)

##########
