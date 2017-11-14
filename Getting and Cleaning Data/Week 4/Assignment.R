# Work in top folder
setwd("/Users/notandi/Data Science Specialization/Getting and Cleaning Data/Week 4/UCI HAR Dataset")

## Pulling out the feature names
features_temp1 <- readLines('features.txt')
features_temp2 <- strsplit(features_temp1, ' ')
features <- sapply(features_temp2, function(data) data[2])
features <- c('Label', features)
rm(list=list(features_temp1, features_temp2))
# Work in train folder
setwd("/Users/notandi/Data Science Specialization/Getting and Cleaning Data/Week 4/UCI HAR Dataset/train")

## Create the train datatable
X_train <- read.table('X_train.txt')
Y_train <- read.table('Y_train.txt')
train <- cbind(Y_train, X_train)
rm(list=list(X_train, Y_train))
## Put the featurenames onto the columns of train
names(train) <- features

# Work in test folder
setwd("/Users/notandi/Data Science Specialization/Getting and Cleaning Data/Week 4/UCI HAR Dataset/test")


## Create the test datatable
X_test <- read.table('X_test.txt')
Y_test <- read.table('Y_test.txt')
test <- cbind(Y_test, X_test)
rm(list=list(X_test, Y_test))
## Put the feature names onto the columns of test
names(test) <- features


# Data tidying
## Merge the training and test sets as rows
library(tidyr)
library(dplyr)

data <- rbind(train, test)

## Search out which features contain the mean and std variables
mean_index <- grep('mean()', features, fixed = TRUE)
std_index <- grep('std()', features, fixed = TRUE)

## Subset the features vector with only correct variables
## then subset the data with only those columns
features_mean_sd <- features[c(1, mean_index, std_index)]
data_mean_sd <- data[, features_mean_sd]

## Correctly name the labels to the labeled activity
data_mean_sd$Label <- as.factor(data_mean_sd$Label)
levels(data_mean_sd$Label) <- c('walking', 'walking_upstairs',
                                'walking_downstairs', 'sitting',
                                'standing', 'laying')



## Create the tidy data by calculating the mean of each column grouping by the labels.
tidy_data <- data_mean_sd %>%
  group_by(Label) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

tidy_data

## Give tidy_data shorter colnames

features_short <- gsub('-', '', features_mean_sd)
features_short <- gsub("\\()", '', features_short)
features_short <- gsub("std", 'Std', features_short)
features_short <- gsub("mean", 'Mean', features_short)
features_short <- gsub("BodyBody", 'Body', features_short)

colnames(tidy_data) <- features_short

## Output the tidy table to a file
setwd('/Users/notandi/Data Science Specialization/Getting and Cleaning Data/Week 4')
write.table(tidy_data, file = 'tidy_data.txt', row.names = FALSE)






