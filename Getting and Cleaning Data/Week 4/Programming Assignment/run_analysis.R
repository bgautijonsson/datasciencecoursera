 # Work in top folder
topdir <- getwd()

## Pulling out the feature names
features_temp1 <- readLines('features.txt')
features_temp2 <- strsplit(features_temp1, ' ')
features <- sapply(features_temp2, function(data) data[2])
features <- c('Label', features)
features <- c('Subject', features)
# Work in train folder
traindir <- paste(topdir, "/train", sep='')
setwd(traindir)

## Create the train datatable
X_train <- read.table('X_train.txt')
Y_train <- read.table('y_train.txt')
train <- cbind(Y_train, X_train)
subjects <- readLines('subject_train.txt')
train <- cbind(subjects, train)
## Put the featurenames onto the columns of train
names(train) <- features

# Work in test folder
testdir <- paste(topdir, "/test", sep='')
setwd(testdir)



## Create the test datatable
X_test <- read.table('X_test.txt')
Y_test <- read.table('y_test.txt')
test <- cbind(Y_test, X_test)
subjects <- readLines('subject_test.txt')
test <- cbind(subjects, test)


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
features_mean_sd <- features[c(1,2, mean_index, std_index)]
data_mean_sd <- data[, features_mean_sd]

## Correctly name the labels to the labeled activity
data_mean_sd$Label <- as.factor(data_mean_sd$Label)
levels(data_mean_sd$Label) <- c('walking', 'walking_upstairs',
                                'walking_downstairs', 'sitting',
                                'standing', 'laying')

data_mean_sd

## Create the tidy data by calculating the mean of each column grouping by the labels.
tidy_data <- data_mean_sd %>%
  group_by(Subject, Label) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))


## Give tidy_data shorter colnames

features_short <- gsub('-', '', features_mean_sd)
features_short <- gsub("\\()", '', features_short)
features_short <- gsub("std", 'Std', features_short)
features_short <- gsub("mean", 'Mean', features_short)
features_short <- gsub("BodyBody", 'Body', features_short)
colnames(tidy_data) <- features_short

## Sort by subject but turn it back into a factor variable afterwards.
tidy_data$Subject <- as.numeric(tidy_data$Subject)
tidy_data <- arrange(tidy_data, Subject)
tidy_data$Subject <- as.factor(tidy_data$Subject)

## Output the tidy table to a file

setwd(topdir)
write.table(tidy_data, file = 'tidy_data.txt', row.names = FALSE)

# Optional: Print out the tidy.data to admire its cleanliness
#tidy_data





