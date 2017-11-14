# Behind the analysis

## The experiment

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

## The database

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

### Important files:

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'tidy_data.txt': The completed tidy data set made by the R analysis code.

- 'run_analysis.R': The file that performs data cleaning and outputting for you


## How the analysis file works

### 1) Finding the feature names

First we scan the file "features.txt" for all the features that were used in the original experiment and store their names in a character vector. We then concatenate Activity Label and Subject identifiers to that vector.

### 2) Reading the train and test data

For the original experiment, the data was split into train and test data, which is common for machine learning problems. We read in the files "X_train" and "Y_train" from the "train" folder. We now have two data tables. We concatenate the single-column table Y_train onto X_train column-wise so that we have only one table. We do the same for the test set, and then we combine test set and train set into one big set. We then add the feature vector as the column names of the data set.

### 3) Tidying up the data table

Now we want to extract only the features we need from the "features" vector. We do that using string sub-searching, looking for parts of the string containing the words "mean" and "std" for example. After we've found out the indexes of the feature vector that contain the words we want, we can subset the big dataset column-wise by asking for only those features. 

### 4) Calculating the column-wise mean for each group

There are 30 subjects and 6 activity labels. We now make one group of data for each activity label for each subject, which makes 180 groups. We calculate the column-wise mean of each feature for each group. 

### 5) Clean up

We add the labels to the activity label feature and shorten the names of the features so that they look a little bit cleaner.

### 6) Export the table

Last thing we do is to export the new tidy data table.
