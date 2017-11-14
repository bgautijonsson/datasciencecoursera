# Getting and Cleaning data
## Peer Graded Assignment


### Packages to install before running: dplyr and tidyr

### run_analysis.R
Running the code in run_analysis.R will extract the raw processed sensor data from the folders test, and train, corresponding to data from the Samsung experiment and turn it into a tidy managable dataset. When running this file, make sure that the working directory of your R client is set to where the source file is located. 

### features.txt and activity_labels.txt
The file features.txt contains all the 562 features recorded in the original experiment. The file activity_labels.txt contains the label corresponding to each row of measurements in the raw data.

### README.txt and features_info.txt
The files README.txt and features_info.txt contain information about the process of the experiment as a whole and how the features themselves were recorded and processed.


### The output
The file tidy_data.txt contains the outputted table I get when I run the code.



# The R code in-depth

## Step 1

Pulling out the features names from the features.txt file and adding the label column as well as a column for the subjects

## Step 2

Pulling out the training data for the features and for the correct labels, then adding them column-wise up to one big dataset. Then doing the same for the test set data

## Step 3

Merging the train and test data into one big table

## Step 4

Search the feature vector for only features that are relevant for us: The means and standard deviations of the data recordings and adding them to a new smaller feature vector.

## Step 5

Subsetting only the chosen features from the big data table into a smaller table.

## Step 6

Creating the tidy data table by grouping the features according to their subject and label, then calculating the mean of each feature based on their groupings.

## Step 7

Output the ready tidy data to a text file.
