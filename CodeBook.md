This code book describes the variables, the data, and any transformations 
or work that i performed to clean up the data

##Introduction

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 


##Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

##Process:

1. Loaded tables:
train/X_train.txt, train/y_train.txt, train/subject_train.txt
test/X_test.txt, test/y_test.txt, test/subject_test.txt
in R


2. Joined tables 
train/X_train.txt, train/y_train.txt, train/subject_train.txt
and 
test/X_test.txt, test/y_test.txt, test/subject_test.txt
then merged result in one big data set


3. Removed all the columns, which are NOT activity, subject columns and NOT contain mean() or std() substrings


4. Replaced column names with readable names
For this purpos took appropriate names from features.txt, replaced "-", "(" and ")" signs, 
example of transformation is shown below:

tBodyAcc-mean()-X   ->  tBodyAccMeanX

Decided not to bring all the letters to lower case because I think it is less readable


5. Grouped data by subject and activity and then summarized data, geting mean value for each subject-activity pair


6. Replaced activity numbers by readable labels, according to activity_labels.txt