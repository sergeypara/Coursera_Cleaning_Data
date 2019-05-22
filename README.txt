==================================================================
Dataset generated for the Getting and Cleaning Data Course Project
Version 1.0
==================================================================
Parakhin Sergey
==================================================================


This dataset was generated as the project for the Getting and Cleaning Data Course on Coursera. Dataset(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)


Description for the original dataset
======================================
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


Changes on the original dataset
======================================

The purpose of the project was to combine the original files (described below) in a single dataset. It has information on the subjects, the activities performed and all measurement features that come from either the accelerometer or the gyroscope normalized to [-1,1] and averaged for all data in a single subject for each activity type


Files combined to generate the final dataset:
=========================================

- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
