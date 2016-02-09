# Code Book for tidy.txt
=================

This code book summarizes the resulting data fields in 'tidy.txt' maked in course project for the Getting and Cleaning Data Coursera course.
The original data is from project 'Human Activity Recognition Using Smartphones Dataset' (see below).


## Identifiers

* 'subject' - The ID of the test subject
* 'activity' - The type of activity performed when the corresponding measurements were taken

## Measurements

* 'tBodyAccMeanX'
* 'tBodyAccMeanY'
* 'tBodyAccMeanZ'
* 'tBodyAccStdX'
* 'tBodyAccStdY'
* 'tBodyAccStdZ'
* 'tGravityAccMeanX'
* 'tGravityAccMeanY'
* 'tGravityAccMeanZ'
* 'tGravityAccStdX'
* 'tGravityAccStdY'
* 'tGravityAccStdZ'
* 'tBodyAccJerkMeanX'
* 'tBodyAccJerkMeanY'
* 'tBodyAccJerkMeanZ'
* 'tBodyAccJerkStdX'
* 'tBodyAccJerkStdY'
* 'tBodyAccJerkStdZ'
* 'tBodyGyroMeanX'
* 'tBodyGyroMeanY'
* 'tBodyGyroMeanZ'
* 'tBodyGyroStdX'
* 'tBodyGyroStdY'
* 'tBodyGyroStdZ'
* 'tBodyGyroJerkMeanX'
* 'tBodyGyroJerkMeanY'
* 'tBodyGyroJerkMeanZ'
* 'tBodyGyroJerkStdX'
* 'tBodyGyroJerkStdY'
* 'tBodyGyroJerkStdZ'
* 'tBodyAccMagMean'
* 'tBodyAccMagStd'
* 'tGravityAccMagMean'
* 'tGravityAccMagStd'
* 'tBodyAccJerkMagMean'
* 'tBodyAccJerkMagStd'
* 'tBodyGyroMagMean'
* 'tBodyGyroMagStd'
* 'tBodyGyroJerkMagMean'
* 'tBodyGyroJerkMagStd'
* 'fBodyAccMeanX'
* 'fBodyAccMeanY'
* 'fBodyAccMeanZ'
* 'fBodyAccStdX'
* 'fBodyAccStdY'
* 'fBodyAccStdZ'
* 'fBodyAccMeanFreqX'
* 'fBodyAccMeanFreqY'
* 'fBodyAccMeanFreqZ'
* 'fBodyAccJerkMeanX'
* 'fBodyAccJerkMeanY'
* 'fBodyAccJerkMeanZ'
* 'fBodyAccJerkStdX'
* 'fBodyAccJerkStdY'
* 'fBodyAccJerkStdZ'
* 'fBodyAccJerkMeanFreqX'
* 'fBodyAccJerkMeanFreqY'
* 'fBodyAccJerkMeanFreqZ'
* 'fBodyGyroMeanX'
* 'fBodyGyroMeanY'
* 'fBodyGyroMeanZ'
* 'fBodyGyroStdX'
* 'fBodyGyroStdY'
* 'fBodyGyroStdZ'
* 'fBodyGyroMeanFreqX'
* 'fBodyGyroMeanFreqY'
* 'fBodyGyroMeanFreqZ'
* 'fBodyAccMagMean'
* 'fBodyAccMagStd'
* 'fBodyAccMagMeanFreq'
* 'fBodyBodyAccJerkMagMean'
* 'fBodyBodyAccJerkMagStd'
* 'fBodyBodyAccJerkMagMeanFreq'
* 'fBodyBodyGyroMagMean'
* 'fBodyBodyGyroMagStd'
* 'fBodyBodyGyroMagMeanFreq'
* 'fBodyBodyGyroJerkMagMean'
* 'fBodyBodyGyroJerkMagStd'
* 'fBodyBodyGyroJerkMagMeanFreq'

## Activity Labels

* 'WALKING' (value '1'): subject was walking during the test
* 'WALKING_UPSTAIRS' (value '2'): subject was walking up a staircase during the test
* 'WALKING_DOWNSTAIRS' (value '3'): subject was walking down a staircase during the test
* 'SITTING' (value '4'): subject was sitting during the test
* 'STANDING' (value '5'): subject was standing during the test
* 'LAYING' (value '6'): subject was laying down during the test

---
Original:

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'
