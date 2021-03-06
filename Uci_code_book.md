Code book for the UCI HAR dataset.
----------------------------------

This Code Book describes the data contained in the output of the run\_analysis.R script contained in this repository.

The original UCI HAR Dataset is a public domain dataset built from the recordings of subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensor (see <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>).

As noted in the above referenced document, the original data comes from experiments that were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (walking, walking\_upstairs, walking\_downstairs, sitting, standing, and laying) wearing a Samsung Galaxy S II smartphone on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity were captured at a constant rate of 50Hz. The experiments were video-recorded to label the data manually.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force was assumed to have only low frequency components, so a filter with a 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The script generates a combined subset of the original data by extracting the mean and standard deviation features for each of the 33 processed signals, for a total of 66 features (out of the 561 available features from the original feature vector). This combined subset contains 10299 observations of 68 variables, with activity and subject appended to the 66 features.

The combined subset is further reduced by calculating the mean of the observations by activity and subject pair to generate 180 observations (6 activities \* 30 subjects) of the same 68 variables. This dataset is tidied to generate a narrow and lean dataset containing 11880 observations with 4 variables each and is saved as a text file in the current working directory with the name tidy\_data.txt

Description of each variable of data UCI HAR Datasets variables.
----------------------------------------------------------------

The Tidy dataset consists of 11880 observations summarized by activity (6 categories) and subject (30 volunteers) pairs. For each observation (row) in the Tidy dataset, the following 4 columns are provided:

-   subject,
-   activity,
-   measurement,
-   mean.

### subject

A numeric identifier (1-30) of the subject who carried out the experiment. activity

### Activity names with the following possible values.

-   'laying',
-   'sitting',
-   'standing',
-   'walking',
-   'walking\_downstairs',
-   'walking\_upstairs'.

### Measurement

The name of the measurement for which the mean is calculated. This variable can contain one of the following 66 variables.

-   body-acceleration-jerk-magnitude-mean-frequency
-   body-acceleration-jerk-magnitude-mean-time
-   body-acceleration-jerk-magnitude-std-frequency
-   body-acceleration-jerk-magnitude-std-time
-   body-acceleration-jerk-mean-x-frequency
-   body-acceleration-jerk-mean-x-time
-   body-acceleration-jerk-mean-y-frequency
-   body-acceleration-jerk-mean-y-time
-   body-acceleration-jerk-mean-z-frequency
-   body-acceleration-jerk-mean-z-time
-   body-acceleration-jerk-std-x-frequency
-   body-acceleration-jerk-std-x-time
-   body-acceleration-jerk-std-y-frequency
-   body-acceleration-jerk-std-y-time
-   body-acceleration-jerk-std-z-frequency
-   body-acceleration-jerk-std-z-time
-   body-acceleration-magnitude-mean-frequency
-   body-acceleration-magnitude-mean-time
-   body-acceleration-magnitude-std-frequency
-   body-acceleration-magnitude-std-time
-   body-acceleration-mean-x-frequency
-   body-acceleration-mean-x-time
-   body-acceleration-mean-y-frequency
-   body-acceleration-mean-y-time
-   body-acceleration-mean-z-frequency
-   body-acceleration-mean-z-time
-   body-acceleration-std-x-frequency
-   body-acceleration-std-x-time
-   body-acceleration-std-y-frequency
-   body-acceleration-std-y-time
-   body-acceleration-std-z-frequency
-   body-acceleration-std-z-time
-   body-gyro-jerk-magnitude-mean-frequency
-   body-gyro-jerk-magnitude-mean-time
-   body-gyro-jerk-magnitude-std-frequency
-   body-gyro-jerk-magnitude-std-time
-   body-gyro-jerk-mean-x-time
-   body-gyro-jerk-mean-y-time
-   body-gyro-jerk-mean-z-time
-   body-gyro-jerk-std-x-time
-   body-gyro-jerk-std-y-time
-   body-gyro-jerk-std-z-time
-   body-gyro-magnitude-mean-frequency
-   body-gyro-magnitude-mean-time
-   body-gyro-magnitude-std-frequency
-   body-gyro-magnitude-std-time
-   body-gyro-mean-x-frequency
-   body-gyro-mean-x-time
-   body-gyro-mean-y-frequency
-   body-gyro-mean-y-time
-   body-gyro-mean-z-frequency
-   body-gyro-mean-z-time
-   body-gyro-std-x-frequency
-   body-gyro-std-x-time
-   body-gyro-std-y-frequency
-   body-gyro-std-y-time
-   body-gyro-std-z-frequency
-   body-gyro-std-z-time
-   gravity-acceleration-magnitude-mean-time
-   gravity-acceleration-magnitude-std-time
-   gravity-acceleration-mean-x-time
-   gravity-acceleration-mean-y-time
-   gravity-acceleration-mean-z-time
-   gravity-acceleration-std-x-time
-   gravity-acceleration-std-y-time
-   gravity-acceleration-std-z-time

### mean and standard deviation

mean and standard deviation of measurements.
