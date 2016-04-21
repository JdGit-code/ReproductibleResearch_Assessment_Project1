---
title: "The CodeBook"
output: html_document
---


No other transformations than the ones done in the script has been made on the data. The script merges many the different files, into one single dataset as it has been stated in the exercise (see readme for the exercise's description).

The input is described in the README.txt of the downloaded input (see "UCI HAR Dataset/README.txt" file once the input is downloaded).

The output represents the average of each variable for each activity and each subject. It is a table ; each variable is represented by a column, and each activity / subject is represented by a row. The columns names are slightly modified from the "features.txt" input file content (see the R script run_analysis.R).

The first column contains the subject : for instance 1 is the subject 1
The second column contains the name of the activity : for instance LAYING, SITTING, ... (see UCI HAR Dataset/activity_labels.txt) 

The others columns are the average of each variable for each activity and each subject.