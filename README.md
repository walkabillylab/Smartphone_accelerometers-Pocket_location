# A list of all the files regarding the Paper #1 and a brief explanation for each file. 

## Important_codes_prepare_data Folder

#### 1_Ethica_reader_extract_from_intervals.R

This code extract Ethica data from raw CSV files.

### 2_Combine_ethica_data.Rmd
This file reads  Ethica files for each participant and different wear locations. Then it imputed the gaps of the accelerometer data, and finally, it binds them together to have a single dataset.

### 3_merge_Ethica_Jaeger.R

This code merge jaeger data with Ethica.

### 4_Add_Metadata.Rmd

This code adds Age Height Weight and Gender to Ethica_jaeger_combined dataset.

### 5_Create_pocket_Counts_Weka_data.R

This code reads labelled Ethica data. It selects data for the pocket location, calculates counts for each participant and creates a file for Weka. Also, it plots counts in X direction for all participants.

### Readme_First.txt

Explains how to prepare the Ethica data.

## Misc codes Folder

### apply_counts.R

This file is an **example** of how to apply counts function. 
It create a dataframe with the same staring and end point as the data, with a frequency of 10 Hz. Fulljoin the two dataset , and impute NA by linear intrapolation.

### Ethica_time_adjustment_report_after_solving.Rmd

This code plots accel data and color it based on the activity. Use this to check if the time adjustment is working.

### feature_generator.R

This is the early version of a feature generator function used to produce various features, written by Fara. **Depreciated**  

### generate-features.R

Use this file to create new features from raw accel data.

### freq_to_30hz.R

An **example** of how to increase the frequency from 10 to 30 Hz.

### issue_with132.R

A crude attempt to investigate and tackle the issue we have with the participant's #132 data.     
Participants 112, 121, 122, and 132 have a peculiar problem. The data seems normal, but when we join the data with a dataset that has all the timesteps from the beginning and the end of the study, they don't merge properly. As a result, after calculating the counts, labelling is distorted. This is shown in the graph produced by the "5_Create_pocket_Counts_Weka_data.R" file.

### plot_counts_ethica_jaeger_combined.Rmd

This file plots counts calculted by the library "activityCounts" for each participant.


## Models_Folder

### Model_for_synching_time.R

This code creates a model which can be used to adjust the timestamps.

### modelA.R

This file contains different models such as LDA, KNN, RF, SVM, etc.

- Model A uses Raw accel data and ignores metadata.
- No CV is used. Split 75% to 25% train and test, or 66% and 33%.
- No parameter tuning is used.

### modelA_working_and_notworking.R

This file has many many models, and some of them might not work. Use this file to test the models and choose between different model providing an algorithm. This is a draft file.


## Validation Folder

### Counts_Plot.R
This file is similar to "5_Create_pocket_Counts_Weka_data.R"
This code reads labelled Ethica data. It selects data for the pocket location, calculates counts for each participant and creates a file for Weka. Also, it plots counts in X direction for all participants

### Imputation_before_counts.Rmd

As there are missing observations for various timestamps in the raw data, different imputation methods are used before applying the Count function.  
This file compares them and suggest a new method for imputation, which seems to the best.

### Sync_Ethica_jaeger.Rmd

This is an *unfinished attempt* to do all the data processing from the beginning and prepare the data. This is similar to "Sync_Ethica_jaeger_with40hz_timingfile.Rmd" file.

### Sync_Ethica_jaeger_with40hz_timingfile.Rmd

This is similar to "Sync_Ethica_jaeger.Rmd" file, Its main steps are:

1. Read Ethica raw data
2. Focus on one participant e.g. 108
3. From Jaeger folder read timing and extract start and end time
5. Loops 10 times and each time
    1. Alter record time, and change the frequency
    2. Create a longer dataframe and join it with timing.csv
    3. join the result and Ethica
    4. Implement a model and calculate store accuracy
6. print the highest accuracy and the time period added to the Ethica


### Test_counts_function_for_different_frequencies.Rmd

As a part of the data preparation process, we need to investigate if have a freq of 30  and a repeated copy of that dataset, which has a pseudo frequency of 90Hz, are actually producing the same counts.


## The main folder files



### Ethica_Data_Preparation_Steps_Explanation .Rmd

This file is an explanation for the data cleaning and extracting process. To extract the data, read the readme file and do not run this code. 

### Steps.Rmd

This file explains the data cleaning and extracting process. The steps that we need to take to prepare the data are

### README.md

A list of all the files inside the main folder and the subfolders, and their functionality.
