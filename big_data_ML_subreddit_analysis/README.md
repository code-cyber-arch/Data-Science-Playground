This repository contains an R script for processing and analyzing Reddit data. The dataset used in this analysis is a filtered subset of Reddit posts from September 2017. The script follows a series of data cleaning, transformation, and analysis steps to prepare the data for modeling and prediction.

Requirements
R (version 3.6 or higher)
R packages: tidyverse, lubridate, tm, SnowballC, forcats, patchwork, broom
Steps in the Script
Step 1: Reading the Dataset
The script sets the working directory and reads the dataset RS_2017-09_filtered70.csv into a dataframe called reddit_df.
Step 2: Removing Index Variable
The first column, which is an index variable, is removed from the dataframe.
Step 3: Identifying and Removing Columns with All NA Values
Columns containing only NA values are identified and removed.
Step 4: Identifying and Removing Columns with All One Value
Columns containing only one unique value are identified and removed.
Step 5: Date and Time Conversion
The created_utc and retrieved_on columns are converted to weekday factors and removed from the dataframe.
Step 6: Text Processing of Titles
Titles are processed using the tm package to convert to lowercase, remove numbers, stop words, punctuation, and extra whitespace, and perform stemming.
An incidence matrix of the processed titles is created, and words appearing in at least 500 posts are added to the dataframe.
A list of these frequent words is saved to list_words.csv.
Step 7: Recording Factors with Low Frequency Levels
Factors where any level occurs fewer than 30 times are identified and lumped into a single category.
Step 8: Recording Factors with Many Levels
Factors with more than 100 levels are amalgamated, with levels beyond 100 lumped into an "other" category.
Step 9: Removing Low Frequency Binary Factors
Binary factors where one level occurs fewer than 30 times are identified and removed.
Step 10: Identifying and Removing Columns with Missing Values
Columns with any missing values are identified and removed.
Step 11: Final Checking
A final check is performed to ensure no columns with all one value remain.
The structure of the dataframe is inspected.
Step 12: Data Transformation
Numerical variables are identified and transformed as necessary.
Plots of score versus other variables are created using ggplot2 and patchwork.
Step 13: Assumptions and Summary
A regression model is created and diagnostic plots are generated.
Step 14: Aliased Factors
Aliased factors (those with NA coefficients in the regression model) are identified and removed.
The updated regression model is checked.
Step 15: Simplification of the Model
Simplification steps are performed by removing specific variables (e.g., author_cakeday, contest_mode) and updating the regression model.
Step 16: Prediction
Predictions with 95% confidence intervals are made for a specific record.
The effects of altering certain variables on the prediction are explored.
Output Files
list_words.csv: List of words that appear in at least 500 posts.
reddit_df.RData: The cleaned and transformed dataframe.
aliased_factors.csv: List of aliased factors identified in the regression model.
Usage
Ensure all required packages are installed.
Set the working directory and place the dataset RS_2017-09_filtered70.csv in the directory.
Source the script to perform the analysis.
r
Copy code
source("reddit_data_analysis.R")
Author
This script was developed by Abdurahman. For any questions or feedback, please contact archabdulrm@gmail.com.
