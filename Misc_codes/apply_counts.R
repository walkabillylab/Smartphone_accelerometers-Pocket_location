# Javad Khataei
# July 10th, 2019


#This code apply the counts function on the raw data

##-----------------------Guide----------------------#
## Create a dataframe with the same staring and end point as the data
## with a frequency of 10 hz
# Fulljoin the two dataset
#impute NA by linear intrapolation

##---------------------------------------------------




#import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(activityCounts)
library(imputeTS)


##-------------------------------------The root folder---------------------------------##
# set the working directory to the folder that has the raw data and the intervals file
OS <- Sys.info()
if (OS["sysname"] == "Windows") {
  main_path <-
    "Z:/Research/dfuller/Walkabilly/studies/smarphone_accel/data/Ethica"
} else {
  main_path <-
    "/Volumes/hkr-storage/Research/dfuller/Walkabilly/studies/smarphone_accel/data/Ethica"
}
setwd(dir = main_path)


##--------------------------------Read a file--------------------------------------------#
working_df <- fread("109/109_back.csv")
working_df$record_time <- working_df$record_time %>% as_datetime()

## Create a dataframe with the same staring and end point as the data
longer_df <-
  seq(
    first(working_df$record_time),
    last(working_df$record_time),
    by = period(num = 0.1, units = "second")
  ) %>%  as.data.frame()
colnames(longer_df) <- "record_time"

# na.interapolation works with numeriv data
# save the wear location and add it later
wear_loc <- first(working_df$wear_location)

# Join the datasets
working_df <- full_join(longer_df, working_df , by = "record_time")


#impute NAs by linear intrapolation
working_df <- working_df %>% 
  select(-wear_location) %>% 
  na_interpolation()

# add  wear location
working_df$wear_location <- wear_loc

working_df$x_axis <- working_df$x_axis /10
working_df$y_axis <- working_df$y_axis /10
working_df$z_axis <- working_df$z_axis /10

##---------------------apply counts-------------------##
c <- counts(data = working_df,hertz = 10,x_axis = 2,y_axis = 3,z_axis = 4,time_column = 1)
