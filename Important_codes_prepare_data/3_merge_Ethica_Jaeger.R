# Javad Khataei
# 2019-06-29

# This code merge jaeger data with Ethica

library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(imputeTS)
library(tm)



## -------------------------------------The root folder---------------------------------##
# set the working directory to the folder that has the raw data and the intervals file
OS <- Sys.info()
if (OS["sysname"] == "Windows") {
  main_path <-
    "Z:/Research/dfuller/Walkabilly/studies/smarphone_accel/data"
} else {
  main_path <-
    "/Volumes/hkr-storage/Research/dfuller/Walkabilly/studies/smarphone_accel/data"
}
setwd(dir = main_path)


# read intervals file to get the participants ids
participant_list <- fread("Ethica/intervals.csv")


# for participants 100 to 107 and 110, there is no Jaeger data. Also, Ethica data is not available in 2018
# so use participats 108, 109 , 111 to 159
participant_list <-
  participant_list %>%
  na.omit() %>%
  slice((9:59)) %>%
  slice(-3) %>%
  select(userid)

# create a list of possible wear locations
wear_locations <- c("hand", "back", "pock")

# create a dataset to store all the merged data
total_jaeger_df <- NULL


# repeat for each participant in the list
participant_list$userid %>% map(function(participant_id) {
  # create Jaeger file path and name based on participant id
  jaeger_file_path <- paste0("Jaeger/", participant_id, "/")
  jaeger_file_name <-
    list.files(path = jaeger_file_path, pattern = "*_labeled.csv")
  jaeger_file_name <- paste0(jaeger_file_path, jaeger_file_name)

  # if we have the jaeger data then do the rest
  if (file.exists(jaeger_file_name[1])) {
    # read the jaeger data
    jaeger_df <- fread(jaeger_file_name[1])

    jaeger_df$participant_id <- participant_id

    # add the data to the end of total dataset
    total_jaeger_df <<- bind_rows(total_jaeger_df, jaeger_df)
  }
})

# trim the activity, remove whitespace, then numbers
total_jaeger_df$trimmed_activity <- total_jaeger_df$activity %>%
  trimws() %>%
  removeWords(c("1-", "2-", "3-", "4-", "5-", "6-", "7-", "8-", "9-"))

total_jaeger_df$record_time <- total_jaeger_df$record_time %>% as_datetime()



## use this for linear imputation
# read Ethica data
ethica_df <- fread("Ethica/Ethica_imputed_combined.csv")
ethica_df$record_time <- ethica_df$record_time %>% as_datetime()

## New line, order before merging to fix the problem of 132,122,21, and possiby 112
ethica_df <- ethica_df %>% arrange(record_time)

# join the data and impute activity , trimmed_activity
total_df <- left_join(ethica_df, total_jaeger_df) %>% fill(activity, trimmed_activity)

# save the file
fwrite(x = total_df, file = "Ethica_Jaeger_Merged/Ethica_jaeger_merged.csv")



################# Use this for special imputation method

# read Ethica data
ethica_df <- fread("Ethica/Ethica_pocket_special_imputed_combined.csv")
ethica_df$record_time <- ethica_df$record_time %>% as_datetime()

## New line, order before merging to fix the problem of 132,122,21, and possiby 112
ethica_df <- ethica_df %>% arrange(record_time)

# join the data and impute activity , trimmed_activity
total_df <- left_join(ethica_df, total_jaeger_df) %>% fill(activity, trimmed_activity)

# save the file
fwrite(x = total_df, file = "Ethica_Jaeger_Merged/Ethica_jaeger_merged_pocket_special_imputation.csv")
