## Javad Khataei
## 7/16/2019
## First Model

#---------------------------------------------------------------#
#This code reads labelled Ethica data.
#It selects data for the pocket location,
#calculates counts for each participant and creates a file for Weka.
#Also, it plots counts in X direction for all participants



#import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(scales)


##-------------------------The root folder----------------------##
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

working_df <- fread("Ethica_Jaeger_Merged/Ethica_jaeger_merged_metadata.csv")

# Filter based on the wear location
## Pocket location
working_df <- working_df %>% filter(wear_location == "pock")  %>% 
  data.frame()

# Eliminate participant 135 and 148 due to missing Jaeger data
working_df  %<>% filter(participant_id != 135 & participant_id != 148)

## change timesteps class
working_df$record_time <- working_df$record_time %>%  as_datetime()


# deselect some Jaeger columns
working_df  %<>% select(-c(7:16))

# convert accel data to normal data (unit was g)
working_df[,2:4] <- working_df[,2:4] / 9.808

# save for Weka
fwrite(working_df, "Ethica_Jaeger_Merged/pocket/pocket.csv")


# create data set for Weka
##---------------------------------------------------------##
main_df <- fread("Ethica_Jaeger_Merged/pocket/pocket.csv")

# convert string to time class
main_df$record_time  %<>% as_datetime()

# an empty dataset for al participants plus their counts
final_df <- NULL

#Get participant list
par_list <- unique(main_df$participant_id)

par_list  %>% map(function(p_id){
  
  # filter for each participant 
  working_df <- main_df %>% filter( participant_id == p_id)
  
  # Repeat the data three times for each timestep
  working_df <- working_df[rep(1:nrow(working_df) , each = 3), ]
  start_time <- working_df$record_time %>%  first() %>%  as_datetime()
  
  
  # Calculate counts
  counts <- working_df  %>% counts(data = .,x_axis = 2,y_axis = 3,z_axis = 4,hertz = 30, start_time = start_time)
  colnames(counts)[1] <- "record_time"
  
  
  #Join with the raw data and impute 
  working_df  %<>% full_join(.,counts) %>% fill(x, y, z)
  
  
  # delete transit
  working_df  %<>% filter(trimmed_activity != "transit")
  
  final_df  <<- bind_rows(final_df,working_df)
  
 
})


#save for Weka
fwrite(final_df,"Ethica_Jaeger_Merged/pocket/All_participants_pocket_with_count_for_Weka.csv")




#plot and save for each paricipant
par_list  %>% map(function(p_id){
  counts_plot <- final_df %>% filter(participant_id == p_id) %>% ggplot() +
  geom_point(aes(x = record_time, y= x ,col = trimmed_activity)) +
    scale_x_datetime(breaks = pretty_breaks(n = 10))
  
  ggsave(filename = paste0("Ethica_Jaeger_Merged/pocket/",p_id,"_counts_plot.jpeg"), plot = counts_plot)
  
})







#### Repeat for special imputation method


##-------------------------The root folder----------------------##
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

working_df <- fread("Ethica_Jaeger_Merged/Ethica_jaeger_merged_metadata_special_imputatiom.csv")

# Filter based on the wear location
## Pocket location
working_df <- working_df %>% filter(wear_location == "pock")  %>% 
  data.frame()

# Eliminate participant 135 and 148 due to missing Jaeger data
working_df  %<>% filter(participant_id != 135 & participant_id != 148)

## change timesteps class
working_df$record_time <- working_df$record_time %>%  as_datetime()


# deselect some Jaeger columns
working_df  %<>% select(-c(7:16))

# convert accel data to normal data (unit was g)
working_df[,2:4] <- working_df[,2:4] / 9.808

# save for Weka
fwrite(working_df, "Ethica_Jaeger_Merged/pocket/pocket_special.csv")


# create data set for Weka
##---------------------------------------------------------##
main_df <- working_df

# convert string to time class
main_df$record_time  %<>% as_datetime()

# an empty dataset for al participants plus their counts
final_df <- NULL

#Get participant list
par_list <- unique(main_df$participant_id)

par_list  %>% map(function(p_id){
  
  # filter for each participant 
  working_df <- main_df %>% filter( participant_id == p_id)
  
  # Repeat the data three times for each timestep
  working_df <- working_df[rep(1:nrow(working_df) , each = 3), ]
  start_time <- working_df$record_time %>%  first() %>%  as_datetime()
  
  
  # Calculate counts
  counts <- working_df  %>% counts(data = .,x_axis = 2,y_axis = 3,z_axis = 4,hertz = 30, start_time = start_time)
  colnames(counts)[1] <- "record_time"
  
  
  #Join with the raw data and impute 
  working_df  %<>% full_join(.,counts) %>% fill(x, y, z)
  
  
  # delete transit
  working_df  %<>% filter(trimmed_activity != "transit")
  
  final_df  <<- bind_rows(final_df,working_df)
  
  
})


#save for Weka
fwrite(final_df,"Ethica_Jaeger_Merged/pocket/All_participants_pocket_with_count_special_imputation.csv")




#plot and save for each paricipant
par_list  %>% map(function(p_id){
  counts_plot <- final_df %>% filter(participant_id == p_id) %>% ggplot() +
    geom_point(aes(x = record_time, y= x ,col = trimmed_activity)) +
    scale_x_datetime(breaks = pretty_breaks(n = 10))
  
  ggsave(filename = paste0("Ethica_Jaeger_Merged/pocket/",p_id,"_counts_plot_special_imputation.jpeg"), plot = counts_plot)
  
})



  