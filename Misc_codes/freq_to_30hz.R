## Javad Khataei
## 7/16/2019




#import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(activityCounts)



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

# Read the data
working_df <- fread("150/150_back.csv") 

# count work on normal data( unit is g)
working_df[,2:4] <- working_df[,2:4] / 9.808

working_df %>%  select(2:4) # /10 %>% mutate()

# Repeat the data three times for each timestep
working_df <- working_df[rep(1:nrow(working_df) , each = 3), ]
start_time <- working_df$record_time %>%  first() %>%  as_datetime()

my_count <- counts(data = working_df,x_axis = 2,y_axis = 3,z_axis = 4,hertz = 30,start_time = start_time)


ggplot(data = my_count) +
  geom_point(aes(x =my_count$Time  ,y = my_count$x ))
