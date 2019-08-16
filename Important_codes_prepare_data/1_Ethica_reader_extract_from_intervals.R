# Javad Khataei
# July 10th, 2019


# This code extract Ethica data from raw CSV files

# import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)

## ------------------------ Convert frequency function------------------------------##
convert_freq <- function(df) {
  df <- df %>% select(record_time, x_axis, y_axis, z_axis)
  # convert character to data
  df$record_time <- df$record_time %>% as_datetime()

  # change frequency to 10 hz
  ## floor or round the datetime to 0.1s intervals
  df <-
    df$record_time %>%
    floor_date(unit = "0.1s") %>%
    mutate(df, record_time = .)

  df_10hrz <-
    df %>%
    group_by(record_time) %>%
    select(x_axis, y_axis, z_axis) %>%
    summarise(
      x_axis = mean(x_axis),
      y_axis = mean(y_axis),
      z_axis = mean(z_axis)
    )

  return(df_10hrz)
}


## -------------------------------------The root folder---------------------------------##
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



## ------------------------- loop through all months and locations---------------------##
mons <- c("01", "02", "03", "04", "05")

dev_ids <- c("2673", "2674", "2675")
# #set the study year
study_year <- "2019"


for (dev_id in dev_ids) {
  if (dev_id == "2673") {
    wear_loc <- "hand"
  }
  if (dev_id == "2674") {
    wear_loc <- "back"
  }
  if (dev_id == "2675") {
    wear_loc <- "pock"
  }


  for (mon in mons) {

    # create study's year and month
    study_year_month <- paste0(study_year, "_", mon)

    # create the file name to read the data
    file_name <-
      paste0(dev_id, "_", study_year, "_", mon, "_accelerometer.csv")


    # read the accelerameter file
    df <- fread(file = file_name)
    df$record_time <- df$record_time %>% as_datetime()



    # change the frequency by calling convert_freq function
    df <- convert_freq(df)


    # read the interval files that has all the intrvals for different participants
    intervals_file <- fread(file = "intervals.csv")


    # get the year and the month out of intervals_file
    intervals_file <-
      as.Date(intervals_file$start) %>%
      format(format = "%Y_%m") %>%
      as.character() %>%
      mutate(intervals_file, year_month = .)

    # Select participants who participated during study_year_month
    Selected_part <-
      intervals_file %>% filter(intervals_file$year_month == study_year_month)


    # for each selected participant
    Selected_part$userid %>% map(function(i) {

      # get the current participant and extract its info,i.e id, start and end time
      current_participant <-
        Selected_part %>% filter(Selected_part$userid == i)

      participant_id <- current_participant$userid

      participant_start_time <-
        current_participant$start %>% as_datetime()

      participant_end_time <- current_participant$end %>% as_datetime()




      # adjust the raw data time using a temporary var
      daylight_saving <- "2019-03-10"


      # Participants after daylight saving
      temp_df <- df
      if (participant_start_time > daylight_saving) {
        if (dev_id == "2673") {
          temp_df$record_time <- temp_df$record_time - hms("2:32:00")
        } else if (dev_id == "2674") {
          temp_df$record_time <- temp_df$record_time - hms("2:38:00")
        } else if (dev_id == "2675") {
          temp_df$record_time <- temp_df$record_time - hms("2:36:00")
        }
      } else {
        # Before daylight saving
        if (dev_id == "2673") {
          temp_df$record_time <- temp_df$record_time - hms("3:32:00")
        } else if (dev_id == "2674") {
          temp_df$record_time <- temp_df$record_time - hms("3:38:00")
        } else if (dev_id == "2675") {
          temp_df$record_time <- temp_df$record_time - hms("3:36:00")
        }
      }

      # find the start and the end of the participant's data
      record_time <-
        temp_df$record_time %>%
        as_datetime() %>%
        floor_date(unit = "second")
      start_time_index <-
        which(record_time > participant_start_time) %>% first()
      end_time_index <-
        which(record_time < participant_end_time) %>% last()


      # slice the participant's part
      df_slice <- temp_df %>% slice(start_time_index:end_time_index)


      # add participant's id and waer location
      df_slice <-
        df_slice %>% mutate(participant_id = participant_id, wear_location = wear_loc)
      p_id <- as.character(participant_id)
      dir.create(p_id, showWarnings = FALSE)
      setwd(paste0(p_id, "/"))
      fwrite(x = df_slice, file = paste0(p_id, "_", wear_loc, ".csv"))
      setwd(dir = main_path)
    })
  }
}
