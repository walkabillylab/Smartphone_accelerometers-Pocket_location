# Javad Khataei
# Sept 10th, 2019

# To extract raw data and change the frequency to 30Hz
# This file is similar to 1_Ethica_reader_extract_from_intervals.R but 
# returns 30Hz data

# import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(signal)

## ------------------------ Convert frequency function------------------------------##
convert_freq <- function(df) {
  df <- df %>% select(record_time, x_axis, y_axis, z_axis)
  # convert character to data
  df$record_time <- df$record_time %>% as_datetime()

  # change frequency to 10 hz
  ## floor or round the datetime to 1s intervals to calculate freqin each second
  df <-
    df$record_time %>%
    floor_date(unit = "1s") %>%
    mutate(df, record_time = .)

  # tally shows the frew for each sec
  df <- df %>%
    group_by(record_time) %>%
    select(x_axis, y_axis, z_axis) %>%
    add_tally()
  
  # Signal package(resample function) uses period not freq (period = 1 /freq)
  # Calculate period
  df$n <- 1/ df$n
  
  # apply resample and get a dataframe which has a list of 30 reading for each sec
  df_30hrz <-
    df %>%
    group_by(record_time) %>%
    select(x_axis, y_axis, z_axis, n)  %>%
    mutate(x_axis_c = x_axis, y_axis_c = y_axis, z_axis_c = z_axis) %>% 
    nest(x_axis_c, y_axis_c, z_axis_c , n) %>% 
    mutate(
      new_x =  map(data, ~signal::resample(.$x_axis_c, first((.$n)), 0.033333)),
      new_y =  map(data, ~signal::resample(.$y_axis_c, first((.$n)), 0.033333)),
      new_z =  map(data, ~signal::resample(.$z_axis_c, first((.$n)), 0.033333)),
      ) %>%  select(record_time , new_x , new_y, new_z)

  # Unlist ( expand) the create dataset for combining with time
  
  df_30hz_unlist <-
    data.frame(
      x_axis = unlist(df_30hrz$new_x),
      y_axis = unlist(df_30hrz$new_y),
      z_axis = unlist(df_30hrz$new_z)
    )
  

  df_extended <- df_30hrz$record_time  %>%  as.data.frame()  

  
  df_extended <- df_extended[rep(1:nrow(df_extended) , each = 30), ] %>%  as.data.frame() 
  colnames(df_extended) <- "record_sec" 
  
  df_extended %<>% group_by(record_sec) %>% 
    mutate(record_time = seq(from = first(record_sec) , by = period("0.033333s") , length.out = 30)) %>%
    select(record_time)
  


  # df_extended has the times and df_30hz_unlist has the readings
  df_final <- cbind(df_extended$record_time, df_30hz_unlist)
  
  
  
  
  return(df_30hrz)
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
      # add 30Hz to the end of each file
      fwrite(x = df_slice, file = paste0(p_id, "_", wear_loc, "_30Hz.csv"))
      setwd(dir = main_path)
    })
  }
}
