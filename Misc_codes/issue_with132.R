
library(data.table)
library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(scales)
library(imputeTS)
library(seewave)


working_df <- fread("Z:/Research/dfuller/Walkabilly/studies/smarphone_accel/data/Ethica/132/132_pock.csv")
working_df$record_time %<>% as_datetime()
working_df$x_axis  <- working_df$x_axis / 9.8
working_df$y_axis  <- working_df$y_axis / 9.8
working_df$z_axis  <- working_df$z_axis / 9.8


# # adjust recordtimes
# working_df$record_time %<>% floor_date(unit = "0.1 sec") 
# 


longer_df <-
  seq(
    first(working_df$record_time),
    last(working_df$record_time),
    by = period(num = 0.1, units = "second")
  ) %>%  as.data.frame()
colnames(longer_df) <- "record_time"




# Join the datasets
working_df <-
  full_join(longer_df, working_df , by = "record_time")


# impute NAs by linear intrapolation
# na.interapolation  only works with numeric data
working_df <- working_df %>%
  select(-wear_location) %>%
  na_interpolation()


# plot raw data
working_df %>%  ggplot() +
  geom_point(aes(x = record_time, y= x_axis )) +
  scale_x_datetime(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks( n = 15)) +
  ggtitle("Raw accel data")

# Repeat the data three times for each timestep
working_df <- working_df[rep(1:nrow(working_df) , each = 3), ]

#n <- nrow(working_df) *3

# # change frew from 10 to 30
# ff <- working_df$x_axis  %>% approx(x = working_df$record_time, y = ., n = n)

start_time <- working_df$record_time %>%  first() %>%  as_datetime()


# Calculate counts
counts <- working_df  %>% counts(data = .,x_axis = 2,y_axis = 3,z_axis = 4,hertz = 30, start_time = start_time)
colnames(counts)[1] <- "record_time"


#Join with the raw data and impute 
working_df  %<>% full_join(.,counts) %>% fill(x, y, z)


# delete transit
#working_df  %<>% filter(trimmed_activity != "transit")

final_df <- working_df

final_df %>%  ggplot() +
  geom_point(aes(x = record_time, y= x )) +
  scale_x_datetime(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 15)) +
  ggtitle("Counts for X axis")

