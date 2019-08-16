# Javad Khataei
# 15/08/2019

# This code applies NN model on Ethica data


library(keras)
library(data.table)
library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(scales)
library(caret)
library(stringr)
library(MLmetrics)
library(PRROC)





# ---------------------------- Read the data ----------------------

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

Ethica_df <-
    fread("Ethica_Jaeger_Merged/pocket/All_participants_pocket_with_count_for_Weka.csv")



working_df  <-
    Ethica_df %>% filter(!(participant_id %in% c(112, 121, 122, 132)))

# Model A only uses raw accel data
working_df   %<>%  dplyr::select(c(x_axis, y_axis, z_axis, trimmed_activity))

# models canot work with strin
working_df$trimmed_activity  %<>% str_replace_all(" ", "_") %>%
    factor()

working_df <-working_df %>% sample_frac(0.01)
data <- working_df %>% select(-trimmed_activity) %>% as.matrix()
labels <- working_df %>% select(trimmed_activity) %>% as.matrix() %>% as.factor() %>% as.integer()

labels <- labels - 1

model <- keras_model_sequential()
set.seed(2020)
opt <- optimizer_adam( clipvalue = 2.0)

# add layers and compile the model
model %>%
    layer_dense(units = 100, activation = 'relu', input_shape = c(3)) %>%
    layer_dense(units = 12, activation = 'relu') %>% 
    layer_dense(units = 6, activation = 'sigmoid') %>%
    compile(
        optimizer = opt,
        loss = 'categorical_crossentropy',
        metrics = c('accuracy')
    )

# # Generate dummy data
# data <- matrix(runif(10000*100), nrow = 10000, ncol = 100)
# labels <- matrix(round(runif(10000, min = 0, max = 2)), nrow = 10000, ncol = 1)
# # Convert labels to categorical one-hot encoding
one_hot_labels <- to_categorical(labels, num_classes = 6)




# Train the model, iterating on the data in batches of 32 samples
model %>% fit(data, one_hot_labels, epochs = 3, batch_size = 10)

