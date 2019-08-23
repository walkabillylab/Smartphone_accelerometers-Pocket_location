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
library(gridExtra)
library(kableExtra)
library(imputeTS)
library(Beap) # It's not on CRAN yet




########################## Data preparation ###########################
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

# Read the file with x,y,z axis and labels.
Ethica_df <-
    fread(paste0(main_path,"/Ethica_Jaeger_Merged/pocket/All_participants_pocket_with_count_for_Weka.csv"))


# ---------------------------- Prepare the data ----------------------------

# Participants 112 , 121 .122 , and 132 are not properly classified
working_df  <-
    Ethica_df %>% filter(!(participant_id %in% c(112, 121, 122, 132)))

# 
working_df %<>%  dplyr::select(c(x_axis, y_axis, z_axis,x ,y ,z, height, weight, age,gender, trimmed_activity))
working_df  %<>% filter(trimmed_activity != "transit")

# models canot work with strin
working_df$trimmed_activity  %<>% str_replace_all(" ", "_") %>%
    factor(
        levels = c(
            "Lying",
            "Sitting",
            "Self_Pace_walk",
            "Running_3_METs",
            "Running_5_METs",
            "Running_7_METs"
        ))



frequency <- 30
window_size_sec <- 2
# The number of features reduces by this
reduction_coef <-  window_size_sec *frequency 
new_features_df <- GenerateFeatures(raw_df = working_df, window_size_sec = window_size_sec, frequency = frequency)


# Increase the frequency
new_features_df <- new_features_df[rep(1:nrow(new_features_df) , each = reduction_coef), ]


# reduce the size of working_df to add new_features
working_df  %<>% slice(1:nrow(new_features_df))
## This model uses new_features too
working_df <- bind_cols(working_df,new_features_df)
# some models cannpt train if there is any NA
working_df  %<>% na_interpolation(option =  "linear")


# delete X, Y, Z axis
#working_df %<>%  dplyr::select(-c(x_axis, y_axis, z_axis))

# To reduce the frequency
## chose n = 60 
working_df  %<>% groupdata2::group(n = 60, method = "greedy") %>%
    rename("id" = .groups)


backup_df <- working_df


working_df  %<>% group_by(id) %>%
    summarise_all(first) %>%
    select(-id)



split_ratio = 0.7
shrink = 1


# Create train and test to train and evalute the model
seed <- 2020
set.seed(seed)

working_df %<>% dplyr::sample_frac(shrink)
message(paste0(shrink * 100, " % of the data will be used"))

training_indices <-
    createDataPartition(working_df$trimmed_activity,
                        p = split_ratio,
                        list = FALSE)

# conver gender to number
working_df$gender  %<>% as.factor() %>% as.integer()

working_df$gender <- working_df$gender -1


# splitting and scalling
training_df <- working_df %>% dplyr::slice(training_indices)

testing_df <- working_df %>% dplyr::slice(-training_indices)
preProcValues <- preProcess(training_df, method = c("center", "scale"))

training_df <- predict(preProcValues, training_df)
testing_df <- predict(preProcValues, testing_df)


message(paste0("Data is devided into training and test set "))
message(paste0(
    "Training set has ",
    nrow(training_df),
    " rows and ",
    ncol(training_df),
    " columns"
))
message(paste0(
    "Testing set has ",
    nrow(testing_df),
    " rows and ",
    ncol(testing_df),
    " columns"
))






######################## End of data preparation ##############################

# models canot work with strin
working_df$trimmed_activity  %<>% str_replace_all(" ", "_") %>%
    factor()





data <- training_df %>% select(-trimmed_activity) %>% as.matrix()
labels <- training_df %>% select(trimmed_activity) %>% as.matrix() %>% as.factor() %>% as.integer()


labels <- labels - 1

model <- keras_model_sequential()
set.seed(2020)
opt <- optimizer_adam( )#clipnorm = 0.1, clipvalue = 10.0)

# add layers and compile the model
model %>%
    layer_dense(units = 65, activation = 'relu', input_shape = c(65)) %>%
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 256, activation = 'relu') %>% 
    layer_dense(units = 256, activation = 'relu') %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
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
model %>% fit(data, one_hot_labels, epochs = 4, batch_size = 10)


#keras::predict_classes()
