# Javad Khataei
# 8/30/2019
# to calculate features importance

#import libraries
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
    fread(paste0(main_path,"/Ethica_Jaeger_Merged/pocket/pocket_with_couns_and_vec_meg_not_duplicated.csv"))


# ---------------------------- Prepare the data ----------------------------

# Participants 112 , 121 .122 , and 132 are not properly classified
working_df  <-
    Ethica_df %>% filter(!(participant_id %in% c(112, 121, 122, 132)))

# Model A_1 only uses raw accel data
working_df %<>%  dplyr::select(c(x_axis, y_axis, z_axis, trimmed_activity))
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
        )
    )









# ------------------------- Add new features -------------------------


frequency <- 10
window_size_sec <- 2
# The number of features reduces by this
reduction_coef <-  window_size_sec * frequency
new_features_df <-
    Beap::GenerateFeatures(raw_df = working_df,
                           window_size_sec = window_size_sec,
                           frequency = frequency) %>%
    dplyr::select(-c("dfr_x", "dfr_y", "dfr_z")) # Eliminates outliers ,i.e. ("dfr_x", "dfr_y", "dfr_z")


# Increase the frequency
new_features_df <-
    new_features_df[rep(1:nrow(new_features_df) , each = reduction_coef),]


# reduce the size of working_df to add new_features
working_df  %<>% slice(1:nrow(new_features_df))
## model A-4 uses new_features too
working_df <- bind_cols(working_df, new_features_df)
# some models cannpt train if there is any NA
working_df  %<>% na_interpolation(option =  "linear")





#------------------------------ split -----------------------

library(rpart)
library(rpart.plot)
set.seed(2020)
# Shrink, center, scale and then split the data
split_ratio <- 0.70
shrink <- 0.01
# Shrink
working_df %<>% dplyr::sample_frac(shrink)
message(paste0(shrink * 100, " % of the data will be used"))

# Split
training_indices <-
    createDataPartition(working_df$trimmed_activity,
                        p = split_ratio,
                        list = FALSE)
training_df <- working_df %>% dplyr::slice(training_indices)
testing_df <- working_df %>% dplyr::slice(-training_indices)




#------------------------------ rpart Ranfom Forest -----------------------

# # Scale and center
# preProcValues <-
#     preProcess(training_df, method = c("center", "scale"))
# training_df <- predict(preProcValues, training_df)
# testing_df <- predict(preProcValues, testing_df)
# message("Data is scaled and centered")

# Create a decision tree model
# minbucket = 10,
tree <- rpart(trimmed_activity ~ ., data = training_df, minsplit = 10,   cp = .000001) # cp = complexity parameter
# Visualize the decision tree with rpart.plot
# rpart.plot(tree,
# box.palette = "RdBu",
# shadow.col = "gray",
# nn = F) # , cex = 0.50 

# training_df_x <- training_df %>% select(-trimmed_activity)
# training_df_y <- training_df %>% select(trimmed_activity)

testing_df_x <- testing_df %>% select(-trimmed_activity)
testing_df_y <- testing_df %>% select(trimmed_activity)

pred_testing_y <- predict(tree, newdata = testing_df_x, type = "class") %>% data.frame()

Accuracy(y_pred = pred_testing_y, y_true = testing_df_y)



# ----------------------------- caret ranger --------------------------------

model_name <- "ranger"
train_control_method <- "none"
model_mtry <- 2
model_splitrule <- "extratrees"
model_min_node_size <- 10
fitControl <-
    trainControl(method = "none", classProbs = TRUE)

model_A <- train(
    trimmed_activity ~ .,
    data = training_df,
    method = model_name,
    trControl = fitControl,
    verbose = FALSE,
    importance = "impurity",
    tuneGrid = data.frame(
        mtry = model_mtry,
        splitrule = model_splitrule,
        min.node.size = model_min_node_size
    ),
    metric = "ROC"
)

pred <- stats::predict(model_A, newdata = testing_df)

# To calculate area AUC we need probabilies and predicted classes in a single dataframe
pred_prob <-
    data.frame(obs =  testing_df$trimmed_activity,
               pred = pred)
pred <-
    stats::predict(model_A, newdata = testing_df, type = "prob")
pred_prob <- bind_cols(pred_prob, pred)

# Calculate different metrics
metrics <-
    multiClassSummary(data = pred_prob,
                      lev = levels(testing_df$trimmed_activity)) %>%
    as.data.frame()

metrics


# ---------------------- Importance ----------------------

importance <- caret::varImp(tree,scale = FALSE)
print(importance)
plot(importance)



importance <- caret::varImp(model_A,scale = FALSE)
print(importance)
plot(importance)



