# Javad Khataei
# 01/08/2019


# Model A uses Raw accel data and igoners metadata
# No CV is use. split 75% to 25% train and test
# No parameter tuning is used


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




# ---------------------------- Prepare the data ----------------------------

# Participants 112 , 121 .122 , and 132 are not properly classified
working_df  <-
  Ethica_df %>% filter(!(participant_id %in% c(112, 121, 122, 132)))

# Model A only uses raw accel data
working_df   %<>%  dplyr::select(c(x_axis, y_axis, z_axis, trimmed_activity))

# models canot work with strin
working_df$trimmed_activity  %<>% str_replace_all(" ", "_") %>%
  factor()

# reduce the data size to check if Random Forest is working
working_df  %<>% sample_frac(0.01)


# Create train and test to train and evalute the model
seed <- 812019
set.seed(seed)
training_indices <-
  createDataPartition(working_df$trimmed_activity, p = .66, list = FALSE)

training_df <- working_df %>% slice(training_indices)
testing_df  <- working_df %>% slice(-training_indices)






#------------------------------ lda model ----------------------
# The method is none becuase we have test and train data
# define control and tunning parameters to save with the results
model_name <- "lda"
train_control_method <- "none"
model_parameter <- 10

fitControl <-
  trainControl(method = train_control_method  , classProbs =  TRUE)

model_A <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(parameter = model_parameter),
  metric = "ROC"
)

pred <- predict(model_A, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )

# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results[["LDA"]] <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A,
    train_control_method = train_control_method,
    tune_parameters = c(model_parameter),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
#save(results, file = paste0("Paper1_results/", model_name, ".RData"))
#--------------------- Results ----------------
# Accuracy : 0.23
# F1: 0.37
# the diagonal should be light blue
cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "beige", high = muted("chocolate")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# P-R curve
PRROC::pr.curve(scores.class0 = )



#------------------------------ Naive Bayes Classifier ----------------------
# The method is none becuase we have test and train data
model_name <- "nb"
train_control_method <- "none"
model_fL <- 3
model_usekernel <- TRUE
model_adjust <- 1.5

fitControl <-
  trainControl(method = train_control_method  , classProbs =  TRUE)


model_A_nb <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(
    fL = model_fL ,
    usekernel = model_usekernel ,
    adjust = model_adjust
  ),
  metric = "ROC"
)

pred <- predict(model_A_nb, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A_nb,
    train_control_method = train_control_method,
    tune_parameters = c(model_fL, model_usekernel, model_adjust),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------
# with fL = 3 , usekernel = T , adjust = 1.5
# Accuracy : 0.45
# F1: 0.69



#------------------------------ Random Forest _ Ranger package----------------------
# Ranger is a fast implementation of random forests (Breiman 2001)
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none", classProbs =  TRUE)
model_name <- "ranger"
train_control_method <- "none"
model_mtry <- 2 
model_splitrule <- "extratrees"
model_min_node_size <- 10


model_A_rf <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(mtry = model_mtry, splitrule = model_splitrule, min.node.size = model_min_node_size),  #splitrule = "gini"
  metric = "ROC"
)

pred <- predict(model_A_rf, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A_rf,
    train_control_method = train_control_method,
    tune_parameters = c(model_mtry, model_splitrule, model_min_node_size),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
#save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------




#------------------------------ k-Nearest Neighbors ----------------------
# using kknn package
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)
model_name <- "kknn"
train_control_method <- "none"
model_kmax <- 10
model_kernel <- "optimal"   # Normal unweighted KNN
model_distance <-
  2             # 1 for Manhatan , 2 for Euclidean distance


model_A_kknn <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(
    kmax = model_kmax ,
    kernel = model_kernel,
    distance = model_distance
  ),
  metric = "Accuracy"
)

pred <- predict(model_A_kknn, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A_kknn,
    train_control_method = train_control_method,
    tune_parameters = c(model_kmax, model_kernel, model_distance),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
#save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------
#  Accuracy : 0.9625477 





#------------------------------ Support Vector Machines with Polynomial Kernel ----------------------
# using  kernlab package
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)
model_name <- "svmPoly"
train_control_method <- "none"
model_degree <- 5
model_scale <- 1
model_C <- .1

model_A_svm <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(degree = model_degree,
                        scale = model_scale,
                        C = model_C
  ),
  metric = "ROC"
)

pred <- predict(model_A_svm, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A_svm,
    train_control_method = train_control_method,
    tune_parameters = c(model_degree, model_scale, model_C),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
#save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------
# Not determined yet



#------------------------------ with Linear Kernel ----------------------
# using  e1071 package
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)
model_name <- "svmLinear2"
train_control_method <- "none"
model_cost <- 1

model_A_svm <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(cost = model_cost
  ),
  metric = "ROC"
)

pred <- predict(model_A_svm, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate accuracy
acc <- mean(pred == testing_df$trimmed_activity)
# F1
f1 <- F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

# create a list of the model and the results to save
results <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A_kknn,
    train_control_method = train_control_method,
    tune_parameters = c(model_kmax, model_kernel, model_distance),
    cf_matrix = cf_matrix,
    predictions = pred,
    accuracy = acc,
    f1_score = f1
  )
save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------
# Not determined yet

#-------------------DT ----------------

model_name <- "C5.0"
train_control_method <- "none"
model_trails <- 10
model_model <- "C5.0"
model_winnow <- TRUE

fitControl <-
    trainControl(method = train_control_method, classProbs = TRUE)


model_A_DT <- train(
    trimmed_activity ~ .,
    data = training_df,
    method = model_name,
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = data.frame( trials = model_trails, model = model_model, winnow = model_winnow
    ),
    metric = "ROC"
)

pred <- predict(model_A_DT, newdata = testing_df)

cf_matrix <-
    confusionMatrix(
        data = pred,
        reference = testing_df$trimmed_activity,
        mode = "prec_recall"
    )
# Calculate accuracy and F1
accuracies["NB", "Acc"] <-
    mean(pred == testing_df$trimmed_activity)

accuracies["NB", "F1"] <-
    F1_Score(
        y_true = testing_df$trimmed_activity,
        y_pred = pred
    )

# Create a list of the model and the results to save
results[["NB"]] <-
    list(
        split_seed = seed,
        model_name = model_name,
        model = model_A_nb,
        train_control_method = train_control_method,
        tune_parameters = c(model_fL, model_usekernel, model_adjust),
        cf_matrix = cf_matrix
    )
if (plot) {
    cf_matrix$table %>%
        data.frame() %>%
        ggplot(aes(Prediction, Reference)) +
        geom_tile(aes(fill = Freq), colour = "gray50") +
        scale_fill_gradient(low = "beige", high = muted("chocolate")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle(model_name)
}
