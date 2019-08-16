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
results <-
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
save(results, file = paste0("Paper1_results/", model_name, ".RData"))
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

# #------------------------------loclda variation ----------------------
# # A localized version of Linear Discriminant Analysis
# # The method is none becuase we have test and train data
# fitControl <- trainControl(method = "none"  ,classProbs =  TRUE)
#
#
# model_A_loclda <- train(
#   trimmed_activity ~ .,
#   data = training_df,
#   method = "loclda",
#   trControl = fitControl,
#   verbose = FALSE,
#   tuneGrid = data.frame(k = 30
#   ),
#   metric = "ROC"
# )
#
# pred <- predict(model_A_loclda, newdata = testing_df)
#
# cf_matrix <- confusionMatrix(data = pred , reference = testing_df$trimmed_activity)
#


#------------------------------ Penalized Multinomial Regression ----------------------
# Fits multinomial log-linear models via neural networks.

# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)


model_A_multinom <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = "multinom",
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(decay = 0.1),
  metric = "ROC"
)

pred <- predict(model_A_multinom, newdata = testing_df)

cf_matrix <-
  confusionMatrix(
    data = pred ,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )

# to compare the results
print(cf_matrix$overall[1])
# Another way to calculate the accuracy
mean(pred == testing_df$trimmed_activity)
# F1
F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)

#--------------------- Results ----------------
# Accuracy : 0.23
# F1: 0.37



#
# #------------------------------ Flexible Discriminant Analysis ----------------------
# #
# # The method is none becuase we have test and train data
# fitControl <- trainControl(method = "none"  ,classProbs =  TRUE)
#
#
# model_A_fda <- train(
#   trimmed_activity ~ .,
#   data = training_df,
#   method = "fda",
#   trControl = fitControl,
#   verbose = FALSE,
#   tuneGrid = data.frame( degree = 1 , nprune = 1000
#   ),
#   metric = "ROC"
# )
#
# pred <- predict(model_A_fda, newdata = testing_df)
#
# cf_matrix <- confusionMatrix(data = pred , reference = testing_df$trimmed_activity, mode = "prec_recall")
#
# # to compare the results
# print(cf_matrix$overall[1])
# # Another way to calculate the accuracy
# mean(pred == testing_df$trimmed_activity )
# # F1
# F1_Score(y_true = testing_df$trimmed_activity, y_pred = pred)
#
# #--------------------- Results ----------------
# # Doesn't work
#







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



#------------------------------ k-Nearest Neighbors ----------------------
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)
model_name <- "knn"
train_control_method <- "none"
model_k <- 5

model_A_knn <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(k = model_k),
  metric = "ROC"
)

pred <- predict(model_A_knn, newdata = testing_df)

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
    model = model_A_knn,
    train_control_method = train_control_method,
    tune_parameters = c(model_k),
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
save(results, file = paste0("Paper1_results/", model_name, ".RData"))

cf_matrix$table %>%
  data.frame() %>%
  ggplot(aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "gray50") +
  scale_fill_gradient(low = "gray99", high = muted("deepskyblue4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------- Results ----------------
# Not determined yet




#------------------------------ Random Forest #1 ----------------------
# From e1071, ranger, dplyr, ordinalForest
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none", classProbs =  TRUE)
model_name <- "ordinalRF"
train_control_method <- "none"
model_nsets <-   10 # #score sets tried prior to the approximation
model_ntreeperdiv <- 50 # #of trees (small RFs)
model_ntreefinal <- 300# of trees (final RF)

model_A_rf <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(nsets = model_nsets,
                        ntreeperdiv = model_ntreeperdiv,
                        ntreefinal = model_ntreefinal
                        ),
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
    tune_parameters = c(model_mtry),
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


#------------------------------ Random Forest #2----------------------
# Default
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none", classProbs =  TRUE)
model_name <- "rf"
train_control_method <- "none"
model_mtry <-
  2 # Number of variables available for splitting at each tree node.


model_A_rf <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(mtry = model_mtry),
  metric = "Accuracy"
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
    tune_parameters = c(model_mtry),
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



#------------------------------ Random Forest #3 Ranger package----------------------
# Ranger is a fast implementation of random forests (Breiman 2001)
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none", classProbs =  TRUE)
model_name <- "ranger"
train_control_method <- "none"
model_cp <- 0.1 # numeric Complexity Parameter


model_A_rf <- train(
  trimmed_activity ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(mtry = 3, splitrule = "gini", min.node.size = 3),
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
    tune_parameters = c(model_mtry),
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










#------------------------------ Support Vector Machines with Polynomial Kernel ----------------------
# using  kernlab package
# The method is none becuase we have test and train data
fitControl <- trainControl(method = "none"  , classProbs =  TRUE)
model_name <- "svmPoly"
train_control_method <- "none"
model_degree <- 2
model_scale <- 1
model_C <- .001

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
    model = model_A_svm,
    train_control_method = train_control_method,
    tune_parameters = c(model_degree, model_scale, model_C),
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






## NN




model_A_nn <- train(
    trimmed_activity ~ .,
    data = training_df,
    method = "nnet",
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = data.frame( size = 70 , decay = 0.1
    ),
    maxit
    metric = "ROC"
)

pred <- predict(model_A_nn, newdata = testing_df)

cf_matrix <-
    confusionMatrix(
        data = pred,
        reference = testing_df$trimmed_activity,
        mode = "prec_recall"
    )
# Calculate accuracy and F1
mean(pred == testing_df$trimmed_activity)


model_A_nn <- train(
    trimmed_activity ~ .,
    data = training_df,
    method = "dnn",
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = data.frame( layer1 =5, layer2=5, layer3= 5, hidden_dropout = 0.2, visible_dropout = 0.2
    ),
    metric = "ROC"
)

pred <- predict(model_A_nn, newdata = testing_df)

cf_matrix <-
    confusionMatrix(
        data = pred,
        reference = testing_df$trimmed_activity,
        mode = "prec_recall"
    )
# Calculate accuracy and F1
mean(pred == testing_df$trimmed_activity)


library(deepnet)
Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
y <- c(rep(1, 50), rep(0, 50))

x <- working_df %>% select(-trimmed_activity) %>%  as.matrix()
y <- working_df %>% select(trimmed_activity) %>% as.matrix()

dnn <-  sae.dnn.train(x, y, hidden = c(10, 10))
library(keras)
install_keras()
