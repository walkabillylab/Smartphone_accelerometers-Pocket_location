# Javad Khataei
#22/07/2019


# This code creates a model which can be used to adjust the timestamps
# First we try several models for a participant and pocket ocation
# pick the most accurate one
# Copy this model for synching scrts in the "Validation" folder


#import libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(scales)
library(caret)





# Read the data
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

working_df <- fread("Ethica_Jaeger_Merged/pocket/All_participants_pocket_witj_count_for_Weka.csv")

working_df  %<>% filter()



#----------------------------- GBM Method -----------------------#


# work with iris
data(iris)
working_df <- iris
#edit this
colnames(working_df)[5] <- "trimmed_activity"
inTraining <-
  createDataPartition(working_df$trimmed_activity, p = .75, list = FALSE)
training <- working_df[inTraining, ]
testing  <- working_df[-inTraining, ]

fitControl <- trainControl(method = "none"  ,classProbs =  TRUE)

set.seed(825)
gbmFit4 <- train(trimmed_activity ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = data.frame(interaction.depth = 4,
                                       n.trees = 100,
                                       shrinkage = .1,
                                       n.minobsinnode = 20),
                 metric = "ROC")


gbmFit4 <- train(
  trimmed_activity ~ .,
  data = training,
  method = "rFerns",
  trControl = fitControl,
  verbose = FALSE,
  ## Only a single model can be passed to the
  ## function when no resampling is used:
  tuneGrid = data.frame(depth = 15)
)
gbmFit4


predict(gbmFit4, newdata = head(testing))

predict(gbmFit4, newdata = head(testing), type = "prob")


pred <- predict(gbmFit4, newdata = testing)



conf_mat <- confusionMatrix(data = pred , reference = testing$trimmed_activity)
conf_mat$table
conf_mat$overall


#rf
fitControl <- trainControl(method = "none", trimmed_activityProbs = TRUE)

set.seed(825)
gbmFit4 <- train(
  trimmed_activity ~ .,
  data = training,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  ## Only a single model can be passed to the
  ## function when no resampling is used:
  tuneGrid = data.frame(
    interaction.depth = 4,
    n.trees = 100,
    shrinkage = .1,
    n.minobsinnode = 20
  ),
  metric = "ROC"
)


gbmFit4


predict(gbmFit4, newdata = head(testing))

predict(gbmFit4, newdata = head(testing), type = "prob")


pred <- predict(gbmFit4, newdata = testing)



conf_mat <- confusionMatrix(data = pred , reference = testing$trimmed_activity)


#######
#lda

gbmFit4 <- train(
  trimmed_activity ~ .,
  data = training,
  method = "RFlda",
  trControl = fitControl,
  verbose = FALSE,
  ## Only a single model can be passed to the
  ## function when no resampling is used:
  tuneGrid = data.frame(q = "maxq" 
  ),
  metric = "ROC"
)

pred <- predict(gbmFit4, newdata = testing)

confusionMatrix(data = pred , reference = testing$trimmed_activity)

#Regularized Logistic Regression
gbmFit4 <- train(
  trimmed_activity ~ .,
  data = training,
  method = "regLogistic",
  trControl = fitControl,
  #verbose = FALSE,
  ## Only a single model can be passed to the
  ## function when no resampling is used:
  tuneGrid = data.frame(cost = 0.3, loss = "L1" , epsilon = 0.001
  ),
  metric = "ROC"
)
pred <- predict(gbmFit4, newdata = testing)

confusionMatrix(data = pred , reference = testing$trimmed_activity)


###-------------------------####
# Real Deal


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

working_df <- fread("Ethica_Jaeger_Merged/pocket/All_participants_pocket_witj_count_for_Weka.csv")

working_df  %<>% filter()


