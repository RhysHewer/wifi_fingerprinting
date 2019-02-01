#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Wifi Fingerprinting
# Version: 1
# Purpose: Building Classification Modelling
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#Data (can change data used)
load("output/locData.RDS")
valData <- read_csv("data/validationData.csv")

modTrain <- locData #Flexible Data entry
modVal <- valData #Flexible Data entry


##### MODELLING PREPARATION ###################################################

#Parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#Cross Validation
fitControl<- trainControl(method = "cv", 
                          number = 3, 
                          savePredictions = TRUE, 
                          allowParallel = TRUE)

#Training (sample of all data) / Testing (validation set)
set.seed(111)
training <- modTrain %>% sample_n(2500)
testing  <- modVal



##### BUILDING CLASSIFICATION #################################################

#Train model
modTrainBuild <- training %>% select(starts_with("WAP"), BUILDINGID)
modTrainBuild$BUILDINGID <- modTrainBuild$BUILDINGID %>% as.factor()

set.seed(111)
model.build <- train(BUILDINGID ~ .,
                     data = modTrainBuild,
                     method = "gbm",
                     trControl = fitControl,
                     verbose = FALSE)

model.build

#Predictions
preds.build <- predict(model.build, testing)
testing$preds.build <- preds.build

#Confusion Matrix
testing$BUILDINGID <- testing$BUILDINGID %>% as.factor()
confMat.build <- confusionMatrix(testing$BUILDINGID, testing$preds.build)
confMat.build

valDataBuildPred <- testing
save(valDataBuildPred, file = "output/valDataBuildPred.RDS")
