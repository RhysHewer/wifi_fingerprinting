#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Wifi Fingerprinting
# Version: 1
# Purpose: Latitude & longitude regression, and floor classification 
#         for building 0
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#Data (can change data used)
load("output/nzvRem.RDS")
load("output/outRemVal.RDS")


##### SUBSET TO B0 ############################################################

modTrain <- nzvRem %>% filter(BUILDINGID == 0)
modVal <- outRemVal %>% filter(preds.build == 0)     


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

#tuning grid gbm
grid <- expand.grid(n.trees = c(150, 200, 250), 
                    interaction.depth = c(3, 4, 5), 
                    shrinkage = 0.1,
                    n.minobsinnode = 10)



##### LATITUDE REGRESSION #####################################################

#Train model
modTrainLat <- training %>% select(starts_with("WAP"), LATITUDE)

model.lat <- train(LATITUDE ~ .,
                   data = modTrainLat,
                   method = "knn",
                   trControl = fitControl,
                   preProcess = c("center","scale"))

model.lat

#Predictions
preds.lat <- predict(model.lat, testing)
testing$preds.lat <- preds.lat

#Metrics
mets.lat <- postResample(pred = testing$preds.lat, obs = testing$LATITUDE)
mets.lat


##### LONGITUDE REGRESSION ####################################################

#Train model
modTrainLong <- training %>% select(starts_with("WAP"), LONGITUDE)


model.long <- train(LONGITUDE ~ .,
                    data = modTrainLong,
                    method = "knn",
                    trControl = fitControl,
                    preProcess = c("center","scale"))

model.long

#Predictions
preds.long <- predict(model.long, testing)
testing$preds.long <- preds.long

#Metrics
mets.long <- postResample(pred = testing$preds.long, obs = testing$LONGITUDE)
mets.long


##### FLOOR CLASSIFICATION ###########################################

#Train model
training$FLOOR <- training$FLOOR %>% as.factor()
modTrainFloor <- training %>% select(starts_with("WAP"), FLOOR)

set.seed(111)
model.floor <- train(FLOOR ~ .,
                  data = modTrainFloor,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  tuneGrid = grid)

model.floor

#Predictions
preds.floor <- predict(model.floor, testing)
testing$preds.floor <- preds.floor %>% as.factor()

#Confusion Matrix
testing$FLOOR <- testing$FLOOR %>% as.factor()
confMat.floor <- confusionMatrix(testing$FLOOR, testing$preds.floor)
confMat.floor

##### RESULTS COLLATION #######################################################

testingB0 <- testing
save(testingB0, file = "output/testingB0.RDS")
