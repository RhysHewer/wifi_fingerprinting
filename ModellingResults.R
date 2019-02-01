#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Wifi Fingerprinting
# Version: 1
# Purpose: Collation and Analysis of results of models
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#Load Data
load("output/testingB0.RDS")
load("output/testingB1.RDS")
load("output/testingB2.RDS")

##### COMBINE TESTING RESULTS #################################################

testRes <- bind_rows(testingB0, testingB1, testingB2)

save(testRes, file = "output/testRes.RDS")

##### OVERALL CLASSIFICATION METRICS (Accuracy : 0.9055) ######################

#create building.floor metric for actual and predictions
#use this metric in confusion matrix
testRes$BF <- paste(testRes$BUILDINGID, 
                    testRes$FLOOR, sep = ".") %>% as.factor

testRes$preds.BF <- paste(testRes$preds.build , 
                          testRes$preds.floor, sep = ".") %>% as.factor


confMat.BF <- confusionMatrix(testRes$BF, testRes$preds.BF)
confMat.BF


##### OVERALL REGRESSION METRICS (avg RMSE 11.0417) ###########################

#latitude (RMSE 11.372944)
mets.lat <- postResample(pred = testRes$preds.lat, obs = testRes$LATITUDE)
mets.lat

#longitude (RMSE 10.7104703)
mets.long <- postResample(pred = testRes$preds.long, obs = testRes$LONGITUDE)
mets.long


