#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Wifi Fingerprinting
# Version: 1
# Purpose: Script to perfom data pre-processing and feature selection for post-building
#          modelling phase
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#Load Data
data <- read_csv("data/trainingData.csv")
load("output/valDataBuildPred.RDS")
valData <- valDataBuildPred


##### DATA CLEANING ###########################################################

#Clean data (both datasets): move 100 values to -105
wapTrainLeft <- data %>% select(starts_with("WAP"))
wapTrainRight <- data %>% select(-starts_with("WAP")) 

wapTrainLeft <- na_if(wapTrainLeft, 100)
wapTrainLeft[is.na(wapTrainLeft)] <- -105
wapTrain <- bind_cols(wapTrainLeft, wapTrainRight)


wapValLeft <- valData %>% select(starts_with("WAP"))
wapValRight <- valData %>% select(-starts_with("WAP")) 

wapValLeft <- na_if(wapValLeft, 100)
wapValLeft[is.na(wapValLeft)] <- -105
wapVal <- bind_cols(wapValLeft, wapValRight)

save(wapTrain, file = "output/wapTrain.RDS")
save(wapVal, file = "output/wapVal.RDS")


##### OUTLIER TREATMENT #######################################################

#Resolve Training Outlier rows (remove > -25, convert < -80 to -105)
#475 rows removed
load("output/wapTrain.RDS")

outTrainLeft <- wapTrain %>% select(starts_with("WAP"))
outTrainRight <- wapTrain %>% select(-starts_with("WAP"))

outRemLeft <- outTrainLeft %>% lapply(function(x) 
        ifelse(x > -25, NA, x)) %>% as.data.frame()

outRemLeft <- outRemLeft %>% lapply(function(x) 
        ifelse(x < -80, -105, x)) %>% as.data.frame()

outRem <- bind_cols(outRemLeft, outTrainRight)
outRem <- outRem[complete.cases(outRem),]

save(outRem, file = "output/outRem.RDS")

#Validation set = unable to remove outlier rows 
#so convert >-25 to -25 and < -80 to -105
load("output/wapVal.RDS")

outValLeft <- wapVal %>% select(starts_with("WAP"))
outValRight <- wapVal %>% select(-starts_with("WAP"))

outRemValLeft <- outValLeft %>% lapply(function(x) 
        ifelse(x > -25, -25, x)) %>% as.data.frame()

outRemValLeft <- outRemValLeft %>% lapply(function(x) 
        ifelse(x < -80, -105, x)) %>% as.data.frame()

outRemVal <- bind_cols(outRemValLeft, outValRight)

save(outRemVal, file = "output/outRemVal.RDS")


##### FEATURE SELECTION #######################################################

#Remove zero variance WAP features = 194 features removed
load("output/outRem.RDS")

outRemLeft <- outRem %>% select(starts_with("WAP"))
outRemRight <- outRem %>% select(-starts_with("WAP"))

nzv <- nrow(outRemLeft) * -105
nzvRem <- outRemLeft[,colSums(outRemLeft) != nzv]
nzvRem <- bind_cols(nzvRem, outRemRight)


save(nzvRem, file = "output/nzvRem.RDS")




