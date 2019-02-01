#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Wifi Fingerprinting
# Version: 1
# Purpose: Script to pre-process data for building classification model
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#Load Data
data <- read_csv("data/trainingData.csv")

##### DATA CLEANING ###########################################################

#Dataset for building prediction only (zero variance WAPS without value cleaning)
locDataLeft <- data %>% select(starts_with("WAP"))
locDataRight <- data %>% select(-starts_with("WAP"))

noVar <- nrow(data) * 100
noVarLeft <- locDataLeft[,colSums(locDataLeft) != noVar]

locData <- bind_cols(noVarLeft, locDataRight)

save(locData, file = "output/locData.RDS")
