#Load libraries
source("scripts/libraries.R")

#load data
load("output/locData.RDS")
load("output/naData.RDS")
load("output/bWap.RDS")

##########EDA###########

###Long + Lat

#long v lat
g1 <- ggplot(locData, aes(LONGITUDE, LATITUDE, colour = )) +
        geom_point()
g1

#long hist
g2 <- ggplot(locData, aes(LONGITUDE)) +
        geom_histogram()
g2

#lat hist
g3 <- ggplot(locData, aes(LATITUDE)) +
        geom_histogram()
g3


###Building/Floor

g4 <- ggplot(locData, aes(BUILDINGID, FLOOR)) + 
        geom_point()
g4

floorCheck <- data %>% group_by(BUILDINGID, FLOOR) %>% summarise(count = n())

##Active WAPS per building
acWap <- bWap %>% group_by(BUILDINGID, FLOOR, WAP) %>% summarise(avgStrength = mean(strength))


g5 <- ggplot(acWap, aes(BUILDINGID, WAP, size = avgStrength)) +
        geom_point()
g5

#massive overlap - possible clearer separation if only stronger signal used.

bWap50 <- bWap %>% filter(strength > -50)
acWap50 <- bWap50 %>% group_by(BUILDINGID, FLOOR, WAP) %>% summarise(avgStrength = mean(strength))

g6 <- ggplot(acWap50, aes(BUILDINGID, WAP, size = avgStrength, colour = FLOOR)) +
        geom_point()
g6

#visualise per floor
bWapFloor <- bWap
bWapFloor$BF <- paste(bWapFloor$BUILDINGID, bWapFloor$FLOOR, sep = ".")

acWapFloor <- bWapFloor %>% group_by(BF, WAP) %>% summarise(avgStrength = mean(strength))

g7 <- ggplot(acWapFloor, aes(BF, WAP, size = avgStrength)) +
        geom_point()
g7

#restrict to > -50
bWapFloor50 <- bWap50
bWapFloor50$BF <- paste(bWapFloor50$BUILDINGID, bWapFloor50$FLOOR, sep = ".")

acWapFloor50 <- bWapFloor50 %>% group_by(BF, WAP) %>% summarise(avgStrength = mean(strength))

g8 <- ggplot(acWapFloor50, aes(BF, WAP, size = avgStrength)) +
        geom_point()
g8

test <- bWapFloor50 %>% head(10)

##lat with WAP strength
acWapLat <- bWap %>% group_by(LATITUDE, WAP) %>% summarise(avgStrength = mean(strength))

g10 <- ggplot(acWapLat, aes(LATITUDE, WAP, size = avgStrength)) +
        geom_point()
g10


##lat with > -50
acWapLat50 <- bWap50 %>% group_by(LATITUDE, WAP) %>% summarise(avgStrength = mean(strength))

g9 <- ggplot(acWapLat50, aes(LATITUDE, WAP, size = avgStrength)) +
        geom_point()
g9

##############Regression Errors#################

load("output/testingLat.boostDataAll.RDS")
load("output/testingLong.boostDataAll.RDS")

#lat errors by building + floor
g10 <- ggplot(testingLat.boostDataAll, aes(preds.knn, LATITUDE, colour = BUILDINGID, shape = FLOOR)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1)
g10 <- ggplotly(g10)
g10

#lat error per WAP ???



#long errors by building + floor
g11 <- ggplot(testingLong.boostDataAll, aes(preds.knn, LONGITUDE, colour = BUILDINGID, shape = FLOOR)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1)
g11 <- ggplotly(g11)
g11

#long errors on validation set
load("output/sigValBoostData.Long.RDS")

sigValBoostData.Long$FLOOR <- sigValBoostData.Long$FLOOR %>% as.factor()
sigValBoostData.Long$BUILDINGID <- sigValBoostData.Long$BUILDINGID %>% as.factor()

g12 <- ggplot(sigValBoostData.Long, aes(preds.knn.long, LONGITUDE, colour = BUILDINGID, shape = FLOOR)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1)

g12



#lat errors on validation set
load("output/sigValBoostData.Lat.RDS")

sigValBoostData.Lat$FLOOR <- sigValBoostData.Lat$FLOOR %>% as.factor()
sigValBoostData.Lat$BUILDINGID <- sigValBoostData.Lat$BUILDINGID %>% as.factor()

g13 <- ggplot(sigValBoostData.Lat, aes(preds.knn.lat, LATITUDE, colour = BUILDINGID, shape = FLOOR)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1)

g13

#####################WAP MAPPING############

load("output/sigBoostData.RDS")

##identify buildings
sigBoostDataB0 <- sigBoostData %>% filter(BUILDINGID == 0)
g15<- ggplot(sigBoostDataB0, aes(LONGITUDE, LATITUDE)) +
        geom_point()
g15

sigBoostDataB1 <- sigBoostData %>% filter(BUILDINGID == 1)
g16<- ggplot(sigBoostDataB1, aes(LONGITUDE, LATITUDE)) +
        geom_point()
g16

###total long/lat space

g14 <- ggplot(sigBoostData, aes(LONGITUDE, LATITUDE, size = WAP002)) +
        geom_point()
g14

################# PhoneID frequencies ################

load("output/qualData.RDS")

g.phoneID <- ggplot(qualData, aes(PHONEID)) + 
        geom_bar()
g.phoneID

valData <- read_csv("data/validationData.csv")        

g.phoneIDVal <- ggplot(valData, aes(PHONEID)) + 
        geom_bar()
g.phoneIDVal

g.valCoords <- ggplot() +
        geom_point(data = locData, aes(LONGITUDE, LATITUDE)) +
        geom_point(data = valData, aes(LONGITUDE, LATITUDE), colour = "red")
g.valCoords


##### PRESENTATION GRAPHS BELOW ###############################################

##### DATA QUALITY +100s ######################################################
data <- read_csv("data/trainingData.csv")
valData <- read_csv("data/validationData.csv")

numData <- data %>% select(starts_with("WAP"))
numDataLong <- numData %>% gather(key = "WAP", value = "strength")

numTots <- numDataLong %>% group_by(strength) %>%  summarise(count = n())
colSums(numTots)


g.data100s <- ggplot(numDataLong, aes(strength)) +
        geom_freqpoly(size = 2) +
        theme_bw(base_size = 20) +
        xlab("Strength") + 
        ggtitle("Distribution of Wifi strengths") 
g.data100s


##### SIGNAL OUTLIERS #########################################################

sigOut <- numDataLong %>% filter(strength != 100)
sigOut$colour <- ifelse(sigOut$strength > -25 & sigOut$strength < -80, 'blue', 'green')


g.wapSig <- ggplot(sigOut, aes(WAP, strength)) +
        geom_point()
g.wapSig

##### OBS V PREDS PLOT ########################################################

load("output/testRes.RDS")
data <- read_csv("data/trainingData.csv")


g.results <- ggplot() +
        geom_point(data = testRes, aes(LONGITUDE, LATITUDE), colour = "#1E1E1B") +
        geom_point(data = testRes, aes(preds.long, preds.lat), colour = "#15AAEA") +
        theme_bw(base_size = 20) +
        ylab("Latitude") + 
        xlab("Longitude") + 
        ggtitle("Validation Set: Observations v Predictions")
g.results


g.results2 <- ggplot() +
        geom_point(aes(LONGITUDE, LATITUDE), colour = "#1E1E1B") +
        geom_point(aes(preds.long, preds.lat), colour = "#15AAEA") +
        theme_bw(base_size = 20) +
        ylab("Latitude") + 
        xlab("Longitude") + 
        ggtitle("Validation Set: Observations v Predictions")
g.results2


##### lat v long: Error per location ##########################################

load("output/testRes.RDS")
testRes$latLongError <- (((testRes$LONGITUDE - testRes$preds.long)^2) + 
        ((testRes$LATITUDE - testRes$preds.lat)^2)) %>% 
        sqrt()

testRes$latError <- testRes$LATITUDE - testRes$preds.lat
testRes$longError <- testRes$LONGITUDE - testRes$preds.long

g.locError <- ggplot(testRes, aes(LONGITUDE, LATITUDE, size = latLongError))+
        geom_point()
g.locError

test <- testRes %>% select(-starts_with("WAP"))

summary(testRes$latLongError)

g.Error <- ggplot(testRes, aes(latLongError)) +
        geom_histogram(fill = "#15AAEA") +
        theme_bw(base_size = 20) +
        ylab("Count") + 
        xlab("Error") + 
        ggtitle("Large Errors")
g.Error


errorHigh <- testRes %>% filter(latLongError > 25)
testError <- errorHigh %>% select(-starts_with("WAP"))


g.errorHigh <- ggplot()+
        geom_point(data = testError, aes(LONGITUDE, LATITUDE, size = latLongError), colour = "#15AAEA") +
        geom_point(data = data, aes(LONGITUDE, LATITUDE)) +
        theme_bw(base_size = 20) +
        theme(legend.position="none") +
        ylab("Latitude") + 
        xlab("Longitude") + 
        ggtitle("Large Error Locations") +
        facet_wrap(. ~ FLOOR)
g.errorHigh


##### ERROR CORRELATION WITH WAPS #############################################

load("output/testRes.RDS")
testRes$latLongError <- (((testRes$LONGITUDE - testRes$preds.long)^2) + 
                                 ((testRes$LATITUDE - testRes$preds.lat)^2)) %>% 
        sqrt()

load("output/testRes.RDS")
testRes$latLongError <- (((testRes$LONGITUDE - testRes$preds.long)^2) + 
                                 ((testRes$LATITUDE - testRes$preds.lat)^2)) %>% 
        sqrt()

topError <- testRes %>% filter(latLongError > 50)



topErrorLong <- topError %>% gather(-noWap, key = "WAP", value = "strength")

g.topErrorLong <- ggplot(topErrorLong, aes(WAP, strength)) +
        geom_point()
g.topErrorLong <- ggplotly(g.topErrorLong)
g.topErrorLong

errorWAP <- testRes %>% select(starts_with("WAP"), latLongError)

set.seed(111)
clusterWAP <- kmeans(errorWAP, 20)
WAPcentres <- clusterWAP$centers
View(WAPcentres)

##### WAP MOVEMENT? ######################################################################

load("output/outRem.RDS")
load("output/outRemVal.RDS")

outRemVal <- outRemVal %>% select(starts_with("WAP"), LONGITUDE, LATITUDE)
outRem <- outRem %>% select(starts_with("WAP"), LONGITUDE, LATITUDE)

outRem$set <- "train"
outRemVal$set <- "val"

outRemTot <- bind_rows(outRem, outRemVal)

outRemTot <- transform(outRemTot, 
                       Cluster_ID = as.numeric(interaction
                                               (LATITUDE, LONGITUDE,
                                                       drop=TRUE)))

clustWAP <- outRemTot %>% group_by(Cluster_ID, set) %>% summarise_all(funs(mean)) 

clustWAP <- clustWAP %>% group_by(Cluster_ID) %>% filter(n() > 1)

clustWAPSD <- clustWAP %>% group_by(Cluster_ID) %>% summarise_all(funs(sd))

clustWAPSD.long <- clustWAPSD %>% gather(-Cluster_ID, -set, key = "WAP", value = "sd")

clustWAPSD.long <- clustWAPSD.long %>% group_by(WAP) %>% summarise(meanSD = mean(sd))

g.clustWAP <- ggplot(clustWAPSD.long, aes(WAP, meanSD)) +
        geom_point()
g.clustWAP

##### Confusion Matrix in GGPLOT ###################

load("output/testRes.RDS")

testRes$BF <- paste(testRes$BUILDINGID, 
                    testRes$FLOOR, sep = ".") %>% as.factor

testRes$preds.BF <- paste(testRes$preds.build , 
                          testRes$preds.floor, sep = ".") %>% as.factor


confMat.BF <- confusionMatrix(testRes$BF, testRes$preds.BF)
confMatrix <- confMat.BF$table %>% as.data.frame()

g.confMatrix <- ggplot(confMatrix, aes(Prediction, Reference, fill = Freq)) +
        geom_tile(color="black") +
        theme_bw() + 
        coord_equal() +
        scale_fill_distiller(palette = "Blues", direction=1) +
        guides(fill=F) +
        labs(title = "Confusion Matrix: Building & Floor") +
        geom_text(aes(label = Freq), color="black")
g.confMatrix


##### B1 Floor Classification Issues ##########################################

load("output/testingB1.RDS")
data <- read_csv("data/trainingData.csv")
dataB1 <- data %>% filter(BUILDINGID == 1)

test <- testingB1 %>% select(-starts_with("WAP"))

testingB1$floorCorrect <- ifelse(testingB1$FLOOR == testingB1$preds.floor, "Y", "N")
floorError <- testingB1 %>% filter(floorCorrect == "N")

floorError$FLOOR <- floorError$FLOOR %>% factor()
dataB1$FLOOR <- dataB1$FLOOR %>% factor()

g.floorError <- ggplot() +
        geom_point(data = dataB1, aes(LONGITUDE, LATITUDE)) +
        geom_point(data = floorError, aes(LONGITUDE, LATITUDE), colour = "#15AAEA") +
        facet_wrap(. ~ FLOOR)
g.floorError
