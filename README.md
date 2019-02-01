# wifi_fingerprinting
Using wifi fingerprinting for indoor location modelling

**Goal**:  Use "wifi fingerprinting" to determine a person's location in indoor spaces by using signals from multiple wifi hotspots.  
**Dataset**: UJIIndoorLoc database which covers three buildings of Universitat Jaume I (Valencia, Spain) with 4 or more floors and an area of almost 110.000m2.

**Files**:  
*libraries.R*: library script.  
*dataprocessingBuilding.R*: initial data processing step to lead into building classification model.  
*ModellingBuilding.R*: building classification model script.  
*dataprocessingPostBuilding.R*: post building-classification data preprocessing. To prepare the data for floor classification and coordinate regression.  
*ModellingB0/1/2.R*: floor classification and coordinate regression for individual buildings within dataset.  
*ModellingResults.R*: concatenated modelling results and overall metrics.  
*WifiFingerprintingPresentation.pdf*: presentation of finalised project.
