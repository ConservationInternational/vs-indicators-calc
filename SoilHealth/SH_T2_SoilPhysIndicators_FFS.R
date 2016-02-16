#######################################################################################
#SOIL HEALTH THREAD
#Tier 2a/4 staging of Soil Physical properties for farm fields (SH15, SH27, SH45)
#Last updated: 13 October 2015 
#NOTE: it would be better if the variable names matched those in data dictionary
#######################################################################################

#Load libraries
library(plyr)
library(VitalSignsUtilities)

arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getInternalData()
vstables$getExternalData()
#Load soil predicted physical and chemical properties from ICRAF lab
Tier4SoilPredicted<- read.csv("eplotsoils_processed_GPS.csv",  quote=",", head=TRUE, stringsAsFactors=TRUE)
#Load farm field measurements
FarmFieldLandcover<-read.csv(vstables$tables[["ffs_measurements.csv"]]$getData(), header=TRUE)
#note that current column names do not conform to data dictionary
#Note that farm field codes are missing from both of the above

#################################################################################################
#SH15 (Soil texture - Sand, Silt, Clay)  
#################################################################################################
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Clay.content.by.hydrometer.method....']<-'Clay_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Sand.content.by.hydrometer.method....']<-'Sand_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Silt.content.by.hydrometer.method....']<-'Silt_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Landscape..']<-'Landscape'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Field..']<-'Field'

names(FarmFieldLandcover)[names(FarmFieldLandcover)=='Field..']<-'Field'
names(FarmFieldLandcover)[names(FarmFieldLandcover)=='Landscape..']<-'Landscape'

#Subset for neccessary ID fields and variables
Tier4SoilPredicted_key <- subset(Tier4SoilPredicted, sample_depth_bottom==20, select=c(Country, Landscape, Household.ID, Field, FarmField.Code,
    sample_depth_bottom,sample_depth_top, Clay_icraf, 
    Sand_icraf, Silt_icraf))

#Assigning measurement codes
Tier4SoilPredicted_key$SH14a <- Tier4SoilPredicted_key$Clay_icraf 
Tier4SoilPredicted_key$SH14b <- Tier4SoilPredicted_key$Silt_icraf 
Tier4SoilPredicted_key$SH14c <- Tier4SoilPredicted_key$Sand_icraf 

###########################################
#SH15 - Erodibility
###########################################
#Generate SH15 particle size distribution for soil erosion modeling
#Taken from SWALIM/ FAO 2009 and utilizing particle size distributions from ICRAF lab
d1.clay <- 0.002 
d2.clay <- 0.0005
d1.sand <- 2
d2.sand <- 0.05
d1.silt <- 0.05
d2.silt <- 0.002

#Erodibility using Renard 1997 with arithmetic mean of particle diameter (mm)
Tier4SoilPredicted_key$dgclay_Renard <- Tier4SoilPredicted_key$Clay_icraf*log((d1.clay+d2.clay)/2)
Tier4SoilPredicted_key$dgsand_Renard <- Tier4SoilPredicted_key$Sand_icraf*log((d1.sand+d2.sand)/2)
Tier4SoilPredicted_key$dgsilt_Renard <- Tier4SoilPredicted_key$Silt_icraf*log((d1.silt+d2.silt)/2)
Tier4SoilPredicted_key$dg_Renard <- exp(0.01 * (Tier4SoilPredicted_key$dgclay_Renard + Tier4SoilPredicted_key$dgsand_Renard + Tier4SoilPredicted_key$dgsilt_Renard))
Tier4SoilPredicted_key$SH15 <- 7.594 * (0.0034 + 0.0405 * exp((-0.5) * (((log10(Tier4SoilPredicted_key$dg_Renard) + 1.659)/0.7101)^2)))
summary(Tier4SoilPredicted_key$SH15)

#####################################################
#SH45 Soil surface indicator
#####################################################
#Calculating soil surface indicator for farm field plots
attach(FarmFieldLandcover) 
FarmFieldLandcover$SH27 <-((b_10_1 + b_10_2 + b_10_3 + b_10_4 + b_10_5)/
    (b_10_1 + b_10_2 + b_10_3 + b_10_4 + b_10_5 + b_10_6 + b_10_7 + b_10_8 + b_10_9))
FarmFieldLandcover$SH45 <- FarmFieldLandcover$SH27/10

FFS_SH27_SH45<-subset(FarmFieldLandcover, (Country=="GHA" | Country=="TZA") & Landscape!=99, select=c(Country,Landscape,Household.ID, Field, SH45,SH27))
detach(FarmFieldLandcover)

FFS_SH27_SH45_Landscape<-ddply(FFS_SH27_SH45, c('Country','Landscape'), function(x) c(
  SH45_Mean=mean(x$SH45),
  SH45_stdev=sd(x$SH45),
  SH27_Mean=mean(x$SH27),
  SH27_stdev=sd(x$SH27)))

# Write outputs
SH_T2_T4_SH15 <- subset(Tier4SoilPredicted_key, (Country=="GHA" | Country=="TZA") & Landscape!=99, select=c(Country, Landscape, Household.ID, Field, SH15, GPS.SE.accuracy, GPS.SE.latitude, GPS.SE.longitude))
SH_T2_T4_SH15_Landscape<-ddply(SH_T2_T4_SH15, c('Country','Landscape'), function(x) c(
  SH15_Mean=mean(x$SH15),
  SH15_stdev=sd(x$SH15)))
write.csv(FFS_SH27_SH45_Landscape, "SH_T2_T4_SH27_SH45_FFS.csv", row.names=FALSE)
write.csv(SH_T2_T4_SH15_Landscape, "SH_T2_T4_SH15_FFS_Landscape.csv", row.names=FALSE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_T2_T4_SH27_SH45_FFS.csv", target_path = "Soil_Health/SH_T2_T4_SH27_SH45_FFS.csv", overwrite = TRUE)
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_T2_T4_SH15_FFS_Landscape.csv", target_path = "Soil_Health/SH_T2_T4_SH15_FFS_Landscape.csv", overwrite = TRUE)

