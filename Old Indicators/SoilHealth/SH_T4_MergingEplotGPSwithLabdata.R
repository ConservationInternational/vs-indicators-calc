############################################################
#SOIL HEALTH THREAD
#Merging eplot.csv and eplotsoils_processed.csv &
#eplot.csv and eplot_subplot_landscapefn.csv &
#eplot.csv and eplotsoils_sample.csv to add GPS coordinates 
#Last updated: 09/22/15
############################################################

#Open libraries
library(openxlsx)
library(VitalSignsUtilities)

arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getInternalData()

#Load data
Main<-read.csv(vstables$tables[["eplot.csv"]]$getData(), header = TRUE)
Lab<-read.csv(vstables$tables[["eplotsoils_processed.csv"]]$getData(), header = TRUE, stringsAsFactors = TRUE)
Landcover<-read.csv(vstables$tables[["eplot_subplot_landscapefn.csv"]]$getData(), header = TRUE)
Samples<-read.csv(vstables$tables[["eplotsoils_sample.csv"]]$getData(), header = TRUE)

#CLEANING STEP - Checking for duplicates in Main
count(duplicated(Main$Eplot.Code))

#CLEANING STEP - Creating a subset of Main with only TZA and GHA observations, without duplicates
Mainsub<-subset(Main, !duplicated(Main$Eplot.Code) & (Country=="TZA" | Country=="GHA"))
count(duplicated(Mainsub$Eplot.Code))
table(Mainsub$Country)

#Removing all non-SE corner coordinate information from Mainsub
Mainsub<-subset(Mainsub, select=c(Eplot.Code, GPS.SE.accuracy: GPS.SE.longitude))

#Merging data
Tier4SoilPredicted<-merge(Lab, Mainsub, by="Eplot.Code", all.x = TRUE)
Tier4landcover<-merge(Landcover, Mainsub, by="Eplot.Code", all.x = TRUE)
SoilSamples<-merge(Samples, Mainsub, by="Eplot.Code", all.x = TRUE)

#Checking merge for Lab data
count(is.na(Lab$SSN))
count(is.na(Tier4SoilPredicted$GPS.SE.accuracy))
#All of the observations that were in the eplotsoils_processed.csv have found a match in eplot.csv

#Checking merge for Landcover data
count(is.na(Landcover$Eplot.Code))
count(is.na(Tier4landcover$GPS.SE.accuracy))
#There are 140 eplots that we have landcover data for that we do not have coordinate data for

#Checking merge for Samples data
count(is.na(Samples$Eplot.Code))
count(is.na(SoilSamples$GPS.SE.accuracy))
#There are 49 eplots that we have depth restriction data for that we do not have coordinate data for

#Export joined file
#setwd(dir_out)
write.csv(Tier4SoilPredicted, "Eplotsoils_processed_GPS.csv")
write.csv(Tier4landcover, "EplotLandcover_GPS.csv")
write.csv(SoilSamples, "EplotSamples_GPS.csv")
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "Eplotsoils_processed_GPS.csv", target_path = "Soil_Health/Eplotsoils_processed_GPS.csv")
s3$writeS3(bucket = "ci-vsindicators", source_path = "EplotLandcover_GPS.csv", target_path = "Soil_Health/EplotLandcover_GPS.csv")
s3$writeS3(bucket = "ci-vsindicators", source_path = "EplotSamples_GPS.csv", target_path = "Soil_Health/EplotSamples_GPS.csv")


#Keep only those dataframes that are to be used in another script
rm(Lab, Landcover, Main, Samples, dir_data1, dir_out)
