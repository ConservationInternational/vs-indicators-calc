#################################################################################
#SOIL HEALTH THREAD
#Tier 2a/4 staging of Soil Physical properties for eplots (SH15, SH27, SH29)
#Last updated: 09/22/15 
#NOTE: it would be better if the variable names matched those in data dictionary
#################################################################################
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

# Load repo
ci.repo <- checkGithubToken()

#This script will load soil predicted physical and chemical properties from ICRAF lab (note that current column
#names do not conform to data dictionary), eplot measurements, and eplot soil measurements with GPS coordinates
run1<-ci.repo$parseRCode("SoilHealth/SH_T4_MergingEplotGPSwithLabdata.R")
eval(run1)
#load farm field soil measurements
FarmFieldLandcover<-read.csv(vstables$tables[["ffs_measurements.csv"]]$getData(), header=TRUE)

#################################################################################################
#SH15 (Soil texture - Sand, Silt, Clay)  
#################################################################################################
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Clay.content.by.hydrometer.method....']<-'Clay_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Sand.content.by.hydrometer.method....']<-'Sand_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Silt.content.by.hydrometer.method....']<-'Silt_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Eplot..']<-'Eplot'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Landscape..']<-'Landscape'

names(Tier4landcover)[names(Tier4landcover)=='Eplot..']<-'Eplot'
names(Tier4landcover)[names(Tier4landcover)=='Landscape..']<-'Landscape'

names(SoilSamples)[names(SoilSamples)=='Eplot..']<-'Eplot'
names(SoilSamples)[names(SoilSamples)=='Landscape..']<-'Landscape'

names(FarmFieldLandcover)[names(FarmFieldLandcover)=='Landscape..']<-'Landscape'
names(FarmFieldLandcover)[names(FarmFieldLandcover)=='Field..']<-'Field'

#Subset lab data for topsoil and for neccessary ID fields and variables
Tier4SoilPredicted_key <- subset(Tier4SoilPredicted, sample_depth_bottom==20, select=c(Eplot, Landscape, Country, 
                  Eplot.Code,sample_depth_bottom,sample_depth_top, Clay_icraf, Sand_icraf, Silt_icraf,
                  GPS.SE.accuracy, GPS.SE.latitude, GPS.SE.longitude))

#Assigning measurement codes
Tier4SoilPredicted_key$SH14a <- Tier4SoilPredicted_key$Clay_icraf 
Tier4SoilPredicted_key$SH14b <- Tier4SoilPredicted_key$Silt_icraf 
Tier4SoilPredicted_key$SH14c <- Tier4SoilPredicted_key$Sand_icraf 

#Generate SH15 particle size distribution for soil erosion modeling
#Taken from SWALIM/ FAO 2009 and utilizing particle size distributions from ICRAF lab
d1.clay <- 0.002 
d2.clay <- 0.0005
d1.sand <- 2
d2.sand <- 0.05
d1.silt <- 0.05
d2.silt <- 0.002

#Calculating erodibility using Renard 1997 equation with arithmetic mean of particle diameter (mm)
Tier4SoilPredicted_key$dgclay_Renard <- Tier4SoilPredicted_key$Clay_icraf*log((d1.clay+d2.clay)/2)
Tier4SoilPredicted_key$dgsand_Renard <- Tier4SoilPredicted_key$Sand_icraf*log((d1.sand+d2.sand)/2)
Tier4SoilPredicted_key$dgsilt_Renard <- Tier4SoilPredicted_key$Silt_icraf*log((d1.silt+d2.silt)/2)
Tier4SoilPredicted_key$dg_Renard <- exp(0.01 * (Tier4SoilPredicted_key$dgclay_Renard + Tier4SoilPredicted_key$dgsand_Renard + Tier4SoilPredicted_key$dgsilt_Renard))
Tier4SoilPredicted_key$SH15 <- 7.594 * (0.0034 + 0.0405 * exp((-0.5) * (((log10(Tier4SoilPredicted_key$dg_Renard) + 1.659)/0.7101)^2)))
summary(Tier4SoilPredicted_key$SH15)

####################################################
#SH45 Soil surface indicator (ranges from 0 to .1)
####################################################
#Calculating soil surface indicator for eplots: 5 measurements per eplot
attach(Tier4landcover)
Tier4landcover$SH27 <-((Rooted.Plants + Litter.Cover + Downed.Wood + Disturbed.Soil + Microfloral.Crust)/
                        (Rooted.Plants + Litter.Cover + Downed.Wood + Disturbed.Soil + Microfloral.Crust + Undisturbed.Bare...Porous.Soil +
                           Undisturbed.but.Sealed.Soil + Sodic.Soil + Termite.Mound))
Tier4landcover$SH45 <- Tier4landcover$SH27/10
detach(Tier4landcover)

#Finding the mean SH45 indicator value for each eplot
Tier4_SH27_SH45<-ddply(Tier4landcover,c('Country','Landscape','Eplot','Eplot.Code'), function(x) c(SH45mean=mean(x$SH45), SH27mean=mean(x$SH27)))
Tier4_SH27_SH45<-merge(Tier4_SH27_SH45, Mainsub, all.x = TRUE)

#Renaming variables
names(Tier4_SH27_SH45)[names(Tier4_SH27_SH45)=='SH27mean']<-'SH27'
names(Tier4_SH27_SH45)[names(Tier4_SH27_SH45)=='SH45mean']<-'SH45'
summary(Tier4landcover$SH27)
summary(Tier4landcover$SH45)

##################################################
#SH46 Soil depth indicator (ranges from 0 to .1)
##################################################
#Soil is sampled to 100cm in the SE corner of the eplot, using this sampling point to create a single sampling depth restriction variable
SoilSamples$SH29 <- ifelse(!is.na(SoilSamples$Depth.to.top.sampling.restriction..SE.cm.), SoilSamples$Depth.to.top.sampling.restriction..SE.cm.,
                             ifelse(!is.na(SoilSamples$Depth.to.subsoil.sampling.restriction..SE.cm.), SoilSamples$Depth.to.subsoil.sampling.restriction..SE.cm.,
                                    ifelse(!is.na(SoilSamples$Depth.to.50.80.sampling.restriction..SE.cm.), SoilSamples$Depth.to.50.80.sampling.restriction..SE.cm.,
                                           ifelse(!is.na(SoilSamples$Depth.to.80.100.sampling.restriction..SE.cm.), SoilSamples$Depth.to.80.100.sampling.restriction..SE.cm., 100))))
summary(SoilSamples$SH29)

#SH46 - Use the single soil depth restriction (SH29) to generate a soil depth indicator
SoilSamples$SH46 <- .004*SoilSamples$SH29 + (-0.1)
SoilSamples$SH46 [SoilSamples$SH29 < 25] <- 0
SoilSamples$SH46 [SoilSamples$SH29 > 50] <- .1
summary(SoilSamples$SH46)

# Write outputs
SH_T2_T4_SH46 <- subset(SoilSamples, select=c(Country, Landscape, Eplot, Eplot.Code, SH46, 
      GPS.SE.accuracy, GPS.SE.longitude, GPS.SE.latitude))
SH_T2_T4_SH15 <- subset(Tier4SoilPredicted_key, select=c(Country, Landscape, Eplot, Eplot.Code, SH15,
  GPS.SE.accuracy, GPS.SE.longitude, GPS.SE.latitude))
write.csv(SH_T2_T4_SH46, "SH_T2_T4_SH46_Eplot.csv", row.names=FALSE)
write.csv(Tier4_SH27_SH45, "SH_T2_T4_SH27_SH45_Eplot.csv", row.names=FALSE)
write.csv(SH_T2_T4_SH15, "SH_T2_T4_SH15_Eplot.csv", row.names=FALSE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_T2_T4_SH46_Eplot.csv", target_path = "Soil_Health/SH_T2_T4_SH46_Eplot.csv")
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_T2_T4_SH27_SH45_Eplot.csv", target_path = "Soil_Health/SH_T2_T4_SH27_SH45_Eplot.csv")
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_T2_T4_SH15_Eplot.csv", target_path = "Soil_Health/SH_T2_T4_SH15_Eplot.csv")

#Remove excess data frames
rm(list=ls()[! ls() %in% c("SH_T2_T4_SH46","Tier4_SH27_SH45","SH_T2_T4_SH15","FFS_SH27_SH45")])
