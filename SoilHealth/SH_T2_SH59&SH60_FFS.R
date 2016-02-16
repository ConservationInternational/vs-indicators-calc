#####################################################################################
#SOIL HEALTH THREAD
#Tier 2a/4 staging of SH59 (soil fertility) and SH60 (soil C deficit) for farm fields
#Last updated: 22 September 2015
#NOTE: it would be better if column names matched those in data dictionary
######################################################################################

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

#Load data
Tier4SoilPredicted<-read.csv("farmfieldsoils_processed2.csv", header = T, stringsAsFactors = F)#vstables$tables[["farmfieldsoils_processed?.csv"]]$getData(), header=TRUE)

#################################################################################################
#SH32 staging of: SH6 (Soil Extractable, Available P, S, Soil Exchangeable, Available Ca, Mg, K), 
#SH8 (pH), SH10 (Soil Ex Ac/Al+++), SH12 (Soil C), SH14 (Soil texture - Sand, Silt, Clay)  
#################################################################################################

#CLEANING STEP - Rename variables
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Carbon.content.for.acid.treated.sample.to.remove.carbonates...']<-'Acidified.Carbon'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Clay.content.for.calgon.dispersed.particles.recorded.after.4.mi']<-'Clay_CNLS'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Clay.content.by.hydrometer.method....']<-'Clay_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Exchangeable.Acidity..cmolc.kg..1.']<-'ExAcidity'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Exchangeable.Aluminium..meq.100g.']<-'Exch.Al'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Exchangeable.calcium.concentration.by.Mehlich.3.extraction..mg']<-'m3.Ca'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Potassium.concentration.by.Mehlich.3.extraction..mg.kg..1.']<-'m3.K'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Exchangeable.Magnesium.by.wet.method..mg.kg..1.']<-'m3.Mg'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Phosphorus.by.Mehlich.3.extraction..mg.kg..1.']<-'m3.P'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Soil.pH.in.water..soil.to.water.ratio.of.1.to.2.weight.to.volum']<-'pH'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Sulphur.by.Mehlich.3.extraction..mg.kg..1.']<-'m3.S'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Sand.content.for.calgon.dispersed.particles.recorded.after.4.mi']<-'Sand_CNLS'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Sand.content.by.hydrometer.method....']<-'Sand_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Silt.content.for.calgon.dispersed.particles.recorded.after.4.mi']<-'Silt_CNLS'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Silt.content.by.hydrometer.method....']<-'Silt_icraf'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Landscape..']<-'Landscape'
names(Tier4SoilPredicted)[names(Tier4SoilPredicted)=='Field..']<-'Field'

summary(Tier4SoilPredicted$sample_depth_bottom)
class(Tier4SoilPredicted$sample_depth_bottom)

#Selecting required variables
Tier4SoilPredicted_key <- subset(Tier4SoilPredicted, select=c(Country, Landscape, Household.ID, Field,
  #FarmField.Code,
  sample_depth_bottom,sample_depth_top,Acidified.Carbon, Clay_CNLS,Clay_icraf, ExAcidity,
  Exch.Al, ExCa, ExK, ExMg, m3.P, pH, m3.S, Sand_CNLS, Sand_icraf, Silt_CNLS, Silt_icraf))

#Assigning measurement codes
Tier4SoilPredicted_key$SH6a <- Tier4SoilPredicted_key$ExCa
Tier4SoilPredicted_key$SH6b <- Tier4SoilPredicted_key$ExMg
Tier4SoilPredicted_key$SH6c <- Tier4SoilPredicted_key$ExK
Tier4SoilPredicted_key$SH6d <- Tier4SoilPredicted_key$m3.P
Tier4SoilPredicted_key$SH6e <- Tier4SoilPredicted_key$m3.S
Tier4SoilPredicted_key$SH8 <- Tier4SoilPredicted_key$pH
Tier4SoilPredicted_key$SH10 <- Tier4SoilPredicted_key$Exch.Al
Tier4SoilPredicted_key$SH12 <- Tier4SoilPredicted_key$Acidified.Carbon
Tier4SoilPredicted_key$SH14a <- Tier4SoilPredicted_key$Clay_icraf 
Tier4SoilPredicted_key$SH14b <- Tier4SoilPredicted_key$Silt_icraf 

##########################################################
#SH32 -Step 1. Convert units from mg kg to cmol kg using 
#molar mass and valence and assign measurement codes
##########################################################
#Tier4SoilPredicted_key$SH6a<-m3.Ca/(400/2)
#Tier4SoilPredicted_key$SH6b<-m3.Mg/(240/2)
#Tier4SoilPredicted_key$SH6c<-m3.K/(390/1)
#detach(Tier4SoilPredicted_key)

###########################################
#MODELS
###########################################
#SH41 - Critical threshold soil percent aluminum saturation 
#(SH10 / SH6a  + SH6b + SH6c + SH10)  x 100 (Sanchez et al. 2003)
Tier4SoilPredicted_key$SH41 <- (Tier4SoilPredicted_key$SH10/(Tier4SoilPredicted_key$SH10 + 
  Tier4SoilPredicted_key$SH6a + Tier4SoilPredicted_key$SH6b + Tier4SoilPredicted_key$SH6c))*100
summary(Tier4SoilPredicted_key$SH41)

###########################################
#SH42-Soil C Capacity 
##########################################
#Using ICRAF texture values
Tier4SoilPredicted_key$Cref <- exp(1.333 + .00994*Tier4SoilPredicted_key$SH14a + 
                                     .00699*Tier4SoilPredicted_key$SH14b-.156*(.923*Tier4SoilPredicted_key$SH8-.52))
Tier4SoilPredicted_key$Cref_adj <- Tier4SoilPredicted_key$Cref*((10)/7)^-.58
Tier4SoilPredicted_key$SH42<-(Tier4SoilPredicted_key$SH12/Tier4SoilPredicted_key$Cref_adj)*100
summary(Tier4SoilPredicted_key$SH42)

###########################################
#INDICATORS
###########################################
#SH59 -Soil fertility indicator
#Ca
Tier4SoilPredicted_key$SHFI1 <- .032*Tier4SoilPredicted_key$SH6a -.064
Tier4SoilPredicted_key$SHFI1 [Tier4SoilPredicted_key$SH6a < 2] <- 0
Tier4SoilPredicted_key$SHFI1 [Tier4SoilPredicted_key$SH6a > 7] <- .167
summary(Tier4SoilPredicted_key$SHFI1)

#Mg
Tier4SoilPredicted_key$SHFI2 <- .032*Tier4SoilPredicted_key$SH6b -.064
Tier4SoilPredicted_key$SHFI2 [Tier4SoilPredicted_key$SH6b < 2] <- 0
Tier4SoilPredicted_key$SHFI2 [Tier4SoilPredicted_key$SH6b > 7] <- .167
summary(Tier4SoilPredicted_key$SHFI2)

#K
Tier4SoilPredicted_key$SHFI3 <- .32*Tier4SoilPredicted_key$SH6c -.064
Tier4SoilPredicted_key$SHFI3 [Tier4SoilPredicted_key$SH6c < .2] <- 0
Tier4SoilPredicted_key$SHFI3 [Tier4SoilPredicted_key$SH6c > .7] <- .167
summary(Tier4SoilPredicted_key$SHFI3)

#P
Tier4SoilPredicted_key$SHFI4 <- .0067*Tier4SoilPredicted_key$SH6d -.033
Tier4SoilPredicted_key$SHFI4 [Tier4SoilPredicted_key$SH6d < 5] <- 0
Tier4SoilPredicted_key$SHFI4 [Tier4SoilPredicted_key$SH6d > 30] <- .167
summary(Tier4SoilPredicted_key$SHFI4)

#S
Tier4SoilPredicted_key$SHFI5 <- .0167*Tier4SoilPredicted_key$SH6e -.0835
Tier4SoilPredicted_key$SHFI5[Tier4SoilPredicted_key$SH6e < 5] <- 0
Tier4SoilPredicted_key$SHFI5[Tier4SoilPredicted_key$SH6e > 15] <-.167
summary(Tier4SoilPredicted_key$SHFI5)

#pH
Tier4SoilPredicted_key$SHFI6 <- 888
Tier4SoilPredicted_key$SHFI6_B<- .167*Tier4SoilPredicted_key$SH8 -.7515
Tier4SoilPredicted_key$SHFI6[Tier4SoilPredicted_key$SH10 >60 | Tier4SoilPredicted_key$SH8 <=4.5] <- 0
Tier4SoilPredicted_key$SHFI6[Tier4SoilPredicted_key$SH8 > 5.5 & Tier4SoilPredicted_key$SHFI6==888 ] <- .167
Tier4SoilPredicted_key$SHFI6[which(Tier4SoilPredicted_key$SH8 <= 5.5 & Tier4SoilPredicted_key$SHFI6==888)] <- Tier4SoilPredicted_key$SHFI6_B[Tier4SoilPredicted_key$SH8 <= 5.5 & Tier4SoilPredicted_key$SHFI6==888 ]
summary(Tier4SoilPredicted_key$SHFI6)

#SH59 -Soil fertility indicator
Tier4SoilPredicted_key$SH59 <- Tier4SoilPredicted_key$SHFI1+ Tier4SoilPredicted_key$SHFI2+ Tier4SoilPredicted_key$SHFI3 + Tier4SoilPredicted_key$SHFI4+ Tier4SoilPredicted_key$SHFI5 + Tier4SoilPredicted_key$SHFI6

#SH60-Soil carbon deficit indicator
Tier4SoilPredicted_key$SH60 <- .03333333*Tier4SoilPredicted_key$SH42 -1.66666667
Tier4SoilPredicted_key$SH60[Tier4SoilPredicted_key$SH42 < 50] <- 0
Tier4SoilPredicted_key$SH60[Tier4SoilPredicted_key$SH42 > 80] <- 1
summary(Tier4SoilPredicted_key$SH60)

#Create data frame for quality checking
SH_Tier2_4_processed <- subset(Tier4SoilPredicted_key, select=c(Country, Landscape, 
                                                                Household.ID,
                                                                Field, #FarmField.Code,
                                                                sample_depth_bottom,sample_depth_top,
   SH6d, SH6e, SH8, SH10, SH12, SH14a, SH14b, SH6a, SH6b, SH6c, SH41, Cref, 
   Cref_adj, SH42, SHFI1, SHFI2, SHFI3, SHFI4, SHFI5, SHFI6, SH59, SH60, SHFI6_B))

#Create data frame for final output
SH_Tier2_4_SH12_SH59_SH60 <- subset(Tier4SoilPredicted_key, select=c(Country, Landscape, Household.ID,
                                                                     Field, #FarmField.Code,
   sample_depth_bottom,sample_depth_top, SH12, SH59, SH60))

#Write output for quality checking
write.csv(SH_Tier2_4_processed,"SH_Tier2_4_processed_FFS.csv", row.names = F)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_Tier2_4_processed_FFS.csv", target_path = "Soil_Health/SH_Tier2_4_processed_FFS.csv", overwrite = TRUE)
#Write output according to indicator deliverable
write.csv(SH_Tier2_4_SH12_SH59_SH60,"SH_Tier2_4_SH12_SH59_SH60_FFS.csv", row.names = F)
s3$writeS3(bucket = "ci-vsindicators", source_path = "SH_Tier2_4_SH12_SH59_SH60_FFS.csv", target_path = "Soil_Health/SH_Tier2_4_SH12_SH59_SH60_FFS.csv", overwrite = TRUE)
