################################################################################################################
# SOIL HEALTH THREAD: SH_TZALSMS_PartialNutrientIndicator.R
# Calculates: Yield for all crops in all seasons, nutrient budgets, nutrient budget indicator
# Uses: Tanzania LSMS 2012-2013
# Written: 08/04/15
# Last updated: 09/28/15
################################################################################################################

#Open libraries
library(plyr)
library(openxlsx)
library(doBy)
library(foreign)
library(data.table)
library(raster)
library(rgdal)
library(utils)
library(VitalSignsUtilities)

arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#Name data sources
#set working directory
TZAGA<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_A.dta"]]$getData())
TZAG2A<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_2A.dta"]]$getData())
TZAG2B<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_2B.dta"]]$getData())
TZAG3A<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_3A.dta"]]$getData())
TZAG3B<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_3B.dta"]]$getData())
TZAG4A<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_4A.dta"]]$getData())
TZAG4B<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_4B.dta"]]$getData())
TZAG5A<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_5A.dta"]]$getData())
TZAG5B<-read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/AG_SEC_5B.dta"]]$getData())

NutrComp<-read.xlsx(vstables$tables[["SoilHealthMatrix_12172014.xlsx"]]$getData(), sheet=15, startRow = 1,colNames =TRUE)
CropCode<-read.xlsx(vstables$tables[["SoilHealthMatrix_12172014.xlsx"]]$getData(), sheet=16, startRow = 1,colNames =TRUE)
FertNutrComp<-read.xlsx(vstables$tables[["SoilHealthMatrix_12172014.xlsx"]]$getData(), sheet=18, startRow = 1,colNames =TRUE)
HarvestIndices<-read.xlsx(vstables$tables[["SoilHealthMatrix_12172014.xlsx"]]$getData(), sheet=19, startRow = 1,colNames =TRUE)
SAGCOThouseholds<-read.xlsx(vstables$tables[["TZA LSMS/NPS 2013/TZA_2013_Geo_revised/LSMS2013_y3hhid_sagcot_without_DarEsSalam_reg.xlsx"]]$getData(), sheet=1, startRow = 1,colNames =TRUE)

#CLEANING STEP - Turning all blanks into NAs
TZAGA[TZAGA == ""]<-NA
TZAG2A[TZAG2A == ""] <- NA
TZAG2B[TZAG2B == ""] <- NA
TZAG3A[TZAG3A == ""] <- NA
TZAG3B[TZAG3B == ""] <- NA
TZAG4A[TZAG4A == ""] <- NA
TZAG4B[TZAG4B == ""] <- NA
TZAG5A[TZAG5A == ""] <- NA
TZAG5B[TZAG5B == ""] <- NA

#CLEANING STEP - Recoding plot numbers in 2b, were miscoded in LSMS
TZAG2B$plotnum2<-ifelse(TZAG2B$plotnum=="V1","M1",
                        ifelse(TZAG2B$plotnum=="V2","M2",
                               ifelse(TZAG2B$plotnum=="V3","M3",
                                      ifelse(TZAG2B$plotnum=="V4","M4",
                                             ifelse(TZAG2B$plotnum=="V5","M5",
                                                    ifelse(TZAG2B$plotnum=="V6","M6",
                                                           ifelse(TZAG2B$plotnum=="V7","M7",
                                                                  ifelse(TZAG2B$plotnum=="V8","M8",
                                                                         ifelse(TZAG2B$plotnum=="V9","M9",
                                                                                ifelse(TZAG2B$plotnum=="V10","M10",
                                                                                       ifelse(TZAG2B$plotnum=="V11","M11",
                                                                                              ifelse(TZAG2B$plotnum=="V12","M12",NA))))))))))))
TZAG2B$plotnum<-NULL
names(TZAG2B)[names(TZAG2B)=='plotnum2']<-'plotnum'

#Adding region information to area information
names(TZAGA)[names(TZAGA)=='ag_a01_2']<-'region'
TZAG2A<-merge(TZAG2A, TZAGA, by="y3_hhid", all.x = TRUE)
TZAG2B<-merge(TZAG2B, TZAGA, by="y3_hhid", all.x = TRUE)

##############################################################
# DETERMINING AREA CULTIVATED PER CROP AND CALCULATING YIELD
##############################################################

#Creating a new dataframe with plot area measurements, derived from GPS measurements and farmer's estimates across seasons
TZAG2Aarea<-subset(TZAG2A, select=c(y3_hhid, plotnum,region,ag2a_04, ag2a_09))
TZAG2Barea<-subset(TZAG2B, select=c(y3_hhid, plotnum,region,ag2b_15, ag2b_20))
TZAG2ABarea<-merge(TZAG2Aarea, TZAG2Barea, by=c("y3_hhid","plotnum"), all=TRUE)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$ag2a_09), TZAG2ABarea$ag2b_20, TZAG2ABarea$ag2a_09)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$areaLRS), TZAG2ABarea$ag2a_04, TZAG2ABarea$areaLRS)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$areaLRS), TZAG2ABarea$ag2b_15, TZAG2ABarea$areaLRS)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$ag2b_20), TZAG2ABarea$ag2a_09, TZAG2ABarea$ag2b_20)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$areaSRS), TZAG2ABarea$ag2b_15, TZAG2ABarea$areaSRS)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$areaSRS), TZAG2ABarea$ag2a_04, TZAG2ABarea$areaSRS)
TZAG2ABarea$region.y<-NULL
names(TZAG2ABarea)[names(TZAG2ABarea)=='region.x']<-'region'

#Choosing relevant variables from Section 4 in both seasons to calculate yield (in kg/ha)
TZAG4Atrim<-subset(TZAG4A, select=c(y3_hhid,plotnum,zaocode,ag4a_01,ag4a_02,ag4a_19,ag4a_20,ag4a_28))
TZAG4Btrim<-subset(TZAG4B, select=c(y3_hhid,plotnum,zaocode,ag4b_01,ag4b_02,ag4b_19,ag4b_20,ag4b_28))

#Merging new area dataset with harvest information - LRS
TZAG2A4A<-merge(TZAG4Atrim, TZAG2ABarea, by=c("y3_hhid","plotnum"), all.x=TRUE)
TZAG2A4A$ag2b_15<-NULL
TZAG2A4A$ag2b_20<-NULL

#Merging new area dataset with harvest information - SRS
TZAG2B4B<-merge(TZAG4Btrim, TZAG2ABarea, by=c("y3_hhid","plotnum"), all.x=TRUE)
TZAG2B4B$ag2a_04<-NULL
TZAG2B4B$ag2a_09<-NULL

#Data staging to determine area planted for long rainy season (A)
TZAG2A4A$areacult<-ifelse(TZAG2A4A$ag4a_01=="NO" & TZAG2A4A$ag4a_02=="1/4",TZAG2A4A$areaLRS * 0.25,
                ifelse(TZAG2A4A$ag4a_01=="NO" & TZAG2A4A$ag4a_02=="1/2",TZAG2A4A$areaLRS * 0.5,
                ifelse(TZAG2A4A$ag4a_01=="NO" & TZAG2A4A$ag4a_02=="3/4",TZAG2A4A$areaLRS * 0.75,
                ifelse(TZAG2A4A$ag4a_01=="YES",TZAG2A4A$areaLRS,NA))))

#Renaming variables for readability
names(TZAG2A4A)[names(TZAG2A4A)=='areacult']<-'areaplantedpercrop_acre'
names(TZAG2A4A)[names(TZAG2A4A)=='ag4a_28']<-'quantcropharvestedkg'
names(TZAG2A4A)[names(TZAG2A4A)=='ag2a_04']<-'fieldareaestimatebyfarmer'
names(TZAG2A4A)[names(TZAG2A4A)=='ag2a_09']<-'totfieldareabyGPS'
names(TZAG2A4A)[names(TZAG2A4A)=='ag4a_01']<-'waswholefieldplanted'
names(TZAG2A4A)[names(TZAG2A4A)=='ag4a_02']<-'whichportionoffieldplanted'
names(TZAG2A4A)[names(TZAG2A4A)=='ag4a_19']<-'wereanycropsharvested'
names(TZAG2A4A)[names(TZAG2A4A)=='ag4a_20']<-'whynocropsharvested'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area in which it was planted) in LRS
TZAG2A4A$areaplantedpercrop_ha<-TZAG2A4A$areaplantedpercrop_acre / 2.47105
TZAG2A4A$cropyieldkgperacre<-TZAG2A4A$quantcropharvestedkg/TZAG2A4A$areaplantedpercrop_acre
TZAG2A4A$cropyieldkgperha<-(TZAG2A4A$quantcropharvestedkg/TZAG2A4A$areaplantedpercrop_ha)

#Data staging to determine area planted for short rainy season (B)
TZAG2B4B$areacult<-ifelse(TZAG2B4B$ag4b_01=="NO" & TZAG2B4B$ag4b_02=="1/4",TZAG2B4B$areaSRS * 0.25,
                ifelse(TZAG2B4B$ag4b_01=="NO" & TZAG2B4B$ag4b_02=="1/2",TZAG2B4B$areaSRS * 0.5,
                ifelse(TZAG2B4B$ag4b_01=="NO" & TZAG2B4B$ag4b_02=="3/4",TZAG2B4B$areaSRS * 0.75,
                ifelse(TZAG2B4B$ag4b_01=="YES",TZAG2B4B$areaSRS,NA))))

#Renaming variables for readability
names(TZAG2B4B)[names(TZAG2B4B)=='areacult']<-'areaplantedpercrop_acre'
names(TZAG2B4B)[names(TZAG2B4B)=='ag4b_28']<-'quantcropharvestedkg'
names(TZAG2B4B)[names(TZAG2B4B)=='ag2b_15']<-'fieldareaestimatebyfarmer'
names(TZAG2B4B)[names(TZAG2B4B)=='ag2b_20']<-'totfieldareabyGPS'
names(TZAG2B4B)[names(TZAG2B4B)=='ag4b_01']<-'waswholefieldplanted'
names(TZAG2B4B)[names(TZAG2B4B)=='ag4b_02']<-'whichportionoffieldplanted'
names(TZAG2B4B)[names(TZAG2B4B)=='ag4b_19']<-'wereanycropsharvested'
names(TZAG2B4B)[names(TZAG2B4B)=='ag4b_20']<-'whynocropsharvested'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area in which it was planted) in SRS
TZAG2B4B$areaplantedpercrop_ha<-TZAG2B4B$areaplantedpercrop_acre / 2.47105
TZAG2B4B$cropyieldkgperacre<-TZAG2B4B$quantcropharvestedkg/TZAG2B4B$areaplantedpercrop_acre
TZAG2B4B$cropyieldkgperha<-(TZAG2B4B$quantcropharvestedkg/TZAG2B4B$areaplantedpercrop_ha)

#Labeling observations with season
TZAG2A4A$season<-"LRS"
TZAG2B4B$season<-"SRS"

#########################################
# STAGING FOR CALCULATING NUTRIENT BUDGET
#########################################

#Selecting management information from Section 3 - LRS
TZAG3Atrim<-subset(TZAG3A, select=c(y3_hhid, plotnum,ag3a_03,ag3a_18,ag3a_19,ag3a_20,ag3a_21,ag3a_40,ag3a_41,
                                    ag3a_42,ag3a_47:ag3a_49,ag3a_54:ag3a_56))
TZAG3Atrim$season<-"LRS"

#Joining management data with yield and area data
TZAG2A4A3A<-merge(TZAG2A4A,TZAG3Atrim, by=c("y3_hhid","plotnum","season"), all.x = TRUE)

#Renaming variables for readability
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_03']<-'use'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_18']<-'anyirrigation'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_19']<-'irrigationtype'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_20']<-'obtainwatermethod'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_21']<-'watersource'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_40']<-'cultivated'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_41']<-'usedorgfert'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_42']<-'quantorgfert'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_47']<-'usedinorgfertA'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_48']<-'typeinorgfertA'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_49']<-'quantinorgfertA'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_54']<-'usedinorgfertB'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_55']<-'typeinorgfertB'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='ag3a_56']<-'quantinorgfertB'
names(TZAG2A4A3A)[names(TZAG2A4A3A)=='areaLRS']<-'totplotarea'
TZAG2A4A3A$areaSRS<-NULL

#Selecting management information from Section 3 - SRS
TZAG3Btrim<-subset(TZAG3B,select=c(y3_hhid,plotnum,ag3b_03,ag3b_18,ag3b_19,ag3b_20,ag3b_21,ag3b_40,ag3b_41,
                                   ag3b_42, ag3b_47:ag3b_49,ag3b_54:ag3b_56))
TZAG3Btrim$season<-"SRS"

#Joining management data with area and yield data
TZAG2B4B3B<-merge(TZAG2B4B, TZAG3Btrim, by=c("y3_hhid","plotnum","season"), all.x=TRUE)

#Renaming variables for readability
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_03']<-'use'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_18']<-'anyirrigation'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_19']<-'irrigationtype'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_20']<-'obtainwatermethod'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_21']<-'watersource'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_40']<-'cultivated'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_41']<-'usedorgfert'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_42']<-'quantorgfert'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_47']<-'usedinorgfertA'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_48']<-'typeinorgfertA'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_49']<-'quantinorgfertA'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_54']<-'usedinorgfertB'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_55']<-'typeinorgfertB'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='ag3b_56']<-'quantinorgfertB'
names(TZAG2B4B3B)[names(TZAG2B4B3B)=='areaSRS']<-'totplotarea'
TZAG2B4B3B$areaLRS<-NULL

#Bringing the short rainy season and long rainy season back together (A=LRS, B=SRS) adding all observations together
TZAG2AB3AB4AB<-rbind(TZAG2A4A3A,TZAG2B4B3B)

#Recoding anyirrigation variable to be 0/1 instead of NO/YES
TZAG2AB3AB4AB$anyirrigation<-ifelse(TZAG2AB3AB4AB$anyirrigation=="YES",1,TZAG2AB3AB4AB$anyirrigation)
TZAG2AB3AB4AB$anyirrigation<-ifelse(TZAG2AB3AB4AB$anyirrigation=="NO",0,TZAG2AB3AB4AB$anyirrigation)

#Dropping all observations with insufficient plot area information (lacking either total plot area, 
#proportion of plot area devoted to cultivating each plot, or both)
TZAG2A3ABB4AB<-subset(TZAG2A3ABB4AB, !is.na(TZAG2A3ABB4AB$areaplantedpercrop_ha))

#DROPPING ALL YIELD OBSERVATIONS GREATER THAN 15,000 KGS PER ACRE
TZAG2A3ABB4AB<-subset(TZAG2AB3AB4AB, TZAG2AB3AB4AB$cropyieldkgperha<15000)
#rm(TZAG2A4Atrim, TZAG2A4Atrim2, TZAG2B4Btrim, TZAG2B4Btrim2, TZAG3Atrim, TZAG3Btrim, TZAG2A4A, TZAG2B4B)

###############################################
#CALCULATING NUTRIENT BUDGET: NUTRIENTS REMOVED
###############################################
#Making sure crop codes are clean - LSMS CLEANING STEP
table(TZAG2AB3AB4AB$zaocode)
names(TZAG2AB3AB4AB)[names(TZAG2AB3AB4AB)=='zaocode']<-'Crop'

# CLEANING STEP - Making crop codes fit with external data table with nutrient composition
CropCode$Crop<-toupper(CropCode$Crop)
TZAG2AB3AB4AB$Crop<-ifelse(TZAG2AB3AB4AB$Crop=="CHICK PEAS", "CHICKPEAS",
                          ifelse(TZAG2AB3AB4AB$Crop=="AMARANTHS", "AMARANTH",
                                 ifelse(TZAG2AB3AB4AB$Crop=="WATER MELLON", "WATERMELON",
                                        ifelse(TZAG2AB3AB4AB$Crop=="EGG PLANT","EGGPLANT",
                                               ifelse(TZAG2AB3AB4AB$Crop=="FIWI","KIWI",
                                                      ifelse(TZAG2AB3AB4AB$Crop=="PUMPKINS","PUMPKIN",as.character(TZAG2AB3AB4AB$Crop)))))))
table(TZAG2AB3AB4AB$Crop)
table(CropCode$Crop)
TZAGNutrComp<-merge(TZAG2AB3AB4AB,CropCode,by="Crop", all.x=TRUE)
#rm(TZAG2AB3AB4AB)

#CHECK - Making sure every obs with a crop code has a crop group
sum(!is.na(TZAGNutrComp$Crop) & is.na(TZAGNutrComp$CropGroup))
#There are 57 observations that have a crop code but do not have a crop group
#This tells us that the only crops without a crop code are the ones labeled "other" 
#or those for which we did not have nutrition composition (e.g., seaweed) - CHECK
which(!is.na(TZAGNutrComp$Crop) & is.na(TZAGNutrComp$CropGroup))
#There are 57 observations that have a crop code but were not matched with a 
#crop group - 46 of these are other, 11 are seaweed

#ASSUMPTION - The mass of C removed is 47% of the mass of harvested product; and the mass of C added by crop residue
#is 47% the mass of the residue
NutrComp$Cropresidue_C<-0.47
NutrComp$Harvested.product_C<-0.47

#Merging with nutrient composition table
names(NutrComp)[names(NutrComp)=='Crop']<-'CropGroup'
TZAGNutrComp2<-merge(TZAGNutrComp,NutrComp,by="CropGroup", all.x=TRUE)

#Making sure every obs with a crop group has nutrient composition values - CHECK
sum(!is.na(TZAGNutrComp2$Crop) & is.na(TZAGNutrComp2$Harvested.product_N))
sum(!is.na(TZAGNutrComp2$CropGroup) & is.na(TZAGNutrComp2$Harvested.product_N))

#Calculating the C, N, P, K removal in kg for each crop for each field
TZAGNutrComp2$Nrmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_N
TZAGNutrComp2$Prmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_P
TZAGNutrComp2$Krmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_K
TZAGNutrComp2$Crmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_C

#CHECK MERGES
sum(is.na(TZAGNutrComp2$quantcropharvestedkg))
sum(is.na(TZAGNutrComp2$Harvested.product_N))
sum(is.na(TZAGNutrComp2$Krmvdkg))

#Calculating the C, N, P, K removal in kg/ha for each crop for each field
attach(TZAGNutrComp2)
TZAGNutrComp2$Nremovedkgperha<-Nrmvdkg/areaplantedpercrop_ha
TZAGNutrComp2$Premovedkgperha<-Prmvdkg/areaplantedpercrop_ha
TZAGNutrComp2$Kremovedkgperha<-Krmvdkg/areaplantedpercrop_ha
TZAGNutrComp2$Cremovedkgperha<-Crmvdkg/areaplantedpercrop_ha
detach(TZAGNutrComp2)
#rm(TZAGNutrComp)

TZAGFertNutrComp<-TZAGNutrComp2

#####################################################################
#CALCULATING NUTRIENT BUDGET: NUTRIENTS ADDED BY INORGANIC FERTILIZER
#####################################################################

#Change names of inorganic fertilizers in dataset to match those in external table
TZAGFertNutrComp$INPUTSA<-ifelse(TZAGFertNutrComp$typeinorgfertA=="DI-AMMOIUM   PHOSPHATE (DAP)", "DI-AMMONIUM PHOSPHATE (DAP)",
                                 ifelse(TZAGFertNutrComp$typeinorgfertA=="UREA","UREA",
                                        ifelse(TZAGFertNutrComp$typeinorgfertA=="NITROGEN PHOSPHATE POTASSIUM (NPK)", "NITROGEN PHOSPHATE POTASSIUM (NPK)",
                                               ifelse(TZAGFertNutrComp$typeinorgfertA=="MINJINGU ROCK PHOSPHATE (MRP)", "ROCK PHOSPHATE(MRP) [MINJINGU]",
                                                      ifelse(TZAGFertNutrComp$typeinorgfertA=="SULPHATE OF AMMONIUM (SA)", "SULPHATE OF AMMONIUM (SA)",
                                                             ifelse(TZAGFertNutrComp$typeinorgfertA=="TRIPLE SUPER   PHOSPHATE (TSP)", "TRIPLE SUPER PHOSPHATE ( TSP)",
                                                                    ifelse(TZAGFertNutrComp$typeinorgfertA=="CALCIUM AMMONIUM NITRATE (CAN)", "CALCIUM AMMONIUM NITRATE (CAN)",NA)))))))

TZAGFertNutrComp$INPUTSB<-ifelse(TZAGFertNutrComp$typeinorgfertB=="DI-AMMOIUM   PHOSPHATE (DAP)", "DI-AMMONIUM PHOSPHATE (DAP)",
                                 ifelse(TZAGFertNutrComp$typeinorgfertB=="UREA","UREA",
                                        ifelse(TZAGFertNutrComp$typeinorgfertB=="NITROGEN PHOSPHATE POTASSIUM (NPK)", "NITROGEN PHOSPHATE POTASSIUM (NPK)",
                                               ifelse(TZAGFertNutrComp$typeinorgfertB=="MINJINGU ROCK PHOSPHATE (MRP)", "ROCK PHOSPHATE(MRP) [MINJINGU]",
                                                      ifelse(TZAGFertNutrComp$typeinorgfertB=="SULPHATE OF AMMONIUM (SA)", "SULPHATE OF AMMONIUM (SA)",
                                                             ifelse(TZAGFertNutrComp$typeinorgfertB=="TRIPLE SUPER   PHOSPHATE (TSP)", "TRIPLE SUPER PHOSPHATE ( TSP)",
                                                                    ifelse(TZAGFertNutrComp$typeinorgfertB=="CALCIUM AMMONIUM NITRATE (CAN)", "CALCIUM AMMONIUM NITRATE (CAN)",NA)))))))

#CHECK - check to make sure numbers of original observations with inputs and new observations with inputs are the same
count(!(is.na(TZAGFertNutrComp$typeinorgfertA)))
count(!(is.na(TZAGFertNutrComp$INPUTSA)))
count(!(is.na(TZAGFertNutrComp$typeinorgfertB)))
count(!(is.na(TZAGFertNutrComp$INPUTSB)))

#ASSUMPTION - Assuming that for organic fertilizer, the mass of C added is 47% of the mass of fertilizer added
names(FertNutrComp)[names(FertNutrComp)=='INPUTS']<-'INPUTSA'
FertNutrComp$C<-ifelse(FertNutrComp$INPUTSA=="ANIMAL MANURE",0.47,0)

#Merge with nutrient table for first batch of inorganic fertilizers
TZAGFertNutrComp2<-merge(TZAGFertNutrComp, FertNutrComp, by="INPUTSA", all.x=TRUE)

#Count NAs to see if merge for inorganicfertA was correct - CHECK
count(!(is.na(TZAGFertNutrComp2$N)))
count(TZAGFertNutrComp2$usedinorgfertA=="YES")

#Merge with nutrient table for second batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='INPUTSA']<-'INPUTSB'
TZAGFertNutrComp3<-merge(TZAGFertNutrComp2, FertNutrComp, by="INPUTSB", all.x=TRUE)

#Count NAs to see if merge B was correct - CHECK
count(!(is.na(TZAGFertNutrComp3$N.y)))
count(TZAGFertNutrComp2$usedinorgfertB=="YES")

#Getting nutrients added (N, P, K, C) for inorganic fertilizer A in kg (for the area in which each crop is planted in each field)  
TZAGFertNutrComp3$NaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$N.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$PaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$P.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$KaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$K.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$CaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$C.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$P205addedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$P205.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$PaddedkgInorgA<-TZAGFertNutrComp3$PaddedkgInorgA+TZAGFertNutrComp3$P205addedkgInorgA*0.4364
TZAGFertNutrComp3$K20addedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$K20.x*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$KaddedkgInorgA<-TZAGFertNutrComp3$KaddedkgInorgA+TZAGFertNutrComp3$K20addedkgInorgA*1.2046

#If there was no inorganic fertilizer reported used, make nutrients added by organic fertilizer 0 (instead of NA)
TZAGFertNutrComp3$NaddedkgInorgA<-ifelse(TZAGFertNutrComp3$usedinorgfertA=="NO",0,TZAGFertNutrComp3$NaddedkgInorgA)
TZAGFertNutrComp3$PaddedkgInorgA<-ifelse(TZAGFertNutrComp3$usedinorgfertA=="NO",0,TZAGFertNutrComp3$PaddedkgInorgA)
TZAGFertNutrComp3$KaddedkgInorgA<-ifelse(TZAGFertNutrComp3$usedinorgfertA=="NO",0,TZAGFertNutrComp3$KaddedkgInorgA)
TZAGFertNutrComp3$CaddedkgInorgA<-ifelse(TZAGFertNutrComp3$usedinorgfertA=="NO",0,TZAGFertNutrComp3$CaddedkgInorgA)

#Getting nutrients added (N, P, K, C) for inorganic fertilizer B (for the area in which each crop is planted in each field) 
TZAGFertNutrComp3$NaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$N.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$PaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$P.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$KaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$K.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$CaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$C.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$P205addedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$P205.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$PaddedkgInorgB<-TZAGFertNutrComp3$PaddedkgInorgB+TZAGFertNutrComp3$P205addedkgInorgB*0.4364
TZAGFertNutrComp3$K20addedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$K20.y*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp3$KaddedkgInorgB<-TZAGFertNutrComp3$KaddedkgInorgB+TZAGFertNutrComp3$K20addedkgInorgB*1.2046

#If there was no inorganic fertilizer reported used, make nutrients added by organic fertilizer 0 (instead of NA)
TZAGFertNutrComp3$NaddedkgInorgB<-ifelse((is.na(TZAGFertNutrComp3$usedinorgfertB) & TZAGFertNutrComp3$usedinorgfertA=="NO") | 
                                          TZAGFertNutrComp3$usedinorgfertB=="NO",0,TZAGFertNutrComp3$NaddedkgInorgB)
TZAGFertNutrComp3$PaddedkgInorgB<-ifelse((is.na(TZAGFertNutrComp3$usedinorgfertB) & TZAGFertNutrComp3$usedinorgfertA=="NO") | 
                                           TZAGFertNutrComp3$usedinorgfertB=="NO",0,TZAGFertNutrComp3$PaddedkgInorgB)
TZAGFertNutrComp3$KaddedkgInorgB<-ifelse((is.na(TZAGFertNutrComp3$usedinorgfertB) & TZAGFertNutrComp3$usedinorgfertA=="NO") | 
                                           TZAGFertNutrComp3$usedinorgfertB=="NO",0,TZAGFertNutrComp3$KaddedkgInorgB)
TZAGFertNutrComp3$CaddedkgInorgB<-ifelse((is.na(TZAGFertNutrComp3$usedinorgfertB) & TZAGFertNutrComp3$usedinorgfertA=="NO") | 
                                           TZAGFertNutrComp3$usedinorgfertB=="NO",0,TZAGFertNutrComp3$CaddedkgInorgB)

#Summing nutrient addition per crop per field for inorganic fertilizers only
TZAGFertNutrComp3$TotNaddedkgInorg<-TZAGFertNutrComp3$NaddedkgInorgA+TZAGFertNutrComp3$NaddedkgInorgB
TZAGFertNutrComp3$TotPaddedkgInorg<-TZAGFertNutrComp3$PaddedkgInorgA+TZAGFertNutrComp3$PaddedkgInorgB
TZAGFertNutrComp3$TotKaddedkgInorg<-TZAGFertNutrComp3$KaddedkgInorgA+TZAGFertNutrComp3$KaddedkgInorgB
TZAGFertNutrComp3$TotCaddedkgInorg<-TZAGFertNutrComp3$CaddedkgInorgA+TZAGFertNutrComp3$CaddedkgInorgB

#Finding total nutrients by all inorganic fertilizers added in kg/ha
TZAGFertNutrComp3$TotNaddedkghaInorg<-TZAGFertNutrComp3$TotNaddedkgInorg/TZAGFertNutrComp3$areaplantedpercrop_ha
TZAGFertNutrComp3$TotPaddedkghaInorg<-TZAGFertNutrComp3$TotPaddedkgInorg/TZAGFertNutrComp3$areaplantedpercrop_ha
TZAGFertNutrComp3$TotKaddedkghaInorg<-TZAGFertNutrComp3$TotKaddedkgInorg/TZAGFertNutrComp3$areaplantedpercrop_ha
TZAGFertNutrComp3$TotCaddedkghaInorg<-TZAGFertNutrComp3$TotCaddedkgInorg/TZAGFertNutrComp3$areaplantedpercrop_ha

#####################################################################
#CALCULATING NUTRIENT BUDGET: NUTRIENTS ADDED BY ORGANIC FERTILIZER
#####################################################################

#ASSUMPTION - All organic fertilizers will have the same nutrient contribution as animal manure
names(FertNutrComp)[names(FertNutrComp)=='INPUTSB']<-'INPUTSC'
FertNutrComp$INPUTSC<-ifelse(FertNutrComp$INPUTSC=="ANIMAL MANURE","ORGANIC FERTILIZER",FertNutrComp$INPUTSC)

#Merging with table with nutrition content of organic fertilizers
TZAGFertNutrComp4<-subset(TZAGFertNutrComp3, select=c(INPUTSB:VSCode.x,NaddedkgInorgA:TotCaddedkghaInorg))
TZAGFertNutrComp4$INPUTSC<-ifelse(TZAGFertNutrComp4$usedorgfert=="YES","ORGANIC FERTILIZER",0)
TZAGFertNutrComp5<-merge(TZAGFertNutrComp4,FertNutrComp, by="INPUTSC", all.x=TRUE)

#CHECK MERGE to make sure that all obs that answered yes to used organic fertilizer were assigned a nutrient composition
count(TZAGFertNutrComp5$usedorgfert=="YES")
count(!is.na(TZAGFertNutrComp5$N))
#rm(TZAGFertNutrComp, TZAGFertNutrComp2, TZAGFertNutrComp3, TZAGFertNutrComp4)

#Getting nutrient addition (N, P, K, C) for organic fertilizer (for the area in which each crop is planted in each field) 
TZAGFertNutrComp5$NaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$N*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$PaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$P*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$KaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$K*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$CaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$C*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$P205addedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$P205*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$PaddedkgOrg<-TZAGFertNutrComp5$PaddedkgOrg + TZAGFertNutrComp5$P205addedkgOrg*0.4364
TZAGFertNutrComp5$K20addedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$K20*(TZAGFertNutrComp3$areaplantedpercrop_acre/TZAGFertNutrComp3$totplotarea)
TZAGFertNutrComp5$KaddedkgOrg<-TZAGFertNutrComp5$KaddedkgOrg + TZAGFertNutrComp5$K20addedkgOrg*1.2046

#Making nutrients added from organic fertilizer 0 if no organic fertilizer added
TZAGFertNutrComp5$NaddedkgOrg<-ifelse(TZAGFertNutrComp5$usedorgfert=="NO",0,TZAGFertNutrComp5$NaddedkgOrg)
TZAGFertNutrComp5$PaddedkgOrg<-ifelse(TZAGFertNutrComp5$usedorgfert=="NO",0,TZAGFertNutrComp5$PaddedkgOrg)
TZAGFertNutrComp5$KaddedkgOrg<-ifelse(TZAGFertNutrComp5$usedorgfert=="NO",0,TZAGFertNutrComp5$KaddedkgOrg)
TZAGFertNutrComp5$CaddedkgOrg<-ifelse(TZAGFertNutrComp5$usedorgfert=="NO",0,TZAGFertNutrComp5$CaddedkgOrg)

#Calculating nutrients (N,P,K,C) added by organic fertilizer in kg/ha
TZAGFertNutrComp5$NaddedkghaOrg<-TZAGFertNutrComp5$NaddedkgOrg/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$PaddedkghaOrg<-TZAGFertNutrComp5$PaddedkgOrg/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$KaddedkghaOrg<-TZAGFertNutrComp5$KaddedkgOrg/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$CaddedkghaOrg<-TZAGFertNutrComp5$CaddedkgOrg/TZAGFertNutrComp5$areaplantedpercrop_ha

#Summing total nutrients added from inorganic and organic fertilizers ON THE FIELD LEVEL - kg
TZAGFertNutrComp5$TotNaddedkgAllFert<-TZAGFertNutrComp5$TotNaddedkgInorg+TZAGFertNutrComp5$NaddedkgOrg
TZAGFertNutrComp5$TotPaddedkgAllFert<-TZAGFertNutrComp5$TotPaddedkgInorg+TZAGFertNutrComp5$PaddedkgOrg
TZAGFertNutrComp5$TotKaddedkgAllFert<-TZAGFertNutrComp5$TotKaddedkgInorg+TZAGFertNutrComp5$KaddedkgOrg
TZAGFertNutrComp5$TotCaddedkgAllFert<-TZAGFertNutrComp5$TotCaddedkgInorg+TZAGFertNutrComp5$CaddedkgOrg
sum(is.na(TZAGFertNutrComp5$TotNaddedkgAllFert))

#Calculating total nutrients added from inorganic and organic fertilizers - kg/ha
TZAGFertNutrComp5$TotNaddedkghaAllFert<-TZAGFertNutrComp5$TotNaddedkgAllFert/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$TotPaddedkghaAllFert<-TZAGFertNutrComp5$TotPaddedkgAllFert/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$TotKaddedkghaAllFert<-TZAGFertNutrComp5$TotKaddedkgAllFert/TZAGFertNutrComp5$areaplantedpercrop_ha
TZAGFertNutrComp5$TotCaddedkghaAllFert<-TZAGFertNutrComp5$TotCaddedkgAllFert/TZAGFertNutrComp5$areaplantedpercrop_ha

#####################################################################
#CALCULATING NUTRIENT BUDGET: NUTRIENTS ADDED BY CROP RESIDUES
#####################################################################

#Subsetting dataset to get ready for crop residue nutrient additions
TZAGCropRes<-subset(TZAGFertNutrComp5, select=c(INPUTSC:VSCode, NaddedkgOrg:TotCaddedkghaAllFert))

#Looking up crop residue use
TZAG5Atrim<-subset(TZAG5A, select=c(y3_hhid, zaocode, ag5a_33_1, ag5a_33_2))
TZAG5Btrim<-subset(TZAG5B, select=c(y3_hhid, zaocode, ag5b_33_1, ag5b_33_2))
names(TZAG5Atrim)[names(TZAG5Atrim)=='ag5a_33_1']<-'cropresuse1'
names(TZAG5Btrim)[names(TZAG5Btrim)=='ag5b_33_1']<-'cropresuse1'
names(TZAG5Atrim)[names(TZAG5Atrim)=='ag5a_33_2']<-'cropresuse2'
names(TZAG5Btrim)[names(TZAG5Btrim)=='ag5b_33_2']<-'cropresuse2'
names(TZAG5Atrim)[names(TZAG5Atrim)=='zaocode']<-'Crop'
names(TZAG5Btrim)[names(TZAG5Btrim)=='zaocode']<-'Crop'
TZAG5Atrim$season<-"LRS"
TZAG5Btrim$season<-"SRS"
TZAG5ABtrim<-rbind(TZAG5Atrim,TZAG5Btrim)
#rm(TZAG5ACropRes, TZAG5Btrim, TZAG5AB)

#Merging crop residue use information with yield and input information
TZAGCropRes2<-merge(TZAGCropRes, TZAG5ABtrim, by=c("y3_hhid","Crop","season"), all.x=TRUE)

#Finding the amount of residue produced per kg of crop harvested (according to its residue group)
HarvestIndices$CropResFactor<-(1-HarvestIndices$Harvest.Index)/HarvestIndices$Harvest.Index
TZAGCropRes3<-merge(TZAGCropRes2, HarvestIndices, by="CropResidueGroup", all.x = TRUE)
TZAGCropRes3$kgscropresidue<-TZAGCropRes3$quantcropharvestedkg*TZAGCropRes3$CropResFactor

#Finding the NPK available in every tonne of residue - in kg
#ASSUMPTION - Here we are assuming that 100% of the nutrients of the crop residue left in the field are being added to the soil

TZAGCropRes3$cropresuse2<-ifelse(!is.na(TZAGCropRes3$cropresuse1) & is.na(TZAGCropRes3$cropresuse2), "NONE", as.character(TZAGCropRes3$cropresuse2))
TZAGCropRes3$NaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_N,0)
TZAGCropRes3$PaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_P,0)
TZAGCropRes3$KaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_K,0)
TZAGCropRes3$CaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_C,0)

#Finding the NPK available in every tonne of residue - in kg/ha
TZAGCropRes3$NaddedkghaCropres<-TZAGCropRes3$NaddedkgCropres/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$PaddedkghaCropres<-TZAGCropRes3$PaddedkgCropres/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$KaddedkghaCropres<-TZAGCropRes3$KaddedkgCropres/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$CaddedkghaCropres<-TZAGCropRes3$CaddedkgCropres/TZAGCropRes3$areaplantedpercrop_ha

#####################################################################
#CALCULATING NUTRIENT BUDGET: COMBINING NUTRIENTS REMOVED AND ADDED
#####################################################################

#Calculating nutrients added from all sources in kgs
attach(TZAGCropRes3)
TZAGCropRes3$NAddedAllSrcskg<-TotNaddedkgAllFert + NaddedkgCropres
TZAGCropRes3$PAddedAllSrcskg<-TotPaddedkgAllFert + PaddedkgCropres
TZAGCropRes3$KAddedAllSrcskg<-TotKaddedkgAllFert + KaddedkgCropres
TZAGCropRes3$CAddedAllSrcskg<-TotCaddedkgAllFert + CaddedkgCropres

#Calculating nutrients added from all sources in kg/ha
TZAGCropRes3$NAddedAllSrcskgha<-TotNaddedkghaAllFert + NaddedkghaCropres
TZAGCropRes3$PAddedAllSrcskgha<-TotPaddedkghaAllFert + PaddedkghaCropres
TZAGCropRes3$KAddedAllSrcskgha<-TotKaddedkghaAllFert + KaddedkghaCropres
TZAGCropRes3$CAddedAllSrcskgha<-TotCaddedkghaAllFert + CaddedkghaCropres
detach(TZAGCropRes3)
#rm(TZAGCropRes,TZAGCropRes2)

#Calculating nutrient budgets in kg
TZAGCropRes3$NutrBudgNkg<-TZAGCropRes3$TotNaddedkgAllFert + TZAGCropRes3$NaddedkgCropres - TZAGCropRes3$Nrmvdkg
TZAGCropRes3$NutrBudgPkg<-TZAGCropRes3$TotPaddedkgAllFert + TZAGCropRes3$PaddedkgCropres - TZAGCropRes3$Prmvdkg
TZAGCropRes3$NutrBudgKkg<-TZAGCropRes3$TotKaddedkgAllFert + TZAGCropRes3$KaddedkgCropres - TZAGCropRes3$Krmvdkg
TZAGCropRes3$NutrBudgCkg<-TZAGCropRes3$TotCaddedkgAllFert + TZAGCropRes3$CaddedkgCropres - TZAGCropRes3$Crmvdkg

#Calculating nutrient budgets in kg/ha
TZAGCropRes3$NutrBudgNkgha<-TZAGCropRes3$NutrBudgNkg/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$NutrBudgPkgha<-TZAGCropRes3$NutrBudgPkg/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$NutrBudgKkgha<-TZAGCropRes3$NutrBudgKkg/TZAGCropRes3$areaplantedpercrop_ha
TZAGCropRes3$NutrBudgCkgha<-TZAGCropRes3$NutrBudgCkg/TZAGCropRes3$areaplantedpercrop_ha

#Subsetting for area, quantity harvested, yield, and management questions related to nutrient budgets 
TZAGSummAll<-subset(TZAGCropRes3, TZAGCropRes3$totplotarea!=0, 
                  select=c(y3_hhid,plotnum,zaocode,region,quantcropharvestedkg,totplotarea,areaplantedpercrop_ha,
                  cropyieldkgperacre, cropyieldkgperha,use,season,anyirrigation:watersource,Nrmvdkg:Cremovedkgperha,
                  usedorgfert:quantinorgfertB,TotNaddedkgInorg:CaddedkghaOrg,cropresuse1,
                  cropresuse2,CropResFactor,NaddedkgCropres:NutrBudgCkgha))
summary(TZAGSummAll)

#Creating partial nutrient budget indicator
TZAGSummAll$NutrPartialIndicatorN<-888
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<(-50) | TZAGSummAll$NutrBudgNkgha>50, 0,TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<20 & TZAGSummAll$NutrBudgNkgha>(-20), 0.333,TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha>(-50) & TZAGSummAll$NutrBudgNkgha<(-20), (1/90) * TZAGSummAll$NutrBudgNkgha + (5/9), TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<50 & TZAGSummAll$NutrBudgNkgha>20, (-1/90) * TZAGSummAll$NutrBudgNkgha + (5/9), TZAGSummAll$NutrPartialIndicatorN)
summary(TZAGSummAll$NutrPartialIndicatorN)

TZAGSummAll$NutrPartialIndicatorP<-888
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<(-20) | TZAGSummAll$NutrBudgPkgha>20, 0,TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<5 & TZAGSummAll$NutrBudgPkgha>(-5), 0.333,TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha>(-20) & TZAGSummAll$NutrBudgPkgha<(-5), (1/45) * TZAGSummAll$NutrBudgPkgha + (4/9), TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<20 & TZAGSummAll$NutrBudgPkgha>5, (-1/45) * TZAGSummAll$NutrBudgPkgha + (4/9), TZAGSummAll$NutrPartialIndicatorP)
summary(TZAGSummAll$NutrPartialIndicatorP)

TZAGSummAll$NutrPartialIndicatorK<-888
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<(-50) | TZAGSummAll$NutrBudgKkgha>50, 0,TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<20 & TZAGSummAll$NutrBudgKkgha>(-20), 0.333,TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha>(-50) & TZAGSummAll$NutrBudgKkgha<(-20), (1/90) * TZAGSummAll$NutrBudgKkgha + (5/9), TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<50 & TZAGSummAll$NutrBudgKkgha>20, (-1/90) * TZAGSummAll$NutrBudgKkgha + (5/9), TZAGSummAll$NutrPartialIndicatorK)
summary(TZAGSummAll$NutrPartialIndicatorK)

#Adding partial nutrient budget indicators to create SH58 indicator
TZAGSummAll$SH58<-TZAGSummAll$NutrPartialIndicatorN + TZAGSummAll$NutrPartialIndicatorP + TZAGSummAll$NutrPartialIndicatorK

#Identifying which households are in SAGCOT using center latitude and longitude of enumeration areas
setwd(dir_data3)
runSAGCOT<-parse("Ag.SubsettingHHtoSagcot4.R")
eval(runSAGCOT)

#Merge with list of households in SAGCOT region
TZAGSummAll<-merge(TZAGSummAll, hh_sagcot, by="y3_hhid", all.x=TRUE)

#Adding a variable that determines whether observations are in SAGCOT or not depending on merge
TZAGSummAll$inSAGCOT=ifelse(!is.na(TZAGSummAll$lon_dd_mod), 1, 0)

#Subsetting for observations in SAGCOT only
TZAGSummSAGCOTonly<-subset(TZAGSummAll, inSAGCOT==1, select=c(y3_hhid, plotnum, zaocode, cropyieldkgperha,
                        NutrPartialIndicatorN, NutrPartialIndicatorP, NutrPartialIndicatorK, SH58))
summary(TZAGSummSAGCOTonly)

#Remove excess data frames
rm(list=ls()[! ls() %in% c("TZAGSummSAGCOTonly")])

#Export data frame as CSV file
write.csv(TZAGSummSAGCOTonly, "LSMS_TZAPartialNutrientIndicator.csv", row.names=FALSE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "LSMS_TZAPartialNutrientIndicator.csv", target_path = "Soil_Health/LSMS_TZAPartialNutrientIndicator.csv")
