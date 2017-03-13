##Calculating yield for each crop per field for the entire year and the nutrient budget for the entire year - using Tanzania LSMS data 2013
#Written: 08/04/15

#Open libraries
library(plyr)
library(openxlsx)
library(doBy)
library(foreign)
library(data.table)

#data sources
dir_data1 <- "C:/Data/Vital Signs/Ag Intensification/Inputs/TZA_2012_LSMS_v01_M_STATA_English_labels"
dir_data2 <- "C:/Data/Vital Signs/Ag Intensification/Inputs"

#loading VS survey data
setwd (dir_data1)
TZAGA<-read.dta("AG_SEC_A.dta")
TZAG2A<-read.dta("AG_SEC_2A.dta")
TZAG2B<-read.dta("AG_SEC_2B.dta")
TZAG3A<-read.dta("AG_SEC_3A.dta")
TZAG3B<-read.dta("AG_SEC_3B.dta")
TZAG4A<-read.dta("AG_SEC_4A.dta")
TZAG4B<-read.dta("AG_SEC_4B.dta")
TZAG5A<-read.dta("AG_SEC_5A.dta")
TZAG5B<-read.dta("AG_SEC_5B.dta")

#loading reference tables
setwd (dir_data2)
NutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=15, startRow = 1,colNames =TRUE)
CropCode<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=16, startRow = 1,colNames =TRUE)
FertNutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=18, startRow = 1,colNames =TRUE)
HarvestIndices<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=19, startRow = 1,colNames =TRUE)

#Turning all blanks into NAs
TZAGA[TZAGA == ""] <- NA
TZAG2A[TZAG2A == ""] <- NA
TZAG2B[TZAG2B == ""] <- NA
TZAG3A[TZAG3A == ""] <- NA
TZAG3B[TZAG3B == ""] <- NA
TZAG4A[TZAG4A == ""] <- NA
TZAG4B[TZAG4B == ""] <- NA
TZAG5A[TZAG5A == ""] <- NA
TZAG5B[TZAG5B == ""] <- NA

#Subsetting for SAGCOT region only
TZAG2ASAGCOT<-merge(TZAG2A, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG2BSAGCOT<-merge(TZAG2B, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG3ASAGCOT<-merge(TZAG3A, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG3BSAGCOT<-merge(TZAG3B, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG4ASAGCOT<-merge(TZAG4A, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG4BSAGCOT<-merge(TZAG4B, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG5ASAGCOT<-merge(TZAG5A, TZAGA, by="y3_hhid", all.x=TRUE)
TZAG5BSAGCOT<-merge(TZAG5B, TZAGA, by="y3_hhid", all.x=TRUE)

TZAG2ASAGCOT$inSAGCOT<-ifelse(TZAG2ASAGCOT$ag_a01_2=='IRINGA' | TZAG2ASAGCOT$ag_a01_2=='MBEYA' | TZAG2ASAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG2ASAGCOT$ag_a01_2=='PWANI' | TZAG2ASAGCOT$ag_a01_2=='RUKWA' | TZAG2ASAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG2BSAGCOT$inSAGCOT<-ifelse(TZAG2BSAGCOT$ag_a01_2=='IRINGA' | TZAG2BSAGCOT$ag_a01_2=='MBEYA' | TZAG2BSAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG2BSAGCOT$ag_a01_2=='PWANI' | TZAG2BSAGCOT$ag_a01_2=='RUKWA' | TZAG2BSAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG3ASAGCOT$inSAGCOT<-ifelse(TZAG3ASAGCOT$ag_a01_2=='IRINGA' | TZAG3ASAGCOT$ag_a01_2=='MBEYA' | TZAG3ASAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG3ASAGCOT$ag_a01_2=='PWANI' | TZAG3ASAGCOT$ag_a01_2=='RUKWA' | TZAG3ASAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG3BSAGCOT$inSAGCOT<-ifelse(TZAG3BSAGCOT$ag_a01_2=='IRINGA' | TZAG3BSAGCOT$ag_a01_2=='MBEYA' | TZAG3BSAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG3BSAGCOT$ag_a01_2=='PWANI' | TZAG3BSAGCOT$ag_a01_2=='RUKWA' | TZAG3BSAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG4ASAGCOT$inSAGCOT<-ifelse(TZAG4ASAGCOT$ag_a01_2=='IRINGA' | TZAG4ASAGCOT$ag_a01_2=='MBEYA' | TZAG4ASAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG4ASAGCOT$ag_a01_2=='PWANI' | TZAG4ASAGCOT$ag_a01_2=='RUKWA' | TZAG4ASAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG4BSAGCOT$inSAGCOT<-ifelse(TZAG4BSAGCOT$ag_a01_2=='IRINGA' | TZAG4BSAGCOT$ag_a01_2=='MBEYA' | TZAG4BSAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG4BSAGCOT$ag_a01_2=='PWANI' | TZAG4BSAGCOT$ag_a01_2=='RUKWA' | TZAG4BSAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG5ASAGCOT$inSAGCOT<-ifelse(TZAG5ASAGCOT$ag_a01_2=='IRINGA' | TZAG5ASAGCOT$ag_a01_2=='MBEYA' | TZAG5ASAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG5ASAGCOT$ag_a01_2=='PWANI' | TZAG5ASAGCOT$ag_a01_2=='RUKWA' | TZAG5ASAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)
TZAG5BSAGCOT$inSAGCOT<-ifelse(TZAG5BSAGCOT$ag_a01_2=='IRINGA' | TZAG5BSAGCOT$ag_a01_2=='MBEYA' | TZAG5BSAGCOT$ag_a01_2=='MOROGORO' | 
                        TZAG5BSAGCOT$ag_a01_2=='PWANI' | TZAG5BSAGCOT$ag_a01_2=='RUKWA' | TZAG5BSAGCOT$ag_a01_2=='DAR ES SALAAM',1,0)

TZAG2A<-subset(TZAG2ASAGCOT,inSAGCOT==1)
TZAG2B<-subset(TZAG2BSAGCOT,inSAGCOT==1)
TZAG3A<-subset(TZAG3ASAGCOT,inSAGCOT==1)
TZAG3B<-subset(TZAG3BSAGCOT,inSAGCOT==1)
TZAG4A<-subset(TZAG4ASAGCOT,inSAGCOT==1)
TZAG4B<-subset(TZAG4BSAGCOT,inSAGCOT==1)
TZAG5A<-subset(TZAG5ASAGCOT,inSAGCOT==1)
TZAG5B<-subset(TZAG5BSAGCOT,inSAGCOT==1)

#Remove extra datasets
rm(TZAG2ASAGCOT,TZAG2BSAGCOT,TZAG3ASAGCOT,TZAG3BSAGCOT,TZAG4ASAGCOT,TZAG4BSAGCOT,TZAG5ASAGCOT,TZAG5BSAGCOT)

#Join sections 2a and 4a of the ag LSMS data using hhid and plotnum, keep only matched obs (this is to determine the area cultivated)
TZAG2A$y3hhid_plotnum<-ifelse(is.na(TZAG2A$y3_hhid)|is.na(TZAG2A$plotnum),NA,(do.call("paste", c(TZAG2A[c('y3_hhid','plotnum')],sep="_"))))
TZAG4A$y3hhid_plotnum<-ifelse(is.na(TZAG4A$y3_hhid)|is.na(TZAG4A$plotnum),NA,(do.call("paste", c(TZAG4A[c('y3_hhid','plotnum')],sep="_"))))
TZAG4A$y3hhid_plotnum_crop<-ifelse(is.na(TZAG4A$zaocode)|is.na(TZAG4A$y3hhid_plotnum),NA,(do.call("paste", c(TZAG4A[c('y3hhid_plotnum','zaocode')],sep="_"))))
TZAG4A$y3hhid_plotnum_crop_season<-ifelse(is.na(TZAG4A$y3hhid_plotnum),NA,(do.call("paste", c(TZAG4A[c('y3hhid_plotnum','zaocode')],"LRS",sep="_"))))

#Dropping all obs that have NA for hhid and/or plotnum 
TZAG2Atrim<-subset(TZAG2A, !is.na(y3hhid_plotnum))
TZAG4Atrim<-subset(TZAG4A, !is.na(y3hhid_plotnum))
TZAG2A4A<-merge(TZAG4Atrim, TZAG2Atrim, by="y3hhid_plotnum", all.x=TRUE)

#Recoding plot numbers in 2b, were miscoded in LSMS
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

#Join sections 2b and 4b of the ag LSMS data using hhid and plotnum, keep only matched obs (this is to determine the area cultivated)
TZAG2B$y3hhid_plotnum<-ifelse(is.na(TZAG2B$y3_hhid)|is.na(TZAG2B$plotnum),NA,(do.call("paste", c(TZAG2B[c('y3_hhid','plotnum')],sep="_"))))
TZAG4B$y3hhid_plotnum<-ifelse(is.na(TZAG4B$y3_hhid)|is.na(TZAG4B$plotnum),NA,(do.call("paste", c(TZAG4B[c('y3_hhid','plotnum')],sep="_"))))
TZAG4B$y3hhid_plotnum_crop<-ifelse(is.na(TZAG4B$zaocode)|is.na(TZAG4B$y3hhid_plotnum),NA,(do.call("paste", c(TZAG4B[c('y3hhid_plotnum','zaocode')],sep="_"))))
TZAG4B$y3hhid_plotnum_crop_season<-ifelse(is.na(TZAG4B$y3hhid_plotnum),NA,(do.call("paste", c(TZAG4B[c('y3hhid_plotnum','zaocode')],"SRS",sep="_"))))

#Dropping all obs that have NA for hhid and/or plotnum 
TZAG2Btrim<-subset(TZAG2B, !is.na(y3hhid_plotnum))
TZAG4Btrim<-subset(TZAG4B, !is.na(y3hhid_plotnum))
TZAG2B4B<-merge(TZAG4Btrim, TZAG2Btrim, by="y3hhid_plotnum", all.x=TRUE)

#Subsetting data to calculate area cultivated A=LRS B=SRS
TZAG2A4Atrim<-subset(TZAG2A4A, select=c(y3hhid_plotnum:zaocode,ag2a_04,ag2a_09,ag4a_01,ag4a_02,ag4a_19,ag4a_20,ag4a_28,y3hhid_plotnum_crop,y3hhid_plotnum_crop_season))
TZAG2B4Btrim<-subset(TZAG2B4B, select=c(y3hhid_plotnum:zaocode,ag2b_15,ag2b_20,ag4b_01,ag4b_02,ag4b_19,ag4b_20,ag4b_28,y3hhid_plotnum_crop,y3hhid_plotnum_crop_season))
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='y3_hhid.x']<-'y3_hhid'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='y3_hhid.x']<-'y3_hhid'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='plotnum.x']<-'plotnum'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='plotnum.x']<-'plotnum'

#Creating a new dataframe with plot area, derived from GPS measurements and farmer's estimates across seasons
TZAG2Aarea<-subset(TZAG2Atrim, select=c(y3hhid_plotnum, ag2a_04, ag2a_09))
TZAG2Barea<-subset(TZAG2Btrim, select=c(y3hhid_plotnum, ag2b_15, ag2b_20))
TZAG2ABarea<-merge(TZAG2Aarea, TZAG2Barea, by="y3hhid_plotnum", all=TRUE)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$ag2a_09), TZAG2ABarea$ag2b_20, TZAG2ABarea$ag2a_09)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$areaLRS), TZAG2ABarea$ag2a_04, TZAG2ABarea$areaLRS)
TZAG2ABarea$areaLRS<-ifelse(is.na(TZAG2ABarea$areaLRS), TZAG2ABarea$ag2b_15, TZAG2ABarea$areaLRS)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$ag2b_20), TZAG2ABarea$ag2a_09, TZAG2ABarea$ag2b_20)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$areaSRS), TZAG2ABarea$ag2b_15, TZAG2ABarea$areaSRS)
TZAG2ABarea$areaSRS<-ifelse(is.na(TZAG2ABarea$areaSRS), TZAG2ABarea$ag2a_04, TZAG2ABarea$areaSRS)

#Merging new areas with existing dataset - LRS
TZAG2A4Atrim<-merge(TZAG2A4Atrim, TZAG2ABarea, by="y3hhid_plotnum", all.x=TRUE)
TZAG2A4Atrim$ag2a_04.y<-NULL
TZAG2A4Atrim$ag2a_09.y<-NULL
TZAG2A4Atrim$ag2b_15<-NULL
TZAG2A4Atrim$ag2b_20<-NULL
TZAG2A4Atrim$areaSRS<-NULL
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag2a_04.x']<-'ag2a_04'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag2a_09.x']<-'ag2a_09'

#Data staging to determine area planted for long rainy season (A)
TZAG2A4Atrim$area2<-ifelse(TZAG2A4Atrim$ag4a_01=="NO" & TZAG2A4Atrim$ag4a_02=="1/4",TZAG2A4Atrim$areaLRS * 0.25,
                           ifelse(TZAG2A4Atrim$ag4a_01=="NO" & TZAG2A4Atrim$ag4a_02=="1/2",TZAG2A4Atrim$areaLRS * 0.5,
                                  ifelse(TZAG2A4Atrim$ag4a_01=="NO" & TZAG2A4Atrim$ag4a_02=="3/4",TZAG2A4Atrim$areaLRS * 0.75,
                                         ifelse(TZAG2A4Atrim$ag4a_01=="YES",TZAG2A4Atrim$areaLRS,NA))))
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='area2']<-'area_planted_percropacre'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag4a_28']<-'quantcropharvestedkg'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag2a_04']<-'fieldareaestimatebyfarmer'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag2a_09']<-'totfieldareabyGPS'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag4a_01']<-'waswholefieldplanted'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag4a_02']<-'whichportionoffieldplanted'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag4a_19']<-'wereanycropsharvested'
names(TZAG2A4Atrim)[names(TZAG2A4Atrim)=='ag4a_20']<-'whynocropsharvested'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area in which it was planted) in LRS
TZAG2A4Atrim$cropyieldkgperacre<-TZAG2A4Atrim$quantcropharvestedkg/TZAG2A4Atrim$area_planted_percropacre
TZAG2A4Atrim$cropyieldkgperha<-(TZAG2A4Atrim$quantcropharvestedkg/TZAG2A4Atrim$area_planted_percropacre)*2.47105

#Merging new areas with existing dataset
TZAG2B4Btrim<-merge(TZAG2B4Btrim, TZAG2ABarea, by="y3hhid_plotnum", all.x=TRUE)
TZAG2B4Btrim$ag2b_15.y<-NULL
TZAG2B4Btrim$ag2b_20.y<-NULL
TZAG2B4Btrim$ag2a_04<-NULL
TZAG2B4Btrim$ag2a_09<-NULL
TZAG2B4Btrim$areaLRS<-NULL
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag2b_15.x']<-'ag2b_15'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag2b_20.x']<-'ag2b_20'

#Data staging to determine area planted for short rainy season (B)
TZAG2B4Btrim$area2<-ifelse(TZAG2B4Btrim$ag4b_01=="NO" & TZAG2B4Btrim$ag4b_02=="1/4",TZAG2B4Btrim$areaSRS * 0.25,
                           ifelse(TZAG2B4Btrim$ag4b_01=="NO" & TZAG2B4Btrim$ag4b_02=="1/2",TZAG2B4Btrim$areaSRS * 0.5,
                                  ifelse(TZAG2B4Btrim$ag4b_01=="NO" & TZAG2B4Btrim$ag4b_02=="3/4",TZAG2B4Btrim$areaSRS * 0.75,
                                         ifelse(TZAG2B4Btrim$ag4b_01=="YES",TZAG2B4Btrim$areaSRS,NA))))
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='area2']<-'area_planted_percropacre'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag4b_28']<-'quantcropharvestedkg'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag2b_15']<-'fieldareaestimatebyfarmer'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag2b_20']<-'totfieldareabyGPS'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag4b_01']<-'waswholefieldplanted'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag4b_02']<-'whichportionoffieldplanted'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag4b_19']<-'wereanycropsharvested'
names(TZAG2B4Btrim)[names(TZAG2B4Btrim)=='ag4b_20']<-'whynocropsharvested'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area in which it was planted) in SRS
TZAG2B4Btrim$cropyieldkgperacre<-TZAG2B4Btrim$quantcropharvestedkg/TZAG2B4Btrim$area_planted_percropacre
TZAG2B4Btrim$cropyieldkgperha<-(TZAG2B4Btrim$quantcropharvestedkg/TZAG2B4Btrim$area_planted_percropacre)*2.47105

#Adding the function of each field from 3A and 3B
keep3a<-c("y3_hhid", "plotnum", "ag3a_03", "ag3a_18", "ag3a_19", "ag3a_20", "ag3a_21")
TZAG3Atrim<-TZAG3A[keep3a]
TZAG3Atrim$y3hhid_plotnum<-ifelse(is.na(TZAG3Atrim$y3_hhid)|is.na(TZAG3Atrim$plotnum),NA,(do.call("paste", c(TZAG3Atrim[c('y3_hhid','plotnum')],sep="_"))))
TZAG2A4Atrim2<-merge(TZAG2A4Atrim,TZAG3Atrim, by="y3hhid_plotnum", all.x = TRUE)
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='ag3a_03']<-'use'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='ag3a_18']<-'anyirrigation'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='ag3a_19']<-'irrigationtype'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='ag3a_20']<-'obtainwatermethod'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='ag3a_21']<-'watersource'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='y3_hhid.x']<-'y3_hhid'
names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='plotnum.x']<-'plotnum'
TZAG2A4Atrim2$y3_hhid.y<-NULL
TZAG2A4Atrim2$plotnum.y<-NULL

keep3b<-c("y3_hhid", "plotnum", "ag3b_03", "ag3b_18", "ag3b_19", "ag3b_20", "ag3b_21")
TZAG3Btrim<-TZAG3B[keep3b]
TZAG3Btrim$y3hhid_plotnum<-ifelse(is.na(TZAG3Btrim$y3_hhid)|is.na(TZAG3Btrim$plotnum),NA,(do.call("paste", c(TZAG3Btrim[c('y3_hhid','plotnum')],sep="_"))))
TZAG2B4Btrim2<-merge(TZAG2B4Btrim,TZAG3Btrim, by="y3hhid_plotnum", all.x = TRUE)
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='ag3b_03']<-'use'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='ag3b_18']<-'anyirrigation'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='ag3b_19']<-'irrigationtype'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='ag3b_20']<-'obtainwatermethod'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='ag3b_21']<-'watersource'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='y3_hhid.x']<-'y3_hhid'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='plotnum.x']<-'plotnum'
TZAG2B4Btrim2$y3_hhid.y<-NULL
TZAG2B4Btrim2$plotnum.y<-NULL

names(TZAG2A4Atrim2)[names(TZAG2A4Atrim2)=='areaLRS']<-'totplotareaGPSest'
names(TZAG2B4Btrim2)[names(TZAG2B4Btrim2)=='areaSRS']<-'totplotareaGPSest'

#Bringing the short rainy season and long rainy season back together (A=LRS, B=SRS) adding all observations together
TZAG2A4Atrim2$season<-"LRS"
TZAG2B4Btrim2$season<-"SRS"
TZAG2AB4AB<-rbind(TZAG2A4Atrim2,TZAG2B4Btrim2)
TZAG2AB4AB$anyirrigation<-ifelse(TZAG2AB4AB$anyirrigation==1,1,TZAG2AB4AB$anyirrigation)
TZAG2AB4AB$anyirrigation<-ifelse(TZAG2AB4AB$anyirrigation==2,0,TZAG2AB4AB$anyirrigation)
TZAG2AB4AB$y3hhid_plotnum_season<-ifelse(is.na(TZAG2AB4AB$y3hhid_plotnum)|is.na(TZAG2AB4AB$season),NA,(do.call("paste", c(TZAG2AB4AB[c('y3hhid_plotnum','season')],sep="_"))))

#DROPPING ALL YIELD OBSERVATIONS GREATER THAN 15,000 KGS PER ACRE
TZAG2AB4AB<-subset(TZAG2AB4AB, TZAG2AB4AB$cropyieldkgperha<15000)

#Removing extra data frames
rm(TZAG2A4Atrim, TZAG2A4Atrim2, TZAG2B4Btrim, TZAG2B4Btrim2, TZAG3Atrim, TZAG3Btrim, TZAG2A4A, TZAG2B4B,
   TZAG2Aarea,TZAG2ABarea, TZAG2Barea,TZAG2Atrim,TZAG2Btrim, TZAG4Atrim,TZAG4Btrim)

#Making sure crop codes are clean - LSMS CLEANING STEP
table(TZAG2AB4AB$zaocode)
names(TZAG2AB4AB)[names(TZAG2AB4AB)=='zaocode']<-'Crop'

#Making crop codes fit with external data table with nutrient composition
CropCode$Crop<-toupper(CropCode$Crop)
TZAG2AB4AB$Crop<-ifelse(TZAG2AB4AB$Crop=="CHICK PEAS", "CHICKPEAS",
                          ifelse(TZAG2AB4AB$Crop=="AMARANTHS", "AMARANTH",
                                 ifelse(TZAG2AB4AB$Crop=="WATER MELLON", "WATERMELON",
                                        ifelse(TZAG2AB4AB$Crop=="EGG PLANT","EGGPLANT",
                                               ifelse(TZAG2AB4AB$Crop=="FIWI","KIWI",
                                                      ifelse(TZAG2AB4AB$Crop=="PUMPKINS","PUMPKIN",as.character(TZAG2AB4AB$Crop)))))))
table(TZAG2AB4AB$Crop)
table(CropCode$Crop)
TZAGNutrComp<-merge(TZAG2AB4AB,CropCode,by="Crop", all.x=TRUE)
rm(TZAG2AB4AB)

#Merging with nutrient composition table
names(NutrComp)[names(NutrComp)=='Crop']<-'CropGroup'
NutrComp$Cropresidue_C<-0.47
NutrComp$Harvested.product_C<-0.47
TZAGNutrComp2<-merge(TZAGNutrComp,NutrComp,by="CropGroup", all.x=TRUE)

#Calculating the C, N, P, K removal in kg for each crop for each field
TZAGNutrComp2$Nrmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_N
TZAGNutrComp2$Prmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_P
TZAGNutrComp2$Krmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_K
TZAGNutrComp2$Crmvdkg<-TZAGNutrComp2$quantcropharvestedkg*0.001*TZAGNutrComp2$Harvested.product_C

#Fertilizer nutrient composition
TZAG3Afert<-subset(TZAG3A,select=c(y3_hhid:plotnum, ag3a_40, ag3a_41,ag3a_42, ag3a_47:ag3a_49,ag3a_54:ag3a_56))
TZAG3Afert$season<-"LRS"
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_40']<-'cultivated'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_41']<-'usedorgfert'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_42']<-'quantorgfert'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_47']<-'usedinorgfertA'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_48']<-'typeinorgfertA'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_49']<-'quantinorgfertA'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_54']<-'usedinorgfertB'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_55']<-'typeinorgfertB'
names(TZAG3Afert)[names(TZAG3Afert)=='ag3a_56']<-'quantinorgfertB'

TZAG3Bfert<-subset(TZAG3B,select=c(y3_hhid:plotnum, ag3b_40, ag3b_41,ag3b_42, ag3b_47:ag3b_49,ag3b_54:ag3b_56))
TZAG3Bfert$season<-"SRS"
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_40']<-'cultivated'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_41']<-'usedorgfert'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_42']<-'quantorgfert'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_47']<-'usedinorgfertA'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_48']<-'typeinorgfertA'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_49']<-'quantinorgfertA'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_54']<-'usedinorgfertB'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_55']<-'typeinorgfertB'
names(TZAG3Bfert)[names(TZAG3Bfert)=='ag3b_56']<-'quantinorgfertB'
TZAG3ABfert<-rbind(TZAG3Afert,TZAG3Bfert)
TZAG3ABfert$quantorgfert<-ifelse(is.na(TZAG3ABfert$quantorgfert),0,TZAG3ABfert$quantorgfert)
TZAG3ABfert$quantinorgfertA<-ifelse(is.na(TZAG3ABfert$quantinorgfertA),0,TZAG3ABfert$quantinorgfertA)
TZAG3ABfert$quantinorgfertB<-ifelse(is.na(TZAG3ABfert$quantinorgfertB),0,TZAG3ABfert$quantinorgfertB)

#Merging datasets to combine yield and fertilizer info
TZAG3ABfert$y3hhid_plotnum_season<-ifelse(is.na(TZAG3ABfert$y3_hhid)|is.na(TZAG3ABfert$plotnum)|is.na(TZAG3ABfert$season),NA,(do.call("paste", c(TZAG3ABfert[c('y3_hhid','plotnum','season')],sep="_"))))
TZAGFertNutrComp<-merge(TZAGNutrComp2,TZAG3ABfert, by="y3hhid_plotnum_season", all.x = TRUE)
TZAGFertNutrComp$y3_hhid.y<-NULL
TZAGFertNutrComp$plotnum.y<-NULL
TZAGFertNutrComp$season.y<-NULL
names(TZAGFertNutrComp)[names(TZAGFertNutrComp)=='y3_hhid.x']<-'y3_hhid'
names(TZAGFertNutrComp)[names(TZAGFertNutrComp)=='plotnum.x']<-'plotnum'
names(TZAGFertNutrComp)[names(TZAGFertNutrComp)=='season.x']<-'season'
rm(TZAG3ABfert, TZAG3Afert, TZAG3Bfert)

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

#Merge with nutrient table for first batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='INPUTS']<-'INPUTSA'
FertNutrComp$C<-ifelse(FertNutrComp$INPUTSA=="ANIMAL MANURE",0.47,0)
TZAGFertNutrComp2<-merge(TZAGFertNutrComp, FertNutrComp, by="INPUTSA", all.x=TRUE)

#Merge with nutrient table for second batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='INPUTSA']<-'INPUTSB'
TZAGFertNutrComp3<-merge(TZAGFertNutrComp2, FertNutrComp, by="INPUTSB", all.x=TRUE)

#Getting nutrients added (N, P, K, C) for inorganic fertilizer 1
TZAGFertNutrComp3$NaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$N.x
TZAGFertNutrComp3$PaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$P.x
TZAGFertNutrComp3$KaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$K.x
TZAGFertNutrComp3$CaddedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$C.x
TZAGFertNutrComp3$P205addedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$P205.x
TZAGFertNutrComp3$PaddedkgInorgA<-TZAGFertNutrComp3$PaddedkgInorgA+TZAGFertNutrComp3$P205addedkgInorgA*0.4364
TZAGFertNutrComp3$K20addedkgInorgA<-TZAGFertNutrComp3$quantinorgfertA*TZAGFertNutrComp3$K20.x
TZAGFertNutrComp3$KaddedkgInorgA<-TZAGFertNutrComp3$KaddedkgInorgA+TZAGFertNutrComp3$K20addedkgInorgA*1.2046

#Making quantity of nutrients added 0 (instead of NA) for plots with no inorganic fertilizer 1 added
TZAGFertNutrComp3$NaddedkgInorgA<-ifelse(is.na(TZAGFertNutrComp3$NaddedkgInorgA),0,TZAGFertNutrComp3$NaddedkgInorgA)
TZAGFertNutrComp3$PaddedkgInorgA<-ifelse(is.na(TZAGFertNutrComp3$PaddedkgInorgA),0,TZAGFertNutrComp3$PaddedkgInorgA)
TZAGFertNutrComp3$KaddedkgInorgA<-ifelse(is.na(TZAGFertNutrComp3$KaddedkgInorgA),0,TZAGFertNutrComp3$KaddedkgInorgA)
TZAGFertNutrComp3$CaddedkgInorgA<-ifelse(is.na(TZAGFertNutrComp3$CaddedkgInorgA),0,TZAGFertNutrComp3$CaddedkgInorgA)

#Getting nutrients added (N, P, K, C) for inorganic fertilizer 2
TZAGFertNutrComp3$NaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$N.y
TZAGFertNutrComp3$PaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$P.y
TZAGFertNutrComp3$KaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$K.y
TZAGFertNutrComp3$CaddedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$C.y
TZAGFertNutrComp3$P205addedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$P205.y
TZAGFertNutrComp3$PaddedkgInorgB<-TZAGFertNutrComp3$PaddedkgInorgB+TZAGFertNutrComp3$P205addedkgInorgB*0.4364
TZAGFertNutrComp3$K20addedkgInorgB<-TZAGFertNutrComp3$quantinorgfertB*TZAGFertNutrComp3$K20.y
TZAGFertNutrComp3$KaddedkgInorgB<-TZAGFertNutrComp3$KaddedkgInorgB+TZAGFertNutrComp3$K20addedkgInorgB*1.2046

#Making quantity of nutrients added 0 (instead of NA) for plots with no inorganic fertilizer 2 added
TZAGFertNutrComp3$NaddedkgInorgB<-ifelse(is.na(TZAGFertNutrComp3$NaddedkgInorgB),0,TZAGFertNutrComp3$NaddedkgInorgB)
TZAGFertNutrComp3$PaddedkgInorgB<-ifelse(is.na(TZAGFertNutrComp3$PaddedkgInorgB),0,TZAGFertNutrComp3$PaddedkgInorgB)
TZAGFertNutrComp3$KaddedkgInorgB<-ifelse(is.na(TZAGFertNutrComp3$KaddedkgInorgB),0,TZAGFertNutrComp3$KaddedkgInorgB)
TZAGFertNutrComp3$CaddedkgInorgB<-ifelse(is.na(TZAGFertNutrComp3$CaddedkgInorgB),0,TZAGFertNutrComp3$CaddedkgInorgB)

#Summing nutrient addition per crop per field for inorganic fertilizers
TZAGFertNutrComp3$TotNaddedkgInorg<-TZAGFertNutrComp3$NaddedkgInorgA+TZAGFertNutrComp3$NaddedkgInorgB
TZAGFertNutrComp3$TotPaddedkgInorg<-TZAGFertNutrComp3$PaddedkgInorgA+TZAGFertNutrComp3$PaddedkgInorgB
TZAGFertNutrComp3$TotKaddedkgInorg<-TZAGFertNutrComp3$KaddedkgInorgA+TZAGFertNutrComp3$KaddedkgInorgB
TZAGFertNutrComp3$TotCaddedkgInorg<-TZAGFertNutrComp3$CaddedkgInorgA+TZAGFertNutrComp3$CaddedkgInorgB

#Finding total nutrients by all inorganic fertilizers added in kg/ha
TZAGFertNutrComp3$TotNaddedkghaInorg<-TZAGFertNutrComp3$TotNaddedkgInorg/TZAGFertNutrComp3$area_planted_percropacre*2.47105
TZAGFertNutrComp3$TotPaddedkghaInorg<-TZAGFertNutrComp3$TotPaddedkgInorg/TZAGFertNutrComp3$area_planted_percropacre*2.47105
TZAGFertNutrComp3$TotKaddedkghaInorg<-TZAGFertNutrComp3$TotKaddedkgInorg/TZAGFertNutrComp3$area_planted_percropacre*2.47105
TZAGFertNutrComp3$TotCaddedkghaInorg<-TZAGFertNutrComp3$TotCaddedkgInorg/TZAGFertNutrComp3$area_planted_percropacre*2.47105

#Merging with table with nutrition content of organic fertilizers
TZAGFertNutrComp4<-subset(TZAGFertNutrComp3, select=c(INPUTSB:quantinorgfertB,NaddedkgInorgA:TotCaddedkghaInorg))
TZAGFertNutrComp4$INPUTSC<-ifelse(TZAGFertNutrComp4$usedorgfert=="YES","ORGANIC FERTILIZER",0)
names(FertNutrComp)[names(FertNutrComp)=='INPUTSB']<-'INPUTSC'
FertNutrComp$INPUTSC<-ifelse(FertNutrComp$INPUTSC=="ANIMAL MANURE","ORGANIC FERTILIZER",FertNutrComp$INPUTSC)
TZAGFertNutrComp5<-merge(TZAGFertNutrComp4,FertNutrComp, by="INPUTSC", all.x=TRUE)
rm(TZAGNutrComp2,TZAGFertNutrComp, TZAGFertNutrComp2, TZAGFertNutrComp3, TZAGFertNutrComp4, TZAGNutrComp)

#Getting nutrient addition (N, P, K, C) for organic fertilizer in kg
TZAGFertNutrComp5$NaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$N
TZAGFertNutrComp5$PaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$P
TZAGFertNutrComp5$KaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$K
TZAGFertNutrComp5$CaddedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$C
TZAGFertNutrComp5$P205addedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$P205
TZAGFertNutrComp5$PaddedkgOrg<-TZAGFertNutrComp5$PaddedkgOrg + TZAGFertNutrComp5$P205addedkgOrg*0.4364
TZAGFertNutrComp5$K20addedkgOrg<-TZAGFertNutrComp5$quantorgfert*TZAGFertNutrComp5$K20
TZAGFertNutrComp5$KaddedkgOrg<-TZAGFertNutrComp5$KaddedkgOrg + TZAGFertNutrComp5$K20addedkgOrg*1.2046

#Changing quantity of nutrients added from organic fertilizer to 0 if no organic fertilizer was added (instead of NA)
TZAGFertNutrComp5$NaddedkgOrg<-ifelse(is.na(TZAGFertNutrComp5$NaddedkgOrg),0,TZAGFertNutrComp5$NaddedkgOrg)
TZAGFertNutrComp5$PaddedkgOrg<-ifelse(is.na(TZAGFertNutrComp5$PaddedkgOrg),0,TZAGFertNutrComp5$PaddedkgOrg)
TZAGFertNutrComp5$KaddedkgOrg<-ifelse(is.na(TZAGFertNutrComp5$KaddedkgOrg),0,TZAGFertNutrComp5$KaddedkgOrg)
TZAGFertNutrComp5$CaddedkgOrg<-ifelse(is.na(TZAGFertNutrComp5$CaddedkgOrg),0,TZAGFertNutrComp5$CaddedkgOrg)

#Getting nutrients added from organic fertilizer in kg/ha
TZAGFertNutrComp5$NaddedkghaOrg<-TZAGFertNutrComp5$NaddedkgOrg/TZAGFertNutrComp5$area_planted_percropacre*2.47105
TZAGFertNutrComp5$PaddedkghaOrg<-TZAGFertNutrComp5$PaddedkgOrg/TZAGFertNutrComp5$area_planted_percropacre*2.47105
TZAGFertNutrComp5$KaddedkghaOrg<-TZAGFertNutrComp5$KaddedkgOrg/TZAGFertNutrComp5$area_planted_percropacre*2.47105
TZAGFertNutrComp5$CaddedkghaOrg<-TZAGFertNutrComp5$CaddedkgOrg/TZAGFertNutrComp5$area_planted_percropacre*2.47105

#Summing total nutrients added from inorganic and organic fertilizers in kg
TZAGFertNutrComp5$TotNaddedkgAllFert<-TZAGFertNutrComp5$TotNaddedkgInorg+TZAGFertNutrComp5$NaddedkgOrg
TZAGFertNutrComp5$TotPaddedkgAllFert<-TZAGFertNutrComp5$TotPaddedkgInorg+TZAGFertNutrComp5$PaddedkgOrg
TZAGFertNutrComp5$TotKaddedkgAllFert<-TZAGFertNutrComp5$TotKaddedkgInorg+TZAGFertNutrComp5$KaddedkgOrg
TZAGFertNutrComp5$TotCaddedkgAllFert<-TZAGFertNutrComp5$TotCaddedkgInorg+TZAGFertNutrComp5$CaddedkgOrg
sum(is.na(TZAGFertNutrComp5$TotNaddedkgAllFert))

#Subsetting dataset to get ready for crop residue nutrient additions
TZAGCropRes<-subset(TZAGFertNutrComp5, select=c(y3hhid_plotnum_season:CropResidueGroup, Cropresidue_N:Cropresidue_K, 
                                                Harvested.product_C, Cropresidue_C, Nrmvdkg:Crmvdkg,
                                                usedorgfert:quantinorgfertB,TotNaddedkgInorg:TotCaddedkghaInorg,
                                                NaddedkgOrg:CaddedkghaOrg,TotNaddedkgAllFert:TotCaddedkgAllFert))
names(TZAGCropRes)[names(TZAGCropRes)=='y3_hhid.x']<-'y3_hhid'
names(TZAGCropRes)[names(TZAGCropRes)=='plotnum.x']<-'plotnum'
names(TZAGCropRes)[names(TZAGCropRes)=='season.x']<-'season'
TZAGCropRes$y3hhid_crop_season<-ifelse(is.na(TZAGCropRes$y3_hhid) | is.na(TZAGCropRes$zaocode), NA, 
                                (do.call("paste", c(TZAGCropRes[c('y3_hhid','Crop','season')],sep="_"))))

#Looking up crop residue use
TZAG5ACropRes<-subset(TZAG5A, select=c(y3_hhid, zaocode, ag5a_33_1, ag5a_33_2))
TZAG5BCropRes<-subset(TZAG5B, select=c(y3_hhid, zaocode, ag5b_33_1, ag5b_33_2))
names(TZAG5ACropRes)[names(TZAG5ACropRes)=='ag5a_33_1']<-'cropresuse1'
names(TZAG5BCropRes)[names(TZAG5BCropRes)=='ag5b_33_1']<-'cropresuse1'
names(TZAG5ACropRes)[names(TZAG5ACropRes)=='ag5a_33_2']<-'cropresuse2'
names(TZAG5BCropRes)[names(TZAG5BCropRes)=='ag5b_33_2']<-'cropresuse2'
TZAG5ACropRes$season<-"LRS"
TZAG5BCropRes$season<-"SRS"
TZAG5AB<-rbind(TZAG5ACropRes,TZAG5BCropRes)
TZAG5ABtrim<-subset(TZAG5AB, !is.na(TZAG5AB$zaocode), select=c(y3_hhid:season))
rm(TZAG5ACropRes, TZAG5BCropRes, TZAG5AB)

#Merging crop residue use information with yield and input information
TZAG5ABtrim$y3hhid_crop_season<-ifelse(is.na(TZAG5ABtrim$y3_hhid)|is.na(TZAG5ABtrim$zaocode), NA, 
                                  (do.call("paste", c(TZAG5ABtrim[c('y3_hhid','zaocode','season')],sep="_"))))
TZAGCropRes2<-merge(TZAGCropRes, TZAG5ABtrim, by="y3hhid_crop_season", all.x=TRUE)
TZAGCropRes2$y3_hhid.y<-NULL
TZAGCropRes2$season.y<-NULL
TZAGCropRes2$zaocode.y<-NULL
names(TZAGCropRes2)[names(TZAGCropRes2)=='y3_hhid.x']<-'y3_hhid'
names(TZAGCropRes2)[names(TZAGCropRes2)=='season.x']<-'season'
names(TZAGCropRes2)[names(TZAGCropRes2)=='zaocode.x']<-'zaocode'

#Finding the amount of residue produced per kg of crop harvested (according to its residue group)
HarvestIndices$CropResFactor<-(1-HarvestIndices$Harvest.Index)/HarvestIndices$Harvest.Index
TZAGCropRes3<-merge(TZAGCropRes2, HarvestIndices, by="CropResidueGroup", all.x = TRUE)
TZAGCropRes3$kgscropresidue<-TZAGCropRes3$quantcropharvestedkg*TZAGCropRes3$CropResFactor

#Finding the NPK available in every tonne of residue - in kg
#Here we are assuming that 100% of the nutrients of the crop residue left in the field are being added to the soil
TZAGCropRes3$cropresuse2<-ifelse(!is.na(TZAGCropRes3$cropresuse1) & is.na(TZAGCropRes3$cropresuse2),"NONE", as.character(TZAGCropRes3$cropresuse2))
TZAGCropRes3$NaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_N,0)
TZAGCropRes3$PaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_P,0)
TZAGCropRes3$KaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_K,0)
TZAGCropRes3$CaddedkgCropres<-ifelse(TZAGCropRes3$cropresuse1=="RESIDUE WAS LEFT IN FIELD" | TZAGCropRes3$cropresuse2=="RESIDUE WAS LEFT IN FIELD",TZAGCropRes3$kgscropresidue*0.001*TZAGCropRes3$Cropresidue_C,0)

#Finding the NPK available in every tonne of residue - in kg/ha
TZAGCropRes3$NaddedkghaCropres<-TZAGCropRes3$NaddedkgCropres/TZAGCropRes3$area_planted_percropacre * 2.47105
TZAGCropRes3$PaddedkghaCropres<-TZAGCropRes3$PaddedkgCropres/TZAGCropRes3$area_planted_percropacre * 2.47105
TZAGCropRes3$KaddedkghaCropres<-TZAGCropRes3$KaddedkgCropres/TZAGCropRes3$area_planted_percropacre * 2.47105
TZAGCropRes3$CaddedkghaCropres<-TZAGCropRes3$CaddedkgCropres/TZAGCropRes3$area_planted_percropacre * 2.47105

#Calculating nutrient budgets in kg
TZAGCropRes3$NutrBudgNkg<-TZAGCropRes3$TotNaddedkgAllFert + TZAGCropRes3$NaddedkgCropres - TZAGCropRes3$Nrmvdkg
TZAGCropRes3$NutrBudgPkg<-TZAGCropRes3$TotPaddedkgAllFert + TZAGCropRes3$PaddedkgCropres - TZAGCropRes3$Prmvdkg
TZAGCropRes3$NutrBudgKkg<-TZAGCropRes3$TotKaddedkgAllFert + TZAGCropRes3$KaddedkgCropres - TZAGCropRes3$Krmvdkg
TZAGCropRes3$NutrBudgCkg<-TZAGCropRes3$TotCaddedkgAllFert + TZAGCropRes3$CaddedkgCropres - TZAGCropRes3$Crmvdkg

#Calculating nutrient budgets in kg/ha
TZAGCropRes3$NutrBudgNkgha<-(TZAGCropRes3$NutrBudgNkg/TZAGCropRes3$area_planted_percropacre)*2.47105
TZAGCropRes3$NutrBudgPkgha<-(TZAGCropRes3$NutrBudgPkg/TZAGCropRes3$area_planted_percropacre)*2.47105
TZAGCropRes3$NutrBudgKkgha<-(TZAGCropRes3$NutrBudgKkg/TZAGCropRes3$area_planted_percropacre)*2.47105
TZAGCropRes3$NutrBudgCkgha<-(TZAGCropRes3$NutrBudgCkg/TZAGCropRes3$area_planted_percropacre)*2.47105

#Calculating the C, N, P, K removal in kg/ha for each crop for each field
attach(TZAGCropRes3)
TZAGCropRes3$Nremovedkgperha<-(Nrmvdkg/area_planted_percropacre)*2.47105
TZAGCropRes3$Premovedkgperha<-(Prmvdkg/area_planted_percropacre)*2.47105
TZAGCropRes3$Kremovedkgperha<-(Krmvdkg/area_planted_percropacre)*2.47105
TZAGCropRes3$Cremovedkgperha<-(Crmvdkg/area_planted_percropacre)*2.47105
detach(TZAGCropRes3)

#Getting NPKC added in kgs/ha 
attach(TZAGCropRes3)
TZAGCropRes3$NAddedAllSrcskgha<-(TotNaddedkgAllFert + NaddedkgCropres)/area_planted_percropacre*2.47105
TZAGCropRes3$PAddedAllSrcskgha<-(TotPaddedkgAllFert + PaddedkgCropres)/area_planted_percropacre*2.47105
TZAGCropRes3$KAddedAllSrcskgha<-(TotKaddedkgAllFert + KaddedkgCropres)/area_planted_percropacre*2.47105
TZAGCropRes3$CAddedAllSrcskgha<-(TotCaddedkgAllFert + CaddedkgCropres)/area_planted_percropacre*2.47105
detach(TZAGCropRes3)
rm(TZAGCropRes,TZAGCropRes2,TZAG5ABtrim, TZAGFertNutrComp5)

#Creating abbreviated data frame
TZAGSummAll<-subset(TZAGCropRes3, !is.na(TZAGCropRes3$cropyieldkgperacre) & !is.na(TZAGCropRes3$zaocode) & 
                    !is.na(TZAGCropRes3$cropresuse1), 
                  select=c(zaocode,y3_hhid,plotnum,y3hhid_plotnum,quantcropharvestedkg,totplotareaGPSest,area_planted_percropacre,
                  cropyieldkgperacre, cropyieldkgperha,use,season,anyirrigation:watersource,Nrmvdkg:Crmvdkg,usedorgfert:quantinorgfertB,TotNaddedkgInorg:CaddedkghaOrg,cropresuse1,
                  cropresuse2,CropResFactor,NaddedkgCropres:NutrBudgCkgha,Nremovedkgperha:CAddedAllSrcskgha))
summary(TZAGSummAll)

#Creating partial nutrient budget indicator for N
TZAGSummAll$NutrPartialIndicatorN<-888
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<(-50) | TZAGSummAll$NutrBudgNkgha>50, 0,TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<20 & TZAGSummAll$NutrBudgNkgha>(-20), 0.333,TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha>(-50) & TZAGSummAll$NutrBudgNkgha<(-20), (1/90) * TZAGSummAll$NutrBudgNkgha + (5/9), TZAGSummAll$NutrPartialIndicatorN)
TZAGSummAll$NutrPartialIndicatorN<-ifelse(TZAGSummAll$NutrBudgNkgha<50 & TZAGSummAll$NutrBudgNkgha>20, (-1/90) * TZAGSummAll$NutrBudgNkgha + (5/9), TZAGSummAll$NutrPartialIndicatorN)
summary(TZAGSummAll$NutrPartialIndicatorN)

#Creating partial nutrient budget indicator for P
TZAGSummAll$NutrPartialIndicatorP<-888
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<(-20) | TZAGSummAll$NutrBudgPkgha>20, 0,TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<5 & TZAGSummAll$NutrBudgPkgha>(-5), 0.333,TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha>(-20) & TZAGSummAll$NutrBudgPkgha<(-5), (1/45) * TZAGSummAll$NutrBudgPkgha + (4/9), TZAGSummAll$NutrPartialIndicatorP)
TZAGSummAll$NutrPartialIndicatorP<-ifelse(TZAGSummAll$NutrBudgPkgha<20 & TZAGSummAll$NutrBudgPkgha>5, (-1/45) * TZAGSummAll$NutrBudgPkgha + (4/9), TZAGSummAll$NutrPartialIndicatorP)
summary(TZAGSummAll$NutrPartialIndicatorP)

#Creating partial nutrient budget indicator for K
TZAGSummAll$NutrPartialIndicatorK<-888
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<(-50) | TZAGSummAll$NutrBudgKkgha>50, 0,TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<20 & TZAGSummAll$NutrBudgKkgha>(-20), 0.333,TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha>(-50) & TZAGSummAll$NutrBudgKkgha<(-20), (1/90) * TZAGSummAll$NutrBudgKkgha + (5/9), TZAGSummAll$NutrPartialIndicatorK)
TZAGSummAll$NutrPartialIndicatorK<-ifelse(TZAGSummAll$NutrBudgKkgha<50 & TZAGSummAll$NutrBudgKkgha>20, (-1/90) * TZAGSummAll$NutrBudgKkgha + (5/9), TZAGSummAll$NutrPartialIndicatorK)
summary(TZAGSummAll$NutrPartialIndicatorK)

#Adding partial nutrient budget indicators to create SH58 indicator
TZAGSummAll$SH58<-TZAGSummAll$NutrPartialIndicatorN + TZAGSummAll$NutrPartialIndicatorP + TZAGSummAll$NutrPartialIndicatorK

#Subsetting for maize observations only
TZAGSummMaizeonly<-subset(TZAGSummAll, TZAGSummAll$zaocode=='11',select=c(zaocode:SH58))
summary(TZAGSummMaizeonly)

#AG INTENSIFICATION THREAD ONLY - Determining whether maize was cropped in 1 or 2 seasons for each household
TZAGMaizeperYear<-subset(TZAGSummMaizeonly, select=c(y3_hhid, plotnum, season, zaocode))
TZAGMaizeperYear$y3hhid_plotnum<-do.call("paste", c(TZAGMaizeperYear[c('y3_hhid','plotnum')],sep="_"))
TZAGMaizeFreqbyplot<-data.frame(table(TZAGMaizeperYear$y3hhid_plotnum))
TZAGMaizeFreqbyplot<-subset(TZAGMaizeFreqbyplot, !duplicated(TZAGMaizeFreqbyplot$Var1))

#AG INTENSIFICATION THREAD ONLY - Determining % of total cultivated area that is devoted to maize
TZAGSummLRSonly<-subset(TZAGSummAll, TZAGSummAll$season=="LRS")
TZAGSummLRSonly$maizeareaacre<-ifelse(TZAGSummLRSonly$zaocode==11, TZAGSummLRSonly$area_planted_percropacre, 0)
cols=c('area_planted_percropacre','maizeareaacre')
TZAGMaizePercent<-TZAGSummLRSonly
TZAGMaizePercent$y3hhid_plotnum<-NULL
TZAGMaizePercent<-data.table(TZAGMaizePercent)
TZAGMaizePercent2<-TZAGMaizePercent[, lapply(.SD,sum), by=y3_hhid, .SDcols=cols]
TZAGMaizePercent2$maizeareapercent<-TZAGMaizePercent2$maizeareaacre/TZAGMaizePercent2$area_planted_percropacre
TZAGMaizePercent2<-as.data.frame(TZAGMaizePercent2)

#AG INTENSIFICATION THREAD ONLY - Subsetting for maize observations in the LRS only
TZAGSummMaizeLRSonly<-subset(TZAGSummMaizeonly, TZAGSummMaizeonly$season=="LRS")

TZAGSummMaizeLRSonlytrim<-subset(TZAGSummMaizeLRSonly, select=c(zaocode:quantcropharvestedkg,area_planted_percropacre,
                        cropyieldkgperha,use:Crmvdkg,usedorgfert,quantorgfert,usedinorgfertA:typeinorgfertB,
                        TotNaddedkgInorg:CaddedkgOrg,NaddedkghaOrg:CaddedkghaOrg,NaddedkgCropres:CAddedAllSrcskgha))

TZAGSummMaizeLRSabbrev<-subset(TZAGSummMaizeLRSonly, select=c(y3_hhid, plotnum, y3hhid_plotnum, cropyieldkgperha,
                          anyirrigation,TotNaddedkghaInorg,TotPaddedkghaInorg))
  
#Removing excess data frames
rm(CropCode, FertNutrComp, HarvestIndices, NutrComp, TZAG2A, TZAG2B, TZAG3A, TZAG3B, TZAG4A, TZAG4B, TZAG5A, TZAG5B,
   TZAGA, TZAGCropRes3, TZAGMaizePercent, TZAGMaizeperYear, TZAGSummAll, TZAGSummMaizeLRSonlytrim, TZAGSummLRSonly,
   TZAGSummMaizeonly, TZAGSummMaizeLRSonly)
rm(cols, keep3a, keep3b)

#Rename data frames
FieldData.TZA.LSMS <- TZAGSummMaizeLRSabbrev
NumCrops.TZA.LSMS <- TZAGMaizeFreqbyplot
PctMaize.TZA.LSMS <- TZAGMaizePercent2
rm(TZAGSummMaizeLRSabbrev, TZAGMaizeFreqbyplot, TZAGMaizePercent2)

rm(dir_data1, dir_data2)