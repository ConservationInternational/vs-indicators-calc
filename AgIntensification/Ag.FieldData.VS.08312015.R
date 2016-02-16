##Calculating yield for maize per field for the LRS and N,P added, and irrigation - using VS Survey data 2013
#Written: 08/21/15
#Last updated: 8/31/15

#Open libraries
library(plyr)
library(openxlsx)
library(doBy)
library(foreign)
library(data.table)
library(lubridate)

#data sources
dir_data1 <- "C:/Users/Administrator/Google Drive/VS Indicators/VS protocol data snapshots"
dir_data2 <- "C:/Data/Vital Signs/Ag Intensification/Inputs"
dir_data3 <- "C:/Data/Vital Signs/Ag Intensification/Outputs"

#loading VS survey data
setwd (dir_data1)
VSAGA<-read.csv("agric_secA.csv", header=TRUE, sep=",")
VSAG2AB<-read.csv("agric_sec2_field_roster.csv", header=TRUE, sep=",")
VSAG3AB<-read.csv("agric_sec3_field_details.csv", header=TRUE, sep=",")
VSAG4AB<-read.csv("agric_sec4_crops_by_field.csv", header=TRUE, sep=",")
VSAG5AB<-read.csv("agric_sec5_crops_by_hh.csv", header=TRUE, sep=",")

#loading reference 
setwd (dir_data2)
NutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=15, startRow = 1,colNames =TRUE)
CropCode<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=16, startRow = 1,colNames =TRUE)
FertNutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=18, startRow = 1,colNames =TRUE)

#Turning all blanks into NAs
VSAGA[VSAGA == ""] <- NA
VSAG2AB[VSAG2AB == ""] <- NA
VSAG3AB[VSAG3AB == ""] <- NA
VSAG4AB[VSAG4AB == ""] <- NA
VSAG5AB[VSAG5AB == ""] <- NA

#Setup to merge area info (Section 2) with harvest info (Section 4) - Section 2
names(VSAG2AB)[names(VSAG2AB)=='Household.ID']<-'y3_hhid'
names(VSAG2AB)[names(VSAG2AB)=='Field.ID']<-'plotnum'
VSAG2AB$country_y3hhid_plotnum<-ifelse(is.na(VSAG2AB$y3_hhid)|is.na(VSAG2AB$plotnum)|is.na(VSAG2AB$Country),
                                       NA,(do.call("paste", c(VSAG2AB[c('Country','y3_hhid','plotnum')],sep="_"))))

#Setup to merge area info (Section 2) with harvest info (Section 4) - Section 4
names(VSAG4AB)[names(VSAG4AB)=='Household.ID']<-'y3_hhid'
names(VSAG4AB)[names(VSAG4AB)=='Field.ID']<-'plotnum'
names(VSAG4AB)[names(VSAG4AB)=='Crop.ID']<-'zaocode'
names(VSAG4AB)[names(VSAG4AB)=='Season']<-'season'

#Creating unique identifiers for section 4
VSAG4AB$country_y3hhid_plotnum<-ifelse(is.na(VSAG4AB$y3_hhid)|is.na(VSAG4AB$plotnum)|is.na(VSAG4AB$Country),
                                     NA,(do.call("paste", c(VSAG4AB[c('Country','y3_hhid','plotnum')],sep="_"))))
VSAG4AB$country_y3hhid_plotnum_crop<-ifelse(is.na(VSAG4AB$zaocode)|is.na(VSAG4AB$country_y3hhid_plotnum),
                                    NA,(do.call("paste", c(VSAG4AB[c('country_y3hhid_plotnum','zaocode')],sep="_"))))
VSAG4AB$country_y3hhid_plotnum_crop_season<-ifelse(is.na(VSAG4AB$country_y3hhid_plotnum_crop)|is.na(VSAG4AB$season),
                                    NA,(do.call("paste", c(VSAG4AB[c('country_y3hhid_plotnum_crop','season')],sep="_"))))
VSAG4AB$country_y3hhid_plotnum_season<-ifelse(is.na(VSAG4AB$country_y3hhid_plotnum)|is.na(VSAG4AB$season),
                                    NA,(do.call("paste", c(VSAG4AB[c('country_y3hhid_plotnum','season')],sep="_"))))

#Dropping all obs that have NA for hhid and/or plotnum and keeping only variables needed to calculate yield
VSAG2ABtrim<-subset(VSAG2AB, !is.na(country_y3hhid_plotnum),select=c(Country,y3_hhid,plotnum, Data.entry.date,
                                      Landscape..,ag2a_04,ag2a_09,country_y3hhid_plotnum))
VSAG4ABtrim<-subset(VSAG4AB, !is.na(country_y3hhid_plotnum),select=c(Country,y3_hhid,plotnum,Landscape..,season,zaocode,
                                      Data.entry.date, Crop.name,ag4a_01,ag4a_02,ag4a_06,ag4a_15, ag4a_15_unit,
                                      country_y3hhid_plotnum:country_y3hhid_plotnum_season))

#Checking for duplicates
count(duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
which(duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
count(duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))
which(duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))
#These duplicates are dropped for now 
VSAG2ABtrim<-subset(VSAG2ABtrim,!duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
VSAG4ABtrim<-subset(VSAG4ABtrim,!duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))

#Merge area info with harvest info
VSAG2AB4AB<-merge(VSAG4ABtrim, VSAG2ABtrim, by="country_y3hhid_plotnum", all.x=TRUE)

#Data staging to determine area planted 
VSAG2AB4AB$totplotarea<-ifelse(is.na(VSAG2AB4AB$ag2a_09), VSAG2AB4AB$ag2a_04, VSAG2AB4AB$ag2a_09)
VSAG2AB4AB$area2<-ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==1,VSAG2AB4AB$totplotarea * 0.25,
                         ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==2,VSAG2AB4AB$totplotarea * 0.5,
                                ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==3,VSAG2AB4AB$totplotarea * 0.75,
                                       ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==4,VSAG2AB4AB$totplotarea * 0.875,
                                              ifelse(VSAG2AB4AB$ag4a_01==1,VSAG2AB4AB$totplotarea,NA)))))
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='area2']<-'area_planted_percropacre'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_15']<-'quantcropharvested'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_15_unit']<-'quantcropharvestedunit'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag2a_04']<-'fieldareaestimatebyfarmer'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag2a_09']<-'totfieldareabyGPS'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_01']<-'waswholefieldplanted'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_02']<-'whichportionoffieldplanted'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_06']<-'wereanycropsharvested'

#ASSUMING ALL UNITS ARE KG BECAUSE AG4A_15_UNIT ARE ALL NA
count(!is.na(VSAG2AB4AB$quantcropharvestedunit))
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='quantcropharvested']<-'quantcropharvestedkg'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area in which it was planted) all year
VSAG2AB4AB$cropyieldkgperacre<-VSAG2AB4AB$quantcropharvestedkg/VSAG2AB4AB$area_planted_percropacre
VSAG2AB4AB$area_planted_percropha<-VSAG2AB4AB$area_planted_percropacre/2.47105
VSAG2AB4AB$cropyieldkgperha<-(VSAG2AB4AB$quantcropharvestedkg/VSAG2AB4AB$area_planted_percropacre)*2.47105

#CHECKS - 
#How many obs do not have any area data? - 19
sum(is.na(VSAG2AB4AB$totplotarea))
#How many obs don't have the portion of field cultivated? - 150
sum(is.na(VSAG2AB4AB$waswholefieldplanted))
#How many obs don't have a crop code? - 1, out of 1233 total
sum(is.na(VSAG2AB4AB$zaocode))
#How many obs don't have identifiers?
sum(is.na(VSAG2AB4AB$y3_hhid.x))
sum(is.na(VSAG2AB4AB$Country.x))
sum(is.na(VSAG2AB4AB$plotnum.x))
#How many do not have at least one of the above? - 167 out of 1233 observations
sum(is.na(VSAG2AB4AB$totplotarea) | is.na(VSAG2AB4AB$waswholefieldplanted) | is.na(VSAG2AB4AB$zaocode))

#DATA CLEANING STEP
#Dropping the ones that do not have a plot area measurement (neither GPS nor farmer estimate)
#no crop code, or no country, hhid, or plotnum
VSAG2AB4AB<-subset(VSAG2AB4AB, !is.na(VSAG2AB4AB$totplotarea) & !is.na(VSAG2AB4AB$waswholefieldplanted) & !is.na(VSAG2AB4AB$zaocode))

#Adding the function of each field from section 3
names(VSAG3AB)[names(VSAG3AB)=='Field.ID']<-'plotnum'
names(VSAG3AB)[names(VSAG3AB)=='Household.ID']<-'y3_hhid'
names(VSAG3AB)[names(VSAG3AB)=='Season']<-'season'
VSAG3AB$country_y3hhid_plotnum_season<-ifelse(is.na(VSAG3AB$Country)|is.na(VSAG3AB$y3_hhid)|is.na(VSAG3AB$plotnum)
        |is.na(VSAG3AB$season),NA, do.call("paste",c(VSAG3AB[c('Country','y3_hhid','plotnum','season')], sep="_")))

#Check for duplicates
count(duplicated(VSAG3AB$country_y3hhid_plotnum_season))
which(duplicated(VSAG3AB$country_y3hhid_plotnum_season))
#Drop duplicates
VSAG3AB<-subset(VSAG3AB, !duplicated(VSAG3AB$country_y3hhid_plotnum_season))

#Select only the variables we need for use and irrigation 
keep3ab<-c("Country", "Landscape..","y3_hhid", "plotnum", "ag3a_03","ag3a_17","ag3a_18","ag3a_20","season")
VSAG3ABtrim<-VSAG3AB[keep3ab]
VSAG3ABtrim$country_y3hhid_plotnum<-ifelse(is.na(VSAG3ABtrim$y3_hhid)|is.na(VSAG3ABtrim$plotnum)|is.na(VSAG3AB$Country),
                                   NA,(do.call("paste", c(VSAG3ABtrim[c('Country','y3_hhid','plotnum')],sep="_"))))
VSAG3ABtrim$country_y3hhid_plotnum_season<-ifelse(is.na(VSAG3ABtrim$country_y3hhid_plotnum)|is.na(VSAG3ABtrim$season),
                                    NA,(do.call("paste", c(VSAG3ABtrim[c('country_y3hhid_plotnum','season')],sep="_"))))

#Check if there are duplicates in section 3
count(duplicated(VSAG3ABtrim$country_y3hhid_plotnum_season))
#Drop duplicates - all 26 of them
VSAG3ABtrim<-subset(VSAG3ABtrim, !duplicated(VSAG3ABtrim$country_y3hhid_plotnum_season))

#Merge yield data with use data from section 3
VSAG2AB4AB2<-merge(VSAG2AB4AB,VSAG3ABtrim, by="country_y3hhid_plotnum_season", all.x = TRUE)
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='ag3a_03']<-'use'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='ag3a_17']<-'anyirrigation'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='ag3a_18']<-'irrigationtype'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='ag3a_20']<-'irrigationsource'
VSAG2AB4AB2$anyirrigation<-ifelse(VSAG2AB4AB2$anyirrigation==1,1,VSAG2AB4AB2$anyirrigation)
VSAG2AB4AB2$anyirrigation<-ifelse(VSAG2AB4AB2$anyirrigation==2,0,VSAG2AB4AB2$anyirrigation)

#Cleaning data frame
VSAG2AB4AB2$y3_hhid<-NULL
VSAG2AB4AB2$plotnum<-NULL
VSAG2AB4AB2$season<-NULL
VSAG2AB4AB2$Country<-NULL
VSAG2AB4AB2$Landscape..<-NULL
VSAG2AB4AB2$y3_hhid.y<-NULL
VSAG2AB4AB2$plotnum.y<-NULL
VSAG2AB4AB2$season.y<-NULL
VSAG2AB4AB2$country_y3hhid_plotnum.y<-NULL
VSAG2AB4AB2$Country.y<-NULL
VSAG2AB4AB2$Landscape...y<-NULL
VSAG2AB4AB2$Data.entry.date.y<-NULL
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='y3_hhid.x']<-'y3_hhid'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='plotnum.x']<-'plotnum'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='season.x']<-'season'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='Country.x']<-'Country'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='country_y3hhid_plotnum.x']<-'country_y3hhid_plotnum'
names(VSAG2AB4AB2)[names(VSAG2AB4AB2)=='Landscape...x']<-'Landscape'

#################################################################################################################################
#CHECKS - POST-YIELD CALCULATION
#See how many obs are there for each use and if there are any NAs
summary(as.factor(VSAG2AB4AB2$use))
#See if there is yield data for fields not listed as cultivated - no
count(VSAG2AB4AB$use=='2' & !is.na(VSAG2AB4AB$quantcropharvestedkg))
count(VSAG2AB4AB$use=='4' & !is.na(VSAG2AB4AB$quantcropharvestedkg))
#Checking to make sure that the plots with no area aren't fallow plots
sum(is.na(VSAG2AB4AB$area_planted_percropacre))
count(VSAG2AB4AB$use=='FALLOW')
count(is.na(VSAG2AB4AB$area_planted_percropacre) & VSAG2AB4AB$use=='FALLOW')
#Highlighting problem observations: if the area is there but the quantity harvested is not, and the plot is listed as cultivated
#VSAG2AB4AB$problem<-ifelse((!is.na(VSAG2AB4AB$area) & is.na(VSAG2AB4AB$quantcropharvestedkg)) & VSAG2AB4AB$use=="CULTIVATED",1,0)
#table(VSAG2AB4AB$problem)
rm(VSAG2AB4AB)
#################################################################################################################################

#Making sure crop codes are clean
table(VSAG2AB4AB2$zaocode)
table(CropCode$zaocode)
VSAGNutrComp<-merge(VSAG2AB4AB2,CropCode,by="zaocode", all.x=TRUE)
#rm(VSAG2AB4AB2)

#CHECK - Making sure every obs with a crop code has a crop group
sum(!is.na(VSAGNutrComp$Crop) & is.na(VSAGNutrComp$CropGroup))
which(!is.na(VSAGNutrComp$Crop) & is.na(VSAGNutrComp$CropGroup))

#Merging with nutrient composition table
names(NutrComp)[names(NutrComp)=='Crop']<-'CropGroup'
NutrComp$Cropresidue_C<-470
NutrComp$Harvested.product_C<-470
VSAGNutrComp2<-merge(VSAGNutrComp,NutrComp,by="CropGroup", all.x=TRUE)

#Making sure every obs with a crop group has nutrient composition values - CHECK
sum(!is.na(VSAGNutrComp2$Crop) & is.na(VSAGNutrComp2$Harvested.product_N))
sum(!is.na(VSAGNutrComp2$CropGroup) & is.na(VSAGNutrComp2$Harvested.product_N))
sum(is.na(VSAGNutrComp2$CropGroup))
sum(is.na(VSAGNutrComp2$Harvested.product_N))
which(is.na(VSAGNutrComp2$CropGroup))
#There are 8 observations with crops that we did not have on the nutrients removed table, so they are NA
#(Cashew nuts, black pepper, lemon, timber)

#Calculating the C, N, P, K removal in kg for each crop for each field
VSAGNutrComp2$Nrmvdkg<-VSAGNutrComp2$quantcropharvestedkg*0.001*VSAGNutrComp2$Harvested.product_N
VSAGNutrComp2$Prmvdkg<-VSAGNutrComp2$quantcropharvestedkg*0.001*VSAGNutrComp2$Harvested.product_P
VSAGNutrComp2$Krmvdkg<-VSAGNutrComp2$quantcropharvestedkg*0.001*VSAGNutrComp2$Harvested.product_K
VSAGNutrComp2$Crmvdkg<-VSAGNutrComp2$quantcropharvestedkg*0.001*VSAGNutrComp2$Harvested.product_C

#Calculating the C, N, P, K removal in kg/ha for each crop for each field
attach(VSAGNutrComp2)
VSAGNutrComp2$Nremovedkgperha<-(Nrmvdkg/area_planted_percropacre)*2.47105
VSAGNutrComp2$Premovedkgperha<-(Prmvdkg/area_planted_percropacre)*2.47105
VSAGNutrComp2$Kremovedkgperha<-(Krmvdkg/area_planted_percropacre)*2.47105
VSAGNutrComp2$Cremovedkgperha<-(Crmvdkg/area_planted_percropacre)*2.47105
detach(VSAGNutrComp2)

#Fertilizer nutrient composition
VSAG3ABfert<-subset(VSAG3AB,select=c(plotnum, season, Country,
  y3_hhid,ag3a_39,ag3a_vs_18a, ag3a_40, ag3a_45, ag3a_46,ag3a_47,ag3a_52,ag3a_53,ag3a_54))
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_39']<-'usedorgfert'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_vs_18a']<-'typeorgfert'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_40']<-'quantorgfert'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_45']<-'usedinorgfertA'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_46']<-'INPUTSA'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_47']<-'quantinorgfertA'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_52']<-'usedinorgfertB'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_53']<-'INPUTSB'
names(VSAG3ABfert)[names(VSAG3ABfert)=='ag3a_54']<-'quantinorgfertB'

#Making quantities of inorganic fertilizer equal 0 (instead of NA) if no inorganic fertilizer was reported used
VSAG3ABfert$quantorgfert<-ifelse(is.na(VSAG3ABfert$quantorgfert),0,VSAG3ABfert$quantorgfert)
VSAG3ABfert$quantinorgfertA<-ifelse(is.na(VSAG3ABfert$quantinorgfertA),0,VSAG3ABfert$quantinorgfertA)
VSAG3ABfert$quantinorgfertB<-ifelse(is.na(VSAG3ABfert$quantinorgfertB),0,VSAG3ABfert$quantinorgfertB)

#Merging datasets to combine yield and fertilizer survey responses
VSAG3ABfert$country_y3hhid_plotnum_season<-ifelse(is.na(VSAG3ABfert$y3_hhid)|is.na(VSAG3ABfert$plotnum)|is.na(VSAG3ABfert$season)|
                  is.na(VSAG3ABfert$Country),NA,(do.call("paste", c(VSAG3ABfert[c('Country','y3_hhid','plotnum','season')],sep="_"))))
VSAGFertNutrComp<-merge(VSAGNutrComp2,VSAG3ABfert, by="country_y3hhid_plotnum_season", all.x = TRUE)
VSAGFertNutrComp$y3_hhid.y<-NULL
VSAGFertNutrComp$plotnum.y<-NULL
VSAGFertNutrComp$season.y<-NULL
VSAGFertNutrComp$Country.y<-NULL
names(VSAGFertNutrComp)[names(VSAGFertNutrComp)=='y3_hhid.x']<-'y3_hhid'
names(VSAGFertNutrComp)[names(VSAGFertNutrComp)=='plotnum.x']<-'plotnum'
names(VSAGFertNutrComp)[names(VSAGFertNutrComp)=='season.x']<-'season'
names(VSAGFertNutrComp)[names(VSAGFertNutrComp)=='Country.x']<-'Country'
names(VSAGFertNutrComp)[names(VSAGFertNutrComp)=='Data.entry.date.x']<-'Data.entry.date'

#Merge with nutrient table for first batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='VSCode']<-'INPUTSA'
FertNutrComp$C<-ifelse(FertNutrComp$INPUTS=="ANIMAL MANURE",0.47,0)
VSAGFertNutrComp2<-merge(VSAGFertNutrComp, FertNutrComp, by="INPUTSA", all.x=TRUE)

#Count NAs to see if merge for inorganicfertA was correct - CHECK
count(!(is.na(VSAGFertNutrComp2$N)))
count(VSAGFertNutrComp2$usedinorgfertA=="1")
count(!is.na(VSAGFertNutrComp2$INPUTSA))

#Merge with nutrient table for second batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='INPUTSA']<-'INPUTSB'
VSAGFertNutrComp3<-merge(VSAGFertNutrComp2, FertNutrComp, by="INPUTSB", all.x=TRUE)

#Count NAs to see if merge B was correct - CHECK
count(!(is.na(VSAGFertNutrComp3$N.y)))
count(VSAGFertNutrComp2$usedinorgfertB=="1")
count(!is.na(VSAGFertNutrComp2$INPUTSB))

#Getting nutrients added (N, P, K, C) for inorganic fertilizer A
VSAGFertNutrComp3$NaddedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$N.x
VSAGFertNutrComp3$PaddedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$P.x
VSAGFertNutrComp3$KaddedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$K.x
VSAGFertNutrComp3$CaddedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$C.x
VSAGFertNutrComp3$P205addedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$P205.x
VSAGFertNutrComp3$PaddedkgInorgA<-VSAGFertNutrComp3$PaddedkgInorgA+VSAGFertNutrComp3$P205addedkgInorgA*0.4364
VSAGFertNutrComp3$K20addedkgInorgA<-VSAGFertNutrComp3$quantinorgfertA*VSAGFertNutrComp3$K20.x
VSAGFertNutrComp3$KaddedkgInorgA<-VSAGFertNutrComp3$KaddedkgInorgA+VSAGFertNutrComp3$K20addedkgInorgA*1.2046

#If no inorganic fertilizer A added, give value 0 instead of NA
VSAGFertNutrComp3$NaddedkgInorgA<-ifelse(is.na(VSAGFertNutrComp3$NaddedkgInorgA),0,VSAGFertNutrComp3$NaddedkgInorgA)
VSAGFertNutrComp3$PaddedkgInorgA<-ifelse(is.na(VSAGFertNutrComp3$PaddedkgInorgA),0,VSAGFertNutrComp3$PaddedkgInorgA)
VSAGFertNutrComp3$KaddedkgInorgA<-ifelse(is.na(VSAGFertNutrComp3$KaddedkgInorgA),0,VSAGFertNutrComp3$KaddedkgInorgA)
VSAGFertNutrComp3$CaddedkgInorgA<-ifelse(is.na(VSAGFertNutrComp3$CaddedkgInorgA),0,VSAGFertNutrComp3$CaddedkgInorgA)

#Getting nutrients added (N, P, K, C) for inorganic fertilizer B
VSAGFertNutrComp3$NaddedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$N.y
VSAGFertNutrComp3$PaddedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$P.y
VSAGFertNutrComp3$KaddedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$K.y
VSAGFertNutrComp3$CaddedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$C.y
VSAGFertNutrComp3$P205addedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$P205.y
VSAGFertNutrComp3$PaddedkgInorgB<-VSAGFertNutrComp3$PaddedkgInorgB+VSAGFertNutrComp3$P205addedkgInorgB*0.4364
VSAGFertNutrComp3$K20addedkgInorgB<-VSAGFertNutrComp3$quantinorgfertB*VSAGFertNutrComp3$K20.y
VSAGFertNutrComp3$KaddedkgInorgB<-VSAGFertNutrComp3$KaddedkgInorgB+VSAGFertNutrComp3$K20addedkgInorgB*1.2046

#If no inorganic fertilizer B added, give value 0 instead of NA
VSAGFertNutrComp3$NaddedkgInorgB<-ifelse(is.na(VSAGFertNutrComp3$NaddedkgInorgB),0,VSAGFertNutrComp3$NaddedkgInorgB)
VSAGFertNutrComp3$PaddedkgInorgB<-ifelse(is.na(VSAGFertNutrComp3$PaddedkgInorgB),0,VSAGFertNutrComp3$PaddedkgInorgB)
VSAGFertNutrComp3$KaddedkgInorgB<-ifelse(is.na(VSAGFertNutrComp3$KaddedkgInorgB),0,VSAGFertNutrComp3$KaddedkgInorgB)
VSAGFertNutrComp3$CaddedkgInorgB<-ifelse(is.na(VSAGFertNutrComp3$CaddedkgInorgB),0,VSAGFertNutrComp3$CaddedkgInorgB)

#Summing nutrient addition per crop per field for inorganic fertilizers only
VSAGFertNutrComp3$TotNaddedkgInorg<-VSAGFertNutrComp3$NaddedkgInorgA+VSAGFertNutrComp3$NaddedkgInorgB
VSAGFertNutrComp3$TotPaddedkgInorg<-VSAGFertNutrComp3$PaddedkgInorgA+VSAGFertNutrComp3$PaddedkgInorgB
VSAGFertNutrComp3$TotKaddedkgInorg<-VSAGFertNutrComp3$KaddedkgInorgA+VSAGFertNutrComp3$KaddedkgInorgB
VSAGFertNutrComp3$TotCaddedkgInorg<-VSAGFertNutrComp3$CaddedkgInorgA+VSAGFertNutrComp3$CaddedkgInorgB

#Finding total nutrients by all inorganic fertilizers added in kg/ha
VSAGFertNutrComp3$TotNaddedkghaInorg<-VSAGFertNutrComp3$TotNaddedkgInorg/VSAGFertNutrComp3$area_planted_percropacre*2.47105
VSAGFertNutrComp3$TotPaddedkghaInorg<-VSAGFertNutrComp3$TotPaddedkgInorg/VSAGFertNutrComp3$area_planted_percropacre*2.47105
VSAGFertNutrComp3$TotKaddedkghaInorg<-VSAGFertNutrComp3$TotKaddedkgInorg/VSAGFertNutrComp3$area_planted_percropacre*2.47105
VSAGFertNutrComp3$TotCaddedkghaInorg<-VSAGFertNutrComp3$TotCaddedkgInorg/VSAGFertNutrComp3$area_planted_percropacre*2.47105

#Merging with table with nutrition content of organic fertilizers
#Here we're assuming that all types of organic fertilizers have the same nutrient composition, equal to that
#labeled "animal manure" in the FertNutrComp table from TEAM
VSAGFertNutrComp4<-subset(VSAGFertNutrComp3, select=c(INPUTSB:quantinorgfertB,NaddedkgInorgA:TotCaddedkghaInorg))
VSAGFertNutrComp4$INPUTSC<-ifelse(VSAGFertNutrComp4$usedorgfert==1,"ORGANIC FERTILIZER",0)
names(FertNutrComp)[names(FertNutrComp)=='INPUTSB']<-'INPUTSC'
FertNutrComp$INPUTSC<-ifelse(FertNutrComp$INPUTSC==2,"ORGANIC FERTILIZER",FertNutrComp$INPUTSC)
VSAGFertNutrComp5<-merge(VSAGFertNutrComp4,FertNutrComp, by="INPUTSC", all.x=TRUE)

#CHECK MERGE to make sure that all obs that answered yes to used organic fertilizer were assigned a nutrient composition
count(VSAGFertNutrComp5$usedorgfert==1)
count(!is.na(VSAGFertNutrComp5$N))
#rm(VSAGFertNutrComp, VSAGFertNutrComp2, VSAGFertNutrComp3, VSAGFertNutrComp4)

#Getting nutrient addition (N, P, K, C) for organic fertilizer in kgs
VSAGFertNutrComp5$NaddedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$N
VSAGFertNutrComp5$PaddedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$P
VSAGFertNutrComp5$KaddedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$K
VSAGFertNutrComp5$CaddedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$C
VSAGFertNutrComp5$P205addedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$P205
VSAGFertNutrComp5$PaddedkgOrg<-VSAGFertNutrComp5$PaddedkgOrg + VSAGFertNutrComp5$P205addedkgOrg*0.4364
VSAGFertNutrComp5$K20addedkgOrg<-VSAGFertNutrComp5$quantorgfert*VSAGFertNutrComp5$K20
VSAGFertNutrComp5$KaddedkgOrg<-VSAGFertNutrComp5$KaddedkgOrg + VSAGFertNutrComp5$K20addedkgOrg*1.2046

#If no organic fertilizer used, giving nutrient added quantity a value of 0 instead of NA
VSAGFertNutrComp5$NaddedkgOrg<-ifelse(is.na(VSAGFertNutrComp5$NaddedkgOrg),0,VSAGFertNutrComp5$NaddedkgOrg)
VSAGFertNutrComp5$PaddedkgOrg<-ifelse(is.na(VSAGFertNutrComp5$PaddedkgOrg),0,VSAGFertNutrComp5$PaddedkgOrg)
VSAGFertNutrComp5$KaddedkgOrg<-ifelse(is.na(VSAGFertNutrComp5$KaddedkgOrg),0,VSAGFertNutrComp5$KaddedkgOrg)
VSAGFertNutrComp5$CaddedkgOrg<-ifelse(is.na(VSAGFertNutrComp5$CaddedkgOrg),0,VSAGFertNutrComp5$CaddedkgOrg)

#Finding nutrients added from organic fertilizer in kg/ha
VSAGFertNutrComp5$NaddedkghaOrg<-VSAGFertNutrComp5$NaddedkgOrg/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$PaddedkghaOrg<-VSAGFertNutrComp5$PaddedkgOrg/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$KaddedkghaOrg<-VSAGFertNutrComp5$KaddedkgOrg/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$CaddedkghaOrg<-VSAGFertNutrComp5$CaddedkgOrg/VSAGFertNutrComp5$area_planted_percropacre*2.47105

#Summing total nutrients added from inorganic and organic fertilizers in kg
VSAGFertNutrComp5$TotNaddedkgAllFert<-VSAGFertNutrComp5$TotNaddedkgInorg+VSAGFertNutrComp5$NaddedkgOrg
VSAGFertNutrComp5$TotPaddedkgAllFert<-VSAGFertNutrComp5$TotPaddedkgInorg+VSAGFertNutrComp5$PaddedkgOrg
VSAGFertNutrComp5$TotKaddedkgAllFert<-VSAGFertNutrComp5$TotKaddedkgInorg+VSAGFertNutrComp5$KaddedkgOrg
VSAGFertNutrComp5$TotCaddedkgAllFert<-VSAGFertNutrComp5$TotCaddedkgInorg+VSAGFertNutrComp5$CaddedkgOrg

#Calculating total nutrients added from inorganic and organic fertilizers in kg/ha
VSAGFertNutrComp5$TotNaddedkghaAllFert<-VSAGFertNutrComp5$TotNaddedkgAllFert/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$TotPaddedkghaAllFert<-VSAGFertNutrComp5$TotPaddedkgAllFert/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$TotKaddedkghaAllFert<-VSAGFertNutrComp5$TotKaddedkgAllFert/VSAGFertNutrComp5$area_planted_percropacre*2.47105
VSAGFertNutrComp5$TotCaddedkghaAllFert<-VSAGFertNutrComp5$TotCaddedkgAllFert/VSAGFertNutrComp5$area_planted_percropacre*2.47105

#Calculating nutrient budgets in kg
VSAGFertNutrComp5$NutrBudgNkg<-VSAGFertNutrComp5$TotNaddedkgAllFert - VSAGFertNutrComp5$Nrmvdkg
VSAGFertNutrComp5$NutrBudgPkg<-VSAGFertNutrComp5$TotPaddedkgAllFert - VSAGFertNutrComp5$Prmvdkg
VSAGFertNutrComp5$NutrBudgKkg<-VSAGFertNutrComp5$TotKaddedkgAllFert - VSAGFertNutrComp5$Krmvdkg
VSAGFertNutrComp5$NutrBudgCkg<-VSAGFertNutrComp5$TotCaddedkgAllFert - VSAGFertNutrComp5$Crmvdkg

#Calculating nutrient budgets in kg/ha
VSAGFertNutrComp5$NutrBudgNkgha<-(VSAGFertNutrComp5$NutrBudgNkg/VSAGFertNutrComp5$area_planted_percropacre)*2.47105
VSAGFertNutrComp5$NutrBudgPkgha<-(VSAGFertNutrComp5$NutrBudgPkg/VSAGFertNutrComp5$area_planted_percropacre)*2.47105
VSAGFertNutrComp5$NutrBudgKkgha<-(VSAGFertNutrComp5$NutrBudgKkg/VSAGFertNutrComp5$area_planted_percropacre)*2.47105
VSAGFertNutrComp5$NutrBudgCkgha<-(VSAGFertNutrComp5$NutrBudgCkg/VSAGFertNutrComp5$area_planted_percropacre)*2.47105


#CHECK- how many observations are we working with?
#all obs for which we have yield data - 823
sum(!is.na(VSAGFertNutrComp5$cropyieldkgperacre) & !is.na(VSAGFertNutrComp5$zaocode))
#all obs for which we have yield + input data - 800
sum(!is.na(VSAGFertNutrComp5$zaocode) & !is.na(VSAGFertNutrComp5$cropyieldkgperacre) & 
      !is.na(VSAGFertNutrComp5$usedorgfert))

#Subset for all field_crops in all seasons, only those with a yield, a crop code, and information about inputs
VSAGSumm1<-subset(VSAGFertNutrComp5, VSAGFertNutrComp5$area_planted_percropacre!=0, 
                  select=c(Country, zaocode,y3_hhid,plotnum,country_y3hhid_plotnum,Landscape, Data.entry.date, quantcropharvestedkg,
                           area_planted_percropha,cropyieldkgperha,use,season,usedorgfert,usedinorgfertA,usedinorgfertB,anyirrigation,
                           Nrmvdkg:Cremovedkgperha,TotNaddedkgInorg:TotCaddedkghaInorg,NaddedkgOrg:CaddedkgOrg, NaddedkghaOrg:CaddedkghaOrg,
                           TotNaddedkgAllFert:TotCaddedkghaAllFert,NutrBudgNkg:NutrBudgCkgha))
VSAGSumm1$Data.entry.date<-ymd_hms(VSAGSumm1$Data.entry.date)
VSAGSumm1$Data.entry.year<-year(VSAGSumm1$Data.entry.date)
summary(VSAGSumm1)

#Subsetting to include only Maize observations
VSAGSummMaizeonly<-subset(VSAGSumm1, VSAGSumm1$zaocode==11)

#Subsetting to include only Maize observations in the LRS
VSAGSummMaizeLRSonly<-subset(VSAGSumm1,VSAGSumm1$zaocode==11 & VSAGSumm1$season=="long_rainy")

#Subsetting yield and input values for all of Ghana
VSAGSummGHA<-subset(VSAGSumm1, Country=="GHA")
VSAGSummGHAMaizeLRSonly<-subset(VSAGSummMaizeLRSonly, Country=="GHA")
VSAGSummGHAabbrev<-subset(VSAGSummGHAMaizeLRSonly, select=c(Country, zaocode, y3_hhid, plotnum, country_y3hhid_plotnum, 
                          Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation, Data.entry.year))

#Subsetting yield and input values for all of Tanzania
VSAGSummTZA<-subset(VSAGSumm1, Country=="TZA")
VSAGSummTZAMaizeLRSonly<-subset(VSAGSummMaizeLRSonly, Country=="TZA")
VSAGSummTZAabbrev<-subset(VSAGSummTZAMaizeLRSonly, select=c(Country, zaocode, y3_hhid, plotnum, country_y3hhid_plotnum, 
                          Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation, Data.entry.year))

#Subsetting yield and input values for each landscape in Tanzania
#VSAGSummTZA_L10abbrev<-subset(VSAGSummTZAMaizeLRSonly, Landscape=="L10", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummTZA_L18abbrev<-subset(VSAGSummTZAMaizeLRSonly, Landscape=="L18", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))

#Subsetting yield and input values for each landscape in Ghana
#VSAGSummGHA_L01abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L01", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L02abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L02", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L03abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L03", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L04abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L04", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L05abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L05", select=c(Country, zaocode, y3_hhid, plotnum,
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L06abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L06", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))

#Determining whether maize was cropped in 1 or 2 seasons for each household - All of Ghana
VSAGMaizeperYearGHA<-subset(VSAGSummMaizeonly, VSAGSummMaizeonly$Country=='GHA', select=c(country_y3hhid_plotnum, season, zaocode))
VSAGMaizeFreqbyplotGHA<-data.frame(table(VSAGMaizeperYearGHA$country_y3hhid_plotnum))
#VSAGMaizeFreqbyplotGHA<-subset(VSAGMaizeFreqbyplotGHA, !duplicated(VSAGMaizeFreqbyplotGHA$Var1))
names(VSAGMaizeFreqbyplotGHA)[names(VSAGMaizeFreqbyplotGHA)=="Var1"]<-"country_y3hhid_plotnum"
names(VSAGMaizeFreqbyplotGHA)[names(VSAGMaizeFreqbyplotGHA)=="Freq"]<-"numcrops"

#Determining whether maize was cropped in 1 or 2 seasons for each household - Tanzania
VSAGMaizeperYearTZA<-subset(VSAGSummMaizeonly, VSAGSummMaizeonly$Country=='TZA', select=c(country_y3hhid_plotnum, season, zaocode))
VSAGMaizeFreqbyplotTZA<-data.frame(table(VSAGMaizeperYearTZA$country_y3hhid_plotnum))
#VSAGMaizeFreqbyplotTZA<-subset(VSAGMaizeFreqbyplotTZA, !duplicated(VSAGMaizeFreqbyplotTZA$Var1))
names(VSAGMaizeFreqbyplotTZA)[names(VSAGMaizeFreqbyplotTZA)=="Var1"]<-"country_y3hhid_plotnum"
names(VSAGMaizeFreqbyplotTZA)[names(VSAGMaizeFreqbyplotTZA)=="Freq"]<-"numcrops"

#Adding in Landscape codes and date of data entry
names(VSAG4AB)[names(VSAG4AB)=='Landscape..']<-'Landscape'
VSAG4ABmerge<-subset(VSAG4AB, select=c("Country", "plotnum", "y3_hhid", "Landscape","Data.entry.date"))
VSAG4ABmerge$Data.entry.date<-ymd_hms(VSAG4ABmerge$Data.entry.date)
VSAG4ABmerge$Data.entry.year<-year(VSAG4ABmerge$Data.entry.date)
VSAG4ABmerge$country_y3hhid_plotnum<-ifelse(is.na(VSAG4ABmerge$Country) | is.na(VSAG4ABmerge$y3_hhid) |
            is.na(VSAG4ABmerge$plotnum), NA, do.call("paste",c(VSAG4ABmerge[c('Country','y3_hhid','plotnum')], sep='_')))
VSAG4ABmerge$country_y3hhid<-ifelse(is.na(VSAG4ABmerge$Country) | is.na(VSAG4ABmerge$y3_hhid), NA, 
            do.call("paste",c(VSAG4ABmerge[c('Country','y3_hhid')], sep='_')))
VSAG4ABmerge<-subset(VSAG4ABmerge, !duplicated(VSAG4ABmerge$country_y3hhid_plotnum))
VSAGMaizeFreqbyplotTZA<-merge(VSAGMaizeFreqbyplotTZA, VSAG4ABmerge, by="country_y3hhid_plotnum", all.x=TRUE)
VSAGMaizeFreqbyplotTZA$Data.entry.date<-NULL
VSAGMaizeFreqbyplotGHA<-merge(VSAGMaizeFreqbyplotGHA, VSAG4ABmerge, by="country_y3hhid_plotnum", all.x=TRUE)
VSAGMaizeFreqbyplotGHA$Data.entry.date<-NULL

#Determining % of total cultivated area that is devoted to maize for each household- set up
VSAGSummLRSonly<-subset(VSAGSumm1, VSAGSumm1$season=="long_rainy")
names(VSAGSummLRSonly)[names(VSAGSummLRSonly)=='Landscape..']<-'Landscape'
VSAGSummLRSonly$maizeareaha<-ifelse(VSAGSummLRSonly$zaocode==11, VSAGSummLRSonly$area_planted_percropha, 0)
cols=c('area_planted_percropha','maizeareaha')
VSAGSummLRSonly$country_y3hhid<-ifelse(is.na(VSAGSummLRSonly$Country)|is.na(VSAGSummLRSonly$y3_hhid), NA,
                do.call("paste",c(VSAGSummLRSonly[c('Country','y3_hhid')],sep="_")))

#Determining % of total cultivated area that is devoted to maize for each household- for Ghana
VSAGSummGHALRSonly<-subset(VSAGSummLRSonly, Country=='GHA')
VSAGMaizePercentGHA<-VSAGSummGHALRSonly
VSAGMaizePercentGHA$country_y3hhid_plotnum<-NULL
VSAGMaizePercentGHA<-data.table(VSAGMaizePercentGHA)
VSAGMaizePercentGHA2<-VSAGMaizePercentGHA[, lapply(.SD,sum), by=country_y3hhid, .SDcols=cols]
VSAGMaizePercentGHA2$maizeareapercent<-VSAGMaizePercentGHA2$maizeareaha/VSAGMaizePercentGHA2$area_planted_percropha
VSAGMaizePercentGHA2<-as.data.frame(VSAGMaizePercentGHA2)

#Determining % of total cultivated area that is devoted to maize - for Tanzania (All)
VSAGSummTZALRSonly<-subset(VSAGSummLRSonly, Country=='TZA')
VSAGMaizePercentTZA<-VSAGSummGHALRSonly
VSAGMaizePercentTZA$country_y3hhid_plotnum<-NULL
VSAGMaizePercentTZA<-data.table(VSAGSummTZALRSonly)
VSAGMaizePercentTZA2<-VSAGMaizePercentTZA[, lapply(.SD,sum), by=country_y3hhid, .SDcols=cols]
VSAGMaizePercentTZA2$maizeareapercent<-VSAGMaizePercentTZA2$maizeareaha/VSAGMaizePercentTZA2$area_planted_percropha
VSAGMaizePercentTZA2<-as.data.frame(VSAGMaizePercentTZA2)

#Adding in Landscape codes and date of data entry
VSAG4ABmerge2<-subset(VSAG4ABmerge, !duplicated(VSAG4ABmerge$country_y3hhid))
VSAGMaizePercentGHA2<-merge(VSAGMaizePercentGHA2, VSAG4ABmerge2, by="country_y3hhid", all.x=TRUE)
VSAGMaizePercentGHA2$Data.entry.date<-NULL
VSAGMaizePercentTZA2<-merge(VSAGMaizePercentTZA2, VSAG4ABmerge2, by="country_y3hhid", all.x=TRUE)
VSAGMaizePercentTZA2$Data.entry.date<-NULL

#Removing excess data frames
rm(CropCode, FertNutrComp, NutrComp, VSAG2AB, VSAG2AB4AB2, VSAG2ABtrim, VSAG3AB, VSAG3ABfert,
   VSAG3ABtrim, VSAG4AB, VSAG4ABtrim, VSAG5AB, VSAGFertNutrComp, VSAGFertNutrComp5, VSAGFertNutrComp4,
   VSAGFertNutrComp3, VSAGFertNutrComp2, VSAGNutrComp, VSAGNutrComp2, 
    cols, keep3ab, VSAGMaizePercentGHA, VSAGMaizePercentTZA, 
   VSAGMaizeperYearTZA, VSAGMaizeperYearGHA, VSAGA, HarvestIndices,SAGCOThouseholds,TZAG2A,TZAG2B,
   TZAG3A,TZAG3B,TZAG4A,TZAG4B,TZAG5A,TZAG5B,TZAGCropRes3,TZAGMaizeperYear,TZAGSummAll,TZAGSummMaizeonly)

rm(VSAGSummGHA, VSAGSummTZA,VSAGSumm1, VSAGSummLRSonly, VSAGSummMaizeLRSonly, VSAGSummMaizeonly,
   VSAGSummGHALRSonly, VSAGSummGHAMaizeLRSonly, VSAGSummTZALRSonly, VSAGSummTZAMaizeLRSonly,
  VSAG4ABmerge, VSAG4ABmerge2, keep3a, keep3b)

#Rename data frames
FieldData.GHA.VS <- VSAGSummGHAabbrev
NumCrops.GHA.VS <- VSAGMaizeFreqbyplotGHA
PctMaize.GHA.VS <- VSAGMaizePercentGHA2
rm(VSAGSummGHAabbrev, VSAGMaizeFreqbyplotGHA, VSAGMaizePercentGHA2)

FieldData.TZA.VS <- VSAGSummTZAabbrev
NumCrops.TZA.VS <- VSAGMaizeFreqbyplotTZA
PctMaize.TZA.VS <- VSAGMaizePercentTZA2
rm(VSAGSummTZAabbrev, VSAGMaizeFreqbyplotTZA, VSAGMaizePercentTZA2)

rm(dir_data1, dir_data2, dir_data3)
