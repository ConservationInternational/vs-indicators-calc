################################################################################################################
# AGRICULTURAL INTENSIFICATION THREAD: Ag.Field.Data.R
# Calculates: Yield for maize-growing plots in the LRS, levels of N,P added by inorganic fertilizer, 
# number of maize crops per year, & percent of cultivated land devoted to maize
# Uses: VS Agriculture Survey data 2013
# Written: 08/21/15
# Last updated: 8/31/15
################################################################################################################

#Open libraries
library(plyr)
library(openxlsx)
library(doBy)
library(foreign)
library(data.table)
library(lubridate)

#Name data sources
dir_data1 <- "C:/Users/Administrator/Google Drive/VS Indicators/VS protocol data snapshots"
dir_data2 <- "C:/Data/Vital Signs/Ag Intensification/Inputs"
dir_data3 <- "C:/Data/Vital Signs/Ag Intensification/Outputs"

#Load VS Ag survey data
setwd (dir_data1)
VSAG2AB<-read.csv("agric_sec2_field_roster.csv", header=TRUE, sep=",")
VSAG3AB<-read.csv("agric_sec3_field_details.csv", header=TRUE, sep=",")
VSAG4AB<-read.csv("agric_sec4_crops_by_field.csv", header=TRUE, sep=",")
VSAG5AB<-read.csv("agric_sec5_crops_by_hh.csv", header=TRUE, sep=",")

#Load reference tables
setwd (dir_data2)
NutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=15, startRow = 1,colNames =TRUE)
CropCode<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=16, startRow = 1,colNames =TRUE)
FertNutrComp<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=18, startRow = 1,colNames =TRUE)
HarvestIndices<-read.xlsx("SoilHealthMatrix_12172014.xlsx", sheet=19, startRow = 1,colNames =TRUE)

#Turn all blanks into NAs
VSAG2AB[VSAG2AB == ""] <- NA
VSAG3AB[VSAG3AB == ""] <- NA
VSAG4AB[VSAG4AB == ""] <- NA
VSAG5AB[VSAG5AB == ""] <- NA

################################################################################################################
# YIELD CALCULATION
# OUTPUT: KG OF CROP HARVESTED PER HA
################################################################################################################

#Setup to merge area info (Section 2) with harvest info (Section 4) - Section 2
names(VSAG2AB)[names(VSAG2AB)=='Household.ID']<-'y3_hhid'
names(VSAG2AB)[names(VSAG2AB)=='Field.ID']<-'plotnum'
names(VSAG2AB)[names(VSAG2AB)=='Landscape..']<-'Landscape'

#Setup to merge area info (Section 2) with harvest info (Section 4) - Section 4
names(VSAG4AB)[names(VSAG4AB)=='Household.ID']<-'y3_hhid'
names(VSAG4AB)[names(VSAG4AB)=='Field.ID']<-'plotnum'
names(VSAG4AB)[names(VSAG4AB)=='Crop.ID']<-'zaocode'
names(VSAG4AB)[names(VSAG4AB)=='Season']<-'season'
names(VSAG4AB)[names(VSAG4AB)=='Landscape..']<-'Landscape'

#Dropping all obs that have NA for country, hhid and/or plotnum and keeping only variables needed to calculate yield
VSAG2ABtrim<-subset(VSAG2AB, !is.na(VSAG2AB$Country) & !is.na(VSAG2AB$y3_hhid), select=c(Country,y3_hhid,plotnum, Data.entry.date,
     Landscape,ag2a_04,ag2a_09))
VSAG4ABtrim<-subset(VSAG4AB, !is.na(VSAG4AB$Country) & !is.na(VSAG4AB$y3_hhid), select=c(Country,y3_hhid,plotnum,season,
      zaocode,Crop.name,ag4a_01,ag4a_02,ag4a_06,ag4a_15, ag4a_15_unit))

#DATA CLEANING STEP - Checking for duplicates in Section 2 and Section 4
VSAG2ABtrim$country_y3hhid_plotnum<-ifelse(is.na(VSAG2ABtrim$y3_hhid)|is.na(VSAG2ABtrim$plotnum)|
  is.na(VSAG2ABtrim$Country),NA,(do.call("paste", c(VSAG2ABtrim[c('Country','y3_hhid','plotnum')],
  sep="_"))))
VSAG4ABtrim$country_y3hhid_plotnum_crop_season<-ifelse(is.na(VSAG4ABtrim$Country)|is.na(VSAG4ABtrim$y3_hhid)|is.na(VSAG4ABtrim$plotnum)|is.na(VSAG4ABtrim$season)|
  is.na(VSAG4ABtrim$zaocode),NA,(do.call("paste", c(VSAG4ABtrim[c('Country','y3_hhid','plotnum','zaocode','season')],
  sep="_"))))
count(duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
which(duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
count(duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))
which(duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))

#DATA CLEANING STEP - Dropping duplicates in Section 2 and Section 4
VSAG2ABtrim<-subset(VSAG2ABtrim,!duplicated(VSAG2ABtrim$country_y3hhid_plotnum))
VSAG4ABtrim<-subset(VSAG4ABtrim,!duplicated(VSAG4ABtrim$country_y3hhid_plotnum_crop_season))

#Merging area info (Section 2) with harvest info (Section 4)
VSAG2AB4AB<-merge(VSAG4ABtrim, VSAG2ABtrim, by=c("Country","y3_hhid","plotnum"), all.x=TRUE)

#Data staging to determine area planted 
VSAG2AB4AB$totplotarea<-ifelse(is.na(VSAG2AB4AB$ag2a_09), VSAG2AB4AB$ag2a_04, VSAG2AB4AB$ag2a_09)
VSAG2AB4AB$area2<-ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==1,VSAG2AB4AB$totplotarea * 0.25,
      ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==2,VSAG2AB4AB$totplotarea * 0.5,
      ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==3,VSAG2AB4AB$totplotarea * 0.75,
      ifelse(VSAG2AB4AB$ag4a_01==2 & VSAG2AB4AB$ag4a_02==4,VSAG2AB4AB$totplotarea * 0.875,
      ifelse(VSAG2AB4AB$ag4a_01==1,VSAG2AB4AB$totplotarea,NA)))))

#Renaming variables for readability
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='area2']<-'areaplantedpercrop_acre'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_15']<-'quantcropharvested'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_15_unit']<-'quantcropharvestedunit'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag2a_04']<-'fieldareaestimatebyfarmer'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag2a_09']<-'totfieldareabyGPS'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_01']<-'waswholefieldplanted'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_02']<-'whichportionoffieldplanted'
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='ag4a_06']<-'wereanycropsharvested'

#Assume that all units are in kg, because units column is all NA
count(!is.na(VSAG2AB4AB$quantcropharvestedunit))
names(VSAG2AB4AB)[names(VSAG2AB4AB)=='quantcropharvested']<-'quantcropharvestedkg'

#Determining the crop yield for each crop (how many kgs of that crop were harvested per unit of area 
#in which it was planted); includes both seasons
VSAG2AB4AB$areaplantedpercrop_ha<-VSAG2AB4AB$areaplantedpercrop_acre/2.47105
VSAG2AB4AB$cropyieldkgperacre<-VSAG2AB4AB$quantcropharvestedkg/VSAG2AB4AB$areaplantedpercrop_acre
VSAG2AB4AB$cropyieldkgperha<-VSAG2AB4AB$quantcropharvestedkg/VSAG2AB4AB$areaplantedpercrop_ha

#DATA CLEANING STEP - DROPPING OBSERVATIONS MISSING AREA AND CROP CODE INFORMATION 
#How many obs do not have any area data? - 19/1303
sum(is.na(VSAG2AB4AB$totplotarea))
#How many obs don't have the portion of field cultivated? - 153/1303
sum(is.na(VSAG2AB4AB$waswholefieldplanted))
#How many obs don't have a crop code? - 1/1303
sum(is.na(VSAG2AB4AB$zaocode))
#How many do not have at least one of the above? - 170/1303 observations
sum(is.na(VSAG2AB4AB$totplotarea) | is.na(VSAG2AB4AB$waswholefieldplanted) | is.na(VSAG2AB4AB$zaocode))
#Dropping the observations that do not have a plot area measurement (neither GPS nor farmer estimate) or crop code
VSAG2AB4AB2<-subset(VSAG2AB4AB, !is.na(VSAG2AB4AB$areaplantedpercrop_acre) & !is.na(VSAG2AB4AB$zaocode))

################################################################################################################
# N, P, K, & C REMOVED BY CROPS CALCULATION
# OUTPUT: KGS OF N, P, K, & C REMOVED PER HA
################################################################################################################

#Setting up Section 3 to add field management information
names(VSAG3AB)[names(VSAG3AB)=='Field.ID']<-'plotnum'
names(VSAG3AB)[names(VSAG3AB)=='Household.ID']<-'y3_hhid'
names(VSAG3AB)[names(VSAG3AB)=='Season']<-'season'
VSAG3AB$country_y3hhid_plotnum_season<-ifelse(is.na(VSAG3AB$Country)|is.na(VSAG3AB$y3_hhid)|is.na(VSAG3AB$plotnum)
    |is.na(VSAG3AB$season),NA, do.call("paste",c(VSAG3AB[c('Country','y3_hhid','plotnum','season')], sep="_")))

#DATA CLEANING STEP - CHECKING FOR DUPLICATES
count(duplicated(VSAG3AB$country_y3hhid_plotnum_season))
which(duplicated(VSAG3AB$country_y3hhid_plotnum_season))

#DATA CLEANING STEP - DROPPING DUPLICATES
VSAG3AB<-subset(VSAG3AB, !duplicated(VSAG3AB$country_y3hhid_plotnum_season))

#Select the variables needed for input calculations
VSAG3ABtrim<-subset(VSAG3AB, select=c(Country,y3_hhid,plotnum,ag3a_03,ag3a_17,ag3a_18,
    ag3a_20,season,ag3a_39,ag3a_vs_18a, ag3a_40, ag3a_45,ag3a_46,ag3a_47,ag3a_52,ag3a_53,ag3a_54))

#Merge yield data (VSAG2AB4AB2) with management data from Section 3 (VSAG3ABtrim)
VSAGNutrBudget<-merge(VSAG2AB4AB2,VSAG3ABtrim, by=c("Country","y3_hhid","plotnum","season"), all.x = TRUE)

#Rename variables for readability
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_03']<-'use'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_17']<-'anyirrigation'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_18']<-'irrigationtype'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_20']<-'irrigationsource'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_39']<-'usedorgfert'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_vs_18a']<-'typeorgfert'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_40']<-'quantorgfert'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_45']<-'usedinorgfertA'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_46']<-'INPUTSA'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_47']<-'quantinorgfertA'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_52']<-'usedinorgfertB'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_53']<-'INPUTSB'
names(VSAGNutrBudget)[names(VSAGNutrBudget)=='ag3a_54']<-'quantinorgfertB'

#Recoding irrigation variable to 0/1 from 1/2
VSAGNutrBudget$anyirrigation<-ifelse(VSAGNutrBudget$anyirrigation==1,1,VSAGNutrBudget$anyirrigation)
VSAGNutrBudget$anyirrigation<-ifelse(VSAGNutrBudget$anyirrigation==2,0,VSAGNutrBudget$anyirrigation)

#Merge by crop code to group each crop into a category for which nutrient removed information is available
table(VSAGNutrBudget$zaocode)
table(CropCode$zaocode)
VSAGNutrBudget2<-merge(VSAGNutrBudget,CropCode,by="zaocode", all.x=TRUE)
VSAGNutrBudget2$Crop<-NULL

#CHECK - Making sure every obs with a crop code has a crop group
sum(!is.na(VSAGNutrBudget2$zaocode) & is.na(VSAGNutrBudget2$CropGroup))
which(!is.na(VSAGNutrBudget2$zaocode) & is.na(VSAGNutrBudget2$CropGroup))

#ASSUMPTION - Assuming that the mass of C removed by crop harvesting is 47% of the weight of harvested 
#product for all crops,and that the mass of C added by crop residue is 47% of the mass of the crop residue 
#for all crops
NutrComp$Cropresidue_C<-470
NutrComp$Harvested.product_C<-470

#Merging with nutrients removed table by the new crop group
names(NutrComp)[names(NutrComp)=='Crop']<-'CropGroup'
VSAGNutrBudget2<-merge(VSAGNutrBudget2,NutrComp,by="CropGroup", all.x=TRUE)

#Making sure every obs with a crop group has nutrient removed values - CHECK
sum(!is.na(VSAGNutrBudget2$Crop.name) & is.na(VSAGNutrBudget2$Harvested.product_N))
sum(!is.na(VSAGNutrBudget2$CropGroup) & is.na(VSAGNutrBudget2$Harvested.product_N))
#There are 9 observations with crop names that do not fall into any of the categories in the table that 
#has nutrients removed by each type of crop, they will be carried through further manipulations to get the 
#nutrient budget as NA (Cashew nuts, black pepper, lemon, timber)

#Calculating the N, P, K, C removal in kg for each crop for each field
attach(VSAGNutrBudget2)
VSAGNutrBudget2$Nrmvdkg<-quantcropharvestedkg*0.001*Harvested.product_N
VSAGNutrBudget2$Prmvdkg<-quantcropharvestedkg*0.001*Harvested.product_P
VSAGNutrBudget2$Krmvdkg<-quantcropharvestedkg*0.001*Harvested.product_K
VSAGNutrBudget2$Crmvdkg<-quantcropharvestedkg*0.001*Harvested.product_C
detach(VSAGNutrBudget2)

#Calculating the N, P, K, C removal in kg/ha for each crop for each field
attach(VSAGNutrBudget2)
VSAGNutrBudget2$Nremovedkgperha<-(Nrmvdkg/areaplantedpercrop_ha)
VSAGNutrBudget2$Premovedkgperha<-(Prmvdkg/areaplantedpercrop_ha)
VSAGNutrBudget2$Kremovedkgperha<-(Krmvdkg/areaplantedpercrop_ha)
VSAGNutrBudget2$Cremovedkgperha<-(Crmvdkg/areaplantedpercrop_ha)
detach(VSAGNutrBudget2)

################################################################################################################
# N, P, K, & C ADDED BY ORGANIC & INORGANIC FERTILIZER CALCULATION
# OUTPUT: KGS OF N, P, K, & C ADDED PER HA FROM INORGANIC FERTILIZER AND ORGANIC FERTILIZER
# DATA FRAMES: VSAGNutrBudget3 (adding inorganic fertilizer nutrients), VSAGNutrBudget4 (adding organic
# fertilizer nutrients)
################################################################################################################

#Making quantities of inorganic fertilizer equal 0 (instead of NA) if no inorganic fertilizer was reported used
VSAGNutrBudget2$quantorgfert<-ifelse(is.na(VSAGNutrBudget2$quantorgfert),0,VSAGNutrBudget2$quantorgfert)
VSAGNutrBudget2$quantinorgfertA<-ifelse(is.na(VSAGNutrBudget2$quantinorgfertA),0,VSAGNutrBudget2$quantinorgfertA)
VSAGNutrBudget2$quantinorgfertB<-ifelse(is.na(VSAGNutrBudget2$quantinorgfertB),0,VSAGNutrBudget2$quantinorgfertB)

#ASSUMPTION - Assuming that mass of C added from organic fertilizer is 47% of the mass of organic fertilizer used
FertNutrComp$C<-ifelse(FertNutrComp$INPUTS=="ANIMAL MANURE",0.47,0)

#Merge with nutrient table for first batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='VSCode']<-'INPUTSA'
VSAGNutrBudget3<-merge(VSAGNutrBudget2, FertNutrComp, by="INPUTSA", all.x=TRUE)

#CHECK - Count NAs to see if merge for inorganic fertilizer A was correct
count(!(is.na(VSAGNutrBudget3$N)))
count(VSAGNutrBudget3$usedinorgfertA=="1")
count(!is.na(VSAGNutrBudget3$INPUTSA))

#Merge with nutrient table for second batch of inorganic fertilizers
names(FertNutrComp)[names(FertNutrComp)=='INPUTSA']<-'INPUTSB'
VSAGNutrBudget3<-merge(VSAGNutrBudget3, FertNutrComp, by="INPUTSB", all.x=TRUE)

#CHECK - Count NAs to see if merge for inorganic fertilizer B was correct
count(!(is.na(VSAGNutrBudget3$N.y)))
count(VSAGNutrBudget3$usedinorgfertB=="1")
count(!is.na(VSAGNutrBudget3$INPUTSB))

#Getting nutrients added (N, P, K, C) for inorganic fertilizer A
VSAGNutrBudget3$NaddedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$N.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$PaddedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$P.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$KaddedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$K.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$CaddedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$C.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$P205addedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$P205.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$PaddedkgInorgA<-VSAGNutrBudget3$PaddedkgInorgA+VSAGNutrBudget3$P205addedkgInorgA*0.4364
VSAGNutrBudget3$K20addedkgInorgA<-VSAGNutrBudget3$quantinorgfertA*VSAGNutrBudget3$K20.x*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$KaddedkgInorgA<-VSAGNutrBudget3$KaddedkgInorgA+VSAGNutrBudget3$K20addedkgInorgA*1.2046

#If no inorganic fertilizer A added, give value 0 instead of NA
VSAGNutrBudget3$NaddedkgInorgA<-ifelse(VSAGNutrBudget3$quantinorgfertA==0,0,VSAGNutrBudget3$NaddedkgInorgA)
VSAGNutrBudget3$PaddedkgInorgA<-ifelse(VSAGNutrBudget3$quantinorgfertA==0,0,VSAGNutrBudget3$PaddedkgInorgA)
VSAGNutrBudget3$KaddedkgInorgA<-ifelse(VSAGNutrBudget3$quantinorgfertA==0,0,VSAGNutrBudget3$KaddedkgInorgA)
VSAGNutrBudget3$CaddedkgInorgA<-ifelse(VSAGNutrBudget3$quantinorgfertA==0,0,VSAGNutrBudget3$CaddedkgInorgA)

#Getting nutrients added (N, P, K, C) for inorganic fertilizer B
VSAGNutrBudget3$NaddedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$N.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$PaddedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$P.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$KaddedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$K.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$CaddedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$C.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$P205addedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$P205.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$PaddedkgInorgB<-VSAGNutrBudget3$PaddedkgInorgB+VSAGNutrBudget3$P205addedkgInorgB*0.4364
VSAGNutrBudget3$K20addedkgInorgB<-VSAGNutrBudget3$quantinorgfertB*VSAGNutrBudget3$K20.y*(VSAGNutrBudget3$areaplantedpercrop_acre/VSAGNutrBudget3$totplotarea)
VSAGNutrBudget3$KaddedkgInorgB<-VSAGNutrBudget3$KaddedkgInorgB+VSAGNutrBudget3$K20addedkgInorgB*1.2046

#If no inorganic fertilizer B added, give value 0 instead of NA
VSAGNutrBudget3$NaddedkgInorgB<-ifelse(VSAGNutrBudget3$quantinorgfertB==0,0,VSAGNutrBudget3$NaddedkgInorgB)
VSAGNutrBudget3$PaddedkgInorgB<-ifelse(VSAGNutrBudget3$quantinorgfertB==0,0,VSAGNutrBudget3$PaddedkgInorgB)
VSAGNutrBudget3$KaddedkgInorgB<-ifelse(VSAGNutrBudget3$quantinorgfertB==0,0,VSAGNutrBudget3$KaddedkgInorgB)
VSAGNutrBudget3$CaddedkgInorgB<-ifelse(VSAGNutrBudget3$quantinorgfertB==0,0,VSAGNutrBudget3$CaddedkgInorgB)

#Summing nutrient addition per crop per field for inorganic fertilizers only
VSAGNutrBudget3$TotNaddedkgInorg<-VSAGNutrBudget3$NaddedkgInorgA+VSAGNutrBudget3$NaddedkgInorgB
VSAGNutrBudget3$TotPaddedkgInorg<-VSAGNutrBudget3$PaddedkgInorgA+VSAGNutrBudget3$PaddedkgInorgB
VSAGNutrBudget3$TotKaddedkgInorg<-VSAGNutrBudget3$KaddedkgInorgA+VSAGNutrBudget3$KaddedkgInorgB
VSAGNutrBudget3$TotCaddedkgInorg<-VSAGNutrBudget3$CaddedkgInorgA+VSAGNutrBudget3$CaddedkgInorgB

#Finding total nutrients by all inorganic fertilizers added in kg/ha
VSAGNutrBudget3$TotNaddedkghaInorg<-VSAGNutrBudget3$TotNaddedkgInorg/VSAGNutrBudget3$areaplantedpercrop_ha
VSAGNutrBudget3$TotPaddedkghaInorg<-VSAGNutrBudget3$TotPaddedkgInorg/VSAGNutrBudget3$areaplantedpercrop_ha
VSAGNutrBudget3$TotKaddedkghaInorg<-VSAGNutrBudget3$TotKaddedkgInorg/VSAGNutrBudget3$areaplantedpercrop_ha
VSAGNutrBudget3$TotCaddedkghaInorg<-VSAGNutrBudget3$TotCaddedkgInorg/VSAGNutrBudget3$areaplantedpercrop_ha

#ASSUMPTION - Here we're assuming that all types of organic fertilizers have the same nutrient composition, 
#equal to that of animal manure
names(FertNutrComp)[names(FertNutrComp)=='INPUTSB']<-'INPUTSC'
FertNutrComp$INPUTSC<-ifelse(FertNutrComp$INPUTSC==2,"ORGANIC FERTILIZER",FertNutrComp$INPUTSC)

#Merging with table with nutrition content of organic fertilizers
VSAGNutrBudget4<-subset(VSAGNutrBudget3, select=c(INPUTSB:INPUTS.x,INPUTS.y,
      NaddedkgInorgA:TotCaddedkghaInorg))
VSAGNutrBudget4$INPUTSC<-ifelse(VSAGNutrBudget4$usedorgfert==1,"ORGANIC FERTILIZER",NA)
VSAGNutrBudget4<-merge(VSAGNutrBudget4,FertNutrComp, by="INPUTSC", all.x=TRUE)

#CHECK MERGE to make sure that all obs that answered yes to used organic fertilizer were assigned a 
#nutrient composition
count(VSAGNutrBudget4$usedorgfert==1)
count(!is.na(VSAGNutrBudget4$N))

#Getting nutrient addition (N, P, K, C) for organic fertilizer in kgs
VSAGNutrBudget4$NaddedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$N*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$PaddedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$P*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$KaddedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$K*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$CaddedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$C*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$P205addedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$P205*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$PaddedkgOrg<-VSAGNutrBudget4$PaddedkgOrg + VSAGNutrBudget4$P205addedkgOrg*0.4364
VSAGNutrBudget4$K20addedkgOrg<-VSAGNutrBudget4$quantorgfert*VSAGNutrBudget4$K20*(VSAGNutrBudget4$areaplantedpercrop_acre/VSAGNutrBudget4$totplotarea)
VSAGNutrBudget4$KaddedkgOrg<-VSAGNutrBudget4$KaddedkgOrg + VSAGNutrBudget4$K20addedkgOrg*1.2046

#If no organic fertilizer used, giving nutrient added quantity a value of 0 instead of NA
VSAGNutrBudget4$NaddedkgOrg<-ifelse(VSAGNutrBudget4$quantorgfert==0,0,VSAGNutrBudget4$NaddedkgOrg)
VSAGNutrBudget4$PaddedkgOrg<-ifelse(VSAGNutrBudget4$quantorgfert==0,0,VSAGNutrBudget4$PaddedkgOrg)
VSAGNutrBudget4$KaddedkgOrg<-ifelse(VSAGNutrBudget4$quantorgfert==0,0,VSAGNutrBudget4$KaddedkgOrg)
VSAGNutrBudget4$CaddedkgOrg<-ifelse(VSAGNutrBudget4$quantorgfert==0,0,VSAGNutrBudget4$CaddedkgOrg)

#Finding nutrients added from organic fertilizer in kg/ha
VSAGNutrBudget4$NaddedkghaOrg<-VSAGNutrBudget4$NaddedkgOrg/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$PaddedkghaOrg<-VSAGNutrBudget4$PaddedkgOrg/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$KaddedkghaOrg<-VSAGNutrBudget4$KaddedkgOrg/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$CaddedkghaOrg<-VSAGNutrBudget4$CaddedkgOrg/VSAGNutrBudget4$areaplantedpercrop_ha

#Summing total nutrients added from inorganic and organic fertilizers in kg
VSAGNutrBudget4$TotNaddedkgAllFert<-VSAGNutrBudget4$TotNaddedkgInorg+VSAGNutrBudget4$NaddedkgOrg
VSAGNutrBudget4$TotPaddedkgAllFert<-VSAGNutrBudget4$TotPaddedkgInorg+VSAGNutrBudget4$PaddedkgOrg
VSAGNutrBudget4$TotKaddedkgAllFert<-VSAGNutrBudget4$TotKaddedkgInorg+VSAGNutrBudget4$KaddedkgOrg
VSAGNutrBudget4$TotCaddedkgAllFert<-VSAGNutrBudget4$TotCaddedkgInorg+VSAGNutrBudget4$CaddedkgOrg

#Calculating total nutrients added from inorganic and organic fertilizers in kg/ha
VSAGNutrBudget4$TotNaddedkghaAllFert<-VSAGNutrBudget4$TotNaddedkgAllFert/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$TotPaddedkghaAllFert<-VSAGNutrBudget4$TotPaddedkgAllFert/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$TotKaddedkghaAllFert<-VSAGNutrBudget4$TotKaddedkgAllFert/VSAGNutrBudget4$areaplantedpercrop_ha
VSAGNutrBudget4$TotCaddedkghaAllFert<-VSAGNutrBudget4$TotCaddedkgAllFert/VSAGNutrBudget4$areaplantedpercrop_ha

################################################################################################################
# N, P, K, & C ADDED BY CROP RESIDUES CALCULATION
# OUTPUT: KGS OF N, P, K, & C ADDED PER HA BY CROP RESIDUES
# DATA FRAMES: VSAGNutrBudget5 (adding nutrients from crop residues)
################################################################################################################

#Subsetting dataset to get ready for crop residue nutrient additions
VSAGNutrBudget5<-subset(VSAGNutrBudget4, select=c(INPUTSC:INPUTS,NaddedkgOrg:TotCaddedkghaAllFert))

#Setting up crop residue information from Section 5 to merge with other management information
names(VSAG5AB)[names(VSAG5AB)=='Household.ID']<-'y3_hhid'
names(VSAG5AB)[names(VSAG5AB)=='Crop.ID']<-'zaocode'
names(VSAG5AB)[names(VSAG5AB)=='Season']<-'season'
VSAG5AB$country_y3hhid_crop_season<-ifelse(is.na(VSAG5AB$Country)|is.na(VSAG5AB$y3_hhid)|
      is.na(VSAG5AB$zaocode)|is.na(VSAG5AB$season), NA, (do.call("paste", c(VSAG5AB[c('Country',
      'y3_hhid','zaocode','season')],sep="_"))))

#DATA CLEANING STEP - Checking for duplicates in Section 5
count(duplicated(VSAG5AB$country_y3hhid_crop_season))
which(duplicated(VSAG5AB$country_y3hhid_crop_season))

#DATA CLEANING STEP - Dropping duplicates in Section 5
VSAG5AB<-subset(VSAG5AB, !duplicated(VSAG5AB$country_y3hhid_crop_season))

#Selecting variable that says what crop residue was used for
VSAG5ABtrim<-subset(VSAG5AB, select=c(y3_hhid, zaocode, ag5a_24, season, Country))
names(VSAG5ABtrim)[names(VSAG5ABtrim)=='ag5a_24']<-'cropresuse'

#Merging crop residue use information with yield and input information
VSAGNutrBudget6<-merge(VSAGNutrBudget5, VSAG5ABtrim, by=c("Country","y3_hhid","zaocode","season"), all.x=TRUE)

#Finding the amount of residue produced per kg of crop harvested (according to its residue group)
HarvestIndices$CropResFactor<-(1-HarvestIndices$Harvest.Index)/HarvestIndices$Harvest.Index
VSAGNutrBudget7<-merge(VSAGNutrBudget6, HarvestIndices, by="CropResidueGroup", all.x = TRUE)
VSAGNutrBudget7$kgscropresidue<-VSAGNutrBudget7$quantcropharvestedkg*VSAGNutrBudget7$CropResFactor

#CHECK - Making sure all observations with a crop code has an associated crop residue group
count(is.na(VSAGNutrBudget7$CropGroup))
count(is.na(VSAGNutrBudget7$Harvest.Index))

#Finding the NPK available in every tonne of residue in kg
VSAGNutrBudget7$NaddedkgCropres<-ifelse(VSAGNutrBudget7$cropresuse==2 ,VSAGNutrBudget7$kgscropresidue*0.001*
    VSAGNutrBudget7$Cropresidue_N,0)
VSAGNutrBudget7$PaddedkgCropres<-ifelse(VSAGNutrBudget7$cropresuse==2 ,VSAGNutrBudget7$kgscropresidue*0.001*
    VSAGNutrBudget7$Cropresidue_P,0)
VSAGNutrBudget7$KaddedkgCropres<-ifelse(VSAGNutrBudget7$cropresuse==2 ,VSAGNutrBudget7$kgscropresidue*0.001*
    VSAGNutrBudget7$Cropresidue_K,0)
VSAGNutrBudget7$CaddedkgCropres<-ifelse(VSAGNutrBudget7$cropresuse==2 ,VSAGNutrBudget7$kgscropresidue*0.001*
    VSAGNutrBudget7$Cropresidue_C,0)

#Finding the NPK available in every tonne of residue in kg/ha
VSAGNutrBudget7$NaddedkghaCropres<-VSAGNutrBudget7$NaddedkgCropres/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$PaddedkghaCropres<-VSAGNutrBudget7$PaddedkgCropres/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$KaddedkghaCropres<-VSAGNutrBudget7$KaddedkgCropres/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$CaddedkghaCropres<-VSAGNutrBudget7$CaddedkgCropres/VSAGNutrBudget7$areaplantedpercrop_ha
################################################################################################################
# CALCULATING NUTRIENT BUDGETS
# OUTPUT: NUTRIENT BUDGETS IN KG AND KG/HA
# DATA FRAMES: VSAGNutrBudget5 
################################################################################################################
#Calculating nutrients added from all sources in kg
VSAGNutrBudget7$NaddedkgAllSrcs<-VSAGNutrBudget7$TotNaddedkgAllFert + VSAGNutrBudget7$NaddedkgCropres
VSAGNutrBudget7$PaddedkgAllSrcs<-VSAGNutrBudget7$TotPaddedkgAllFert + VSAGNutrBudget7$PaddedkgCropres
VSAGNutrBudget7$KaddedkgAllSrcs<-VSAGNutrBudget7$TotKaddedkgAllFert + VSAGNutrBudget7$KaddedkgCropres
VSAGNutrBudget7$CaddedkgAllSrcs<-VSAGNutrBudget7$TotCaddedkgAllFert + VSAGNutrBudget7$CaddedkgCropres

#Calculating nutrients added from all sources in kg/ha
VSAGNutrBudget7$NaddedkghaAllSrcs<-VSAGNutrBudget7$NaddedkgAllSrcs/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$PaddedkghaAllSrcs<-VSAGNutrBudget7$PaddedkgAllSrcs/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$KaddedkghaAllSrcs<-VSAGNutrBudget7$KaddedkgAllSrcs/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$CaddedkghaAllSrcs<-VSAGNutrBudget7$CaddedkgAllSrcs/VSAGNutrBudget7$areaplantedpercrop_ha

#Calculating nutrient budgets in kg
VSAGNutrBudget7$NutrBudgNkg<-VSAGNutrBudget7$NaddedkgAllSrcs - VSAGNutrBudget7$Nrmvdkg
VSAGNutrBudget7$NutrBudgPkg<-VSAGNutrBudget7$PaddedkgAllSrcs - VSAGNutrBudget7$Prmvdkg
VSAGNutrBudget7$NutrBudgKkg<-VSAGNutrBudget7$KaddedkgAllSrcs - VSAGNutrBudget7$Krmvdkg
VSAGNutrBudget7$NutrBudgCkg<-VSAGNutrBudget7$CaddedkgAllSrcs - VSAGNutrBudget7$Crmvdkg

#Calculating nutrient budgets in kg/ha
VSAGNutrBudget7$NutrBudgNkgha<-VSAGNutrBudget7$NutrBudgNkg/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$NutrBudgPkgha<-VSAGNutrBudget7$NutrBudgPkg/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$NutrBudgKkgha<-VSAGNutrBudget7$NutrBudgKkg/VSAGNutrBudget7$areaplantedpercrop_ha
VSAGNutrBudget7$NutrBudgCkgha<-VSAGNutrBudget7$NutrBudgCkg/VSAGNutrBudget7$areaplantedpercrop_ha

#CHECK- how many observations are we working with?
#all obs for which we have yield data - 894
sum(!is.na(VSAGNutrBudget7$cropyieldkgperha) & !is.na(VSAGNutrBudget7$zaocode))
#all obs for which we have yield + input data - 863
sum(!is.na(VSAGNutrBudget7$zaocode) & !is.na(VSAGNutrBudget7$cropyieldkgperacre) & 
      !is.na(VSAGNutrBudget7$usedorgfert))
#all obs for which we have yield + input + crop residue data - 816
sum(!is.na(VSAGNutrBudget7$zaocode) & !is.na(VSAGNutrBudget7$cropyieldkgperha) & 
      !is.na(VSAGNutrBudget7$usedorgfert) & !is.na(VSAGNutrBudget7$cropresuse))

#Subset for all field_crops in all seasons, only those with a yield, a crop code, and information about inputs
VSAGSumm1<-subset(VSAGNutrBudget7, VSAGNutrBudget7$areaplantedpercrop_ha!=0, 
  select=c(Country, zaocode,y3_hhid,plotnum,country_y3hhid_plotnum,Landscape, Data.entry.date, quantcropharvestedkg,
  areaplantedpercrop_ha,cropyieldkgperha,use,season,usedorgfert,usedinorgfertA,usedinorgfertB,anyirrigation,
  Nrmvdkg:Cremovedkgperha,TotNaddedkgInorg:TotCaddedkghaInorg,NaddedkgOrg:CaddedkgOrg, NaddedkghaOrg:CaddedkghaOrg,
  TotNaddedkgAllFert:TotCaddedkghaAllFert,NaddedkgCropres:CaddedkgCropres,NaddedkgAllSrcs:CaddedkghaAllSrcs,
  NutrBudgNkg:NutrBudgCkgha))
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
VSAGSummGHAabbrev<-subset(VSAGSummGHAMaizeLRSonly, select=c(Country, zaocode, y3_hhid, plotnum, 
    country_y3hhid_plotnum,Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,
    anyirrigation, Data.entry.year))

#Subsetting yield and input values for all of Tanzania
VSAGSummTZA<-subset(VSAGSumm1, Country=="TZA")
VSAGSummTZAMaizeLRSonly<-subset(VSAGSummMaizeLRSonly, Country=="TZA")
VSAGSummTZAabbrev<-subset(VSAGSummTZAMaizeLRSonly, select=c(Country, zaocode, y3_hhid, plotnum, 
      country_y3hhid_plotnum,Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,
      anyirrigation, Data.entry.year))

#Subsetting yield and input values for each landscape in Tanzania
#VSAGSummTZA_L10abbrev<-subset(VSAGSummTZAMaizeLRSonly, Landscape=="L10", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummTZA_L18abbrev<-subset(VSAGSummTZAMaizeLRSonly, Landscape=="L18", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))

#Subsetting yield and input values for each landscape in Ghana
#VSAGSummGHA_L04abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L04", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))
#VSAGSummGHA_L06abbrev<-subset(VSAGSummGHAMaizeLRSonly, Landscape=="L06", select=c(Country, zaocode, y3_hhid, plotnum, 
#      country_y3hhid_plotnum, Landscape, cropyieldkgperha,TotNaddedkghaInorg,TotPaddedkghaInorg,anyirrigation))

#Determining whether maize was cropped in 1 or 2 seasons for each household - All of Ghana
VSAGMaizeperYearGHA<-subset(VSAGSummMaizeonly, VSAGSummMaizeonly$Country=='GHA', select=c(country_y3hhid_plotnum, season, zaocode))
VSAGMaizeFreqbyplotGHA<-data.frame(table(VSAGMaizeperYearGHA$country_y3hhid_plotnum))
names(VSAGMaizeFreqbyplotGHA)[names(VSAGMaizeFreqbyplotGHA)=="Var1"]<-"country_y3hhid_plotnum"
names(VSAGMaizeFreqbyplotGHA)[names(VSAGMaizeFreqbyplotGHA)=="Freq"]<-"numcrops"

#Determining whether maize was cropped in 1 or 2 seasons for each household - Tanzania
VSAGMaizeperYearTZA<-subset(VSAGSummMaizeonly, VSAGSummMaizeonly$Country=='TZA', select=c(country_y3hhid_plotnum, season, zaocode))
VSAGMaizeFreqbyplotTZA<-data.frame(table(VSAGMaizeperYearTZA$country_y3hhid_plotnum))
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
VSAGSummLRSonly$maizeareaha<-ifelse(VSAGSummLRSonly$zaocode==11, VSAGSummLRSonly$areaplantedpercrop_ha, 0)
cols=c('areaplantedpercrop_ha','maizeareaha')
VSAGSummLRSonly$country_y3hhid<-ifelse(is.na(VSAGSummLRSonly$Country)|is.na(VSAGSummLRSonly$y3_hhid), NA,
                do.call("paste",c(VSAGSummLRSonly[c('Country','y3_hhid')],sep="_")))

#Determining % of total cultivated area that is devoted to maize for each household- for Ghana
VSAGSummGHALRSonly<-subset(VSAGSummLRSonly, Country=='GHA')
VSAGMaizePercentGHA<-VSAGSummGHALRSonly
VSAGMaizePercentGHA$country_y3hhid_plotnum<-NULL
VSAGMaizePercentGHA<-data.table(VSAGMaizePercentGHA)
VSAGMaizePercentGHA2<-VSAGMaizePercentGHA[, lapply(.SD,sum), by=country_y3hhid, .SDcols=cols]
VSAGMaizePercentGHA2$maizeareapercent<-VSAGMaizePercentGHA2$maizeareaha/VSAGMaizePercentGHA2$areaplantedpercrop_ha
VSAGMaizePercentGHA2<-as.data.frame(VSAGMaizePercentGHA2)

#Determining % of total cultivated area that is devoted to maize - for Tanzania (All)
VSAGSummTZALRSonly<-subset(VSAGSummLRSonly, Country=='TZA')
VSAGMaizePercentTZA<-VSAGSummGHALRSonly
VSAGMaizePercentTZA$country_y3hhid_plotnum<-NULL
VSAGMaizePercentTZA<-data.table(VSAGSummTZALRSonly)
VSAGMaizePercentTZA2<-VSAGMaizePercentTZA[, lapply(.SD,sum), by=country_y3hhid, .SDcols=cols]
VSAGMaizePercentTZA2$maizeareapercent<-VSAGMaizePercentTZA2$maizeareaha/VSAGMaizePercentTZA2$areaplantedpercrop_ha
VSAGMaizePercentTZA2<-as.data.frame(VSAGMaizePercentTZA2)

#Adding in Landscape codes and date of data entry
VSAG4ABmerge2<-subset(VSAG4ABmerge, !duplicated(VSAG4ABmerge$country_y3hhid))
VSAGMaizePercentGHA2<-merge(VSAGMaizePercentGHA2, VSAG4ABmerge2, by="country_y3hhid", all.x=TRUE)
VSAGMaizePercentGHA2$Data.entry.date<-NULL
VSAGMaizePercentTZA2<-merge(VSAGMaizePercentTZA2, VSAG4ABmerge2, by="country_y3hhid", all.x=TRUE)
VSAGMaizePercentTZA2$Data.entry.date<-NULL

#Removing excess data frames
rm(list=ls()[! ls() %in% c("VSAGSummGHAabbrev","VSAGMaizeFreqbyplotGHA","VSAGMaizePercentGHA2",
      "VSAGSummTZAabbrev","VSAGMaizeFreqbyplotTZA","VSAGMaizePercentTZA2","dir_data1","dir_data2","dir_data3")])

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
