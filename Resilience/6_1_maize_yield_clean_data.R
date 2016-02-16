#---------------------------
# Ravic Nijbroek
# November 7, 2013
#---------------------------
# Read in Living Standards Measurement Study (LSMS) Integrated Survey on Agriculture (ISA) data files
# downloaded from The World Bank website on September 6, 2013
#---------------------------

source('0_settings.R')

library(foreign)
library(sp)
library(dplyr)


#------------------------------------
#TABLE1: Read in gov't extension service data
#------------------------------------

sec12A <- read.dta(file.path(lsms_folder, "TZNPS2AGRDTA", "AG_SEC12A.dta"))

#keep: ag12a_01 (= was advice received), sourceid (= source of the advice), ag12a_02_1 (= agricultural production)
#ag12a_02_2 (= agricultural processing), ag12a_03 (= rating of the advice received)

extens <- sec12A[ , c("y2_hhid", "sourceid", "ag12a_01", "ag12a_02_1", "ag12a_02_2", "ag12a_03")]

#Add a variable that will either be a 1 (good or average government extension advice) or 0 (no or bad advice)
extens[ , "var1"] <- 0

#-------------------------
#Clean up table and missing values
#-------------------------

###### UPDATE: Look to see if any sourceid is significant
###### REMOVE criteria for Agricultural processing

#Cannot have missing values for type of advice received, make them NO. They are only missing where No advice received
extens[is.na(extens$ag12a_02_1), "ag12a_02_1"] <- "NO"
extens[is.na(extens$ag12a_02_2), "ag12a_02_2"] <- "NO"
extens[is.na(extens$ag12a_03), "ag12a_03"] <- "BAD"

#If advice was from government extension AND it was for ag production OR for ag processing then make var1 = 1
#extens[extens$ag12a_01 == "YES" & extens$ag12a_02_1 == "YES" | extens$ag12a_02_2 == "YES", "var1"] <- 1
extens[extens$ag12a_01 == "YES" & extens$ag12a_02_1 == "YES" & !(extens$ag12a_03 == "BAD"), "var1"] <- 1

#table(extens$var1)
#extens[extens[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401"), ]

#-------------------------
#NEW Add columns for each type of extension service
#-------------------------
extens[ ,"GOVT"] <- 0
extens[ ,"NGO"] <- 0
extens[ ,"COOP"] <- 0
extens[ ,"PRIVATE"] <- 0
extens[ ,"OTHER"] <- 0

extens[extens$sourceid == "GOVERNMENT EXTENSION"             & extens$var1 == 1, "GOVT"]    <- 1
extens[extens$sourceid == "NGO"                              & extens$var1 == 1, "NGO"]     <- 1
extens[extens$sourceid == "COOPERATIVE/FARMER'S ASSOCIATION" & extens$var1 == 1, "COOP"]    <- 1
extens[extens$sourceid == "LARGE SCALE FARMER"               & extens$var1 == 1, "PRIVATE"] <- 1
extens[extens$sourceid == "OTHER"                            & extens$var1 == 1, "OTHER"]   <- 1

#extens[extens[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401","0101014002076101"), ]

sum_extens <-  group_by(extens, y2_hhid) %>%
    summarize(GOVT=sum(GOVT),
              NGO=sum(NGO),
              COOP=sum(COOP),
              PRIVATE=sum(PRIVATE),
              OTHER=sum(OTHER))

#-------------------------
#TABLE2: Get Yields
#-------------------------

# keep y2_hhid, plotnum, ag4a_04 (= was crop intercropped), ag4a_06 (= was crop harvested in long rainy season)#
# ag4a_08 (= area harvested in acres), ag4a_15 (= quantity harvested in KGs), ag4a_19 (= did you purchase any seeds)

sec4A <- read.dta(file.path(lsms_folder, "TZNPS2AGRDTA", "AG_SEC4A.dta"))

yield <- sec4A[ , c("y2_hhid","plotnum","ag4a_04","ag4a_06","ag4a_08","ag4a_15","ag4a_19")]

yield[yield[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401","0101014002017101"), ]


#----------------------------
#exclude intercropped fields
#keep plots where crop was harvested in long rainy season (ag4a_06 = YES)
#----------------------------

yield <- subset(yield, ag4a_04 == "NO")
yield <- subset(yield, ag4a_06 == "YES")

#Calculate yield as kg per ha
yield[ ,"yield_kg_ha"] = (yield[ ,"ag4a_15"] / yield[ ,"ag4a_08"]) * 2.47105       #1ha = 2.47105 acres

#Note: some hh's have multiple M1 fields with varying crop yields on each
#i.e. hhid 0101014002017101 has two M1 fields. one has 1977 kg/ha and the other has 2372 kg/ha

#Only keep necessary variables
yield <- yield[ , c("y2_hhid","plotnum","ag4a_19","yield_kg_ha")]

#Note that 32701 did not grow maize and is excluded from table
yield[yield[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401"), ] 

#-------------------------------------------------------------
#Some parts of the same field measure different yields. See output for 17101:

#             y2_hhid plotnum ag4a_04 ag4a_06 ag4a_08 ag4a_15 ag4a_19
#1   0101014002017101      M1      NO     YES    0.25     200      NO
#2   0101014002017101      M1      NO     YES    0.25     240      NO
#3   0101014002017101      M2      NO     YES    0.25     100      NO

# Therefore, calculate mean by plotnum

yield_new <- group_by(yield, y2_hhid, plotnum, ag4a_19) %>%
    summarize(yield_kg_ha=mean(yield_kg_ha)) %>%
    arrange(y2_hhid, plotnum, ag4a_19)

#yield[yield[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401","0103013004074501"), ] 

#Different combinations are possible for each household:
#Multiple plots, same seed application (ex: 76101 has M1 and M2 with NO seeds purchased for both)
#Multiple plots, different seed application (ex: 74501 has M1 with seeds purchased and M2 with no seeds purchased)


#----------------------
#TABLE4: merge tables farm management (crops) and crop yield (yield)
#----------------------

#keep y2_hhid, plotnum, ag3a_03 (= was plot cultivated), zaocode (= crop code, keep MAIZE only)#
#ag3a_10 (= soil quality: GOOD, AVERAGE, BAD), ag3a_17 (= irrigated: YES, NO), 
#ag3a_39 (=organic fertilizer: YES, NO), ag3a_45 (=inorganic fertilizer: YES,NO)

sec3A <- read.dta(file.path(lsms_folder, "TZNPS2AGRDTA", "AG_SEC3A.dta"))

#crops[crops[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401"), ] #two plots for 76101

crops_maize <- subset(sec3A, zaocode == "Maize") #This automatically keep cultivated 
crops_maize <- crops_maize[ , c("y2_hhid","plotnum","ag3a_10", "ag3a_17","ag3a_39","ag3a_45")]
cropyield_maize <- merge(crops_maize, yield, by = c("y2_hhid","plotnum"), all.crops = T)
cropyield_maize <- cropyield_maize[!(is.na(cropyield_maize$ag3a_10)), ]           #keep only records from crops table
cropyield_maize <- cropyield_maize[!(is.na(cropyield_maize$yield_kg_ha)), ]       #keep only records from crops table
names(cropyield_maize)[names(cropyield_maize) == "yield_kg_ha"] <- "yield_kg_ha_maize"

#cropyield[cropyield[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401","0103013004074501"), ]

# merge tables: farmers who received ext assistance (sum_extens) and (cropyield)
#
# table.extens has list of hh who received good or average advice and those that did not
# table.cropyield has yield (kg/ha) for farmers that planted maize in the large rainy season
yieldext <- merge(sum_extens, cropyield_maize, by = "y2_hhid", all=T)

#yieldext[yieldext[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0402012002008401","0103013004074501"), ]

#-----------------------------------------------------------
#Get region and district to control for these variables
#-----------------------------------------------------------


#---------------------------
#HH_SEC_A.dta has y2_hhid, clusterid, strataid, region, district, and ward
#keep y2_hhid, region, district

secA <- read.dta(file.path(lsms_folder, "TZNPS2HH1DTA", "HH_SEC_A.dta"))
all_regions <- secA[ ,c("y2_hhid","y2_rural","region","district")] 

all_regions[all_regions$region == 1, "Region"]  <- "Dodoma"
all_regions[all_regions$region == 2, "Region"]  <- "Arusha"			##Not in SAGCOT
all_regions[all_regions$region == 3, "Region"]  <- "Kilimanjaro"			##
all_regions[all_regions$region == 4, "Region"]  <- "Tanga"
all_regions[all_regions$region == 5, "Region"]  <- "Morogoro"
all_regions[all_regions$region == 6, "Region"]  <- "Pwani"
all_regions[all_regions$region == 7, "Region"]  <- "Dar es Salaam"
all_regions[all_regions$region == 8, "Region"]  <- "Lindi"
all_regions[all_regions$region == 9, "Region"]  <- "Mtwara"			##
all_regions[all_regions$region == 10, "Region"] <- "Ruvuma"
all_regions[all_regions$region == 11, "Region"] <- "Iringa"
all_regions[all_regions$region == 12, "Region"] <- "Mbeya"
all_regions[all_regions$region == 13, "Region"] <- "Singida"
all_regions[all_regions$region == 14, "Region"] <- "Tabora"
all_regions[all_regions$region == 15, "Region"] <- "Rukwa"
all_regions[all_regions$region == 16, "Region"] <- "Kigoma"			##
all_regions[all_regions$region == 17, "Region"] <- "Shinyanga"			##
all_regions[all_regions$region == 18, "Region"] <- "Kagera"			##
all_regions[all_regions$region == 19, "Region"] <- "Mwanza"			##
all_regions[all_regions$region == 20, "Region"] <- "Mara"				##
all_regions[all_regions$region == 21, "Region"] <- "Manyara"			##
all_regions[all_regions$region == 51, "Region"] <- "Kaskazini Unguja"		##
all_regions[all_regions$region == 52, "Region"] <- "Kusini Unguja"		##
all_regions[all_regions$region == 53, "Region"] <- "Mjin/Magharibi Unguja"	##
all_regions[all_regions$region == 54, "Region"] <- "Kaskazini Pemba"		##
all_regions[all_regions$region == 55, "Region"] <- "Kusini Pemba"		##

all_regions <- all_regions[ ,c("y2_hhid","y2_rural","district","Region")] 

#---------------------------
#get household size for each household
#---------------------------
secB <- read.dta(file.path(lsms_folder, "TZNPS2HH1DTA", "HH_SEC_B.dta"))
hhsize <- group_by(secB, y2_hhid) %>% summarise(hh_size=sum(hh_b09_1 == "Yes"))

all_regions <- secA[ ,c("y2_hhid","y2_rural","region","district")] 

#---------------------------
#merge tables (yieldext) and regional information (all_regions)
#---------------------------

fnlyield <- merge(yieldext, hhsize, by = "y2_hhid")
fnlyield <- merge(fnlyield, all_regions, by = "y2_hhid")

fnlyield[fnlyield[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0202012001018501","0103013004074501"), ]

#-----------------------------------------------------------
#TABLE%: Get education level for head of household
#-----------------------------------------------------------

secC <- read.dta(file.path(lsms_folder, "TZNPS2HH1DTA", "HH_SEC_C.dta"))
educ <- secC[ ,c("y2_hhid","indidy2","hh_c07")]
educ <- subset(educ, indidy2 == 1)

#Remove missing (NA) values
educ2 <- educ[!(is.na(educ$hh_c07)), ]

educ2[educ2$hh_c07 %in% c("D1","D2","D3","D4","D5","D6","D7","D8","PREFORM 1","MS+COURSE"), "Level"] <- "Primary"
educ2[educ2$hh_c07 %in% c("F1","F2","F3","F4","'O'+COURSE","F5","F6","'A'+COURSE","DIPLOMA"), "Level"] <- "Secondary"
educ2[educ2$hh_c07 %in% c("U1","U2","U3","U4","U5&+"), "Level"] <- "University"
educ2[is.na(educ2$Level), "Level"] <- "Other"

educ2 <- educ2[ ,c("y2_hhid","Level")] 

#----------------------------
#Merge with all of Tanzania crop yield
#----------------------------

yld <- merge(educ2, fnlyield, by = "y2_hhid", all = T)

#Remove missing (NA) values
yld <- yld[!(is.na(yld$GOVT)), ]

yld[yld[ ,"y2_hhid"] %in% c("0101014002076101","0204001001032701","0202012001018501","0103013004074501"), ]

#-------------------
#remove outliers
#-------------------

yld$yield_kg_ha_maize[yld$yield_kg_ha_maize > 10000] <- NA

#yld[yld[ ,"y2_hhid"] %in% 
#c("0101014002076101","0204001001032701","0202012001018501","0103013004074501"), 
#]

#---------------------------
#Run statistics
#---------------------------

#independent variables:
#Level	Education level of head of household (Primary, Secondary, University or Other)
#GOVT 	Advice from Government Extension (0 or 1)
#NGO 		Advice from NGO (0 or 1)
#COOP 	Advice from farmer's cooperative (0 or 1)
#PRIVATE 	Advice from large farm (0 or 1)
#OTHER	Advice from other source (0 or 1)
#ag3a_10 	soil quality (GOOD, AVERAGE, BAD)
#ag3a_17 	irrigated (YES, NO)
#ag3a_39 	organic fertilizer (YES, NO)
#ag3a_45 	inorganic fertilizer (YES,NO)
#ag4a_19 	did you purchase any seeds (YES, NO)
#dependent variable: yield_kg_ha


colnames(yld)[2]  = "Educ.Level"     #Level
colnames(yld)[9]  = "Soil.Quality"   #ag3a_10
colnames(yld)[10] = "Irrigation"     #ag3a_17
colnames(yld)[11] = "Org.Fert"       #ag3a_39
colnames(yld)[12] = "InOrg.Fert"     #ag3a_45
colnames(yld)[13] = "Seed.Purchased" #ag4a_19

yld$Soil.Quality <- relevel(yld$Soil.Quality, ref = "BAD")
yld$Irrigation   <- relevel(yld$Irrigation, ref = "NO")
yld$Org.Fert     <- relevel(yld$Org.Fert, ref = "NO")
yld$InOrg.Fert   <- relevel(yld$InOrg.Fert, ref = "NO")

################################################################################
################################################################################
### Merge precip data
################################################################################
################################################################################

load("hh_spi12.RData")
hh_spi12 <- hh_spi
names(hh_spi12)[names(hh_spi12) == "SPI"] <- "SPI12"
hh_spi$SPI12 <- hh_spi12$SPI12

hh_spi$HHID <- factor(as.character(hh_spi$HHID))

yld$resp_ID <- substr(yld$y2_hhid, nchar(yld$y2_hhid) - 1, nchar(yld$y2_hhid))
yld$hh_ID <- substr(yld$y2_hhid, 1, nchar(yld$y2_hhid) - 2)

table(yld$hh_ID %in% hh_spi$HHID)
table(table(yld$hh_ID))

#hh_spi$hh_ID <- with(hh_spi, sprintf("%i%03i%03i%03i%04i", CYCLE, DISTRICT, 
#WARD, EA, HHNO))

hh_spi_oct2009 <- filter(hh_spi, date == as.Date("2009/10/1"))

dim(yld)
yld <- merge(yld, hh_spi_oct2009, by.x="hh_ID", by.y="HHID", all.x=TRUE)
dim(yld)

save(yld, file=file.path(intermediate_folder, paste0(ISO3, '_merged_yielddata.RData')))
