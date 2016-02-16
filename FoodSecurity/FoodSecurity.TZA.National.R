#############################
# Workflow - Food Security
# scale - National
# 14 August 2015 - ongoing
#
# input the following modules from the 2012-2013 LSMS from Tanzania
#
# calculate the food security indicators:
#   food availability
#   food utilization
#   food access
#
# output the indicator at the given scale for the given time period
#############################

#################
# Setup
#################

# Package needed to import STATA files
library(VitalSignsUtilities)
library(foreign)
library(reshape2)
library(plyr)

# Create instance of vital signs tables collector
arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()

# Load the github repo object
ci.repo <- checkGithubToken()

# source Poverty functions
ci.repo$sourceRCode("Poverty/Poverty.R")

# set name for output file
outfile <- "TZA_National"

#################
# Loading
#################

# Metadata, sampling weights
hh_sec_a <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_A.dta"]]$getData())

# Household Food consumption
hh_sec_j1 <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_J1.dta"]]$getData())

# frequency of food group consumption
hh_sec_j3 <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_J3.dta"]]$getData())

# Household Consumption
hh_cons <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/ConsumptionNPS3.dta"]]$getData())

# Staple foods
staple <- read.csv(vstables$tables[["staple.csv"]]$getData(), stringsAsFactors = F)

#################
# Staging
#################

# region, district, sampling weights
hh_sec_a <- subset(hh_sec_a, 
                   select = c(y3_hhid, hh_a01_1, hh_a02_2, strataid, y3_weight))

# monthly expenditures, hhsize, adult equivalents
hh_cons <- subset(hh_cons, 
                  select = c(y3_hhid, foodbev, foodbevR, expmR, hhsize, adulteq))

# set staple itemcodes
staple$item <- toupper(staple$item)

# non-staple and valued nominal food consumption
hh_sec_j1 <- hh_sec_j1[hh_sec_j1$hh_j01 == "YES",]

nonstaple <- merge(staple[staple$non.staple == "Y",], 
                   hh_sec_j1,  by.x = "item", by.y = "itemcode")

nonstaple.cons <- ddply(nonstaple, 
                        .(y3_hhid), 
                        summarise, 
                        nonstaple = sum(hh_j04, na.rm = TRUE))

nonstaple.cons$nonstaple <- (nonstaple.cons$nonstaple / 7) * 365.24

hh_cons <- merge(hh_cons, 
                   nonstaple.cons,  by = "y3_hhid")


#################
# Analysis
#################

# FS 17 proxy gap assessment
gap <- mean(hh_cons$nonstaple / hh_cons$foodbev)

# FS 18 buffer assessment
buffer <- 1 - mean(hh_cons$foodbevR / (hh_cons$expmR))

# FS 19 food access score
access <- gap * buffer

# FS21.1 daily dietary diversity score
# sum p(consumed food item yesterday) as x / 7 for all x
hh_sec_j3$hh_j09_3 <- hh_sec_j3$hh_j09_3 / 7

daily <- ddply(hh_sec_j3, 
                  .(y3_hhid), 
                  summarise, 
                  daily = mean(hh_j09_3, na.rm = TRUE))

# FS21.2 weekly dietary diversity score
hh_sec_j3$hh_j09_3 <- hh_sec_j3$hh_j09_3 > 0 

weekly <- ddply(hh_sec_j3, 
               .(y3_hhid), 
               summarise, 
               weekly = mean(hh_j09_3, na.rm = TRUE))

# FS22 calc food utilization
utilization <- mean(daily$daily * 0.5 + weekly$weekly * 0.5)
    
#################
# Format Output
#################
  
foodsec.df <- as.data.frame(list(country = "TZA",
                             scale = "National",
                             year = 2013,
                             landscape = NA,
                             foodsec = NA,
                             availability = NA,
                             access = access,
                             utilization = utilization))

outfile <- sprintf("Foodsec_%s.csv", outfile)

s3 <- newS3()
write.csv(foodsec.df, outfile, row.names = F)
s3$writeS3("ci-vsindicators", source_path = outfile, paste0("Food_Security/", outfile), TRUE)

#vstables$saveData(foodsec.df, out_filepath, "csv", "0By1xoBEcyFy_VVVwa3JSZm5nRXM")

