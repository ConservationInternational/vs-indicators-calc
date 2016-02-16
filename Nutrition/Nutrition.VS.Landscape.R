#############################
# Workflow - Nutrition
# scale - VS, Landscape
# 29 July 2015 - ongoing
#
# input the following modules from the VS Server
#   hh_sec_a, 
#   hh_sec_b, 
#   hh_sec_u

# calculate the four nutrition indicators:
#   % stunted
#   % wasted
#   % underweight
#   % overweight
#
# output the 4 indicators at the given scale for the given time period
#############################

#################
# Setup
#################
# libraries
# Utility package for CI Vital Signs project
library(VitalSignsUtilities)
library(plyr)
library(zoo)

# Create instance of vital signs tables collector
arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Load the github repo object
ci.repo <- checkGithubToken()

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#################
# Staging
#################

# source nutrition functions
ci.repo$sourceRCode("Nutrition/Nutrition.R")
ci.repo$sourceRCode("Nutrition/VS.R")
# source WHO functions
ci.repo$sourceRCode("Nutrition/igrowup_standard.r")

# set name for output file
infile <- "VS_Landscape"
outfile <- "VS_Landscape"

# Read datasets and only keep variables for nutrition thread
hh_sec_a <- readVS(file = vstables$tables[["hh_secA.csv"]]$getData(), survey = "HH", section = "A")
hh_sec_b <- readVS(file = vstables$tables[["hh_secB.csv"]]$getData(), survey = "HH", section = "B")
hh_sec_u <- readVS(file = vstables$tables[["hh_secU.csv"]]$getData(), survey = "HH", section = "U")

# ISSUE32 need to add date of interview when it is added
# Date of interview 
hh_sec_a <- subset(hh_sec_a, 
                   select = c(Country, Region, Landscape.., District, Household.ID, 
                              Data.entry.date))

# Sex, age
hh_sec_b <- subset(hh_sec_b, 
                   select = c(Household.ID, Individual.ID, hh_b02, hh_b03))

# weight=hh_v03; lenhei=hh_v04;  armc=hh_v07; measure=hh_v05 
hh_sec_u <- subset(hh_sec_u, 
                   select = c(Household.ID, Individual.ID, 
                              u1_01, u2_01, u3_01, u4_01, u5_01, u6_01, u7_01))

# Restore reference data sets
weianthro <- read.table(vstables$tables[["WHO Anthro reference tables/weianthro.txt"]]$getData(), header = T)
lenanthro <- read.table(vstables$tables[["WHO Anthro reference tables/lenanthro.txt"]]$getData(), header = T)
bmianthro <- read.table(vstables$tables[["WHO Anthro reference tables/bmianthro.txt"]]$getData(), header = T)
hcanthro <- read.table(vstables$tables[["WHO Anthro reference tables/hcanthro.txt"]]$getData(), header = T)
acanthro <- read.table(vstables$tables[["WHO Anthro reference tables/acanthro.txt"]]$getData(), header = T)
ssanthro <- read.table(vstables$tables[["WHO Anthro reference tables/ssanthro.txt"]]$getData(), header = T)
tsanthro <- read.table(vstables$tables[["WHO Anthro reference tables/tsanthro.txt"]]$getData(), header = T)
wflanthro <- read.table(vstables$tables[["WHO Anthro reference tables/wflanthro.txt"]]$getData(), header = T)
wfhanthro <- read.table(vstables$tables[["WHO Anthro reference tables/wfhanthro.txt"]]$getData(), header = T)

# Merge datasets into one "nutrition" dataset
nutrition <- merge(hh_sec_a, hh_sec_b, 
                   by = "Household.ID", all = TRUE)
nutrition <- merge(nutrition, hh_sec_u, 
                   by = c("Household.ID", "Individual.ID"), all = TRUE)

rm(hh_sec_a, hh_sec_b, hh_sec_u)

# Household and individual ID
# N23. Gender (M=1, F=2)
nutrition$sex <- as.integer(nutrition$hh_b02)

nutrition$hh_b02 <- NULL

# N24. Age
nutrition$hh_b03 <- as.Date(substr(nutrition$hh_b03, 1, 10))
# ISSUE32 change to Date of the interview when added
nutrition$Data.entry.date <- as.Date(nutrition$Data.entry.date)

# age in months
# ISSUE32 change to Date of the interview when added
nutrition$age <- (as.yearmon(nutrition$Data.entry.date) - 
  as.yearmon(nutrition$hh_b03)) * 12

nutrition$intyr <- as.integer(format(nutrition$Data.entry.date, "%Y"))

# N25. Height / N26. Weight / N27. Upper arm circumference

names(nutrition)[names(nutrition) == "u2_01"] <- "weight"
names(nutrition)[names(nutrition) == "u3_01"] <- "lenhei"
names(nutrition)[names(nutrition) == "u5_01"] <- "armc"
names(nutrition)[names(nutrition) == "u4_01"] <- "measure"

nutrition$measure[nutrition$measure == "STANDING"] <- "1"
nutrition$measure[nutrition$measure == "LYING DOWN"] <- "2"

# ISSUE32 need to add date of interview when it is added
vars <- c("Country", "Region", "District", "Landscape..", 
          "Household.ID", "Individual.ID",  "intyr", "age", 
          "weight", "lenhei", "armc", "measure", "sex")

nutrition <- nutrition[ , vars]

#################
# Analysis
#################

nutrition_df <- data.frame(country = character(), 
                           scale = character(), 
                           year = integer(), 
                           landscape = integer(),
                           underweight = double(),
                           underweight.severe = double(),
                           stunting = double(),
                           stunting.severe = double(),
                           wasting = double(),
                           wasting.severe = double(),
                           overweight = double(),
                           overweight.severe =  double())

# execute WHO anthro prevalence for each landscape within each country

for (ctr in unique(nutrition$Country)) {
  
  for (land in unique(nutrition[nutrition$Country == ctr, "Landscape.."])) {
    
    nutrition_ls <- nutrition[nutrition$Country == ctr & nutrition$Landscape.. == land, ]
    
    outfile_slice <- paste(outfile, ctr, land, sep="_")
    nutrition.subset <- nutrition[nutrition$Country==ctr & 
                                    nutrition$Landscape.. == land,]
    igrowup.standard(FilePath = getwd(),
                   FileLab = outfile_slice,
                   mydf = nutrition.subset,
                   sex = sex,
                   age = age,
                   age.month = T, 
                   weight = weight, 
                   lenhei = lenhei,
                   measure = measure, 
                   armc = armc)
    
#################
# Format Output
#################
    
    # get average year of survey enumeration
    yr <- round(mean(nutrition.subset$intyr))
    print(outfile_slice)
    z_file <- sprintf("%s/%s_z_st.csv", getwd(), outfile_slice)
    print(read.csv(z_file, sep = ",", header = T))
    nutrition_df_slice <- out_nutrition(outpath = getwd(), 
                                        outfile = outfile_slice, 
                                        wts = FALSE)

meta <- list(country = ctr, scale = "Landscape", year = yr, landscape = land)

    nutrition_df_slice <- cbind(meta, nutrition_df_slice)
    
    nutrition_df <- rbind(nutrition_df, nutrition_df_slice)
  }
}

out_filepath <- sprintf("Nutrition_%s.csv", outfile)

# Load the landscape map and merge to output
#coord_alias <- vstables$tables[["coordinates_landscapes__wgs_84.csv"]]$getData(returnData = T)
coord_alias <- read.csv(vstables$tables[["coordinates_landscapes__wgs_84.csv"]]$getData(), stringsAsFactors = F)

nutrition_df$key <- with(nutrition_df, paste0(country, landscape))
coord_alias <- read.csv(vstables$tables[["coordinates_landscapes__wgs_84.csv"]]$getData(), stringsAsFactors = F)
coord_alias <- rename(coord_alias, replace = c("Landscape.." = "Landscape"))
coord_alias$key <- with(coord_alias, paste0(Country, Landscape))
nutrition_df.out <- merge(nutrition_df,
                        coord_alias,
                        by.x="key", by.y="key", all.x = T)
nutrition_df.final <- nutrition_df.out[, c("country",
                                           "scale",
                                           "year",
                                           "landscape",
                                           "underweight",
                                           "underweight.severe",
                                           "stunting",
                                           "stunting.severe",
                                           "wasting",
                                           "wasting.severe",
                                           "overweight",
                                           "obese",
                                           "wkt_geom",
                                           "Center_X",
                                           "Center_Y")]
# Loading the output files and saving them to S3

s3 <- newS3()
write.csv(nutrition_df.final, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Nutrition/", out_filepath), TRUE)
sapply(list.files()[grep(paste0("^", outfile), list.files())], function(X) {s3$writeS3("ci-vsindicators", source_path = X, paste0("Nutrition/supporting_files/", X), TRUE)})

#vstables$saveData(nutrition_df.out, out_title, "csv", "0B6qHMVAGJ36UYVl1OUxzUTBfYjg")
#nutrition.df.outfile <- paste0(vs_tempdir, "/", "Nutrition_", outfile, "_prev_st.csv")
#vstables$saveData(read.csv(nutrition.df.outfile, stringsAsFactors = F), nutrition.df.outfile, "csv", "0B6qHMVAGJ36UYVl1OUxzUTBfYjg")
#nutrition_z_st.df <- paste0(vs_tempdir, "/", "Nutrition_", outfile, "_z_st.csv")
#vstables$saveData(read.csv(nutrition_z_st.df, stringsAsFactors = F), nutrition_z_st.df, "csv", "0B6qHMVAGJ36UYVl1OUxzUTBfYjg") 

