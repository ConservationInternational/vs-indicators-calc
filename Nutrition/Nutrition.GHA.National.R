#############################
# Workflow - Nutrition
# scale - National, Ghana
# 24 July 2015 - ongoing
#
# input the following modules from the 2006 GLSS from Ghana
#   hh_sec_l, 
#   hh_sec_i

# note that 4 variables empty/missing have been attributed default values:
# weight, lenhei, measure, armc

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
# Utility package for CI Vital Signs project
library(VitalSignsUtilities)

# Create instance of vital signs tables collector
arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

# Load the github repo object
ci.repo <- checkGithubToken()

#################
# Staging
#################

# source WHO functions
ci.repo$sourceRCode("Nutrition/igrowup_standard.r")

# set name for output file
outfile <- "GHA_National"

# Date of interview
  #not available in the dataset
# Household (HID), individual ID (INDID), Sex (SEX)
# Sample weighting coefficient (WTA_HH)
hh_sec_l <- vstables$tables[["GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8/GHA_2005_L.dta"]]$getData(returnData = TRUE)
#Age (AGEM), Height (HEIGHT), Weight (WEIGHT), Upper arm circumference (?), Measure (?), 
hh_sec_i <- vstables$tables[["GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8/GHA_2005_I.dta"]]$getData(returnData = TRUE)

# sex = SEX
hh_sec_l <- subset(hh_sec_l, 
                   select = c(HID, INDID, SEX, WTA_HH))
# weight = WEIGHT; height = HEIGHT;  armc = ARMC ; measure = MEASURE; age (in years) = AGEM
hh_sec_i <- subset(hh_sec_i, 
                   select = c(HID, INDID, WEIGHT, HEIGHT, AGEM))

# Restore reference data sets
weianthro <- vstables$tables[["WHO Anthro reference tables/weianthro.txt"]]$getData(returnData = TRUE)
lenanthro <- vstables$tables[["WHO Anthro reference tables/lenanthro.txt"]]$getData(returnData = TRUE)
bmianthro <- vstables$tables[["WHO Anthro reference tables/bmianthro.txt"]]$getData(returnData = TRUE)
hcanthro <- vstables$tables[["WHO Anthro reference tables/hcanthro.txt"]]$getData(returnData = TRUE)
acanthro <- vstables$tables[["WHO Anthro reference tables/acanthro.txt"]]$getData(returnData = TRUE)
ssanthro <- vstables$tables[["WHO Anthro reference tables/ssanthro.txt"]]$getData(returnData = TRUE)
tsanthro <- vstables$tables[["WHO Anthro reference tables/tsanthro.txt"]]$getData(returnData = TRUE)
wflanthro <- vstables$tables[["WHO Anthro reference tables/wflanthro.txt"]]$getData(returnData = TRUE)
wfhanthro <- vstables$tables[["WHO Anthro reference tables/wfhanthro.txt"]]$getData(returnData = TRUE)

# Merge datasets into one "nutrition" dataset
nutrition <- merge(hh_sec_l, hh_sec_i, by = c("HID", "INDID"))

rm(hh_sec_l, hh_sec_i)

# Household and individual ID
# N23. Gender (M=1, F=2)
nutrition$sex[nutrition$SEX == "Male"] <- 1; nutrition$sex[nutrition$SEX == "Female"] <- 2
nutrition$SEX <- NULL

# N24. Age
nutrition$age <- nutrition$AGEM
nutrition$AGEM <- NULL

# N25. Height / N26. Weight / N27. Upper arm circumference

# height: data not publically available

# weight: data not publically available

# armc: not collected; 

# measure: not available; defaulted to "lying down" = 2
nutrition$measure <- 2

vars <- c("age", "weight", "lenhei", "armc", "measure", "sex", "WTA_HH")

nutrition <- nutrition[ , vars]

#################
# Analysis
#################

# execute WHO anthro prevalence
igrowup.standard(FilePath = "",
                 FileLab = outfile,
                 mydf = nutrition,
                 sex = sex,
                 age = age,
                 age.month = T, 
                 weight = weight, 
                 lenhei = lenhei,
                 measure = measure, 
                 sw = WTA_HH)

#################
# Format Output
#################

# Loading the output files and saving them to S3
s3 <- newS3()
write.csv(nutrition_df.out, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Nutrition/", out_filepath), TRUE)
# nutrition.df.outfile <- paste0(dir_output, outfile, "_prev_st.csv")
# vstables$saveData(read.csv(nutrition.df.outfile, stringsAsFactors = F), nutrition.df.outfile, "csv", "0B6qHMVAGJ36UYVl1OUxzUTBfYjg")
# nutrition_z_st.df <- paste0(outfile, "_z_st.csv")
# vstables$saveData(read.csv(nutrition_z_st.df, stringsAsFactors = F), nutrition_z_st.df, "csv", "0B6qHMVAGJ36UYVl1OUxzUTBfYjg") 
