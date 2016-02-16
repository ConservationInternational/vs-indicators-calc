#############################
# Workflow - Nutrition
# scale - National, Tanzania
# 24 July 2015 - ongoing
#
# input the following modules from the 2012-2013 LSMS from Tanzania
#   hh_sec_a, 
#   hh_sec_b, 
#   hh_sec_v

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

# Load the github repo object
ci.repo <- checkGithubToken()



# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
# source nutrition functions
ci.repo$sourceRCode("Nutrition/Nutrition.R")
# source WHO functions
ci.repo$sourceRCode("Nutrition/igrowup_standard.r")

# set name for output file
outfile <- "TZA_National"

#################
# Loading
#################

# Date of interview
hh_sec_a <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_A.dta"]]$getData())
# Household, individual ID, Sex, birthdate
hh_sec_b <-  read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_B.dta"]]$getData())
# Height, Weight, Upper arm circumference, Measure
hh_sec_v <-  read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_V.dta"]]$getData())


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

#################
# Staging
#################

# Date of interview: day=hh_a18_1; month=hh_a18_2; year=hh_a18_3
hh_sec_a <- subset(hh_sec_a, 
                   select = c(y3_hhid, hh_a18_1, hh_a18_2,hh_a18_3, y3_weight))
# sex = hh_b02, year of birth=hh_b03_1, month of birth=hh_b03_2
hh_sec_b <- subset(hh_sec_b, 
                   select = c(y3_hhid, indidy3, hh_b02, hh_b03_1, hh_b03_2))
# weight=hh_v03; lenhei=hh_v04;  armc=hh_v07; measure=hh_v05 
hh_sec_v <- subset(hh_sec_v, 
                   select = c(y3_hhid, indidy3, hh_v03, hh_v04, hh_v07, hh_v05))

# Merge datasets into one "nutrition" dataset
nutrition <- merge(hh_sec_a, hh_sec_b, by = "y3_hhid", all = TRUE)
nutrition <- merge(nutrition, hh_sec_v, by = c("y3_hhid", "indidy3"))

rm(hh_sec_a, hh_sec_b, hh_sec_v)

# Household and individual ID
# N23. Gender (M=1, F=2)
nutrition$sex <- as.integer(nutrition$hh_b02)

nutrition$hh_b02 <- NULL

# N24. Age

nutrition$interviewmonth <- as.integer(as.factor(nutrition$hh_a18_2))

# remove DON'T KNOWS for birth month
nutrition <- nutrition[nutrition$hh_b03_2 != "DON'T KNOW",]
nutrition$birthmonth <- as.integer(nutrition$hh_b03_2)

# age in months
nutrition$age <- (nutrition$hh_a18_3 - nutrition$hh_b03_1) * 12 + 
  (nutrition$interviewmonth - nutrition$birthmonth)

# retain only age calculable
nutrition <- nutrition[!is.na(nutrition$age), ]
nutrition <- nutrition[nutrition$age <= 60, ]

# N25. Height / N26. Weight / N27. Upper arm circumference

# weight:hh_v03; lenhei:hh_v04;  armc: hh_v07; measure: hh_v05
names(nutrition)[names(nutrition) == "hh_v03"] <- "weight"
names(nutrition)[names(nutrition) == "hh_v04"] <- "lenhei"
names(nutrition)[names(nutrition) == "hh_v07"] <- "armc"

# measure:hh_v05
nutrition$measure[nutrition$hh_v05 == "STANDING"] <- "1"
nutrition$measure[nutrition$hh_v05 == "LYING DOWN"] <- "2"

vars <- c("age", "weight", "lenhei", "armc", "measure", "sex", "y3_weight", 
          "hh_a18_3")

nutrition <- nutrition[ , vars]

#################
# Analysis
#################

# execute WHO anthro prevalence
igrowup.standard(FilePath = getwd(),
                 FileLab = outfile,
                 mydf = nutrition,
                 sex = sex,
                 age = age,
                 age.month = T, 
                 weight = weight, 
                 lenhei = lenhei,
                 measure = measure, 
                 armc = armc,
                 sw = y3_weight)

#################
# Format Output
#################

nutrition.df <- out_nutrition(outpath = getwd(), 
              outfile = outfile, 
              wts = TRUE)

# get average year of survey enumeration
yr <- round(mean(nutrition[, "hh_a18_3"]))

meta <- list(country = "TZA", scale = "National", year = yr, landscape = "NA")

nutrition.df <- cbind(meta, nutrition.df)

out_filepath <- sprintf("Nutrition_%s.csv", outfile)

s3 <- newS3()
write.csv(nutrition_df.out, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Nutrition/", out_filepath), TRUE)

