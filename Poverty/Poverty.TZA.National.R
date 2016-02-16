#############################
# Workflow - Poverty
# scale - National, Tanzania
# 2 August 2015 - ongoing
#
# input the following modules from the 2012-2013 LSMS from Tanzania
#   Consumption_NPS3, 
#   hh_sec_a, 

# calculate the four nutrition indicators:
#   FGT_0 Headcount Ratio
#   FGT_1 Poverty Gap
#   Mean Per Capita Consumption, 2011 Real national currency
#
# output the 3 indicators at the given scale for the given time period
#############################

#################
# Setup
#################
# Utility package for CI Vital Signs project
library(foreign)
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

# source Poverty functions
ci.repo$sourceRCode("Poverty/Poverty.R")

# set name for output file
outfile <- "TZA_National"

#################
# Loading
#################

# Date of interview
hh_sec_a <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_A.dta"]]$getData(), stringsAsFactors = F)

# Household Consumption
hh_cons <- read.dta(vstables$tables[["TZA LSMS/NPS 2013/TZA_2012_LSMS_v01_M_STATA_English_labels/ConsumptionNPS3.dta"]]$getData(), stringsAsFactors = F)

pline <- read.csv(vstables$tables[["povertylines.csv"]]$getData(), stringsAsFactors = F)

cpi <- read.csv(vstables$tables[["World Bank WDI - national CPI.csv"]]$getData(), stringsAsFactors = F)

#################
# Staging
#################

# region, district, sampling weights
hh_sec_a <- subset(hh_sec_a, 
                   select = c(y3_hhid, hh_a01_1, hh_a02_2, strataid, y3_weight))
# monthly expenditures, hhsize, adult equivalents
hh_cons <- subset(hh_cons, 
                   select = c(y3_hhid, expmR, hhsize, adulteq))

# Merge datasets into one "poverty" dataset
poverty <- merge(hh_sec_a, hh_cons, by = "y3_hhid", all = TRUE)

# remove missing values for monthly expenditure
poverty <- poverty[!is.na(poverty$expmR),]

# Tanzania National Panel Survey Wave 3, Final Report

# The total poverty line per adult equivalent per 28 days stands at 
# TSh. 23,933 at NPS2 prices, that is, prices from October 2010 to September 
# 2011. 

pline <- pline$povertyline365[pline$Country == "TZA"]

# The Fisher food price index between the NPS 2010/11 and the NPS 2012/13 is 
# estimated at 1.34, that is, the cost of an average food bundle consumed in 
# the country increased by 34% between those two rounds of the NPS. This 
# inflation will be employed to adjust the consumption aggregate and the 
# poverty lines across the NPS 2010/11 and the NPS 2012/13.

pline <- pline * 1.34

#################
# Analysis
#################

# create per capita income metric
poverty$inc <- poverty$expmR / poverty$adulteq

# mean per capita income, weighted by household size and sampling weights
pc_cons <- apply_ef(poverty, col = "inc", house = TRUE, hhsize = "hhsize",
                    ef = "y3_weight", weights = TRUE, func = "mean")

# deflate pc_cons to 2011 national currency level
pc_cons <- deflate(base_x = cpi[cpi$Country.Code == "TZA", "X2013"],
                   base_y = cpi[cpi$Country.Code == "TZA", "X2011"],
                   nominal_x = pc_cons)

# headcount ratio, weighted by household size and sampling weights
headcount <- fgt(poverty, inc = "inc", pline, x = 0, hhsize = "hhsize", 
                 ef = "y3_weight", weights = TRUE)

# headcount ratio, weighted by household size and sampling weights
poverty_gap <- fgt(poverty, inc = "inc", pline, x = 1, hhsize = "hhsize", 
                   ef = "y3_weight", weights = TRUE)
  
#################
# Format Output
#################

poverty.df <- as.data.frame(list(country = "TZA",
                             scale = "National",
                             year = "2013",
                             landscape = NA,
                             pc_cons_2011 = max(pc_cons, 0),
                             headcount = headcount,
                             poverty_gap = poverty_gap))

out_filepath <- sprintf("Poverty_%s.csv", outfile)

s3 <- newS3()
write.csv(poverty.df, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Archive/Poverty/", out_filepath), TRUE)

