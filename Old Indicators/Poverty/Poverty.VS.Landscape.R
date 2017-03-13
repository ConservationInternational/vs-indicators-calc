#############################
# Workflow - Poverty
# scale - Landscape
# 5 August 2015 - ongoing
#
# input the following modules from the VS Household Survey

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
library(VitalSignsUtilities)
library(reshape2)
library(plyr)

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
ci.repo$sourceRCode("Nutrition/VS.R")

# set name for output file
outfile <- "VS_Landscape"

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#################
# Loading
#################

# Read datasets 
hh_sec_a <- readVS(file = vstables$tables[["hh_secA.csv"]]$getData(), survey = "HH", section = "A")
hh_sec_b <- readVS(file = vstables$tables[["hh_secB.csv"]]$getData(), survey = "HH", section = "B")
hh_sec_k1 <- readVS(file = vstables$tables[["hh_secK1.csv"]]$getData(), survey = "HH", section = "K1")
hh_sec_l <- readVS(file = vstables$tables[["hh_secL.csv"]]$getData(), survey = "HH", section = "L")


tza_adulteq <- read.csv(vstables$tables[["AdultEquivalencies/TZA.adult.equivalents.csv"]]$getData(), stringsAsFactors = F)

gha_adulteq <- read.csv(vstables$tables[["AdultEquivalencies/GHA.adult.equivalents.csv"]]$getData(), stringsAsFactors = F)


pline <- read.csv(vstables$tables[["povertylines.csv"]]$getData(), stringsAsFactors = F)

cpi <- read.csv(vstables$tables[["World Bank WDI - national CPI.csv"]]$getData(), stringsAsFactors = F)

#################
# Staging
#################

# replace null values in adult eq tables with max age value
tza_adulteq$Age.upper[is.na(tza_adulteq$Age.upper)] <- 150
gha_adulteq$Age.upper[is.na(gha_adulteq$Age.upper)] <- 150

hh_sec_a <- subset(hh_sec_a, 
                   select = c(Country, Region, Landscape.., District, 
                              Household.ID, Data.entry.date))

# Sex, age
hh_sec_b <- subset(hh_sec_b, 
                   select = c(Household.ID, Individual.ID, hh_b02, hh_b03))

# household size
hhsize <- ddply(hh_sec_b, 
                .(Household.ID), 
                summarise, 
                hhsize = length(Household.ID))

# adult equivalent

#   calculate age in years
hh_sec_ab <- merge(hh_sec_a, hh_sec_b, 
                   by = "Household.ID", all = TRUE)

hh_sec_ab$hh_b03 <- as.Date(substr(hh_sec_ab$hh_b03, 1, 10))
hh_sec_ab$age <- floor((hh_sec_ab$Data.entry.date - hh_sec_ab$hh_b03) / 365.24)

hh_sec_ab$sex[hh_sec_ab$hh_b02 == 1] <- "Male"
hh_sec_ab$sex[hh_sec_ab$hh_b02 == 2] <- "Female"

hh_sec_ab$hh_b02 <- NULL

hh_sec_ab$adult_eq <- 1

hh_sec_ab$adult_eq <- adulteq(hh_sec_ab, tza_adulteq, country = "TZA")
hh_sec_ab$adult_eq <- adulteq(hh_sec_ab, gha_adulteq, country = "GHA")

adult_eq <- ddply(hh_sec_ab, 
                  .(Household.ID), 
                  summarise, 
                  adult_eq = sum(adult_eq, na.rm = TRUE))

# valued food consumption

food.cons <- foodcons(hh_sec_k1)

# remove question 1 from each nonfood item
#  we're cbinding the metadata columns(1-3) with every other subsequent column

nonfood.cons <- nonfoodcons(hh_sec_l)

# merge together
poverty <- merge(hhsize, adult_eq, by = "Household.ID", all = TRUE)
poverty <- merge(poverty, food.cons, by = "Household.ID", all = TRUE)
poverty <- merge(poverty, nonfood.cons, by = "Household.ID", all = TRUE)
poverty <- merge(poverty, hh_sec_a, by = "Household.ID", all = TRUE)

# get total consumption
poverty$expmR <- poverty$food_cons + poverty$nonfood_cons

# Tanzania National Panel Survey Wave 3, Final Report

# The total poverty line per adult equivalent per 28 days stands at 
# TSh. 23,933 at NPS2 prices, that is, prices from October 2010 to September 
# 2011. 

tza_pline <- pline$povertyline365[pline$Country == "TZA"]

# The Fisher food price index between the NPS 2010/11 and the NPS 2012/13 is 
# estimated at 1.34, that is, the cost of an average food bundle consumed in 
# the country increased by 34% between those two rounds of the NPS. This 
# inflation will be employed to adjust the consumption aggregate and the 
# poverty lines across the NPS 2010/11 and the NPS 2012/13.

tza_pline <- tza_pline * 1.34

# the inflation between 2012/13 to the time of the survey can be measured by
# deflating expenditure from the time of the survey to the time of the poverty
# line, using the available CPI.

tza.cpi <- cpi[cpi$Country.Code == "TZA", c("X2013..YR2013.", "X2014..YR2014.")]

ctr.pline <- as.data.frame(list(Country = "TZA", 
                                pline = deflate(tza.cpi[[2]], 
                                                tza_pline, 
                                                tza.cpi[[1]])))

# Poverty Profile in Ghana, 2005-2013

# A lower poverty line of 792.05 Ghana cedis per adult per year: this focuses
# on what is needed to meet the nutritional requirements of household members. 
# Individuals whose total expenditure falls below this line are considered to
# be in extreme poverty, since even if they allocated their entire budget to
# food, they would not be able to meet their minimum nutrition requirements
# (if they consume the average consumption basket).

gha_pline <- pline$povertyline365[pline$Country == "GHA"]

# the inflation between 2012/13 to the time of the survey can be measured by
# deflating expenditure from the time of the survey to the time of the poverty
# line, using the available CPI.

gha.cpi <- cpi[cpi$Country.Code == "GHA", c("X2013..YR2013.", "X2014..YR2014.")] 

ctr.pline <- rbind(ctr.pline,
                   as.data.frame(list(Country = "GHA", 
                                      pline = deflate(gha.cpi[[2]], 
                                                      gha_pline, 
                                                      gha.cpi[[1]]))))

#################
# Analysis
#################

# create per capita income metric
poverty$inc <- poverty$expmR / poverty$adult_eq

# drop if missing
poverty <- poverty[!is.na(poverty$inc), ]

poverty.df <- as.data.frame(list(country = NULL,
                                 scale = NULL,
                                 year = NULL,
                                 landscape = NULL,
                                 pc_cons_2011 = NULL,
                                 headcount = NULL,
                                 poverty_gap = NULL))

for (ctr in c("TZA", "GHA")) {
  
  # ISSUE34 need to add landscape loop when landscape data gets added
  for (land in unique(poverty[poverty$Country == ctr, "Landscape.."])) {
    
    poverty_ls <- poverty[poverty$Country == ctr & poverty$Landscape.. == land, ]
    
    # get average year of survey enumeration
    yr <- format(round(mean(poverty_ls$Data.entry.date)), "%Y")
    
    # mean per capita income, weighted by household size and sampling weights
    pc_cons <- apply_ef(poverty_ls, col = "inc", house = TRUE, hhsize = "hhsize",
                        ef = "", weights = FALSE, func = "mean")
    
    # deflate pc_cons to 2011 national currency level
    # note: cpi data not available for 2015
    pc_cons <- deflate(base_x = cpi[cpi$Country.Code == ctr, "X2014"],
                       base_y = cpi[cpi$Country.Code == ctr, "X2011"],
                       nominal_x = pc_cons)
    
    # headcount ratio, weighted by household size and sampling weights
    headcount <- fgt(poverty_ls, inc = "inc", 
                     pline = ctr.pline[ctr.pline$Country == ctr, "pline"], 
                     x = 0, 
                     hhsize = "hhsize", 
                     ef = "", 
                     weights = FALSE)
    
    # headcount ratio, weighted by household size and sampling weights
    poverty_gap <- fgt(poverty_ls, inc = "inc", 
                       pline = ctr.pline[ctr.pline$Country == ctr, "pline"], 
                       x = 1, 
                       hhsize = "hhsize", 
                       ef = "", 
                       weights = FALSE)
    
    #################
    # Format Output
    #################
    
    poverty.df1 <- as.data.frame(list(country = ctr,
                                      scale = "Landscape",
                                      year = yr,
                                      landscape = land,
                                      pc_cons_2011 = max(pc_cons, 0),
                                      headcount = headcount,
                                      poverty_gap = poverty_gap))
    
    poverty.df <- rbind(poverty.df, poverty.df1)
  }
}

poverty.df <- poverty.df[!is.na(poverty.df$country),]

coord_alias <- read.csv(vstables$tables[["coordinates_landscapes__wgs_84.csv"]]$getData(), stringsAsFactors = F)
poverty.df.out <- merge(poverty.df,
                        coord_alias,
                        by.x=c("country", "landscape"), by.y=c("Country", "Landscape.."), all.x = T)

poverty.df.final <- poverty.df.out[, c("country",
                                       "scale",
                                       "year",
                                       "landscape",
                                       "pc_cons_2011",
                                       "headcount",
                                       "poverty_gap",
                                       "wkt_geom",
                                       "Center_X",
                                       "Center_Y")]


out_filepath <- sprintf("Poverty_%s.csv", outfile)
s3 <- newS3()
write.csv(poverty.df.final, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Poverty/", out_filepath), TRUE)

