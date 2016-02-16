#############################
# Workflow - Food Security
# scale - Landscape
# 14 August 2015 - ongoing
#
# input the following modules from the VS Household Survey
#
# calculate the food security indicators:
#   food utilization
#
# output the indicator at the given scale for the given time period
#############################

#################
# Setup
#################

# Package needed to import STATA files
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

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

# source functions
ci.repo$sourceRCode("Nutrition/VS.R")

# set name for output file
outfile <- "VS_Landscape"

#################
# Loading
#################

# Date of interview
hh_sec_a <- readVS(file = vstables$tables[["hh_secA.csv"]]$getData(), survey = "HH", section = "A")
hh_sec_k1 <- readVS(file = vstables$tables[["hh_secK1.csv"]]$getData(), survey = "HH", section = "K1")
hh_sec_k2 <- readVS(file = vstables$tables[["hh_secK2.csv"]]$getData(), survey = "HH", section = "K2")
hh_sec_l <- readVS(file = vstables$tables[["hh_secL.csv"]]$getData(), survey = "HH", section = "L")

# Staple foods
staple <- read.csv(vstables$tables[["staple.csv"]]$getData(), stringsAsFactors = F)

#################
# Staging
#################

# need to add landscape
hh_sec_a <- subset(hh_sec_a, 
                   select = c(Country, Region, District, Household.ID, Landscape..,
                              Data.entry.date))

# valued food consumption
hh_sec_k1 <- subset(hh_sec_k1,
                    select = c(Country, Household.ID, k_item, k_item_code, k_04, k_05a))

hh_sec_k2$Landscape.. <- NULL

food.cons <- foodcons(hh_sec_k1)

nonfood.cons <- nonfoodcons(hh_sec_l)

# valued non staple food consumption
hh_sec_k1$k_04[is.na(hh_sec_k1$k_04) ] <- 0
hh_sec_k1$k_05a[is.na(hh_sec_k1$k_05a) ] <- 0

nonstaple <- merge(staple[staple$non.staple == "Y",], 
                   hh_sec_k1,  by.x = "item", by.y = "k_item")

nonstaple.cons <- ddply(nonstaple, 
                        .(Household.ID), 
                        summarise, 
                        staple_cons = sum(k_04 + k_05a, na.rm = TRUE))

# merge together
foodsec <- merge(nonfood.cons, food.cons, by = "Household.ID", all = TRUE)
foodsec <- merge(foodsec, nonstaple.cons, by = "Household.ID", all = TRUE)
foodsec <- merge(foodsec, hh_sec_a, by = "Household.ID", all = TRUE)

food_util <- merge(hh_sec_k2, hh_sec_a, by = "Household.ID", all = TRUE)

#################
# Analysis
#################

foodsec.df <- as.data.frame(list(country = NULL,
                                 scale = NULL,
                                 year = NULL,
                                 landscape = NULL,
                                 foodsec = NULL,
                                 availability = NULL,
                                 access = NULL,
                                 utilization = NULL))

for (ctr in c("TZA", "GHA")) {
  
  for (land in unique(foodsec[foodsec$Country == ctr, "Landscape.."])) {
    
    foodsec_ls <- foodsec[foodsec$Country == ctr & foodsec$Landscape.. == land, ]
    food_util_ls <- food_util[food_util$Country.y == ctr & food_util$Landscape.. == land, ]
    
    # get average year of survey enumeration
    yr <- format(round(mean(foodsec_ls$Data.entry.date)), "%Y")
    
    # FS 17 proxy gap assessment
    gap <- mean(foodsec_ls$staple_cons / foodsec_ls$food_cons)
    
    # FS 18 buffer assessment
    buffer <- 1 - mean(foodsec_ls$food_cons / 
                         (foodsec_ls$food_cons + foodsec_ls$nonfood_cons))
    
    # FS 19 food access score
    access <- gap * buffer

    # FS21.1 daily dietary diversity score
    # sum p(consumed food item yesterday) as x / 7 for all x
    f_groups <- c("k2_8_a", "k2_8_b", "k2_8_c", "k2_8_d", "k2_8_e",
                  "k2_8_f","k2_8_g",  "k2_8_h", "k2_8_i", "k2_8_j")
    
    daily <- rowSums(food_util_ls[f_groups] / 7) / length(f_groups)
    
    # FS21.2 weekly dietary diversity score
    weekly <- rowSums(food_util_ls[f_groups] > 0) / length(f_groups)
    
    # FS22 calc food utilization
    utilization <- mean(daily * 0.5 + weekly * 0.5)
    
#################
# Format Output
#################
  
  foodsec.df1 <- as.data.frame(list(country = ctr,
                               scale = "Landscape",
                               year = yr,
                               landscape = land,
                               foodsec = NA,
                               availability = NA,
                               access = access,
                               utilization = utilization))
  
  foodsec.df <- rbind(foodsec.df, foodsec.df1)

  }
}

foodsec.df$key <- with(foodsec.df, paste0(country, landscape))
coord_alias <- read.csv(vstables$tables[["coordinates_landscapes__wgs_84.csv"]]$getData(), stringsAsFactors = F)
coord_alias <- rename(coord_alias, replace = c("Landscape.." = "Landscape"))
coord_alias$key <- with(coord_alias, paste0(Country, Landscape))
foodsec.df.out <- merge(foodsec.df,
                        coord_alias,
                        by.x="key", by.y="key", all.x = T)
foodsec.df.final <- foodsec.df.out[, c("country",
                                       "scale",
                                       "year",
                                       "landscape",
                                       "foodsec",
                                       "availability",
                                       "access",
                                       "utilization",
                                       "wkt_geom",
                                       "Center_X",
                                       "Center_Y")]



out_filepath <- sprintf("Foodsec_%s.csv", outfile)

#vstables$saveData(foodsec.df.out, out_filepath, "csv", "0By1xoBEcyFy_VVVwa3JSZm5nRXM")
s3 <- newS3()
write.csv(foodsec.df.final, out_filepath, row.names = F)
s3$writeS3("ci-vsindicators", source_path = out_filepath, paste0("Food_Security/", out_filepath), TRUE)
#sapply(list.files()[grep(paste0("^", outfile), list.files())], function(X) {s3$writeS3("ci-vsindicators", source_path = X, paste0("Archive/Nutrition/supporting_files/", X), TRUE)})

