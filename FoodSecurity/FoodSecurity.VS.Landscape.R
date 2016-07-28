#############################
# Workflow - Food Security
# Scale - Landscape
# 09 May 2016
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

library(reshape2)
library(plyr)

#################
# Loading
#################

# Vital Signs Datasets
hh_sec_a <- read.csv("hh_secA.csv")
hh_sec_k1 <- read.csv("hh_secK1.csv")
hh_sec_k2 <- read.csv("hh_secK2.csv")
hh_sec_l <- read.csv("hh_secL.csv")

# External Datasets
staple <- read.csv("staple.csv")


#################
# Define Functions
#################

foodcons <- function(df) {
  
  df$k_04[is.na(df$k_04) ] <- 0  #NA counts as 0 spending
  df$k_05a[is.na(df$k_05a) ] <- 0  #NA counts as 0 hypothetical spending
  
  food <- ddply(df, 
                .(Household.ID), 
                summarise, 
                food_cons = sum(k_04 + k_05a, na.rm = TRUE))
  
  # annualize
  food$food_cons <- food$food_cons / 7 * 365.24 
  
  return (food)
}

nonfoodcons <- function (df, sec_m = TRUE) {
  
  # keep the first 4 columns, while subsequently dropping every other column
  df <- cbind(df[1:4], df[c(F, T)][c(-1, -2)])
  
  # melt to long shape
  df <- melt(df, 
             id.vars = c("Country", "Landscape..", "Household.ID", "Data.entry.date"),
             variable.name = "nonfood.code",
             value.name = "amount.spent")
  
  #list of items measured weekly.  All other items measured monthly
  weekly <- c('l_101_2', 'l_102_2', 'l_103_2', 'l_199_2', 'l_204_2', 'l_206_2', 'l_207_2', 'l_207_2a')
  
  df[df$nonfood.code %in% weekly,'amount.spent'] <- df[df$nonfood.code %in% weekly,'amount.spent']/7*365.24
  df[!df$nonfood.code %in% weekly,'amount.spent'] <- df[!df$nonfood.code %in% weekly,'amount.spent']/31*365.24
  
  
  # sum valued nonfood consumption by household
  nonfood <- ddply(df, 
                   .(Household.ID), 
                   summarise, 
                   nonfood_cons = sum(amount.spent, na.rm = TRUE))
  
  
  #THERE IS AN ERROR HERE HE DOESNT ANNUALIZE CORRECTLY
  # annualize nonfood consumption
  
  return(nonfood)
  
}


#################
# Staging
#################

hh_sec_a <- subset(hh_sec_a, Country=='UGA',
                   select = c(Country, Region, District, Household.ID, Landscape..,
                              Data.entry.date))

# valued food consumption
hh_sec_k1 <- subset(hh_sec_k1, Country=='UGA',
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

