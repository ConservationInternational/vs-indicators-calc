#############################
# calculate the four nutrition indicators:
#   % stunted
#   % wasted
#   % underweight
#   % overweight
#
#############################

#################
# Setup
#################

library(dplyr)
library(zoo)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Nutrition/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

# Read datasets and only keep variables for nutrition thread
hh <- tbl(vs_db, "c__household") %>%
  select(country, landscape_no, hh_refno, round, hh_interview_date) %>%
  collect

hh_ind <- tbl(vs_db, "c__household_individual") %>%
  filter(hh_u1) %>%
  select(hh_refno, round, ind_refno, sex=hh_b02, dob=hh_b03, 
         weight=hh_u2, lenhei=hh_u3, measure=hh_u4, armc=hh_u5) %>%
  collect

landscape <- tbl(vs_db, 'landscape') %>%
  collect

# Restore reference data sets
weianthro <- read.table("WHO Anthro reference tables/weianthro.txt", header=T)
lenanthro <- read.table("WHO Anthro reference tables/lenanthro.txt", header=T)
wflanthro <- read.table("WHO Anthro reference tables/wflanthro.txt", header=T)
wfhanthro <- read.table("WHO Anthro reference tables/wfhanthro.txt", header=T)
hcanthro <- read.table("WHO Anthro reference tables/hcanthro.txt", header=T)
acanthro <- read.table("WHO Anthro reference tables/acanthro.txt", header=T)
bmianthro <- read.table("WHO Anthro reference tables/bmianthro.txt", header=T)
ssanthro <- read.table("WHO Anthro reference tables/ssanthro.txt", header=T)
tsanthro <- read.table("WHO Anthro reference tables/tsanthro.txt", header=T)

# Merge datasets into one "nutrition" dataset
nutrition <- merge(hh, hh_ind, by = c("hh_refno", "round"), all = TRUE)


# N24. Age
nutrition$dob <- as.Date(substr(nutrition$dob, 1, 10))
nutrition$hh_interview_date <- as.Date(nutrition$hh_interview_date)

# age in months
nutrition$age <- (as.yearmon(nutrition$hh_interview_date) - 
  as.yearmon(nutrition$dob)) * 12

nutrition$intyr <- as.integer(format(nutrition$Data.collection.date, "%Y"))

nutrition$measure[nutrition$measure == "Standing"] <- "1"
nutrition$measure[nutrition$measure == "Lying Down"] <- "2"

# ISSUE32 need to add date of interview when it is added
vars <- c("country", "landscape_no", "round",
          "hh_refno", "ind_refno",  "intyr", "age", 
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


outfile_slice <- paste("igrowup_outfile")
nutrition.subset <- nutrition[nutrition$age<60,]

source('igrowup_standard.r')

igrowup.standard(mydf = nutrition.subset,
               sex = sex,
               age = age,
               age.month = T, 
               weight = weight, 
               lenhei = lenhei,
               measure = measure, 
               armc = armc)

matz$zlen[matz$flen==1] <- NA
matz$zwei[matz$fwei==1] <- NA
matz$zwfl[matz$fwfl==1] <- NA
matz <- matz[matz$age > 6, ]

nutrition_df <- matz[,c('country', 'landscape_no', 'hh_refno', 'ind_refno', 'round', 'zlen', 'zwei', 'zwfl')] 

nutrition_df$stunting <- ifelse(nutrition_df$zlen < -2, 1, 0)
nutrition_df$severe_stunting <- ifelse(nutrition_df$zlen < -3, 1, 0)
nutrition_df$underweight <- ifelse(nutrition_df$zwei < -2, 1, 0)
nutrition_df$severe_underweight <- ifelse(nutrition_df$zwei < -3, 1, 0)
nutrition_df$wasting <- ifelse(nutrition_df$zwfl < -2, 1, 0)
nutrition_df$severe_wasting <- ifelse(nutrition_df$zwfl < -3, 1, 0)
nutrition_df$overweight <- ifelse(nutrition_df$zwei > 1, 1, 0)

nutrition_df$CIAF <- as.numeric(nutrition_df$stunting | nutrition_df$underweight | nutrition_df$wasting)

nutrition_df$LandscapeCode <- paste(nutrition_df$country, nutrition_df$landscape_no, sep='-')

landscape$LandscapeCode <- paste(landscape$country, landscape$landscape_no, sep='-')
landscape$latitude <- rowMeans(landscape[, c('lower_right_latitude', 'lower_left_latitude', 'upper_right_latitude', 'upper_left_latitude')], na.rm=T)
landscape$longitude <- rowMeans(landscape[, c('lower_right_longitude', 'lower_left_longitude', 'upper_right_longitude', 'upper_left_longitude')], na.rm=T)

nutrition_coords <- merge(nutrition_df, landscape[,c('LandscapeCode', 'latitude', 'longitude')], by='LandscapeCode', all.x=T)
nutrition_coords$LandscapeCode <- NULL

nutrition_landscape <- group_by(nutrition_coords, country, landscape_no) %>% 
  summarise(mean_zlen=mean(zlen, na.rm=T), mean_zwei=mean(zwei, na.rm=T), mean_zwfl=mean(zwfl, na.rm=T),
            percent_stunted=mean(stunting, na.rm=T)*100, percent_severe_stunted=mean(severe_stunting, na.rm=T)*100,
            percent_underweight=mean(underweight, na.rm=T)*100, percent_severe_underweight=mean(severe_underweight, na.rm=T)*100,
            percent_wasting=mean(wasting, na.rm=T)*100, percent_server_wasting=mean(severe_wasting, na.rm=T)*100,
            percent_overweight=mean(overweight, na.rm=T)*100, percent_Composite_Index_Anthropometric_Failure=mean(CIAF, na.rm=T)*100)

nut_hh <- nutrition_coords %>% group_by(country, landscape_no, hh_refno, round) %>% 
  summarise(mean_zlen=mean(zlen, na.rm=T), mean_zwei=mean(zwei, na.rm=T), mean_zwfl=mean(zwfl, na.rm=T),
            percent_stunted=mean(stunting, na.rm=T)*100, percent_severe_stunted=mean(severe_stunting, na.rm=T)*100,
            percent_underweight=mean(underweight, na.rm=T)*100, percent_severe_underweight=mean(severe_underweight, na.rm=T)*100,
            percent_wasting=mean(wasting, na.rm=T)*100, percent_server_wasting=mean(severe_wasting, na.rm=T)*100,
            percent_overweight=mean(overweight, na.rm=T)*100, percent_Composite_Index_Anthropometric_Failure=mean(CIAF, na.rm=T)*100)

#########################################
#Write
#################################

library(aws.s3)
aws.signature::use_credentials()

writeS3 <- function(df, name){
  names(df) <- gsub('.', '_', names(df), fixed=T)
  names(df)[names(df)=='Landscape__'] <- 'Landscape'
  
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}

writeS3(nutrition_coords, 'Nutrition_Individual.csv')
writeS3(nutrition_landscape, 'Nutrition_Landscape.csv')
writeS3(nut_hh, 'Nutrition_HH.csv')
