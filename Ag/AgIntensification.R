library(dplyr)
library(ineq)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Ag')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

#accumulator
allvars <- tbl(con, "flagging__agric") %>%
  select(survey_uuid, Country, `Landscape #`, `Household ID`, Round) %>%
  data.frame %>%
  select(survey_uuid, Country, Landscape.., Household.ID, Round)


# Field Size - agric_field_roster
#   ag2a_09 - GPS MEASUREMENT ((what is the area of the field taken by GPS tracking (Acres)?)

field_size <- tbl(con, "flagging__agric_field_roster") %>%
  select(survey_uuid, ag2a_09, flag) %>%
  data.frame %>%
  group_by(survey_uuid) %>%
  summarize(median_field_size = median(ag2a_09, na.rm=T), Total_Area_Farmed = sum(ag2a_09, na.rm=T))

allvars <- merge(allvars, field_size, all.x=T)

#####################
# Intercropping  - agric_crops_by_field
#   ag4a_04 - Was cultivation intercropped? {1: 'Yes', 2: 'No'}

intercropping <- tbl(con, "flagging__agric_crops_by_field") %>%
  select(survey_uuid, ag4a_04, flag) %>%
  data.frame

intercropping$ag4a_04 <- as.numeric(intercropping$ag4a_04)
intercropping$ag4a_04[intercropping$ag4a_04==2] <- 0

intercropping <- intercropping %>% group_by(survey_uuid) %>%
  summarize(intercrop_rate = mean(ag4a_04, na.rm=T))

allvars <- merge(allvars, intercropping, all.x=T)

#################################
# Inputs - agric_field_details
#  ag3a_34 - What was the main type of pesticide/herbicide that you applied? {1: 'Pesticide', 2: 'Herbicide', 3: 'Fungicide'}

#  fd35_24a_* - Select all the types of inorganic fertilizer that  you used on this field
#   fd35_24a_dap
#   fd35_24a_urea
#   fd35_24a_tsp
#   fd35_24a_can
#   fd35_24a_sa
#   fd35_24a_npk
#   fd35_24a_mrp

inputs <- tbl(con, "flagging__agric_field_details") %>%
  select(survey_uuid, ag3a_34, ag3a_33,
         fd35_24a_dap, fd35_24a_urea, fd35_24a_tsp,
         fd35_24a_can, fd35_24a_sa, fd35_24a_npk, fd35_24a_mrp, flag) %>%
  data.frame

inputs$pesticide <- inputs$ag3a_34==1
inputs$herbicide <- inputs$ag3a_34==2
inputs$fungicide <- inputs$ag3a_34==3

inputs$pesticide[inputs$ag3a_33=='2'] <- 0
inputs$herbicide[inputs$ag3a_33=='2'] <- 0
inputs$fungicide[inputs$ag3a_33=='2'] <- 0


#Fertilizer data is missing for all forms before May 2016:
#'
#'SELECT DISTINCT b.xform_name, a.fd35_24a_dap IS NULL FROM agric_field_details a
#'JOIN form_log b ON a.survey_uuid = b.survey_uuid
#'ORDER BY xform_name
#'
#'So leave out forms for now
# 
# inputs[inputs=='t'] <- 1
# inputs[inputs=='f'] <- 0
# 
# inputs$fd35_24a_urea <- as.numeric(inputs$fd35_24a_urea)
# inputs$fd35_24a_dap <- as.numeric(inputs$fd35_24a_dap)
# inputs$fd35_24a_tsp <- as.numeric(inputs$fd35_24a_tsp)
# inputs$fd35_24a_can <- as.numeric(inputs$fd35_24a_can)
# inputs$fd35_24a_sa <- as.numeric(inputs$fd35_24a_sa)
# inputs$fd35_24a_npk <- as.numeric(inputs$fd35_24a_npk)
# inputs$fd35_24a_mrp <- as.numeric(inputs$fd35_24a_mrp)

inputs[is.na(inputs)] <- 0

inputs <- inputs %>% group_by(survey_uuid) %>%
  summarize(pesticide = mean(pesticide, na.rm=T), herbicide = mean(herbicide, na.rm=T),
            fungicide = mean(fungicide, na.rm=T)#, 
#             dap = mean(fd35_24a_dap, na.rm=T),
#             urea = mean(fd35_24a_urea, na.rm=T), tsp = mean(fd35_24a_tsp, na.rm=T),
#             can = mean(fd35_24a_can, na.rm=T), sa = mean(fd35_24a_sa, na.rm=T),
#             npk = mean(fd35_24a_npk, na.rm=T), mrp = mean(fd35_24a_mrp, na.rm=T)
            )

allvars <- merge(allvars, inputs, all.x=T)

########################
# Yields - get the Z score for crop-unit across the whole country
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
yields <- tbl(con, "flagging__agric_crops_by_field") %>%
  filter(!is.na(ag4a_15) & !is.na(ag4a_08) & ag4a_08 > 0) %>%
  select(survey_uuid, Country, `Crop name`, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
  data.frame

yields$yield <- yields$ag4a_15/yields$ag4a_08

yields$cs <- paste0(yields$Crop.name, yields$ag4a_15_unit, yields$Country)

for (c in unique(yields$cs)){
  sel <- yields$yield[yields$cs==c]
  if (length(sel) > 4){
    sel <- sel + rnorm(length(sel), 0, .1) #slight jitter, because cut() doesnt work with repeats
    sel <- as.integer(cut(sel, quantile(sel, probs=0:4/4), include.lowest=TRUE))
    yields$yield_quantile[yields$cs==c] <- sel
  }else{
    yields$yield_quantile[yields$cs==c] <- NA
  }
}

yields <- yields %>% group_by(survey_uuid) %>% summarize(yield_quantile = mean(yield_quantile, na.rm=T))

allvars <- merge(allvars, yields, all.x=T)



#########################################################
#Amount Sold
#   ag5a_01 - Did you sell any of the ${fd5_crop_name} produced
#   ag5a_02_1 - Amount
#   ag5a_02_2 - Unit {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
sold <- tbl(con, "flagging__agric_crops_by_hh") %>%
  select(survey_uuid, `Crop name`, Season, ag5a_01, ag5a_02_1, ag5a_02_2, flag) %>%
  data.frame

#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

# yields <- tbl(con, "flagging__agric_crops_by_field") %>%
#   filter(!is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
#   select(survey_uuid, `Crop name`, Season, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
#   data.frame %>%
#   group_by(survey_uuid, Crop.name, ag4a_15_unit) %>%
#   summarize(ag4a_15=sum(ag4a_15, na.rm=T))
# 
# names(yields)[names(yields)=='ag4a_15_unit'] <- 'ag5a_02_2'
# 
# sold <- merge(sold, yields, all=T)

sold$ag5a_01 <- as.numeric(sold$ag5a_01)
sold$ag5a_01[sold$ag5a_01==2] <- 0

# sold$percent_sold <- sold$ag5a_02_1/sold$ag4a_15
# sold$percent_sold[sold$percent_sold > 1] <- NA

sold <- sold %>% group_by(survey_uuid) %>%
  summarize(#avg_pct_harvest_sold = mean(percent_sold, na.rm=T),
            avg_pct_crops_any_sold = mean(ag5a_01, na.rm=T)) %>%
  data.frame

allvars <- merge(allvars, sold, all.x=T)

###########################################
# Irrigaion
#   ag3a_09 - Was this FIELD irrigated in the last completed

irrig <- tbl(con, 'flagging__agric_field_details') %>% 
  select(survey_uuid, flag, ag3a_09) %>%
  data.frame

irrig$ag3a_09 <- as.numeric(irrig$ag3a_09)
irrig$ag3a_09[irrig$ag3a_09==2] <- 0

irrig <- irrig %>% group_by(survey_uuid) %>%
  summarize(pct_fields_irrigated = mean(ag3a_09, na.rm=T))

allvars <- merge(allvars, irrig, all.x=T)

############################
# Inorganic Fertilizers

inorg <- tbl(con, 'flagging__agric_field_details') %>% 
  select(survey_uuid, flag, ag3a_23) %>%
  data.frame

inorg$ag3a_23 <- as.numeric(inorg$ag3a_23)
inorg$ag3a_23[inorg$ag3a_23==2] <- 0

inorg <- inorg %>% group_by(survey_uuid) %>%
  summarize(pct_fields_inorganic_fert = mean(ag3a_23, na.rm=T))

allvars <- merge(allvars, inorg, all.x=T)


######################
#Seed Variety
# agric_crops_by_field
#   ag4a_19 - Did you purchase any SEED for ${fd4_crop_name} in the last completed Long Rainy Season / Major Cropping Season?
#   ag4a_21 - What type of seed did you purchase ?  {1: 'Traditional', 2: 'Purchased Improved Seeds', 3: 'Saved Improved Seeds'}
seed <- tbl(con, 'flagging__agric_crops_by_field') %>%
  select(survey_uuid, flag, ag4a_19, ag4a_21) %>%
  data.frame

seed$ag4a_19 <- as.numeric(seed$ag4a_19)
seed$ag4a_19[seed$ag4a_19==2] <- 0

seed <- seed %>% group_by(survey_uuid) %>% summarize(pct_buy_seed=mean(ag4a_19, na.rm=T))

allvars <- merge(allvars, seed, all.x=T)

################################
#Average number of fields farmed in either season or both
numfields <- tbl(con, 'flagging__agric_field_roster') %>%
  group_by(survey_uuid) %>% summarize(number_fields=n()) %>%
  data.frame

allvars <- merge(allvars, numfields, all.x=T)

################################
#Total Area Owned

ownership <- tbl(con, 'flagging__agric_field_details') %>%
  filter(ag3a_14=='1') %>%
  select(survey_uuid, `Field ID`) %>% data.frame

fieldsize <- tbl(con, 'flagging__agric_field_roster') %>%
  select(survey_uuid, `Field ID`, ag2a_09) %>% 
  data.frame

combo <- merge(ownership, fieldsize)
combo$ag2a_09[is.na(combo$ag2a_09)] <- 0

hh_total <- combo %>% group_by(survey_uuid) %>% 
  summarize(Total_Area_Owned=sum(ag2a_09))

allvars <- merge(allvars, hh_total, all.x=T)

allsum <- allvars %>% group_by(Country, Landscape..) %>%
  summarize(median_field_size=median(median_field_size, na.rm=T), 
            Mean_HH_Area_Farmed=mean(Total_Area_Farmed, na.rm=T),
            Intercrop_Rate=mean(intercrop_rate, na.rm=T), 
            Pesticide_Rate=mean(pesticide, na.rm=T),
            Herbicide_Rate=mean(herbicide, na.rm=T), 
            Fungicide_Rate=mean(fungicide, na.rm=T),
            Mean_Yield_Quantile=mean(yield_quantile, na.rm=T), 
            Avg_Pct_Crops_Any_Sold=mean(avg_pct_crops_any_sold, na.rm=T),
            Pct_Fields_Irrigated=mean(pct_fields_irrigated, na.rm=T),
            Pct_Fields_Inorg_Fert=mean(pct_fields_inorganic_fert, na.rm=T),
            Pct_Fields_Purchased_Seed=mean(pct_buy_seed, na.rm=T),
            Mean_HH_Number_Fields=mean(number_fields, na.rm=T), 
            Mean_Area_Owned=mean(Total_Area_Owned, na.rm=T),
            Area_Owned_Ineq_Gini=ineq(Total_Area_Owned))


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

writeS3(allvars, 'AgIntensification_HH.csv')
writeS3(allsum, 'AgIntensification_Landscape.csv')
