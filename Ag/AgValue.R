library(dplyr)
library(ineq)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Ag')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                    user=pg_conf$user, password=pg_conf$pass,
                    port=pg_conf$port)

#####################################
#Get estiamted Ag value
#####################################


ag0 <- tbl(con, 'c__agric') %>%
  select(country, landscape_no, hh_refno, round) %>%
  data.frame

#  agric_crops_by_field;
#    ag4a_16  What is the estimated value of the harvested crop in local currency?
#    ag4a_21 - What was the total value of seeds purchased?

agvalue <- tbl(con, 'c__agric_crops_by_field') %>%
  select(hh_refno, round, ag4a_16, ag4a_21) %>%
  data.frame

agvalue <- agvalue %>% group_by(hh_refno, round) %>% summarize(Crops = sum(ag4a_16, na.rm=T) - sum(ag4a_21, na.rm=T))

agvalue[is.na(agvalue)] <- 0

agvalue <- merge(ag0, agvalue, all.x=T)

#  agric_field_details
#    ag3a_32 What was the total value of [FERTILIZER] purchased?
#    ag3a_61 What was the total value of this pesticides/ herbicides purchased?
#    ag3a_22 What was the total value of organic fertilizer purchased?

ag1 <- tbl(con, 'c__agric_field_details') %>%
  select(hh_refno, round, ag3a_32, ag3a_61, ag3a_22) %>%
  data.frame

ag1[is.na(ag1)] <- 0

ag1 <- ag1 %>% group_by(hh_refno, round) %>% summarize(Inputs = sum(ag3a_22, na.rm=T) + sum(ag3a_61, na.rm=T) + sum(ag3a_22, na.rm=T))

agvalue <- merge(agvalue, ag1, all.x=T)

agvalue$Crops <- agvalue$Crops - agvalue$Inputs
agvalue$Inputs <- NULL

#  agric_perm_crop
#    ag6a_vs_09  What is the estimated value of the total amount of ${fd6aii_crop_name} harvested in the past 12 months?

ag2 <- tbl(con, 'c__agric_perm_crops_by_field') %>%
  select(hh_refno, round, ag6a_vs_09) %>%
  data.frame

ag2 <- ag2 %>% group_by(hh_refno, round) %>% summarize(Permanent.Crops = sum(ag6a_vs_09, na.rm=T))

agvalue <- merge(agvalue, ag2, all.x=T)

agvalue$Permanent.Crops[is.na(agvalue$Permanent.Crops)] <- 0

#  agric_livestock_byproduct
#    ag10b_1b  What was the total value of total ${fd10_other_b_byproduct_name} produced in the last 12 months?

ag3 <- tbl(con, 'c__agric_livestock_byproduct') %>%
  select(hh_refno, round, ag10b_1b) %>%
  data.frame

ag3 <- ag3 %>% group_by(hh_refno, round) %>% summarize(Livestock.Byproducts = sum(ag10b_1b, na.rm=T))

agvalue <- merge(agvalue, ag3, all.x=T)
agvalue$Livestock.Byproducts[is.na(agvalue$Livestock.Byproducts)] <- 0

#  agric_livestock
#    ag10a_25   How many ${fd10_animal_name} did you slaughter in the past 12 months?
#    ag10a_26   How many of the ${fd10_animal_name} slaughtered did you sell?
#    ag10a_27   What was the total value of the sold slaughtered ${fd10_animal_name}?
#    ag10a_20		How many ${fd10_animal_name} have you sold alive in the past 12 months?
#    ag10a_21	  What was the total value of sales?
#    - ag10a_34	What was the total cost of this labor for ${fd10_animal_name} in the past 12 months?

ag4 <- tbl(con, 'c__agric_livestock') %>%
  select(country, landscape_no, hh_refno, `Animal name`, round, ag10a_25, ag10a_26, ag10a_27, ag10a_20, ag10a_21, ag10a_34) %>%
  data.frame

ag4$Home.Use <- ag4$ag10a_25 - ag4$ag10a_26

ag4$Total.Sold <- mapply(ag4$ag10a_26, ag4$ag10a_20, FUN=sum, na.rm=T)
ag4$Total.Value <- mapply(ag4$ag10a_27, ag4$ag10a_21, FUN=sum, na.rm=T)

ag4$PerAnimalValue[ag4$Total.Sold!=0] <- ag4$Total.Value[ag4$Total.Sold!=0]/ag4$Total.Sold[ag4$Total.Sold!=0]

landscape_value <- ag4 %>% group_by(country, landscape_no, Animal.name) %>% summarize(lvalue=mean(PerAnimalValue, na.rm=T))
country_value <- ag4 %>% group_by(country, Animal.name) %>% summarize(cvalue=mean(PerAnimalValue, na.rm=T))

value <- merge(landscape_value, country_value)
value$lvalue[is.nan(value$lvalue)] <- value$cvalue[is.nan(value$lvalue)]
value <- value[!is.nan(value$lvalue) & !is.infinite(value$lvalue), ] %>% select(-cvalue)

ag4 <- merge(ag4, value)

ag4$value <- mapply(ag4$Total.Value, ag4$Home.Use*ag4$lvalue, FUN=sum, na.rm=T)

ag4 <- ag4 %>% group_by(hh_refno, round) %>% summarize(Livestock=(sum(value)-sum(ag10a_34, na.rm=T)))

agvalue <- merge(agvalue, ag4, all.x=T)

agvalue$Livestock[is.na(agvalue$Livestock)] <- 0

#  agric_byprod
#   - ag09_11   What were the total costs of these additional expenses?
#   ag09_04_1	AMOUNT - What is the quantity produced in the last 12 months?  
#   ag09_04_2	UNIT
#   ag09_05	Was any [BY-PRODUCT/PROCESSED PRODUCT] sold?
#   ag09_06_1	AMOUNT - How much was sold?
#   ag09_06_2	UNIT
#   ag09_08	What was total sales in local currency?

#since the units are missing for a lot, assume every crop is in the same units
ag5 <- tbl(con, 'c__agric_byprod') %>%
  select(country, landscape_no, hh_refno, round, ag09_11, ag09_04_1, ag09_05, ag09_06_1, ag09_08, `Crop ID`) %>%
  data.frame

ag5$Not.Sold <- mapply(ag5$ag09_04_1, -ag5$ag09_06_1, FUN=sum, na.rm=T)

ag5$PerUnitValue <- ag5$ag09_08/ag5$ag09_06_1

landscape_value <- ag5 %>% group_by(country, landscape_no, Crop.ID) %>% summarize(lvalue=mean(PerUnitValue, na.rm=T))
country_value <- ag5 %>% group_by(country, Crop.ID) %>% summarize(cvalue=mean(PerUnitValue, na.rm=T))

value <- merge(landscape_value, country_value)
value$lvalue[is.nan(value$lvalue)] <- value$cvalue[is.nan(value$lvalue)]
value <- value[!is.nan(value$lvalue) & !is.infinite(value$lvalue), ] %>% select(-cvalue)

ag5 <- merge(ag5, value)

ag5$value <- mapply(ag5$ag09_08, ag5$Not.Sold*ag5$lvalue, FUN=sum, na.rm=T)

ag5 <- ag5 %>% group_by(hh_refno, round) %>% summarize(Crop.Byproducts=(sum(value)-sum(ag09_11, na.rm=T)))

agvalue <- merge(agvalue, ag5, all.x=T)

agvalue$Crop.Byproducts[is.na(agvalue$Crop.Byproducts)] <- 0

agvalue <- merge(agvalue, data.frame(country = c('GHA', 'RWA', 'UGA', 'TZA'),
                                     Rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c('Crops', 'Permanent.Crops', 'Livestock.Byproducts', 'Livestock', 'Crop.Byproducts')

agvalue[ , rateadjust] <- agvalue[ , rateadjust]/agvalue$Rate

agvalue$total <- rowSums(agvalue[ , c("Crops", "Permanent.Crops", 
                                      "Livestock.Byproducts", "Livestock", "Crop.Byproducts")])

agvaluesum <- agvalue %>% group_by(country, landscape_no) %>%
  summarize(AnnualCropProduction=mean(Crops, na.rm=T),
            PermanentCropProduction=mean(Permanent.Crops, na.rm=T),
            LivestockByproductProduction=mean(Livestock.Byproducts, na.rm=T),
            LivestockProduction=mean(Livestock, na.rm=T),
            CropByproductProduction=mean(Crop.Byproducts, na.rm=T),
            TotalAgriculturalProduction=mean(total, na.rm=T),
            Production_Inequality_Gini=ineq(total, type = "Gini"))

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

writeS3(agvaluesum, 'AgValue_Landscape.csv')
writeS3(agvalue, 'AgValue_HH.csv')
