library(dplyr)
library(reshape2)

setwd('../NaturalResources/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                    user=pg_conf$user, password=pg_conf$pass,
                    port=pg_conf$port)

######################################
#Get NR Value
######################################

allvars <- tbl(con, 'c__household') %>%
  select(country, landscape_no, hh_refno, round) %>% 
  collect

resource <- tbl(con, 'c__household_resource') %>%
  collect

foods <- c('Wild meat', 'Wild insects', 'Fish from local rivers/creeks', 'Nuts or seeds',
           'Honey', 'Other - Snail', 'Other - Crabs', "Other - Crabs, snails and mushrooms", 
           "Other - Crabs,  snails and mushrooms", "Other - MUSHROOMS", 
           "Other - Mushrooms", "Other - Vegetables", "Other - Green vegetables", 
           "Other - Mushroom")
nonfoods <- c("Building materials (e.g. wood that is not used as a fuel source) ", 
              "Medicinal Plants", "Items for special ceremonies",
              "Other - Palms for making mats", "Other - Palms for weaving mats", 
              "Other - Sisal", "Other - Grass")

resource$food[resource$resource %in% foods] <- resource$hh_hv2_10[resource$resource %in% foods]
resource$nonfood[resource$resource %in% nonfoods] <- resource$hh_hv2_10[resource$resource %in% nonfoods]

resource$in_decline <- resource$hh_hv2_15 == 'Declined'

resource$multiplier[resource$hh_hv2_12 == "Annually"] <- 1
resource$multiplier[resource$hh_hv2_12 == "Monthly"] <- 12
resource$multiplier[resource$hh_hv2_12 == "Weekly"] <- 52
resource$multiplier[resource$hh_hv2_12 == "Seasonally"] <- 3

resource$nr_value <- resource$hh_hv2_14 * resource$multiplier

resource <- resource %>%
  group_by(hh_refno, round) %>%
  summarize(in_decline = mean(in_decline, na.rm=T),
            nr_value = sum(nr_value, na.rm=T),
            food = any(food),
            nonfood = any(nonfood))

resource[ c('food', 'nonfood')][is.na(resource[ c('food', 'nonfood')])] <- FALSE
resource$in_decline[is.nan(resource$in_decline)] <- NA

allvars <- left_join(allvars, resource)




#Fuelwood Value
fw_ind <- tbl(con, 'c__household_individual') %>%
  select(country, landscape_no, hh_refno, round, hh_hv104, hh_hv105) %>%
  collect

fw_ind <- merge(fw_ind, data.frame(hh_hv104=c('Weekly', 'Monthly', 'Seasonally', 'Annually'), multiplier=c(52, 12, 4, 1)))
fw_ind$bundles <- fw_ind$hh_hv105*fw_ind$multiplier
fw_ind <- fw_ind %>% group_by(country, landscape_no, hh_refno, round) %>% summarize(bundles=sum(bundles, na.rm=T))

fw_value <- tbl(con, 'c__household') %>% 
  select(country, landscape_no, hh_refno, round, hh_hv105b_01) %>% 
  collect %>%
  group_by(country) %>%
  summarize(cost=median(hh_hv105b_01, na.rm=T)) %>% data.frame %>%
  merge(fw_ind)
fw_value$total_fuelwood_value <- fw_value$cost * fw_value$bundles

#Other HH-level fuelwood stats
fw_hh <- tbl(con, 'c__household') %>% 
  select(country, landscape_no, hh_refno, round, hh_hv109_01, hh_hv109b_01, hh_hv105c_01) %>% 
  collect

fw_hh$fw_is_decreasing <- fw_hh$hh_hv109_01 == "Decreased"
fw_hh$fw_from_wilderness <- fw_hh$hh_hv105c_01 %in% c('Woodland', 'Forest', 'Shrubland', 'Grasland', 'From Private Or Community Woodlots')

fw_hh <- fw_hh %>% select(fw_is_decreasing, not_enough_fw_past_year=hh_hv109b_01, fw_from_wilderness, country, landscape_no, hh_refno, round)


#################
#Combine and write
##################
nr <- Reduce(f=function(x,y){merge(x,y,all=T)}, x=list(allvars, fw_value, fw_hh))

hv <- merge(nr, data.frame(country = c('GHA', 'RWA', 'UGA', 'TZA'),
                                     Rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c("total_fuelwood_value", "nr_value")
hv[ , rateadjust] <- hv[ , rateadjust]/hv$Rate

hv_ls <- hv %>% group_by(country, landscape_no) %>%
  summarize(Fuelwood_Decreasing = mean(fw_is_decreasing, na.rm=T),
            Fuelwood_Shortage_Past_Year = mean(not_enough_fw_past_year, na.rm=T),
            Fuelwood_From_Natural_Areas = mean(fw_from_wilderness, na.rm=T),
            Mean_HH_Fuelwood_Value = mean(total_fuelwood_value, na.rm=T),
            Percent_Collect_Food = mean(food, na.rm=T),
            Percent_Collect_Nonfood = mean(nonfood, na.rm=T),
            Percent_Resource_Decline = mean(in_decline, na.rm=T),
            Mean_HH_NR_Value = mean(nr_value, na.rm=T))

#########################################
#Write
#################################

library(aws.s3)
aws.signature::use_credentials()

writeS3 <- function(df, name){
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}

writeS3(hv, 'NaturalResources_Landscape.csv')
writeS3(hv_ls, 'NaturalResources_HH.csv')

