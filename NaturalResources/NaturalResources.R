library(dplyr)
library(reshape2)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/NaturalResources/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                    user=pg_conf$user, password=pg_conf$pass,
                    port=pg_conf$port)

######################################
#Get NR Value
######################################

hh <- tbl(con, 'flagging__household') %>%
  select(Country, `Landscape #`, `Household ID`, Round) %>% 
  data.frame %>% unique

#Fuelwood Value
hv1.1 <- tbl(con, 'flagging__household_secHV1') %>%
  select(Country, `Landscape #`, `Household ID`, Round, hh_hv104, hh_hv105) %>%
  data.frame

hv1.1 <- merge(hv1.1, data.frame(hh_hv104=c('1', '2', '3', '4'), multiplier=c(52, 12, 4, 1)))
hv1.1$bundles <- hv1.1$hh_hv105*hv1.1$multiplier
hv1.1 <- hv1.1 %>% group_by(Country, Landscape.., Household.ID, Round) %>% summarize(bundles=sum(bundles, na.rm=T))

hv1.2 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, `Household ID`, Round,
         hh_hv105b_01) %>% data.frame %>%
  group_by(Country) %>%
  summarize(cost=median(hh_hv105b_01, na.rm=T)) %>% data.frame %>%
  merge(hv1.1)
hv1.2$total_fuelwood_value <- hv1.2$cost * hv1.2$bundles

hv1 <- hv1.2 %>% group_by(Country, Landscape..) %>% 
  summarize(Mean_Annual_Fuelwood_Value=mean(total_fuelwood_value, na.rm=T))

#Other HH-level fuelwood stats
hv2.1 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, `Household ID`, Round, hh_hv109_01, hh_hv109b_01, hh_hv105c_01) %>% 
  data.frame

hv2.1$fw_is_decreasing <- hv2.1$hh_hv109_01 == "1"
hv2.1$not_enough_fw_past_year <- hv2.1$hh_hv109b_01 == "1"
hv2.1$fw_from_wilderness <- hv2.1$hh_hv105c_01 %in% c('1', '1a', '1b', '2', '6')

hv2.1 <- hv2.1 %>% select(fw_is_decreasing, not_enough_fw_past_year, fw_from_wilderness, Country, Landscape.., Household.ID, Round)

hv2.1.1 <- hv2.1 %>% group_by(Country, Landscape..) %>%
  summarize(Fuelwood_Decreasing = mean(fw_is_decreasing, na.rm=T),
            Fuelwood_Shortage_Past_Year = mean(not_enough_fw_past_year, na.rm=T),
            Fuelwood_From_Natural_Areas = mean(fw_from_wilderness, na.rm=T)) %>%
  data.frame

#Natural Resource Value
# hv2_14_01 - Wild meat
# hv2_14_02 - Wild insects
# hv2_14_03 - Fish from local rivers/creeks
# hv2_14_04 - Nuts or seeds
# hv2_14_05 - Building materials (e.g. wood that is not used as a fuel source) 
# hv2_14_06 - Medicinal Plants
# hv2_14_07 - Items for special ceremonies
# hv2_14_08 - Honey
# hv2_14_09 - Other

#Nonfuel expenditures
hv2.2.1 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, Round, `Household ID`, hv2_14_01, hv2_14_02, hv2_14_03, hv2_14_04,
         hv2_14_05, hv2_14_06, hv2_14_07, hv2_14_08, hv2_14_09) %>% data.frame %>%
  melt(id.vars=c("Country", "Landscape..", "Round", "Household.ID"), value.name = 'price')
hv2.2.1$variable <- gsub('14', '', hv2.2.1$variable)

hv2.2.2 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, Round, `Household ID`, hv2_12_01, hv2_12_02, hv2_12_03, hv2_12_04,
         hv2_12_05, hv2_12_06, hv2_12_07, hv2_12_08, hv2_12_09) %>% data.frame %>%
  melt(id.vars=c("Country", "Landscape..", "Round", "Household.ID"), value.name = 'freq')
hv2.2.2$variable <- gsub('12', '', hv2.2.2$variable)

hv2.2.3 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, Round, `Household ID`, hv2_15_01, hv2_15_02, hv2_15_03, hv2_15_04,
         hv2_15_05, hv2_15_06, hv2_15_07, hv2_15_08, hv2_15_09) %>% data.frame %>%
  melt(id.vars=c("Country", "Landscape..", "Round", "Household.ID"), value.name = 'avail')
hv2.2.3$variable <- gsub('15', '', hv2.2.3$variable)

hv2.2 <- merge(merge(hv2.2.1, hv2.2.2, all=T), hv2.2.3, all=T)
hv2.2 <- merge(hv2.2, data.frame(freq=c("1", "2", "3", "4"), rate=c(52, 12, 4, 1)), all=T)

hv2.2$annual_price <- hv2.2$price * hv2.2$rate
hv2.2$nr_is_decreasing <- hv2.2$avail == "1"

hv2.3 <- hv2.2 %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(hh_annual_nonfuel_nr_value = sum(annual_price, na.rm=T),
            Nonfuel_NR_decreasing = mean(nr_is_decreasing, na.rm=T))

hv2.4 <- hv2.3 %>% group_by(Country, Landscape..) %>%
  summarize(Nonfuel_NR_annual_value = mean(hh_annual_nonfuel_nr_value, na.rm=T),
            Nonfuel_NR_decreasing = mean(Nonfuel_NR_decreasing, na.rm=T))

#% of households that collect any natural resources
hv2.5 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, `Household ID`, Round, hv2_10_01, hv2_10_02, hv2_10_03, hv2_10_04,
         hv2_10_05, hv2_10_06, hv2_10_07, hv2_10_08, hv2_10_09) %>% data.frame %>%
  melt(id.vars=c("Country", "Landscape..", "Household.ID", 'Round'), value.name = 'collects') %>%
  group_by(Country, Landscape.., Household.ID, Round) %>% summarize(collects=any(collects=='1')) %>%
  group_by(Country, Landscape..) %>% summarize(Collects_Nonfuel_Resources=mean(collects, na.rm=T))


hv <- Reduce(f=function(x,y){merge(x,y,all=T)}, x=list(hv1, hv2.1.1, hv2.4, hv2.5))

write.csv(hv, 'NaturalResources.Landscape.csv', row.names=F)

#HH Level Vars

hv.hh <- Reduce(f=function(x,y){merge(x,y,all=T)}, x=list(hv1.1, hv1.2, hv2.3, hh))
write.csv(hv.hh, 'NaturalResources.HH.csv', row.names=F)

