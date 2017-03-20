library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/WaterSanitation/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

toilet <- tbl(vs_db, 'flagging__household_secJ1') %>%
  select(Country, `Landscape #`, `Household ID`, Round, hh_j09, hh_j08, hh_j20, hh_j16_1) %>%
  data.frame

toilet$No_Toilet <- toilet$hh_j09 == '1'
toilet$Flush_Toilet <- toilet$hh_j09 %in% c('2', '3')
toilet$Pit_Latrine <- toilet$hh_j09 %in% c('4', '5', '6', '7')

toilet$Satisfied_Drinking_Water <- toilet$hh_j20 %in% c('1', '2')
toilet$Unsatisfied_Drinking_Water <- toilet$hh_j20 %in% c('4', '5')

toilet$Dispose_Garbage_Within_Compound <- toilet$hh_j08 == '4'

toilet$No_Measure_Safe_Drinking_Water <- toilet$hh_j16_1 == '7'

toilet <- toilet %>% select(Country, Landscape.., Household.ID, Round, No_Toilet, Flush_Toilet, Pit_Latrine, Satisfied_Drinking_Water,
                            Unsatisfied_Drinking_Water, Dispose_Garbage_Within_Compound, No_Measure_Safe_Drinking_Water)

write.csv(toilet, 'WatSan.HH.csv', row.names=F)

toilet_sum <- toilet %>% group_by(Country, Landscape..) %>%
  summarize(No_Toilet = mean(No_Toilet, na.rm=T),
            Flush_Toilet = mean(Flush_Toilet, na.rm=T),
            Pit_Latrine = mean(Pit_Latrine, na.rm=T),
            Satisfied_Drinking_Water = mean(Satisfied_Drinking_Water, na.rm=T),
            Unsatisfied_Drinking_Water = mean(Unsatisfied_Drinking_Water, na.rm=T),
            Dispose_Garbage_Within_Compound = mean(Dispose_Garbage_Within_Compound, na.rm=T),
            No_Measure_Safe_Drinking_Water = mean(No_Measure_Safe_Drinking_Water, na.rm=T))

write.csv(toilet_sum, 'WatSan.Landscape.csv', row.names=F)
