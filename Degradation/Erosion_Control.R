library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Degradation/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

hh <- tbl(vs_db, 'flagging__agric') %>%
  select(Country, `Household ID`, Round, `Landscape #`) %>%
  data.frame

eros <- tbl(vs_db, 'flagging__agric_field_details') %>%
  select(Country, `Landscape #`, `Household ID`, Round, ag3a_07) %>%
  data.frame

eros <- merge(eros, hh, all=T)

eros$erosion_control <- eros$ag3a_07 == '1'

eros_hh <- eros %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(erosion_control=any(erosion_control, na.rm=T)) %>%
  data.frame


sq <- tbl(vs_db, 'flagging__agric_field_details') %>%
  select(Country, `Landscape #`, `Household ID`, Round, ag3a_06) %>%
  data.frame

sq <- merge(sq, hh, all=T)

sq$good <- sq$ag3a_06 == '1'
sq$bad <- sq$ag3a_06 == '3'

sq_hh <- sq %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(Good_Soil=mean(good, na.rm=T),
            Bad_Soil=mean(bad, na.rm=T))

eros_hh <- merge(sq_hh, eros_hh, all=T)

eros <- eros_hh %>% group_by(Country, Landscape..) %>%
  summarize(Good_Soil=mean(Good_Soil, na.rm=T),
            Bad_Soil=mean(Bad_Soil, na.rm=T),
            erosion_control_household_percent = mean(erosion_control, na.rm=T))

write.csv(eros_hh, 'ErosionControl.HH.csv', row.names=F)

write.csv(eros, 'ErosionControl.Landscape.csv', row.names=F)
