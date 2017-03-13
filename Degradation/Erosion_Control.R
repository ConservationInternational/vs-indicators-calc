library(dplyr)

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

eros <- tbl(vs_db, 'flagging__agric_field_details') %>%
  select(Country, `Landscape #`, survey_uuid, ag3a_07) %>%
  data.frame

eros$erosion_control <- eros$ag3a_07 == '1'

eros <- eros %>% group_by(Country, Landscape.., survey_uuid) %>%
  summarize(erosion_control=any(erosion_control, na.rm=T)) %>%
  group_by(Country, Landscape..) %>%
  summarize(erosion_control_household_percent = mean(erosion_control, na.rm=T))


sq <- tbl(vs_db, 'flagging__agric_field_details') %>%
  select(Country, `Landscape #`, survey_uuid, ag3a_06) %>%
  data.frame

sq$good <- sq$ag3a_06 == '1'
sq$bad <- sq$ag3a_06 == '3'

sq <- sq %>% group_by(Country, Landscape..) %>%
  summarize(Good_Soil=mean(good, na.rm=T),
            Bad_Soil=mean(bad, na.rm=T))

eros <- merge(eros, sq)

write.csv(eros, 'ErosionControl.csv', row.names=F)
