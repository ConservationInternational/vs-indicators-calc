library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Degradation/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

hh <- tbl(vs_db, 'c__agric') %>%
  select(country, hh_refno, round, landscape_no) %>%
  data.frame

eros <- tbl(vs_db, 'c__agric_field_details') %>%
  select(country, landscape_no, hh_refno, round, ag3a_07) %>%
  data.frame

eros <- merge(eros, hh, all=T)

eros$erosion_control <- eros$ag3a_07 == '1'

eros_hh <- eros %>% group_by(country, landscape_no, hh_refno, round) %>%
  summarize(erosion_control=any(erosion_control, na.rm=T)) %>%
  data.frame


sq <- tbl(vs_db, 'c__agric_field_details') %>%
  select(country, landscape_no, hh_refno, round, ag3a_06) %>%
  data.frame

sq <- merge(sq, hh, all=T)

sq$good <- sq$ag3a_06 == '1'
sq$bad <- sq$ag3a_06 == '3'

sq_hh <- sq %>% group_by(country, landscape_no, hh_refno, round) %>%
  summarize(Good_Soil=mean(good, na.rm=T),
            Bad_Soil=mean(bad, na.rm=T))

eros_hh <- merge(sq_hh, eros_hh, all=T)

eros <- eros_hh %>% group_by(country, landscape_no) %>%
  summarize(Good_Soil=mean(Good_Soil, na.rm=T),
            Bad_Soil=mean(Bad_Soil, na.rm=T),
            erosion_control_household_percent = mean(erosion_control, na.rm=T))

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

writeS3(eros_hh, 'ErosionControl_HH.csv')
writeS3(eros, 'ErosionControl_Landscape.csv')
