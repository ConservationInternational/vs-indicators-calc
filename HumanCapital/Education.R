library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/HumanCapital/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

df <- tbl(vs_db, 'c__household_secC') %>%
  select(country, landscape_no, hh_refno, round, hh_c02, hh_c07, hh_c03, uuid) %>%
  data.frame

age <- tbl(vs_db, 'c__household_secB') %>%
  select(country, landscape_no, hh_refno, round, hh_b04, uuid) %>%
  data.frame

df <- merge(df, age, all=T)

years <- read.csv('YearsOfSchooling.csv')

df$school_code <- as.numeric(df$hh_c07)
df$literate <- df$hh_c02 != '5'

ed <- merge(df, years, by=c('country', 'school_code'), all.x=T)
ed$years[ed$hh_c03=='2'] <- 0

hh_size <- ed %>% group_by(country, landscape_no, hh_refno, round) %>%
  summarize(size=n())

capital <- ed %>% group_by(country, landscape_no, hh_refno, round) %>%
  summarize(literate=mean(literate, na.rm=T), years=mean(years, na.rm=T), age=mean(hh_b04))

household_capital <- merge(hh_size, capital)

hc <- household_capital %>% group_by(country, landscape_no) %>%
  summarize(size=mean(size, na.rm=T),
            literate = mean(literate, na.rm=T),
            years = mean(years, na.rm=T),
            age = mean(age, na.rm=T))


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

writeS3(household_capital, 'Capital_HH.csv')
writeS3(hc, 'Capital_Landscape.csv')
