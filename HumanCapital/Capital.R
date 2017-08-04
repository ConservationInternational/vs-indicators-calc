library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/HumanCapital/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

df <- tbl(vs_db, 'c__household_individual') %>%
  select(country, landscape_no, hh_refno, round, 
         hh_b04, hh_c02, label=hh_c07, hh_c03) %>%
  collect

years <- read.csv('YearsOfSchooling.csv')

df$literate <- df$hh_c02 != 'No'

ed <- merge(df, years, by=c('country', 'label'), all.x=T)
ed$years[!ed$hh_c03] <- 0

capital <- ed %>% 
  group_by(country, landscape_no, hh_refno, round) %>%
  summarize(literate=mean(literate, na.rm=T), 
            years=mean(years, na.rm=T), 
            age=mean(hh_b04),
            size=n())

hc <- capital %>% 
  group_by(country, landscape_no) %>%
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
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}

writeS3(capital, 'Capital_HH.csv')
writeS3(hc, 'Captial_Landscape.csv')
