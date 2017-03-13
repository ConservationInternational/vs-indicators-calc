library(dplyr)

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

df <- tbl(vs_db, 'flagging__household_secC') %>%
  data.frame

age <- tbl(vs_db, 'flagging__household_secB') %>%
  data.frame

df <- merge(df, age, by=c('uuid', 'Country', 'latitude', 'longitude', 'Household.ID', 'Landscape..', 'survey_uuid'))

years <- read.csv('YearsOfSchooling.csv')

df$school_code <- as.numeric(df$hh_c07)
df$literate <- df$hh_c02 != '5'

ed <- merge(df, years, by=c('Country', 'school_code'), all.x=T)
ed$years[ed$hh_c03=='2'] <- 0

hh_size <- ed %>% group_by(Country, Landscape.., latitude, longitude, survey_uuid) %>%
  summarize(n=n()) %>%
  group_by(Country, Landscape.., latitude, longitude) %>%
  summarize(size=mean(n))

capital <- ed %>% group_by(Country, Landscape.., latitude, longitude) %>%
  summarize(literate=mean(literate, na.rm=T), years=mean(years, na.rm=T), age=mean(hh_b04))

household_capital <- merge(hh_size, capital)
write.csv(household_capital, 'hh_captital.csv', row.names=F)

ed$is_female <- ed$hh_b02=='2'
gender <- ed %>% filter(hh_b05=='1') %>% group_by(Country, Landscape.., latitude, longitude) %>%
  summarize(female=mean(is_female))



