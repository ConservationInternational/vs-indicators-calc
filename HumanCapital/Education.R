library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/HumanCapital/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

df <- tbl(vs_db, 'flagging__household_secC') %>%
  select(Country, `Landscape #`, `Household ID`, Round, hh_c02, hh_c07, hh_c03, uuid) %>%
  data.frame

age <- tbl(vs_db, 'flagging__household_secB') %>%
  select(Country, `Landscape #`, `Household ID`, Round, hh_b04, uuid) %>%
  data.frame

df <- merge(df, age, all=T)

years <- read.csv('YearsOfSchooling.csv')

df$school_code <- as.numeric(df$hh_c07)
df$literate <- df$hh_c02 != '5'

ed <- merge(df, years, by=c('Country', 'school_code'), all.x=T)
ed$years[ed$hh_c03=='2'] <- 0

hh_size <- ed %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(size=n())

capital <- ed %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(literate=mean(literate, na.rm=T), years=mean(years, na.rm=T), age=mean(hh_b04))

household_capital <- merge(hh_size, capital)
write.csv(household_capital, 'Capital.HH.csv', row.names=F)

hc <- household_capital %>% group_by(Country, Landscape..) %>%
  summarize(size=mean(size, na.rm=T),
            literate = mean(literate, na.rm=T),
            years = mean(years, na.rm=T),
            age = mean(age, na.rm=T))

write.csv(hc, 'Capital.Landscape.csv', row.names=F)
