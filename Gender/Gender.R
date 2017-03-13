library(plyr)
library(dplyr)

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)
vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

hh_sec_b <- tbl(vs_db, build_sql('SELECT * FROM "curation__household_secB"')) %>%
  data.frame

hh_sec_c <- tbl(vs_db, build_sql('SELECT * FROM "curation__household_secC"')) %>%
  data.frame

hh_sec_e <- tbl(vs_db, build_sql('SELECT * FROM "curation__household_secE"')) %>%
  data.frame

hh_sec_hv1 <- tbl(vs_db, build_sql('SELECT * FROM "curation__household_secHV1"')) %>%
  data.frame

hh_all <- 
  merge(hh_sec_b, hh_sec_c, by=c("Household.ID", "Individual.ID", "Landscape..", "Country")) %>%
  select(Country, Landscape.., Household.ID, Individual.ID, hh_b02, hh_b04, hh_c02, hh_c03, hh_c07) %>%
  
  merge(hh_sec_e, by=c("Household.ID", "Individual.ID", "Landscape..", "Country")) %>%
  select(Country, Landscape.., Household.ID, Individual.ID, hh_b02, hh_b04, hh_c02, hh_c03, hh_c07, hh_e04, hh_e25, hh_e24_1, hh_e24_2, hh_e52, hh_e06, hh_e65_1, hh_e65_2) %>%
  
  merge(hh_sec_hv1, by=c("Household.ID", "Individual.ID", "Landscape..", "Country")) %>%
  select(Country, Landscape.., Household.ID, Individual.ID, hh_b02, hh_b04, hh_c02, hh_c03, hh_c07, hh_e04, hh_e25, hh_e24_1, hh_e24_2, hh_e52, hh_e06, hh_e65_1, hh_e65_2, hh_hv103, hh_hv104, hh_hv105, hh_hv105_unit, hh_hv105a) %>%
  
  data.frame

hh_all$Gender <- revalue(hh_all$hh_b02, c(`1`='Male', `2`='Female'))


#Education
## Literacy
#Can [NAME] read or write? (In English or Local Languages)      

hh_all$hh_c02 <- revalue(hh_all$hh_c02, c(`1`=1, `2`=1, `3`=1, `4`=1, `5`=0)) %>% as.numeric

all_data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Literacy.Rate=mean(hh_c02, na.rm=T)) %>% data.frame


## School Attendence Rate
##Has [NAME] ever attended school?       
  

hh_all$hh_c03 <- revalue(hh_all$hh_c03, c(`1`=1, `2`=0)) %>% as.numeric

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Percent.Attended.School=mean(hh_c03, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)

#Labor
## Worked in the past week
#Did you do any work for any type of pay, profit, barter or home use during the last 7 days? (includes farm activites)    
hh_all$hh_e04 <- revalue(hh_all$hh_e04, c(`1`=1, `2`=0)) %>% as.numeric

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Percent.Worked.Recently=mean(hh_e04, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)


## Hours Worked In Previous Week
hh_all$Hours.Worked.Last.Week <- hh_all$hh_e25

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Mean.Hours.Worked.Last.Week=mean(hh_e25, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)


## Businesses
hh_all$hh_e52 <- revalue(hh_all$hh_e52, c(`1`=1, `2`=0)) %>% as.numeric

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Percent.Operated.Business=mean(hh_e52, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)


#Fuelwood
## Who collects?
hh_all$hh_hv103 <- revalue(hh_all$hh_hv103, c(`1`=1, `2`=0)) %>% as.numeric

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Percent.Collects.Firewood=mean(hh_hv103, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)




## Hours Spent Collecting Fuelwood
hh_all$Hours.Spent <- hh_all$hh_hv105a

data <- hh_all %>% group_by(Country, Landscape.., Gender) %>% summarize(Mean.Hours.Spent=mean(Hours.Spent, na.rm=T)) %>% data.frame

all_data <- merge(all_data, data, all=T)


#order
male_df <- all_data %>% filter(Gender=='Male')
names(male_df) <- c("Country", "Landscape..", paste0("Male.", names(male_df)[3:10]))

female_df <- all_data %>% filter(Gender=='Female')
names(female_df) <- c("Country", "Landscape..", paste0("Female.", names(female_df)[3:10]))

all <- merge(male_df, female_df)

write.csv(all, 'Gender.csv', row.names=F)
