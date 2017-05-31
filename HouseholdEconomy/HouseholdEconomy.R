library(dplyr)
library(ineq)

setwd('../HouseholdEconomy/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

##############################################
#Get accumulator
##############################################

allvars <- tbl(con, "c__household") %>%
  select(country, landscape_no, hh_refno, round) %>%
  collect

######################
# Income from business and wages
hhe <- tbl(con, "c__household_individual") %>%
  select(hh_e65_1, hh_e65_2, hh_e64_1, hh_e64_2, 
         hh_e22_1, hh_e22_2, hh_e24_1, hh_e24_2,
         hh_e08, hh_e51,
         hh_refno, round) %>%
  collect 

hhe$net_income_business <- 0
hhe$net_income_business[which(hhe$hh_e65_2=='Week')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='Week')]*52.14286
hhe$net_income_business[which(hhe$hh_e65_2=='Month')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='Month')]*12

hhe$gross_income_business <- 0
hhe$gross_income_business[which(hhe$hh_e64_2=='Week')] <- hhe$hh_e64_1[which(hhe$hh_e64_2=='Week')]*52.14286
hhe$gross_income_business[which(hhe$hh_e64_2=='Month')] <- hhe$hh_e64_1[which(hhe$hh_e64_2=='Month')]*12

times <- c("Hour", "Day", "Week", "Fortnight", "Month", "Quarter", "Half Year", "Year")
annualized <- c(8*356, 365, 52.14286, 26.07143, 12, 4, 2, 1)


hhe$income_wage <- mapply(hhe$hh_e22_1*annualized[match(hhe$hh_e22_2, times)],
                          hhe$hh_e24_1*annualized[match(hhe$hh_e24_2, times)],
                          0, FUN=sum, na.rm=T)

hhe <- hhe %>% 
  group_by(hh_refno, round) %>% 
  summarize(net_income_business=sum(net_income_business, na.rm=T),
            gross_income_business=sum(gross_income_business, na.rm=T),
            income_wage=sum(income_wage, na.rm=T),
            does_wage_labor=mean(hh_e08, na.rm=T),
            has_own_business=mean(hh_e51, na.rm=T))

allvars <- left_join(allvars, hhe)

###########################
#rent

rent <- tbl(con, 'c__household') %>%
  select(hh_refno, round, hh_j03) %>%
  collect()

rent$cost_annual_rent <- rent$hh_j03*12
rent$hh_j03 <- NULL

allvars <- left_join(allvars, rent)

###########################
# Extension Services
extension <- tbl(con, 'c__household_extension') %>%
  select(hh_refno, round, ag12a_05) %>%
  collect

extension <- extension %>% 
  group_by(hh_refno, round) %>%
  summarize(cost_extension=sum(ag12a_05, na.rm=T))

allvars <- left_join(allvars, extension)

###########################
# Price Info
price <- tbl(con, 'c__household_priceinfo') %>%
  collect %>%
  group_by(hh_refno, round) %>%
  summarize(cost_priceinfo=sum(ag12b_09, na.rm=T))

allvars <- left_join(allvars, price)

#############################
# Water
water <- tbl(con, 'c__household_water') %>%
  collect

water$cy[which(water$hh_j23_2=='Day')] <- water$hh_j23_1[which(water$hh_j23_2=='Day')]*365
water$cy[which(water$hh_j23_2=='Week')] <- water$hh_j23_1[which(water$hh_j23_2=='Week')]*52
water$cy[which(water$hh_j23_2=='Month')] <- water$hh_j23_1[which(water$hh_j23_2=='Month')]*12

water$cy[which(water$hh_j22=="Drums/Tanks (Metal Or Plastic)")] <- water$cy[which(water$hh_j22=="Drums/Tanks (Metal Or Plastic)")]/10
water$cy[which(water$hh_j22=="Overhead Tank")] <- water$cy[which(water$hh_j22=="Overhead Tank")]/10
water$cy[which(water$hh_j22=="Underground Tank")] <- water$cy[which(water$hh_j22=="Underground Tank")]/10

water$cost_annual <- water$cy*water$hh_j24

water <- water %>%
  group_by(hh_refno, round) %>%
  summarize(cost_water=sum(cost_annual, na.rm=T))

allvars <- left_join(allvars, water)

############################################
#renting field income
field <- tbl(con, 'c__household_field_season') %>%
  select(hh_refno, round, ag3a_04) %>%
  collect

field <- field %>%
  group_by(hh_refno, round) %>%
  summarize(income_field_rent=sum(ag3a_04, na.rm=T))

allvars <- left_join(allvars, field)

#####################################
#household expenditures
expense <- tbl(con, 'c__household_expenditure') %>%
  collect()

expense$hh_paid[expense$hh_period=='week'] <- expense$hh_paid[expense$hh_period=='week']*52
expense$hh_paid[expense$hh_period=='month'] <- expense$hh_paid[expense$hh_period=='month']*12

expense <- expense %>%
  group_by(hh_refno, round) %>%
  summarize(cost_expenditures=sum(hh_paid, na.rm=T))

allvars <- left_join(allvars, expense)

#############################################################
#Adjust Currencies and combine
allvars <- merge(allvars, data.frame(country = c('GHA', 'RWA', 'UGA', 'TZA'),
                             rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c('net_income_business', 'gross_income_business', 'income_wage',
                'cost_annual_rent', 'cost_extension', 'cost_expenditures', 'cost_priceinfo',
                'cost_water', 'income_field_rent')

allvars[ , rateadjust] <- allvars[ , rateadjust]/allvars$rate

allvars <- mutate(allvars, total_nonag_income = gross_income_business + income_wage)
allvars <- mutate(allvars, total_nonag_costs = cost_annual_rent +cost_extension + 
                                                cost_expenditures + cost_priceinfo + cost_water)


allvars_landscape <- allvars %>% group_by(country, landscape_no) %>%
  summarize(net_income_business=mean(net_income_business, na.rm=T),
            gross_income_business=mean(gross_income_business, na.rm=T),
            income_wage=mean(income_wage, na.rm=T),
            does_wage_labor=mean(does_wage_labor, na.rm=T),
            has_own_business=mean(has_own_business, na.rm=T),
            cost_annual_rent=mean(cost_annual_rent, na.rm=T),
            cost_extension=mean(cost_extension, na.rm=T),
            cost_priceinfo=mean(cost_priceinfo, na.rm=T),
            cost_water=mean(cost_water, na.rm=T),
            income_field_rent=mean(income_field_rent, na.rm=T),
            cost_expenditures=mean(cost_expenditures, na.rm=T),
            total_nonag_income=mean(total_nonag_income, na.rm=T),
            total_nonag_costs=mean(total_nonag_costs, na.rm=T),
            ineq_total_nonag_income = ineq(total_nonag_income, type='Gini'))

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


writeS3(allvars, 'HouseholdEconomy_HH.csv')
writeS3(allvars_landscape, 'HouseholdEconomy_Landscape.csv')
