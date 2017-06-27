library(dplyr)

setwd('../FoodSecurity/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

# hh_i031
# How many meals, including breakfast are taken per day in your household? (Adults: over 5 Years Old)
# 
# hh_i08
# In the last 12 months, have you been faced with a situation when you did not have enough food to feed the household?

fs <- tbl(vs_db, 'c__household') %>% 
  collect

# 2  Rely on less preferred foods? 
# 2  Limit the variety of foods eaten? 
# 5  Limit portion size at meal-times? 
# 5  Reduce number of meals eaten in a day? 
# 7  Restrict consumption by adults for small children to eat? 
# 7  Borrow food, or rely on help from a friend or relative? 
# 10  Have no food of any kind in your house-hold? 
# 10  Go a whole day and night without eating anything?

fs$months_insecurity <- rowSums(fs[ , c(paste0('hh_i09a_', seq(1,12)), paste0('hh_i09b_', seq(1,12)))], na.rm=T)

fs$hfias <- rowSums(fs[ , c('hh_i02_1', 'hh_i02_2', 'hh_i02_3', 'hh_i02_4', 'hh_i02_5', 'hh_i02_6', 'hh_i02_7', 'hh_i02_8')], na.rm=T)


f_groups <- c("hh_k2_8_a", "hh_k2_8_b", "hh_k2_8_c", "hh_k2_8_d", "hh_k2_8_e",
              "hh_k2_8_f","hh_k2_8_g",  "hh_k2_8_h", "hh_k2_8_i", "hh_k2_8_j")

fs$diversity <- rowSums(fs[f_groups] / 7, na.rm=T) / length(f_groups)

fs <- fs %>% 
  select(country, landscape_no, hh_refno, round, shortage_year=hh_i08, 
         months_insecurity, number_meals=hh_i031, hfias, diversity)

##Nonfood spending
##Do we need Sec L?

nfs <- tbl(vs_db, "c__household_expenditure") %>% 
  collect

nfs$hh_paid[nfs$hh_period=='week'] <- nfs$hh_paid[nfs$hh_period=='week']*52.14
nfs$hh_paid[nfs$hh_period=='year'] <- nfs$hh_paid[nfs$hh_period=='year']*12

nfs <- nfs %>%
  group_by(country, landscape_no, hh_refno, round) %>%
  summarize(Nonfood.Spending = sum(hh_paid, na.rm=T))


##Food Spending
food <- tbl(vs_db, "c__household_food") %>% 
  collect

food <- food %>% 
  rowwise() %>%
  mutate(FCV = sum(hh_k_04, hh_k_05a, na.rm=T)) %>%
  group_by(country, landscape_no, hh_refno, round) %>%
  summarise(Food.Consumption.Value = sum(FCV, na.rm = TRUE)*52.14, Food.Spending = sum(hh_k_04, na.rm=T)*52.14)

#Combine and aggregate
out <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, nfs, food))
out$Food_As_Percent_Total_Spending <- (out$Food.Spending/(out$Food.Spending + out$Nonfood.Spending))*100

out <- merge(out, data.frame(country = c('GHA', 'RWA', 'UGA', 'TZA'),
                                     Rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c('Nonfood.Spending', 'Food.Spending', 'Food.Consumption.Value')

out[ , rateadjust] <- out[ , rateadjust]/out$Rate

out_ls <- out %>% group_by(country, landscape_no) %>%
  summarize(avg_meals = mean(number_meals, na.rm=T),
            Percent_Shortage_Past_Year = mean(shortage_year, na.rm=T)*100,
            Mean_Months_Insecurity = mean(months_insecurity, na.rm=T),
            Mean_Diet_Diversity = mean(diversity, na.rm=T),
            Mean_Nonfood_Spending = mean(Nonfood.Spending, na.rm=T),
            Mean_Food_Consumption_Value = mean(Food.Consumption.Value, na.rm=T),
            Mean_Food_Spending = mean(Food.Spending, na.rm=T),
            Food_As_Percent_Total_Spending = mean(Food_As_Percent_Total_Spending, na.rm=T))

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


writeS3(out, 'FoodSecurity_HH.csv')
writeS3(out_ls, 'FoodSecurity_Landscape.csv')
