library(dplyr)
library(reshape2)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/FoodSecurity/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)



# hh_i031
# How many meals, including breakfast are taken per day in your household? (Adults: over 5 Years Old)
# 
# hh_i08
# In the last 12 months, have you been faced with a situation when you did not have enough food to feed the household?

fs <- tbl(vs_db, 'flagging__household_secI') %>% data.frame

fs$shortage_year <- fs$hh_i08=="1"

fs <- fs %>% select(Country, Landscape.., Household.ID, Round, shortage_year, number_meals=hh_i031)

diet <- tbl(vs_db, 'flagging__household_secK2') %>% data.frame

f_groups <- c("k2_8_a", "k2_8_b", "k2_8_c", "k2_8_d", "k2_8_e",
              "k2_8_f","k2_8_g",  "k2_8_h", "k2_8_i", "k2_8_j")

diet$diversity <- rowSums(diet[f_groups] / 7, na.rm=T) / length(f_groups)

diet <- diet %>% select(Country, Landscape.., Household.ID, Round, diversity)

##Nonfood spending
##Do we need Sec L?

nonfood_spending <- tbl(vs_db, "flagging__household_secL") %>% data.frame
nonfood_spending_b <- tbl(vs_db, "flagging__household_secM") %>% data.frame

# keep the first 4 columns, while subsequently dropping every other column
df <- nonfood_spending[ , c('Country', 'Landscape..', 'Household.ID', 'Round', names(nonfood_spending)[grepl('_2$', names(nonfood_spending))])]

# melt to long shape
df <- melt(df, 
           id.vars = c("Country", "Landscape..", "Household.ID", "Round"),
           variable.name = "nonfood.code",
           value.name = "amount.spent")

#list of items measured weekly.  All other items measured monthly
weekly <- c('l_101_2', 'l_102_2', 'l_103_2', 'l_199_2', 'l_204_2', 'l_206_2', 'l_207_2', 'l_207_2a')

df[df$nonfood.code %in% weekly,'amount.spent'] <- df[df$nonfood.code %in% weekly,'amount.spent']/7*365.24
df[!df$nonfood.code %in% weekly,'amount.spent'] <- df[!df$nonfood.code %in% weekly,'amount.spent']/31*365.24

nfs <- df %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(total.spent.a=sum(amount.spent, na.rm=T))


df2 <- nonfood_spending_b[ , c('Country', 'Landscape..', 'Household.ID', 'Round', names(nonfood_spending_b)[grepl('_2$', names(nonfood_spending_b))])]

df2 <- melt(df2,
            id.vars = c("Country", "Landscape..", "Household.ID", "Round"),
            variable.name = "nonfood.code",
            value.name = "amount.spent")

nfs2 <- df2 %>% group_by(Country, Landscape.., Household.ID, Round) %>%
  summarize(total.spent.b=sum(amount.spent, na.rm=T))

nfs3 <- merge(nfs, nfs2)

nfs3$Nonfood.Spending <- nfs3$total.spent.a + nfs3$total.spent.b

nfs3 <- nfs3 %>% select(Country, Landscape.., Household.ID, Round, Nonfood.Spending)


##Food Spending
df <- tbl(vs_db, "flagging__household_secK1") %>% data.frame #FS 6

food <- df %>% group_by(Country, Landscape.., Household.ID, Round) %>% 
  summarise(Food.Consumption.Value = sum(k_04 + k_05a, na.rm = TRUE)*52.14, Food.Spending = sum(k_04, na.rm=T)*52.14)

#Combine and aggregate
out <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, diet, nfs3, food))
out$Food_As_Percent_Total_Spending <- (out$Food.Spending/(out$Food.Spending + out$Nonfood.Spending))*100

out <- merge(out, data.frame(Country = c('GHA', 'RWA', 'UGA', 'TZA'),
                                     Rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c('Nonfood.Spending', 'Food.Spending', 'Food.Consumption.Value')

out[ , rateadjust] <- out[ , rateadjust]/out$Rate

out_ls <- out %>% group_by(Country, Landscape..) %>%
  summarize(avg_meals = mean(number_meals, na.rm=T),
            Percent_Shortage_Past_Year = mean(shortage_year, na.rm=T)*100,
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
  names(df) <- gsub('.', '_', names(df), fixed=T)
  names(df)[names(df)=='Landscape__'] <- 'Landscape'
  
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}


writeS3(out, 'FoodSecurity_HH.csv')
writeS3(out_ls, 'FoodSecurity_Landscape.csv')
