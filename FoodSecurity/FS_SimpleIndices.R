library(dplyr)
library(reshape2)

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

fs <- tbl(vs_db, 'flagging__household_secI') %>% data.frame

# hh_i031
# How many meals, including breakfast are taken per day in your household? (Adults: over 5 Years Old)
# 
# hh_i08
# In the last 12 months, have you been faced with a situation when you did not have enough food to feed the household?

fs$shortage_year <- fs$hh_i08=="1"

result <- fs %>% group_by(Country, Landscape..) %>%
  summarize(avg_meals = mean(hh_i031), food_shortage = mean(shortage_year))


diet <- tbl(vs_db, 'flagging__household_secK2') %>% data.frame

f_groups <- c("k2_8_a", "k2_8_b", "k2_8_c", "k2_8_d", "k2_8_e",
              "k2_8_f","k2_8_g",  "k2_8_h", "k2_8_i", "k2_8_j")

diet$diversity <- rowSums(diet[f_groups] / 7, na.rm=T) / length(f_groups)

dd <- diet %>% group_by(Country, Landscape..) %>%
  summarize(diet_diversity = mean(diversity))


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

nfs3 <- merge(nfs, nfs2) %>% group_by(Country, Landscape..) %>%
  summarize(Nonfood.Spending=mean(total.spent.a + total.spent.b, na.rm=T))

##Food Spending
df <- tbl(vs_db, "flagging__household_secK1") %>% data.frame #FS 6

food <- df %>% group_by(Country, Landscape.., Household.ID, Round) %>% 
  summarise(Food.Consumption.Value = sum(k_04 + k_05a, na.rm = TRUE)*52.14, Food.Spending = sum(k_04, na.rm=T)*52.14) %>%
  group_by(Country, Landscape..) %>%
  summarize(Food.Consumption.Value = mean(Food.Consumption.Value, na.rm=T), Food.Spending = mean(Food.Spending, na.rm=T))

out <- Reduce(merge, list(result, dd, nfs3, food))

write.csv(out, 'fs_outputs.csv', row.names=F)
