library(dplyr)

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

result <- fs %>% group_by(Country, Landscape.., latitude, longitude) %>%
  summarize(avg_meals = mean(hh_i031), food_shortage = mean(shortage_year))


diet <- tbl(vs_db, 'flagging__household_secK2') %>% data.frame

f_groups <- c("k2_8_a", "k2_8_b", "k2_8_c", "k2_8_d", "k2_8_e",
              "k2_8_f","k2_8_g",  "k2_8_h", "k2_8_i", "k2_8_j")

diet$diversity <- rowSums(diet[f_groups] / 7, na.rm=T) / length(f_groups)

dd <- diet %>% group_by(Country, Landscape.., latitude, longitude) %>%
  summarize(diet_diversity = mean(diversity))

