library(dplyr)
library(reshape2)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Crops/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

hh <- tbl(vs_db, 'flagging__agric') %>%
  select(Country, `Household ID`, Round, `Landscape #`) %>%
  data.frame

crops <- tbl(vs_db, 'flagging__agric_field_roster') %>% 
  select(`Household ID`, Round, Country, `Landscape #`, ag2a_vs_2d1, ag2a_vs_2b2_1) %>%
  data.frame

crops <- merge(hh, crops, all=T)

crops <- melt(crops, id.vars=c('Household.ID', 'Round', 'Country', 'Landscape..'))

sum_hh <- crops %>% group_by(Household.ID, Round, Country, Landscape..) %>%
  summarize(Bananas = '71' %in% value | '71a' %in% value | '71b' %in% value | '71c' %in% value,
            Maize = "11" %in% value,
            Paddy = "12" %in% value,
            Sorghum = "13" %in% value,
            Bulrush.Millet = "14" %in% value,
            Finger.Millet = "15" %in% value,
            Wheat = "16" %in% value,
            Cassava = "21" %in% value,
            Sweet.Potatoes = "22" %in% value,
            Irish.Potatoes = "23" %in% value,
            Yams = "24" %in% value,
            Onions = "26" %in% value,
            Beans = "31" %in% value,
            Pigeon.Pea = "34" %in% value,
            Field.Peas = "37" %in% value,
            Sunflower = "41" %in% value,
            Sesame.Simsim = "42" %in% value,
            Groundnut = "43" %in% value,
            Palm.Oil = "44" %in% value,
            Coconut = "45" %in% value,
            Cashew.Nut = "46" %in% value,
            Soyabeans = "47" %in% value,
            Cotton = "50" %in% value,
            Pyrethrum = "52" %in% value,
            Coffee = "54" %in% value,
            Tea = "55" %in% value,
            Cocoa = "56" %in% value,
            Rubber = "57" %in% value,
            Sugar.Cane = "60" %in% value,
            Pineapple = "75" %in% value,
            Orange = "76" %in% value,
            Tomatoes = "87" %in% value,
            Chilies = "90" %in% value,
            Egg.Plant = "94" %in% value,
            Okra = "100" %in% value,
            Timber = "304" %in% value) %>% data.frame

sum <- sum_hh %>% group_by(Country, Landscape..) %>%
  summarize(Bananas = mean(Bananas),
            Maize = mean(Maize),
            Paddy = mean(Paddy),
            Sorghum = mean(Sorghum),
            Bulrush.Millet = mean(Bulrush.Millet),
            Finger.Millet = mean(Finger.Millet),
            Wheat = mean(Wheat),
            Cassava = mean(Cassava),
            Sweet.Potatoes = mean(Sweet.Potatoes),
            Irish.Potatoes = mean(Irish.Potatoes),
            Yams = mean(Yams),
            Onions = mean(Onions),
            Beans = mean(Beans),
            Pigeon.Pea = mean(Pigeon.Pea),
            Field.Peas = mean(Field.Peas),
            Sunflower = mean(Sunflower),
            Sesame.Simsim = mean(Sesame.Simsim),
            Groundnut = mean(Groundnut),
            Palm.Oil = mean(Palm.Oil),
            Coconut = mean(Coconut),
            Cashew.Nut = mean(Cashew.Nut),
            Soyabeans = mean(Soyabeans),
            Cotton = mean(Cotton),
            Pyrethrum = mean(Pyrethrum),
            Coffee = mean(Coffee),
            Tea = mean(Tea),
            Cocoa = mean(Cocoa),
            Rubber = mean(Rubber),
            Sugar.Cane = mean(Sugar.Cane),
            Pineapple = mean(Pineapple),
            Orange = mean(Orange),
            Tomatoes = mean(Tomatoes),
            Chilies = mean(Chilies),
            Egg.Plant = mean(Egg.Plant),
            Okra = mean(Okra),
            Timber = mean(Timber))

write.csv(sum_hh, 'crops.hh.csv', row.names=F)

write.csv(sum, 'crops.landscape.csv', row.names=F)
