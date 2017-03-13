library(dplyr)
library(ineq)
library(ggplot2)

setwd('../Ag')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)


##Get Maize Yields

#  agric_crops_by_field
#    ag4a_21 - What was the total value of seeds purchased?

allvars <- tbl(con, 'flagging__agric_crops_by_field') %>%
  filter(`Crop name` == 'Maize') %>%
  select(Country, `Landscape #`, `Household ID`, `Field ID`, Season, Round,
         ag4a_15, ag4a_21) %>%
  data.frame

allvars$Yield <- allvars$ag4a_15

#  agric_field_details
#    ag3a_32 What was the total value of [FERTILIZER] purchased?
#    ag3a_61 What was the total value of this pesticides/ herbicides purchased?
#    ag3a_22 What was the total value of organic fertilizer purchased?

inc7 <- tbl(con, 'flagging__agric_field_details') %>%
  select(Country, `Landscape #`, `Household ID`, `Field ID`, Season, Round, ag3a_32, ag3a_61, ag3a_22) %>%
  data.frame

allvars <- merge(allvars, inc7, all.x=F, all.y=F)

###################
##Results
###################


allvars$Investment <- rowSums(allvars[ , c("ag4a_21", "ag3a_32", "ag3a_61", "ag3a_22")], na.rm=T)

allvars <- merge(allvars, data.frame(Country = c("TZA", "RWA", "UGA"), USD=c(2230.5, 826, 3589)))

allvars$Investment <- allvars$Investment/allvars$USD

ggplot(allvars[allvars$Investment > 0 & allvars$Country =='TZA', ], aes(x=log(Investment), y=log(Yield))) + geom_point(aes(color=Landscape..)) +
  geom_smooth(method = "lm", se = FALSE)
