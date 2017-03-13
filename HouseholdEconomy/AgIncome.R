library(dplyr)
library(ineq)

setwd('../Ag')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

##############################################
#Get accumulator
##############################################

allvars <- tbl(con, "flagging__agric") %>%
  select(survey_uuid, Country, `Landscape #`, `Household ID`, Round) %>%
  data.frame

# Income
#   Business Income - hh_e65_1 - income
#   
#   From - household_secE

hhe <- tbl(con, "flagging__household_secE") %>%
  select(hh_e65_1, hh_e65_2, #hh_e28, hh_e27, hh_e26,
         hh_e22_1, hh_e22_2, hh_e24_1, hh_e24_2,
         `Household ID`, Round) %>%
  data.frame 

hhe$income_own <- 0
hhe$income_own[which(hhe$hh_e65_2=='week')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='week')]*52.14286
hhe$income_own[which(hhe$hh_e65_2=='month')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='month')]*12

times <- c(1, 2, 3, 4, 5, 6, 7, 8)
annualized <- c(8*356, 365, 52.14286, 26.07143, 12, 4, 2, 1)

hhe$income_wage <- mapply(hhe$hh_e22_1*annualized[match(as.numeric(hhe$hh_e22_2), times)],
                          hhe$hh_e24_1*annualized[match(as.numeric(hhe$hh_e24_2), times)],
                          0, FUN=sum, na.rm=T)

hhe <- hhe %>% group_by(Household.ID, Round) %>% summarize(income_own=sum(income_own, na.rm=T),
                                                    income_wage=sum(income_wage, na.rm=T))

allvars <- merge(allvars, hhe, all.x=T)

# Ag Income - (((ag10a_21+ag10a_27)-ag10a_34) + ag7a_04 + ag10b_06 + (ag09_08 - ag09_11) + (ag5a_03 + ag5a_26)) * [[exchange rate]]
#  agric_crops_by_hh; 
#    ag5a_03
#    ag5a_26

inc1 <- tbl(con, 'flagging__agric_crops_by_hh') %>%
  select(`Household ID`, Round,
         ag5a_03, ag5a_26) %>%
  data.frame

inc1$income_crops <- mapply(inc1$ag5a_03, inc1$ag5a_26, FUN=sum, na.rm=T)

inc1[is.na(inc1)] <- 0

inc1 <- inc1 %>% group_by(Household.ID, Round) %>% summarize(income_crops=sum(income_crops, na.rm=T))

allvars <- merge(allvars, inc1, all.x=T)
allvars$income_crops[is.na(allvars$income_crops)] <- 0

#  agric_perm_crop; 
#    ag7a_04

inc2 <- tbl(con, 'flagging__agric_perm_crop') %>%
  select(`Household ID`, Round, ag7a_04) %>%
  data.frame

inc2[is.na(inc2)] <- 0
  
inc2 <- inc2 %>%
  group_by(Household.ID, Round) %>%
  summarize(income_perm_crop=sum(ag7a_04, na.rm=T))

allvars <- merge(allvars, inc2, all.x=T)
allvars$income_perm_crop[is.na(allvars$income_perm_crop)] <- 0

#  agric_livestock_byproduct; 
#    ag10b_06

inc3 <- tbl(con, 'flagging__agric_livestock_byproduct') %>%
  select(`Household ID`, Round, ag10b_06) %>%
  data.frame
  
inc3[is.na(inc3)] <- 0
  
inc3 <- inc3 %>%
  group_by(Household.ID, Round) %>%
  summarize(income_lvstk_byprod=sum(ag10b_06, na.rm=T))

allvars <- merge(allvars, inc3, all.x=T)
allvars$income_lvstk_byprod[is.na(allvars$income_lvstk_byprod)] <- 0

#  agric_byproduct; 
#    ag09_08
#    ag09_11

inc4 <- tbl(con, 'flagging__agric_byprod') %>%
  select(`Household ID`, Round, ag09_08, ag09_11) %>%
  data.frame

inc4$ag09_11[is.na(inc4$ag09_11)] <- 0

inc4$income_byprod <- inc4$ag09_08 - inc4$ag09_11

inc4[is.na(inc4)] <- 0

inc4 <- inc4 %>% group_by(Household.ID, Round) %>% summarize(income_byprod=sum(income_byprod, na.rm=T))

allvars <- merge(allvars, inc4, all.x=T)
allvars$income_byprod[is.na(allvars$income_byprod)] <- 0

#  agric_livestock
#    ag10a_21
#    ag10a_27
#    ag10a_34

inc5 <- tbl(con, 'flagging__agric_livestock') %>%
  select(`Household ID`, Round, ag10a_21, ag10a_27, ag10a_34) %>%
  data.frame

inc5$income_lvstk <- mapply(inc5$ag10a_21, inc5$ag10a_27, FUN=sum, na.rm=T)

inc5$ag10a_34[is.na(inc5$ag10a_34)] <- 0

inc5$income_lvstk <- inc5$income_lvstk - inc5$ag10a_34

inc5[is.na(inc5)] <- 0

inc5 <- inc5 %>% group_by(Household.ID, Round) %>% summarize(income_lvstk = sum(income_lvstk, na.rm=T))

allvars <- merge(allvars, inc5, all.x=T)
allvars$income_lvstk[is.na(allvars$income_lvstk)] <- 0


#  agric_crops_by_field
#    ag4a_21 - What was the total value of seeds purchased?

inc6 <- tbl(con, 'flagging__agric_crops_by_field') %>%
  select(`Household ID`, Round, ag4a_21) %>%
  data.frame

inc6 <- inc6 %>% group_by(Household.ID, Round) %>% summarize(cost_seeds = sum(ag4a_21, na.rm=T))

inc6[is.na(inc6)] <- 0

allvars <- merge(allvars, inc6)

#  agric_field_details
#    ag3a_32 What was the total value of [FERTILIZER] purchased?
#    ag3a_61 What was the total value of this pesticides/ herbicides purchased?
#    ag3a_22 What was the total value of organic fertilizer purchased?

inc7 <- tbl(con, 'flagging__agric_field_details') %>%
  select(`Household ID`, Round, ag3a_32, ag3a_61, ag3a_22) %>%
  data.frame

inc7 <- inc7 %>% group_by(Household.ID, Round) %>% summarize(cost_org_fert = sum(ag3a_22, na.rm=T), 
                                                      cost_syn_fert = sum(ag3a_32, na.rm=T),
                                                      cost_pesticide = sum(ag3a_61, na.rm=T))

inc7[is.na(inc7)] <- 0

allvars <- merge(allvars, inc7)

###################
##Results
###################


allvars$Total_Income <- rowSums(allvars[ , c("income_own", "income_wage", "income_crops", "income_perm_crop", 
                                            "income_lvstk_byprod", "income_byprod", "income_lvstk")], na.rm = T)

allvars$AgIncome <- rowSums(allvars[ , c("income_crops", "income_perm_crop", 
                                            "income_lvstk_byprod", "income_byprod", "income_lvstk")], na.rm = T)

allvars$NonAgIncome <- rowSums(allvars[ , c("income_own", "income_wage")], na.rm = T)

allvars$AgInvestments <- rowSums(allvars[ , c("cost_seeds", "cost_org_fert", "cost_syn_fert",
                                              "cost_pesticide")], na.rm = T)

income <- allvars %>% group_by(Country, Landscape..) %>%
  summarize(TotalIncome = mean(Total_Income),
            AgIncome = mean(AgIncome),
            NonAgIncome = mean(NonAgIncome),
            AgCosts = mean(AgInvestments),
            Income_Inequality_Gini = ineq(Total_Income, type='Gini'))

write.csv(income, 'income.csv', row.names=F)
