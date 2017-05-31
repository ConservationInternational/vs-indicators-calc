library(dplyr)
library(ineq)

setwd('../Ag')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

allvars <- tbl(con, "c__household") %>%
  select(country, landscape_no, hh_refno, round) %>%
  collect

########################
# Field Level Variables
field_size <- tbl(con, "c__household_field") %>%
  select(hh_refno, round, ag2a_09) %>%
  collect %>%
  group_by(hh_refno, round) %>%
  summarize(median_field_size = median(ag2a_09, na.rm=T), 
            total_area_farmed = sum(ag2a_09, na.rm=T),
            number_of_fields = n())

allvars <- left_join(allvars, field_size)

#####################################################################
# Field-Season level variables (inputs), summarized to household level
inputs <- tbl(con, "c__household_field_season") %>%
  select(hh_refno, round, ag3a_17, ag3a_45,
         ag3a_59, ag3a_58, ag3a_39,
         fert_residue=ag3a_39_1,
         fert_manure=ag3a_39_2,
         fert_naturalfallow=ag3a_39_4,
         fert_legumefallow=ag3a_39_5,
         fert_covercrop=ag3a_39_6,
         fert_biomass=ag3a_39_7,
         fert_compost=ag3a_39_8,
         fert_dap=ag3a_45_dap,
         fert_urea=ag3a_45_urea,
         fert_tsp=ag3a_45_tsp,
         fert_can=ag3a_45_can,
         fert_sa=ag3a_45_sa,
         fert_npk=ag3a_45_npk,
         fert_mrp=ag3a_45_mrp,
         ag3a_43, ag3a_49, ag3a_56,
         ag31_13, ag3a_61,
         ag3a_72_1d, ag3a_72_2d, ag3a_72_3d, ag3a_72_4d) %>%
  collect

inputs$hired_labor <- rowSums(inputs[ , c('ag3a_72_1d', 'ag3a_72_2d', 'ag3a_72_3d', 'ag3a_72_4d')], na.rm=T)

inputs$ag3a_59[!inputs$ag3a_58] <- FALSE

inputs$pesticide <- inputs$ag3a_59==1
inputs$herbicide <- inputs$ag3a_59==2
inputs$fungicide <- inputs$ag3a_59==3

inputs$fert_total_paid <- rowSums(data.frame(inputs$ag3a_49, inputs$ag3a_56), na.rm=T)

inputs <- inputs %>% group_by(hh_refno, round) %>%
  summarize(pct_fields_irrigated = mean(ag3a_17, na.rm=T),
          pct_fields_inorganic_fert = mean(ag3a_45, na.rm=T),
          pct_fields_organic_fert = mean(ag3a_39, na.rm=T),
          pesticide = mean(pesticide, na.rm=T), 
          herbicide = mean(herbicide, na.rm=T),
          fungicide = mean(fungicide, na.rm=T),
          fert_residue=mean(fert_residue, na.rm=T),
          fert_manure=mean(fert_manure, na.rm=T),
          fert_naturalfallow=mean(fert_naturalfallow, na.rm=T),
          fert_legumefallow=mean(fert_legumefallow, na.rm=T),
          fert_covercrop=mean(fert_covercrop, na.rm=T),
          fert_biomass=mean(fert_biomass, na.rm=T),
          fert_compost=mean(fert_compost, na.rm=T),
          fert_dap=mean(fert_dap, na.rm=T),
          fert_urea=mean(fert_urea, na.rm=T),
          fert_tsp=mean(fert_tsp, na.rm=T),
          fert_can=mean(fert_can, na.rm=T),
          fert_sa=mean(fert_sa, na.rm=T),
          fert_npk=mean(fert_npk, na.rm=T),
          fert_mrp=mean(fert_mrp, na.rm=T),
          cost_org_fert=sum(ag3a_43, na.rm=T),
          cost_syn_fert=sum(fert_total_paid, na.rm=T),
          cost_irrigation=sum(ag31_13, na.rm=T),
          cost_herbpesticides=sum(ag3a_61, na.rm=T),
          cost_hired_labor=sum(hired_labor, na.rm=T))
          

allvars <- left_join(allvars, inputs)

#####################################
# Field - Season - Crop level
crops <- tbl(con, "c__household_field_season_fieldcrop") %>%
  collect %>%
  group_by(hh_refno, round) %>%
  summarize(intercrop_rate = mean(ag4a_04, na.rm=T),
            pct_buy_seed=mean(ag4a_19, na.rm=T),
            cost_seeds_purchased=mean(ag4a_21, na.rm=T),
            value_fieldcrop_harvest=mean(ag4a_16, na.rm=T))

allvars <- left_join(allvars, crops)


########################
# Yields - get the Z score for crop-unit across the whole country
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
yields <- tbl(con, "c__household_field_season_fieldcrop") %>%
  filter(!is.na(ag4a_15) & !is.na(ag4a_08) & ag4a_08 > 0) %>%
  select(hh_refno, round, country, crop_name, ag4a_08, ag4a_15, ag4a_15_unit) %>%
  collect

yields$yield <- yields$ag4a_15/yields$ag4a_08

yields$cs <- paste0(yields$crop_name, yields$ag4a_15_unit, yields$country)

for (c in unique(yields$cs)){
  sel <- yields$yield[yields$cs==c]
  if (length(sel) > 4){
    sel <- sel + rnorm(length(sel), 0, .1) #slight jitter, because cut() doesnt work with repeats
    sel <- as.integer(cut(sel, quantile(sel, probs=0:4/4), include.lowest=TRUE))
    yields$yield_quantile[yields$cs==c] <- sel
  }else{
    yields$yield_quantile[yields$cs==c] <- NA
  }
}

yields <- yields %>% 
  group_by(hh_refno, round) %>% 
  summarize(yield_quantile = mean(yield_quantile, na.rm=T))

allvars <- left_join(allvars, yields)

############################
#Total Area Owned
ownership <- tbl(con, 'c__household_field_season') %>%
  filter(ag3a_14=='1') %>%
  select(hh_refno, round, field_no) %>% 
  collect

fieldsize <- tbl(con, 'c__household_field') %>%
  select(hh_refno, round, field_no, ag2a_09) %>% 
  collect

combo <- merge(ownership, fieldsize)
combo$ag2a_09[is.na(combo$ag2a_09)] <- 0

hh_total <- combo %>% group_by(hh_refno, round) %>% 
  summarize(total_area_owned=sum(ag2a_09))

allvars <- left_join(allvars, hh_total)


###############################
#Was any sold Sales
sold <- tbl(con, "c__household_fieldcrop") %>%
  select(hh_refno, round, ag5a_01,
         ag5a_20, ag5a_22, ag5a_03, ag5a_26) %>%
  collect

sold <- sold %>% group_by(hh_refno, round) %>%
  summarize(pct_crops_any_sold = mean(ag5a_01, na.rm=T),
            pct_crops_losses = mean(ag5a_20, na.rm=T),
            overall_pct_lost = mean(ag5a_22, na.rm=T),
            income_fieldcrop_sales = sum(ag5a_03, na.rm=T),
            income_residue_sales = sum(ag5a_26, na.rm=T)) %>%
  collect

allvars <- left_join(allvars, sold)


#######################
#  Permanent Crops Field
perm <- tbl(con, 'c__household_field_permcrop') %>%
  collect %>%
  group_by(hh_refno, round) %>% 
  summarize(value_permcrops = sum(ag6a_09, na.rm=T))

perm_hh <- tbl(con, 'c__household_permcrop') %>%
  collect %>%
  group_by(hh_refno, round) %>% 
  summarize(income_permcrops = sum(ag7a_04, na.rm=T))

perm <- full_join(perm, perm_hh)

perm$value_permcrops <- perm$value_permcrops - perm$income_permcrops

allvars <- left_join(allvars, perm)

###################################
#  livestock_byproduct
livestock_byprod <- tbl(con, 'c__household_livestockbyprod') %>%
  collect %>%
  group_by(hh_refno, round) %>% 
  summarize(value_livestockbyprod_consumed = sum(ag10b_1b, na.rm=T) - sum(ag10b_06, na.rm=T),
            income_livestockbyprod_sales = sum(ag10b_06, na.rm=T))

allvars <- left_join(allvars, livestock_byprod)


############################################################
#  livestock, estimating value of home consumption
livestock <- tbl(con, 'c__household_livestock') %>%
  select(country, landscape_no, hh_refno, animal_name, round, ag10a_25, ag10a_26, 
         ag10a_27, ag10a_20, ag10a_21, ag10a_34) %>%
  collect

livestock$Home.Use <- livestock$ag10a_25 - livestock$ag10a_26

livestock$Total.Sold <- mapply(livestock$ag10a_20, livestock$ag10a_25, FUN=sum, na.rm=T)
livestock$Total.Value <- mapply(livestock$ag10a_27, livestock$ag10a_21, FUN=sum, na.rm=T)

livestock$PerAnimalValue[livestock$Total.Sold!=0] <- livestock$Total.Value[livestock$Total.Sold!=0]/livestock$Total.Sold[livestock$Total.Sold!=0]

landscape_value <- livestock %>% 
  group_by(country, landscape_no, animal_name) %>% 
  summarize(lvalue=mean(PerAnimalValue, na.rm=T))
country_value <- livestock %>% 
  group_by(country, animal_name) %>%
  summarize(cvalue=mean(PerAnimalValue, na.rm=T))

value <- merge(landscape_value, country_value)
value$lvalue[is.nan(value$lvalue)] <- value$cvalue[is.nan(value$lvalue)]
value <- value[!is.nan(value$lvalue) & !is.infinite(value$lvalue), ] %>% select(-cvalue)

livestock <- merge(livestock, value)

livestock$home.value <- livestock$Home.Use*livestock$lvalue

livestock <- livestock %>% 
  group_by(hh_refno, round) %>% 
  summarize(value_consumed_livestock=sum(home.value, na.rm=T),
            income_livestock_sales=sum(Total.Value, na.rm=T),
            cost_livestock_labor=sum(ag10a_34, na.rm=T))

allvars <- left_join(allvars, livestock)


#######################################################
#  byprods, estimating household consumption
#   - ag09_11   What were the total costs of these additional expenses?
#   ag09_04_1	AMOUNT - What is the quantity produced in the last 12 months?  
#   ag09_04_2	UNIT
#   ag09_05	Was any [BY-PRODUCT/PROCESSED PRODUCT] sold?
#   ag09_06_1	AMOUNT - How much was sold?
#   ag09_06_2	UNIT
#   ag09_08	What was total sales in local currency?

#since the units are missing for a lot, assume every crop is in the same units
cropbyprod <- tbl(con, 'c__household_cropbyprod') %>%
  select(country, landscape_no, hh_refno, round, ag09_11, ag09_04_1, ag09_05, ag09_06_1, ag09_08, crop_name) %>%
  collect

cropbyprod$Not.Sold <- mapply(cropbyprod$ag09_04_1, -cropbyprod$ag09_06_1, FUN=sum, na.rm=T)
cropbyprod$PerUnitValue <- cropbyprod$ag09_08/cropbyprod$ag09_06_1

landscape_value <- cropbyprod %>% 
  group_by(country, landscape_no, crop_name) %>% 
  summarize(lvalue=mean(PerUnitValue, na.rm=T))
country_value <- cropbyprod %>% 
  group_by(country, crop_name) %>% 
  summarize(cvalue=mean(PerUnitValue, na.rm=T))

value <- merge(landscape_value, country_value)
value$lvalue[is.nan(value$lvalue)] <- value$cvalue[is.nan(value$lvalue)]
value <- value[!is.nan(value$lvalue) & !is.infinite(value$lvalue), ] %>% select(-cvalue)

cropbyprod <- merge(cropbyprod, value)

cropbyprod$value <- cropbyprod$Not.Sold*cropbyprod$lvalue

cropbyprod <- cropbyprod %>% 
  group_by(hh_refno, round) %>% 
  summarize(value_consumed_cropbyprods=sum(value),
            income_cropbyprods_sold=sum(ag09_08),
            cropbyprod_pct_sold=mean(ag09_05),
            cost_cropbyprod_expenses=sum(ag09_11, na.rm=T))

allvars <- merge(allvars, cropbyprod, all.x=T)


#############################################################
#Adjust Currencies and combine

allvars <- merge(allvars, data.frame(country = c('GHA', 'RWA', 'UGA', 'TZA'),
                                     rate    = c(4.348, 838.8, 3595, 2236)), all.x=T)

rateadjust <- c('cost_org_fert', 'cost_syn_fert', 'cost_irrigation', 'cost_herbpesticides', 
                'cost_seeds_purchased', 'cost_hired_labor', 'value_fieldcrop_harvest', 'income_fieldcrop_sales',
                'income_residue_sales', 'value_permcrops', 'income_permcrops', 'value_consumed_livestock', 'income_livestock_sales',
                'cost_livestock_labor', 'value_consumed_cropbyprods', 'income_cropbyprods_sold', 
                'cost_cropbyprod_expenses', 'value_livestockbyprod_consumed', 'income_livestockbyprod_sales')

allvars[ , rateadjust] <- allvars[ , rateadjust]/allvars$rate

allvars$total_ag_sales_income <- rowSums(allvars[ , grepl('income', names(allvars))], na.rm=T)
allvars$total_ag_production_value <- rowSums(allvars[ , c(grepl('value', names(allvars)))], na.rm=T) + allvars$total_ag_sales_income
allvars$total_ag_costs <- rowSums(allvars[ , grepl('cost', names(allvars))], na.rm=T)
allvars$net_ag_income <- allvars$total_ag_sales_income - allvars$total_ag_costs
allvars$net_ag_production_value <- allvars$total_ag_production_value - allvars$total_ag_costs
allvars$crop_commodification_index <- allvars$total_ag_sales_income/allvars$total_ag_production_value


#############################################################
#Get landscape level 

allvars_landscape <- allvars %>%
  group_by(country, landscape_no, round) %>%
  summarize(median_field_size=mean(median_field_size, na.rm=T),
            total_area_farmed=mean(total_area_farmed, na.rm=T),
            number_of_fields=mean(number_of_fields, na.rm=T),
            pct_fields_irrigated=mean(pct_fields_irrigated, na.rm=T),
            pct_fields_inorganic_fert=mean(pct_fields_inorganic_fert, na.rm=T),
            pct_fields_organic_fert=mean(pct_fields_organic_fert, na.rm=T),
            pesticide=mean(pesticide, na.rm=T),
            herbicide=mean(herbicide, na.rm=T),
            fungicide=mean(fungicide, na.rm=T),
            fert_residue=mean(fert_residue, na.rm=T),
            fert_manure=mean(fert_manure, na.rm=T),
            fert_naturalfallow=mean(fert_naturalfallow, na.rm=T),
            fert_legumefallow=mean(fert_legumefallow, na.rm=T),
            fert_covercrop=mean(fert_covercrop, na.rm=T),
            fert_biomass=mean(fert_biomass, na.rm=T),
            fert_compost=mean(fert_compost, na.rm=T),
            fert_dap=mean(fert_dap, na.rm=T),
            fert_urea=mean(fert_urea, na.rm=T),
            fert_tsp=mean(fert_tsp, na.rm=T),
            fert_can=mean(fert_can, na.rm=T),
            fert_sa=mean(fert_sa, na.rm=T),
            fert_npk=mean(fert_npk, na.rm=T),
            fert_mrp=mean(fert_mrp, na.rm=T),
            cost_org_fert=mean(cost_org_fert, na.rm=T),
            cost_syn_fert=mean(cost_syn_fert, na.rm=T),
            cost_irrigation=mean(cost_irrigation, na.rm=T),
            cost_herbpesticides=mean(cost_herbpesticides, na.rm=T),
            cost_hired_labor=mean(cost_hired_labor, na.rm=T),
            intercrop_rate=mean(intercrop_rate, na.rm=T),
            pct_buy_seed=mean(pct_buy_seed, na.rm=T),
            cost_seeds_purchased=mean(cost_seeds_purchased, na.rm=T),
            value_fieldcrop_harvest=mean(value_fieldcrop_harvest, na.rm=T),
            yield_quantile=mean(yield_quantile, na.rm=T),
            total_area_owned=mean(total_area_owned, na.rm=T),
            pct_crops_any_sold=mean(pct_crops_any_sold, na.rm=T),
            pct_crops_losses=mean(pct_crops_losses, na.rm=T),
            overall_pct_lost=mean(overall_pct_lost, na.rm=T),
            income_fieldcrop_sales=mean(income_fieldcrop_sales, na.rm=T),
            income_residue_sales=mean(income_residue_sales, na.rm=T),
            value_permcrops=mean(value_permcrops, na.rm=T),
            income_permcrops=mean(income_permcrops, na.rm=T),
            value_livestockbyprod_consumed=mean(value_livestockbyprod_consumed, na.rm=T),
            value_consumed_livestock=mean(value_consumed_livestock, na.rm=T),
            income_livestock_sales=mean(income_livestock_sales, na.rm=T),
            cost_livestock_labor=mean(cost_livestock_labor, na.rm=T),
            value_consumed_cropbyprods=mean(value_consumed_cropbyprods, na.rm=T),
            income_cropbyprods_sold=mean(income_cropbyprods_sold, na.rm=T),
            cropbyprod_pct_sold=mean(cropbyprod_pct_sold, na.rm=T),
            cost_cropbyprod_expenses=mean(cost_cropbyprod_expenses, na.rm=T),
            income_livestockbyprod_sales=mean(income_livestockbyprod_sales, na.rm=T),
            total_ag_sales_income=mean(total_ag_sales_income, na.rm=T),
            total_ag_costs=mean(total_ag_costs, na.rm=T),
            net_ag_income=mean(net_ag_income, na.rm=T),
            total_ag_production_value=mean(total_ag_production_value, na.rm=T),
            net_ag_production_value=mean(net_ag_production_value, na.rm=T),
            crop_commodification_index=mean(crop_commodification_index, na.rm=T),
            ineq_net_ag_income=Gini(net_ag_income),
            ineq_total_ag_production_value=Gini(total_ag_production_value),
            ineq_total_area_owned=Gini(total_area_owned))

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

writeS3(allvars_landscape, 'Ag_Landscape.csv')
writeS3(allvars, 'Ag_HH.csv')
