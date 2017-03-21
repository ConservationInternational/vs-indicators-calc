setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Combine/')

fs <- read.csv('../FoodSecurity/FoodSecurity.Landscape.csv')
ag_intens <- read.csv('../Ag/AgIntensification.Landscape.csv')
ag_value <- read.csv('../Ag/AgValue.Landscape.csv')
crop <- read.csv('../Crops/crops.landscape.csv')
degr_eros <- read.csv('../Degradation/ErosionControl.Landscape.csv')
degr <- read.csv('../Degradation/VS_landscapes_degradation_PRELIMINARY.CSV') %>%
  filter(time_period == '2003-2015' & ISO != 'KEN') %>%
  select(Country = ISO, Landscape..=Landscape, perc_degraded=degraded_area_km, perc_improved=improved_area_km)
gend <- read.csv('../Gender/Gender.Landscape.csv')
hh_econ <- read.csv('../HouseholdEconomy/Income.Landscape.csv')
h_cap <- read.csv('../HumanCapital/Capital.Landscape.csv')
nr <- read.csv('../NaturalResources/NaturalResources.Landscape.csv')
nut <- read.csv('../Nutrition/Nutrition.Landscape.csv') %>%
  select(-mean_zwei, -mean_zlen, -mean_zwfl)
ws <- read.csv('../WaterSanitation/WatSan.Landscape.csv')
lc <- read.csv('../LandCover/LandCover_2010.csv') %>%
  select(Landscape.., Country, NR_Total, AG_Total)

ls <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros, degr, 
                         gend, hh_econ, h_cap, nr, nut, ws, lc))

ls <- merge(ls, data.frame(Country = c('GHA', 'UGA', 'RWA', 'TZA'),
                           Rate = c(4.5085, 3584, 816.9, 2226)), all=T)

ls$Mean_Nonfood_Spending <- ls$Mean_Nonfood_Spending / ls$Rate
ls$Mean_Food_Consumption_Value <- ls$Mean_Food_Consumption_Value / ls$Rate
ls$Mean_Food_Spending <- ls$Mean_Food_Spending / ls$Rate
ls$TotalAgriculturalProduction <- ls$TotalAgriculturalProduction / ls$Rate
ls$TotalIncome <- ls$TotalIncome / ls$Rate
ls$AgIncome <- ls$AgIncome / ls$Rate
ls$NonAgIncome <- ls$NonAgIncome / ls$Rate
ls$Mean_Annual_Fuelwood_Value <- ls$Mean_Annual_Fuelwood_Value / ls$Rate
ls$Nonfuel_NR_annual_value <- ls$Nonfuel_NR_annual_value / ls$Rate


ls$CropCommercializationIndex <- ls$AgIncome / ls$TotalAgriculturalProduction

write.csv(ls, 'landscape_level.csv', row.names=F)

bio <- read.csv('../Biophysical/Biophysical.csv')
write.csv(bio, 'eplot_level.csv', row.names=F)

fs <- read.csv('../FoodSecurity/FoodSecurity.HH.csv')
ag_intens <- read.csv('../Ag/AgIntensification.HH.csv')
ag_value <- read.csv('../Ag/AgValue.HH.csv')
crop <- read.csv('../Crops/crops.HH.csv')
degr_eros <- read.csv('../Degradation/ErosionControl.HH.csv')
gend <- read.csv('../Gender/Gender.HH.csv')
hh_econ <- read.csv('../HouseholdEconomy/Income.HH.csv')
h_cap <- read.csv('../HumanCapital/Capital.HH.csv')
nr <- read.csv('../NaturalResources/NaturalResources.HH.csv')
nut <- read.csv('../Nutrition/Nutrition.HH.csv')
ws <- read.csv('../WaterSanitation/WatSan.HH.csv')

hh <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros,
                                                      gend, hh_econ, h_cap, nr, nut, ws))

hh <- merge(hh, data.frame(Country = c('GHA', 'UGA', 'RWA', 'TZA'),
                           Rate = c(4.5085, 3584, 816.9, 2226)), all=T)

hh$cost <- hh$cost / hh$Rate
hh$Food.Spending <- hh$Food.Spending / hh$Rate
hh$total_fuelwood_value <- hh$total_fuelwood_value / hh$Rate
hh$Nonfood.Spending <- hh$Nonfood.Spending / hh$Rate
hh$Total_Income <- hh$Total_Income / hh$Rate
hh$NonAgIncome <- hh$NonAgIncome / hh$Rate
hh$income_wage <- hh$income_wage / hh$Rate
hh$Nonfood.Spending <- hh$Nonfood.Spending / hh$Rate
hh$Food.Spending <- hh$Food.Spending / hh$Rate
hh$total <- hh$total/ hh$Rate
hh$Food.Consumption.Value <- hh$Food.Consumption.Value / hh$Rate
hh$Crops <- hh$Crops /hh$Rate
hh$AgIncome <- hh$AgIncome / hh$Rate
hh$income_crops <- hh$income_crops / hh$Rate
hh$Livestock <- hh$Livestock / hh$Rate
hh$income_lvstk <- hh$income_lvstk / hh$Rate
hh$income_own <- hh$income_own / hh$Rate
hh$Permanent.Crops <- hh$Permanent.Crops /hh$Rate
hh$Livestock.Byproducts <- hh$Livestock.Byproducts / hh$Rate
hh$hh_annual_nonfuel_nr_value <- hh$hh_annual_nonfuel_nr_value / hh$Rate
hh$income_perm_crop <- hh$income_perm_crop / hh$Rate
hh$AgInvestments <- hh$AgInvestments / hh$Rate
hh$income_lvstk_byprod <- hh$income_lvstk_byprod / hh$Rate
hh$cost_seeds <- hh$cost_seeds / hh$Rate
hh$cost_pesticide <- hh$cost_pesticide / hh$Rate
hh$Crop.Byproducts <- hh$Crop.Byproducts / hh$Rate
hh$cost <- hh$cost / hh$Rate
hh$cost_syn_fert <- hh$cost_syn_fert / hh$Rate
hh$cost_org_fert <- hh$cost_org_fert / hh$Rate
hh$income_byprod <- hh$income_byprod / hh$Rate

hh$CropCommercializationIndex <- hh$AgIncome / ls$TotalAgriculturalProduction

hh <- hh %>% filter(Country != 'GHA' & Round == 1)


write.csv(hh, 'hh_level.csv', row.names=F)
