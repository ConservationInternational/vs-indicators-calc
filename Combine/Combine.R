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

ls$Nonfood.Spending <- ls$Nonfood.Spending / ls$Rate
ls$Food.Consumption.Value <- ls$Food.Consumption.Value / ls$Rate
ls$Food.Spending <- ls$Food.Spending / ls$Rate
ls$TotalAgriculturalProduction <- ls$TotalAgriculturalProduction / ls$Rate
ls$TotalIncome <- ls$TotalIncome / ls$Rate
ls$AgIncome <- ls$AgIncome / ls$Rate
ls$NonAgIncome <- ls$NonAgIncome / ls$Rate
ls$Mean_Annual_Fuelwood_Value <- ls$Mean_Annual_Fuelwood_Value / ls$Rate
ls$Nonfuel_NR_annual_value <- ls$Nonfuel_NR_annual_value / ls$Rate

write.csv(ls, 'landscape_level.csv', row.names=F)

bio <- read.csv('../Biophysical/Biophysical.csv')
write.csv(bio, 'eplot_level.csv')

fs <- read.csv('../FoodSecurity/FoodSecurity.HH.csv')
ag_intens <- read.csv('../Ag/AgIntensification.HH.csv')
ag_value <- read.csv('../Ag/AgValue.HH.csv')
crop <- read.csv('../Crops/crops.HH.csv')
degr_eros <- read.csv('../Degradation/ErosionControl.HH.csv')
gend <- read.csv('../Gender/Gender.HH.csv')
hh_econ <- read.csv('../HouseholdEconomy/Income.HH.csv')
h_cap <- read.csv('../HumanCapital/Capital.HH.csv')
nr <- read.csv('../NaturalResources/NaturalResources.HH.csv')
nut <- read.csv('../Nutrition/Nutrition.HH.csv') %>%
  select(-mean_zwei, -mean_zlen, -mean_zwfl)
ws <- read.csv('../WaterSanitation/WatSan.HH.csv')

hh <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros,
                                                      gend, hh_econ, h_cap, nr, nut, ws))

hh <- hh %>% filter(Country != 'GHA' & Round == 1)

write.csv(hh, 'household_level.csv', row.names=F)
