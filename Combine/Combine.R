setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Combine/')

fs <- read.csv('../FoodSecurity/fs_outputs.csv')
ag_intens <- read.csv('../Ag/AgIntensification.csv')
ag_value <- read.csv('../Ag/AgValue.Landscape.csv')
crop <- read.csv('../Crops/crops.csv')
degr_eros <- read.csv('../Degradation/ErosionControl.csv')
degr <- read.csv('../Degradation/VS_landscapes_degradation_PRELIMINARY.CSV') %>%
  filter(time_period == '2003-2015' & ISO != 'KEN') %>%
  select(Country = ISO, Landscape..=Landscape, perc_degraded=degraded_area_km, perc_improved=improved_area_km)
gend <- read.csv('../Gender/Gender.csv') %>%
  select(-Male.Gender, -Female.Gender)
hh_econ <- read.csv('../HouseholdEconomy/income.csv')
h_cap <- read.csv('../HumanCapital/hh_captital.csv')
nr <- read.csv('../NaturalResources/NaturalResources.Landscape.csv')
nut <- read.csv('../Nutrition/Nutrition.Landscape.csv') %>%
  select(-mean_zwei, -mean_zlen, -mean_zwfl)
ws <- read.csv('../WaterSanitation/WatSan.csv')

hh <- Reduce(mergea <- function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros, degr, 
                         gend, hh_econ, h_cap, nr, nut, ws))

hh <- merge(hh, data.frame(Country = 'GHA', 'UGA', 'RWA', 'TZA',
                           Rate = 4.5085, 3584, 816.9, 2226))

hh$Nonfood.Spending <- hh$Nonfood.Spending * hh$Rate
hh$Food.Consumption.Value <- hh$Food.Consumption.Value * hh$Rate
hh$Food.Spending <- hh$Food.Spending * hh$Rate
hh$TotalAgriculturalProduction <- hh$TotalAgriculturalProduction * hh$Rate
hh$TotalIncome <- hh$TotalIncome * hh$Rate
hh$AgIncome <- hh$AgIncome * hh$Rate
hh$NonAgIncome <- hh$NonAgIncome * hh$Rate
hh$Mean_Annual_Fuelwood_Value <- hh$Mean_Annual_Fuelwood_Value * hh$Rate
hh$Nonfuel_NR_annual_value <- hh$Nonfuel_NR_annual_value * hh$Rate

write.csv(hh, 'hh_level.csv', row.names=F)

bio <- read.csv('../Biophysical/Biophysical.csv')
write.csv(bio, 'eplot_level.csv')
