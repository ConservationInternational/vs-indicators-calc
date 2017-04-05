setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Combine/')

library(aws.s3)
aws.signature::use_credentials()

read <- function(x){
  read.csv(text=rawToChar(get_object(x, bucket='vs-cdb-indicators')))
}

###############################
#Landscape Level
###############################
fs <- read('FoodSecurity_Landscape.csv')
ag_intens <- read('AgIntensification_Landscape.csv')
ag_value <- read('AgValue_Landscape.csv')
crop <- read('Crops_Landscape.csv')
degr_eros <- read('ErosionControl_Landscape.csv')
degr <- read.csv('../LandCover/VS_landscapes_degradation_PRELIMINARY.CSV') %>%
  filter(time_period == '2003-2015' & ISO != 'KEN') %>%
  select(Country = ISO, Landscape..=Landscape, perc_degraded=degraded_area_km, perc_improved=improved_area_km)
gend <- read('Gender_Landscape.csv')
hh_econ <- read('Income_Landscape.csv')
h_cap <- read('Capital_Landscape.csv')
nr <- read('NaturalResources_Landscape.csv')
nut <- read('Nutrition_Landscape.csv') %>%
  select(-mean_zwei, -mean_zlen, -mean_zwfl)
ws <- read('WatSan_Landscape.csv')
lc <- read('LandCover_Landscape.csv') %>%
  select(Landscape.., Country, NR_Total, AG_Total)

ls <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros, degr, 
                         gend, hh_econ, h_cap, nr, nut, ws, lc))

ls$CropCommercializationIndex <- ls$AgIncome / ls$TotalAgriculturalProduction

write.csv(ls, 'landscape_level.csv', row.names=F)

###############################
#Eplot Level
###############################
bio <- read('../Biophysical/Biophysical.csv')
write.csv(bio, 'eplot_level.csv', row.names=F)

###############################
#Household Level
###############################
fs <- read('FoodSecurity_HH.csv')
ag_intens <- read('AgIntensification_HH.csv')
ag_value <- read('AgValue_HH.csv')
crop <- read('Crops_HH.csv')
degr_eros <- read('ErosionControl_HH.csv')
gend <- read('Gender_HH.csv')
hh_econ <- read('Income_HH.csv')
h_cap <- read('Capital_HH.csv')
nr <- read('NaturalResources_HH.csv')
nut <- read('Nutrition_HH.csv')
ws <- read('WatSan_HH.csv')

hh <- Reduce(function(x, y){merge(x, y, all=T)}, list(fs, ag_intens, ag_value, crop, degr_eros,
                                                      gend, hh_econ, h_cap, nr, nut, ws))

hh$CropCommercializationIndex <- hh$AgIncome / hh$total
hh$CropCommercializationIndex[hh$CropCommercializationIndex < 0] <- 0

write.csv(hh, 'hh_level.csv', row.names=F)
