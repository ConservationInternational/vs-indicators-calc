library(ggplot2)
library(dplyr)

setwd('../Combine')

nr <- read.csv('../NaturalResources/NaturalResources.HH.csv')
nut <- read.csv('../Nutrition/Nutrition.Individual.csv')
ag <- read.csv('../Ag/AgValue.HH.csv')

df <- Reduce(merge, list(nr, nut, ag))

df <- df %>% filter(Country != 'GHA')

df$NR_Value <- df$total_fuelwood_value + df$hh_annual_nonfuel_nr_value

df$Ag_Value <- rowSums(df[ , c("Crops", "Permanent.Crops", 
                              "Livestock.Byproducts", "Livestock", "Crop.Byproducts")])

df <- merge(df, data.frame(Country=c("RWA", "UGA", "TZA"), USD=c(824.3686, 3594.0071, 2232.2166)))

df$NR_Value <- df$NR_Value/df$USD
df$Ag_Value <- df$Ag_Value/df$USD

ggplot(df) + geom_point(aes(x=log(Ag_Value), y=log(NR_Value), color=zlen))
