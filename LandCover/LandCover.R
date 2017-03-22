library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/LandCover/')

LandCover <- read.csv('LandCover_2010.csv') %>%
  select(Country, Landscape=Landscape.., NR_Total, AG_Total)

Deg <- read.csv('VS_landscapes_degradation_PRELIMINARY.CSV') %>%
  filter(time_period=='2003-2015' & ISO != 'KEN') %>%
  select(Country=ISO, Landscape, percent_degraded=degraded_area_km, percent_improved=improved_area_km)

final <- merge(Deg, LandCover)

#########################################
#Write
#################################

library(aws.s3)
aws.signature::use_credentials()

writeS3 <- function(df, name){
  names(df) <- gsub('.', '_', names(df), fixed=T)
  names(df)[names(df)=='Landscape__'] <- 'Landscape'
  
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}


writeS3(final, 'LandCover_Landscape.csv')