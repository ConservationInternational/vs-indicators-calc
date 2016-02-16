library(raster)
library(gdalUtils)
library(stringr)
library(tidyr)
library(dplyr)
library(foreach)

in_folder <- 'O:/Data/Vital_Signs/S3/ci-vsindicators'

match_col_order <- function(a, b) {
    stopifnot(all(names(b) %in% names(a)))
    stopifnot(all(names(a) %in% names(b)))
    return(a[match(names(b), names(a))])
}

###############################################################################
# Resilience

# in_files <- list.files(file.path(in_folder, 'Resilience'), pattern='.csv', 
#                        full.names=TRUE)
#
# d <- foreach(in_file=in_files) %do% {
#     read.csv(in_files
# }

################################################################################
# Food Security

# Read in and combine the landscape-level food security data
f <- file.path(in_folder, 'Food_Security', 'Foodsec_VS_Landscape.csv')
d <- read.csv(f)
d <- rename(d, ISO3=country)
d$landscape <- paste0(d$ISO3, sprintf('%02i', d$landscape))
d$month <- NA
d$day <- NA
d$scale <- 'landscape'
d$thread <- 'Food security'
d <- select(d, -Center_X, -Center_Y, -wkt_geom)
d <- gather(d, variable, value, foodsec:utilization)
landscape <- d

# Read in and combine the national-level food security data
fs_natl <- foreach(f=list.files(file.path(in_folder, 'Food_Security'), 
                     pattern='National.csv', full.names=TRUE),
        .combine=rbind) %do% {
    d <- read.csv(f)
    d <- rename(d, ISO3=country)
    d <- select(d, -landscape)
    d$month <- NA
    d$day <- NA
    d$thread <- 'Food security'
    gather(d, variable, value, foodsec:utilization)
}
national <- fs_natl

################################################################################
# Soil Health

# Below code is VERY kludgy. The soils data needs a lot of cleaning:
#   - column names vary between files
#   - formattting isn't consistent - some of the files have extra left-hand 
#   columns with no header because row.names=FALSE wasn't passed when they were 
#   output from R
#   - variable names are meaningless except to those familiar with the 
#   workflow, we need to use meaningful names
#   - data for the same scale is split across multiple files

## Below doesn't have any indicators - just SH27 and SH45
# d <- read.csv(file.path(in_folder, 'Soil_Health', 'SH_T2_T4_SH27_SH45_Eplot.csv'))
# d <- rename(d, ISO3=Country, eplot=Eplot.Code, lat=GPS.SE.latitude, 
#             long=GPS.SE.longitude, landscape=Landscape)
# d <- select(d, -Eplot, -GPS.SE.accuracy)
# d <- gather(d, variable, value, SH45:SH27)
# print(group_by(d, ISO3, variable) %>% summarise(n()))

## Below just has SH46, so skip - SH46 isn't an indicator
# d <- read.csv(file.path(in_folder, 'Soil_Health', 'SH_T2_T4_SH46_Eplot.csv'))
# d <- rename(d, ISO3=Country, eplot=Eplot.Code, lat=GPS.SE.latitude, 
#             long=GPS.SE.longitude, landscape=Landscape)
# d <- select(d, -Eplot, -GPS.SE.accuracy)
# d <- gather(d, variable, value, SH46)
# print(group_by(d, ISO3, variable) %>% summarise(n()))

d <- read.csv(file.path(in_folder, 'Soil_Health', 'SH_T2_T4_SH61_Eplot.csv'))
d <- select(d, -eplot, -gpsseaccuracy)
d <- rename(d, ISO3=country, eplot=eplotcode, lat=gpsselatitude, 
            long=gpsselongitude)
d <- gather(d, variable, value, sh27:sh61)
d <- filter(d, variable== 'sh61')
d$variable <- 'Soil physical indicator'
#print(group_by(d, ISO3, variable) %>% summarise(n()))
d$scale <- 'eplot'
d$thread <- 'Soil health'
eplots <- d

d <- read.csv(file.path(in_folder, 'Soil_Health', 'SH_Tier2_4_SH12_SH59_SH60_Eplot.csv'))
d <- rename(d, ISO3=Country, eplot=Eplot.Code, lat=GPS.SE.latitude, 
            long=GPS.SE.longitude, landscape=Landscape)
d <- select(d, -Eplot, -GPS.SE.accuracy, -sample_depth_bottom, -sample_depth_top, -X)
d <- gather(d, variable, value, SH12:SH60)
d <- filter(d, variable == 'SH60')
d$variable <- 'Soil carbon deficit indicator'
d$scale <- 'eplot'
d$thread <- 'Soil health'
#print(group_by(d, ISO3, variable) %>% summarise(n()))

eplots <- rbind(eplots, match_col_order(d, eplots))

# Save eplot coordinates to CSV
eplot_coords <- group_by(eplots, eplot) %>%
    summarise(ISO3=ISO3[1], y=lat[1], x=long[1])
write.csv(eplot_coords, file='coordinates_eplots_wgs84.csv', row.names=FALSE)

# Make sure there aren't different coordinates for the same plot:
coord_check <- group_by(eplots, eplot) %>%
    summarise(n_lat=length(unique(lat)), n_long=length(unique(long)))
stopifnot(all(coord_check$n_lat == 1))
stopifnot(all(coord_check$n_long == 1))

###
# Landscape scale (farm field scale = FFS)
d <- read.csv(file.path(in_folder, 'Soil_Health', 'VS_PartialNutrient.csv'))
d <- rename(d, ISO3=Country, landscape=Landscape)
d <- gather(d, variable, value, SH58_Mean:SH58_stdev)
d$variable <- gsub('SH58_', 'Partial nutrient budget indicator ', d$variable)
d$variable <- gsub('Mean', 'mean', d$variable)
d$year <- NA
d$month <- NA
d$day <- NA
d$scale <- 'landscape'
d$thread <- 'Soil health'

landscape <- rbind(landscape, match_col_order(d, landscape))

################################################################################
# Output data

###
# Data


###
# Spatial referencing

# eplots
write.csv(eplots, file='vs-indicators-eplot.csv', row.names=FALSE)

# landscapes
write.csv(landscape, 'vs-indicators-landscape.csv', row.names=FALSE)

# national level
write.csv(national, 'vs-indicators-national.csv', row.names=FALSE)
