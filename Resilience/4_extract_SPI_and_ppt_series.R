###############################################################################
# Extracts precipitation timeseries for LSMS HH locations
###############################################################################

source('0_settings.R')

library(raster)
library(stringr)
library(reshape2)
library(lubridate)
library(rgdal)
library(dplyr)

library(doParallel)
library(foreach)

# Number of CPUs to use for extracting dataset for LSMS point locations
n_cpus <- 4

dates <- seq(chirps_start_date, chirps_end_date, by='months')
dates <- dates[dates >= chirps_start_date & dates <= chirps_end_date]

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

spi_file <- file.path(chirps_proc_folder,
    paste0(ISO3, '_CHIRPS_monthly_', date_limits_string, '_SPI_12mth.grd'))
spi <- brick(spi_file)

###############################################################################
# First extract timeseries for LSMS households
lsms_hhs <- readOGR(lsms_pt_folder, paste0(ISO3, "_HH_pt_locations"))
lsms_hhs <- spTransform(lsms_hhs, CRS(s_srs))

beginCluster(n_cpus)
hh_spi <- extract(spi, lsms_hhs, df=TRUE)
endCluster()
hh_spi <- hh_spi[names(hh_spi) != 'ID']
hh_spi <- cbind(DISTRICT=lsms_hhs$DISTRICT,
                WARD=lsms_hhs$WARD, 
                EA=lsms_hhs$EA, 
                HHNO=lsms_hhs$HHNO, 
                CYCLE=lsms_hhs$CYCLE, 
                HHID=lsms_hhs$HHID, 
                hh_spi)
hh_spi <- melt(hh_spi, id.vars=c("DISTRICT", "WARD", "EA", "HHNO", "CYCLE", "HHID"),
               variable.name='date', value.name='SPI')
hh_spi$date <- as.numeric(str_extract(hh_spi$date, '[0-9]*$'))
hh_spi$date <- dates[hh_spi$date]
hh_spi$year <- year(hh_spi$date)
hh_spi$month <- month(hh_spi$date)

# Save this file locally as it cannot be transferred to the server
save(hh_spi, file=paste0(ISO3, '_hh_spi12.RData'))
