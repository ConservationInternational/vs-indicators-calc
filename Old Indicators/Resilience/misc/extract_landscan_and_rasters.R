library(raster)
library(rgdal)
library(rgeos)

in_folder <- 'O:/Data/Vital_Signs/CHIRPS_Data'

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is EXCLUSIVE of the end date
chirps_end_date <- as.Date('2014/5/1')
date_limits_string <- '198101-201404'
dates <- seq(chirps_start_date, chirps_end_date, by='months')


spi <- raster(file.path(in_folder, paste0('monthly_VitalSigns_SPI_12.envi')),
              band=which(dates == as.Date('2009/10/1')))
writeRaster(spi, 'October_2009_SPI.tif')

# Extract landscan:
landscan <- raster('O:/Data/Landscan/2011/RasterGISbinary/lspop2011.flt')
tza <- gConvexHull(readOGR("O:/Data/Global/GADM", "TZA_adm0"))
tza <- gBuffer(tza, width=.5)
tza <- spTransform(tza, CRS=proj4string(landscan))

landscan <- crop(landscan, tza)
writeRaster(landscan, 'Landscan_2011_population.tif')
