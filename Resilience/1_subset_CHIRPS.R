###############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of the LSMS survey data
###############################################################################

source('0_settings.R')

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(teamlucc)

tifs <- dir(chirps_raw_folder, pattern='.tif$')

datestrings <- gsub('.tif', '', (str_extract(tifs, '[0-9]{4}\\.[0-9]{2}.tif$')))
datestrings <- gsub('[.]', '/', datestrings)
dates <- as.Date(paste0(datestrings, '/01'))

# Filter to only include files within the chosen date limits
tifs <- tifs[dates >= chirps_start_date & dates <= chirps_end_date]

# Build a VRT with all dates in a single layer stacked VRT file (this stacks 
# the tifs, but with delayed computation - the actual cropping and stacking 
# computations won't take place until the gdalwarp line below that is run for 
# each aoi)
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(chirps_raw_folder, tifs), vrt_file, separate=TRUE, 
             overwrite=TRUE)

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

# Read in the bounding box from a country polygon. Buffer around the polygon so 
# that mapping is easier (so that areas aren't missing on polygon edges due to 
# inaccurate digitization, etc.)
aoi <- readOGR(file.path(external_datasets_folder, 'Boundaries'), paste0(ISO3, '_adm0'))
aoi <- gConvexHull(aoi)
aoi_utm <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
aoi_utm <- gBuffer(aoi_utm, width=100000)
aoi <- spTransform(aoi_utm, CRS(s_srs))

te <- as.numeric(bbox(aoi))

dstfile <- file.path(chirps_proc_folder,
    paste0(ISO3, '_CHIRPS_monthly_', date_limits_string, '.tif'))

# Crop tifs for this site
timestamp()
print('Cropping...')
gdalwarp(vrt_file, dstfile, s_srs=s_srs, t_srs=s_srs, te=te, 
         overwrite=TRUE, verbose=TRUE)
timestamp()
