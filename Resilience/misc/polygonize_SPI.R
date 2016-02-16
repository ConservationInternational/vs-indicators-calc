library(tools)
library(raster)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
library(dplyr)

in_folder <- 'O:/Data/Vital_Signs/CHIRPS-2.0'
out_folder <- 'O:/Data/Vital_Signs/CHIRPS-2.0'

precip_file <- file.path(in_folder, 'Tanzania_CHIRPS_monthly_19850101-20141201_trend_decadal_10-11-12.gri')

smooth_cell <- 7
threshold <- -1

smooth_func <- function(x) {
    round(sum(x, na.rm=TRUE))
}

file_basename <- file.path(out_folder, 
                           paste0('Tanzania_CHIRPS_decadal_trend_thresholded'))

# Note that band 2 is p value, band 1 is trend
precip <- raster(precip_file, bands=c(1))
# Save a geotiff for Stamen
writeRaster(precip, paste0(file_path_sans_ext(precip_file), '_geotiff.tif'), 
            overwrite=TRUE)


# Divide by three so that it is a 30 year trend equal to the numerator
threshold_val <- -15/3

# Setup a thresholding function
precip_recode <- calc(precip, function(x) {
    ret <- rep(0, length(x))
    ret[!is.na(x) & (x < threshold_val)] <- 1
    ret[is.na(x)] <- NA
    ret
})

# Smooth input
weight_mat <- matrix(1/(smooth_cell^2), nrow=smooth_cell, ncol=smooth_cell)
precip_smooth <- focal(precip_recode, weight_mat, smooth_func)

precip_smooth[precip_smooth == 0] <- NA
precip_polys <- rasterToPolygons(precip_smooth)
precip_polys_dissolve <- unionSpatialPolygons(precip_polys, precip_polys$layer)

# Multiple threshold_val by 3 so it is a 30 year trend
key <- data.frame(code=c(1),
                  change=c(threshold_val*3))
row.names(key) <- key$code
# Eliminate classes not on the map
key <- key[which(row.names(key) %in% freq(precip_smooth)), ]

precip_polys_dissolve <- SpatialPolygonsDataFrame(precip_polys_dissolve, key)
writeOGR(precip_polys_dissolve, in_folder,
         paste0(basename(file_basename), '_polygons'), 
         driver="ESRI Shapefile", overwrite=TRUE)
