###############################################################################
# Calculates Standardized Precipitation Index (SPI). SPI is a drought 
# indicator.
###############################################################################

source('0_settings.R')

library(raster)
library(SPEI)
library(foreach)
library(doParallel)
library(tools)
library(spatial.tools)

spi_periods <- c(12, 24)

cl <- makeCluster(20)
registerDoParallel(cl)

chirps_file <- file.path(chirps_proc_folder,
    paste0(ISO3, '_CHIRPS_monthly_', date_limits_string, '.tif'))
chirps <- brick(chirps_file)

# Define function to calculate SPI
calc_spi <- function(p, spi_period, ...) {
    browser()
    p[p == -9999] <- NA
    dim_p <- dim(p)
    # SPI expects each timeseries in its own column. So first store the pixels 
    # in rows:
    p <- t(array(p, dim=c(dim_p[1] * dim_p[2], dim_p[3])))
    # Multiply by 1000 and round so results can be stored as INT2S
    out <- round(spi(p, spi_period, na.rm=TRUE)$fitted * 1000)
    # Convert back to having timeseries in the z-direction
    out <- array(t(out), dim=dim_p)
    # rasterEngine does not properly handle NAs, so recode these to -32767
    out[is.na(out)] <- -32767
    out
}

out_basename <- file_path_sans_ext(chirps_file)

for (spi_period in spi_periods) {
    timestamp()
    print(paste("Calculating", spi_period, "month SPI..."))
    spi_out <- rasterEngine(p=chirps, args=list(spi_period=spi_period),
        fun=calc_spi, outbands=nlayers(chirps), datatype='INT2S', 
        processing_unit="chunk", outfiles=1, .packages=c('SPEI'),
        filename=paste0(out_basename, '_SPI_', spi_period, 'mth'))
}
timestamp()

stopCluster(cl)
