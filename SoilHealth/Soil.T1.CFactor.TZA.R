########################################
#Tier 1 - C Factor - Cover factor for soil erosion modeling- TZA
########################################
#Need to load the ph layer?
#load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)
library(utils)
library(VitalSignsUtilities)

arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#load raster data
#fapar_2002_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2002.tif")
fapar_2003_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2003.tif"]]$getData())
fapar_2004_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2004.tif"]]$getData())
fapar_2005_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2005.tif"]]$getData())
fapar_2006_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2006.tif"]]$getData())
fapar_2007_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2007.tif"]]$getData())
fapar_2008_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2008.tif"]]$getData())
fapar_2009_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2009.tif"]]$getData())
fapar_2010_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2010.tif"]]$getData())
fapar_2011_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2011.tif"]]$getData())
fapar_2012_tza <- raster(vstables$tables[["fAPAR/TZA/cumulative_annual_fpar_1km_2012.tif"]]$getData())

#load boundary data
unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")


aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
sagcot_aea <- spTransform(sagcot, crs(aea))
e <-extent(sagcot_aea)

fapar_2003_sagcot <- crop(fapar_2003_tza, e, snap ="out")
fapar_2004_sagcot <- crop(fapar_2004_tza, e, snap ="out")
fapar_2005_sagcot <- crop(fapar_2005_tza, e, snap ="out")
fapar_2006_sagcot <- crop(fapar_2006_tza, e, snap ="out")
fapar_2007_sagcot <- crop(fapar_2007_tza, e, snap ="out")
fapar_2008_sagcot <- crop(fapar_2008_tza, e, snap ="out")
fapar_2009_sagcot <- crop(fapar_2009_tza, e, snap ="out")
fapar_2010_sagcot <- crop(fapar_2010_tza, e, snap ="out")
fapar_2011_sagcot <- crop(fapar_2011_tza, e, snap ="out")
fapar_2012_sagcot <- crop(fapar_2012_tza, e, snap ="out")

r_sagcot <- rasterize(sagcot_aea, fapar_2012_sagcot)
fapar2003sagcot <- mask(x = fapar_2003_sagcot, mask = r_sagcot)
fapar2004sagcot <- mask(x = fapar_2004_sagcot, mask = r_sagcot)
fapar2005sagcot <- mask(x = fapar_2005_sagcot, mask = r_sagcot)
fapar2006sagcot <- mask(x = fapar_2006_sagcot, mask = r_sagcot)
fapar2007sagcot <- mask(x = fapar_2007_sagcot, mask = r_sagcot)
fapar2008sagcot <- mask(x = fapar_2008_sagcot, mask = r_sagcot)
fapar2009sagcot <- mask(x = fapar_2009_sagcot, mask = r_sagcot)
fapar2010sagcot <- mask(x = fapar_2010_sagcot, mask = r_sagcot)
fapar2011sagcot <- mask(x = fapar_2011_sagcot, mask = r_sagcot)
fapar2012sagcot <- mask(x = fapar_2012_sagcot, mask = r_sagcot)

rm(fapar_2003_tza, fapar_2004_tza, fapar_2005_tza, fapar_2006_tza, fapar_2007_tza, fapar_2008_tza, fapar_2009_tza, fapar_2010_tza, fapar_2011_tza, fapar_2012_tza)
rm(fapar_2003_sagcot, fapar_2004_sagcot, fapar_2005_sagcot, fapar_2006_sagcot, fapar_2007_sagcot, fapar_2008_sagcot, fapar_2009_sagcot, fapar_2010_sagcot, fapar_2011_sagcot, fapar_2012_sagcot)


#build stack of normalized cumulative fapar
cumfaparstack <- stack(fapar2003sagcot)
cumfaparstack <- addLayer(cumfaparstack, c(fapar2004sagcot, fapar2005sagcot, fapar2006sagcot, fapar2007sagcot, fapar2008sagcot, fapar2009sagcot, fapar2010sagcot, fapar2011sagcot, fapar2012sagcot))

#normalize function
normalize <- function(x) {(x - min(x, na.rm=TRUE)*1)/(max(x,na.rm=TRUE) -
                                                                min(x, na.rm=TRUE)-0)}

norm_cum_fapar <- normalize(cumfaparstack)

#take average value for the time series
mean_norm_cum_fapar <- mean(norm_cum_fapar)
#convert to C factor
c_factor <- 1 - mean_norm_cum_fapar

#resample
# use the pH layer to create a reference for resampling:
#dimensions  : 2177, 4083, 8888691  (nrow, ncol, ncell)
#resolution  : 235, 266  (x, y)
#extent      : 554167.7, 1513673, -1254644, -675562  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
sagcot_ref <- raster(nrow=2177, ncol=4083, crs=aea, res=c(235, 266), extent(c(554167.7, 1513673, -1254644, -675562)))
sagcot_ref <- setValues(sagcot_ref, NA)

# Set up higher resolution for erosivity (apply a grid with MORE pixels within the same extent) and resample:
#ero2 <- raster(nrow=2177, ncol=4083)
c_factor2 <- resample(c_factor, sagcot_ref, method='bilinear')

#write ouput
writeRaster(mean_norm_cum_fapar, filename="cumfAPAR_norm_sagcot.tif", format='GTiff', overwrite=TRUE)
writeRaster(c_factor2, filename="cfactor_sagcot.tif", format='GTiff', overwrite=TRUE)
plot(mean_norm_cum_fapar, main="fapar")

s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "cumfAPAR_norm_sagcot.tif", target_path = "Soil_Health/cumfAPAR_norm_sagcot.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "cfactor_sagcot.tif", target_path = "Soil_Health/cfactor_sagcot.tif")

