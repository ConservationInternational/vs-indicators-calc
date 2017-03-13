####################################################
# Formatting Rainfall Erosivity for use as RUSLE R Factor
# Date - August 12, 2015
####################################################
# resamples from 25 miles resolution to 250 m resolution
library(sp)
library(raster)
library(GISTools)
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

# load data
# erosivity dataset is from Vrieling 2014
ero <- raster(vstables$tables[["ErosivityAfrica/R_EI30rescaled_TMPA1998-2012_Africa_geotiff.tif"]]$getData())

unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")

sagcot_ph <- raster("sagcot_ph.tif")

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject SAGCOT boundary shapefile )
sagcot <- spTransform(sagcot, crs(aea))
# reproject erosivity: 
ero <- projectRaster(ero, crs = aea)
# reproject reference raster pH raster file for resampling
sagcot_aea <- projectRaster(sagcot_ph, crs = aea)

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(sagcot)
ero.sagcot <- crop(ero, e, snap ="out")
rsagcot <- rasterize(sagcot, ero.sagcot)
ero <- mask(x = ero.sagcot, mask = rsagcot)

# Set NA to "-999" values
ero[-999] <- NA

# use the pH layer to create a reference for resampling:
sagcot_aea
#dimensions  : 2177, 4083, 8888691  (nrow, ncol, ncell)
#resolution  : 235, 266  (x, y)
#extent      : 554167.7, 1513673, -1254644, -675562  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
sagcot_ref <- raster(nrow=2177, ncol=4083, crs=aea, res=c(235, 266), extent(c(554167.7, 1513673, -1254644, -675562)))
sagcot_ref <- setValues(sagcot_ref, NA)

# Note: same extent, same projection, but different resolution
ero 
sagcot_aea

# Set up higher resolution for erosivity (apply a grid with MORE pixels within the same extent) and resample:
#ero2 <- raster(nrow=2177, ncol=4083)
ero2 <- resample(ero, sagcot_ref, method='bilinear')
ero2[ero2<0] <- NA

#write outputs
writeRaster(ero2, filename="erosivity_sagcot.tif", format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "erosivity_sagcot.tif", target_path = "Soil_Health/erosivity_sagcot.tif")

####################################################################################

