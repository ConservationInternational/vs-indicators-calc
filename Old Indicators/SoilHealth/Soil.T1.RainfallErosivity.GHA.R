####################################################
# Formatting Rainfall Erosivity for use as RUSLE R Factor
# Date - August 12, 2015
####################################################
# 25 miles resolution to 250 m resolution
library(sp)
library(raster)
library(GISTools)
library(rgdal)
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

unzip(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()))
gha <- readOGR(dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()), "GHA_adm1")

gha_ph <- raster("gha_ph.tif")

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject GHANA boundary shapefile )
gha <- spTransform(gha, crs(aea))
# reproject erosivity: 
ero <- projectRaster(ero, crs = aea)
# reproject reference raster pH raster file for resampling
gha_aea <- projectRaster(gha_ph, crs = aea)

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(gha)
ero.gha <- crop(ero, e, snap ="out")
rgha <- rasterize(gha, ero.gha)
ero <- mask(x = ero.gha, mask = rgha)

# Set NA to "-999" values
ero[-999] <- NA

# use the pH layer to create a reference for resampling:
gha_aea
#dimensions  : 2966, 2102, 6234532  (nrow, ncol, ncell)
#resolution  : 240, 260  (x, y)
#extent      : -2972559, -2468079, 538097.3, 1309257  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
gha_ref <- raster(nrow=2966, ncol=2102, crs=aea, res=c(240, 260), extent(c(-2972559, -2468079, 538097.3, 1309257)))
gha_ref <- setValues(gha_ref, NA)

# Note: same extent, same projection, but different resolution
ero 
gha_aea

# Set up higher resolution for erosivity (apply a grid with MORE pixels within the same extent) and resample:
#ero2 <- raster(nrow=2966, ncol=2102)
ero2 <- resample(ero, gha_ref, method='bilinear')
ero2[ero2<0] <- NA

#write outputs
writeRaster(ero, filename="erosivity_gha_25miles.tif", format='GTiff', overwrite=TRUE)
writeRaster(ero2, filename="erosivity_gha.tif", format='GTiff', overwrite=TRUE)

s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "erosivity_gha_25miles.tif", target_path = "Soil_Health/erosivity_gha_25miles.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "erosivity_gha.tif", target_path = "Soil_Health/erosivity_gha.tif")

####################################################################################

