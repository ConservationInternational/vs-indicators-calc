####################################################
# SH46 - Soil depth indicator (=Soil depth indicator / depth restriction)
# Date - August 12, 2015
####################################################

library(sp)
library(raster)
library(GISTools)
library(rgdal)
library(utils)
library(VitalSignsUtilities)

#set directory
vs_tempdir <- tempdir()
setwd(vs_tempdir)

#arguments <- commandArgs(trailingOnly = FALSE)
#service_credentials <- gsub("[^=]+={1}", "",
#                            arguments[grep("credentials", arguments)])
credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#load raster data (dr=depth restriction)
dr <- raster(vstables$tables[["AfSIS/af_BDRICM_T__M_250m/af_BDRICM_T__M_250m.tif"]]$getData()) #dr = depth restriction
unzip(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()))
gha <- readOGR(dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()), "GHA_adm1")

# project the GHANA polygon layer into LAEA:
#reproject GHANA polygon boundary shapefile to Lambert equal area:
laea <- '+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
#gha_aeac <- spTransform(gha, crs(aeac))
gha <- spTransform(gha, crs(laea))

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(gha)
dr.gha <- crop(dr, e, snap ="out")
rgha <- rasterize(gha, dr.gha) # 9 min
dr <- mask(x = dr.gha, mask = rgha)

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject SAGCOT boundary shapefile )
gha <- spTransform(gha, crs(aea))
# reproject depth restriction: 
SH32 <- projectRaster(dr, crs = aea) 

# Check extent (vs. gha_ph layer): 
SH32
#dimensions  : 2966, 2102, 6234532  (nrow, ncol, ncell)
#resolution  : 240, 260  (x, y)
#extent      : -2972559, -2468079, 538097.3, 1309257  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 

# Calculate SH46 - Soil depth indicator
SH46 <- .004*SH32 - .1
SH46 [SH32 < 25] <- 0
SH46 [SH32 > 50] <- .1

# Write output
writeRaster(SH32, filename="soil_depth_gha.tif", format='GTiff', overwrite=TRUE)
writeRaster(SH46, filename="soil_depth_indicator_gha.tif", format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_depth_gha.tif", target_path = "Soil_Health/soil_depth_gha.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_depth_indicator_gha.tif", target_path = "Soil_Health/soil_depth_indicator_gha.tif")

###################################################################################

#y2=0 {x2<25}; 
#y2= -.004x2 + .2 {25<x2<50}; 
#y2=.1 { x2>50}

#display 
setwd(dir_out1) 
pdf("SH32_SH46_gha.pdf")
plot(SH32, main="GHA - Soil Depth (cm)")
hist(SH32, main="GHA - Soil Depth (cm)", col="blue")
plot(SH46, main="GHA - Soil Depth Indicator")
hist(SH46, main="GHA - Soil Depth Indicator", col="blue")
dev.off()