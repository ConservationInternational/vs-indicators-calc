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
unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")

# project the SAGCOT polygon layer into LAEA:
#reproject SAGCOT polygon boundary shapefile to Lambert equal area:
laea <- '+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
#sagcot_aeac <- spTransform(sagcot, crs(aeac))
sagcot <- spTransform(sagcot, crs(laea))

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(sagcot)
dr.sagcot <- crop(dr, e, snap ="out")
rsagcot <- rasterize(sagcot, dr.sagcot) # 9 min
dr <- mask(x = dr.sagcot, mask = rsagcot)

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject SAGCOT boundary shapefile )
sagcot <- spTransform(sagcot, crs(aea))
# reproject depth restriction: 
SH32 <- projectRaster(dr, crs = aea) 

# Check extent (vs. sagcot_ph layer): 
SH32
#dimensions  : 2177, 4083, 8888691  (nrow, ncol, ncell)
#resolution  : 235, 266  (x, y)
#extent      : 554167.7, 1513673, -1254644, -675562  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 

# Calculate SH46 - Soil depth indicator
SH46 <- .004*SH32 - .1
SH46 [SH32 < 25] <- 0
SH46 [SH32 > 50] <- .1

# Write output
writeRaster(SH32, filename="soil_depth_sagcot.tif", format='GTiff', overwrite=TRUE)
writeRaster(SH46, filename="soil_depth_indicator_sagcot.tif", format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_depth_sagcot.tif", target_path = "Soil_Health/soil_depth_sagcot.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_depth_indicator_sagcot.tif", target_path = "Soil_Health/soil_depth_indicator_sagcot.tif")

###################################################################################

#y2=0 {x2<25}; 
#y2= -.004x2 + .2 {25<x2<50}; 
#y2=.1 { x2>50}

#display 
pdf("SH32_SH46_sagcot.pdf")
plot(SH32, main="TZA - Soil Depth (cm)")
hist(SH32, main="TZA - Soil Depth (cm)", col="blue")
plot(SH46, main="TZA - Soil Depth Indicator")
hist(SH46, main="TZA - Soil Depth Indicator", col="blue")
dev.off()
