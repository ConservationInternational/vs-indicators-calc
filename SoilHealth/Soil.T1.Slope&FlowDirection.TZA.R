####################################################
#Slope and Flow direction rasters
#Date - August 27, 2015
#TZA
#Runtime: ~80 min
####################################################

# initialize processing time: 
ptm <- proc.time()

#Data available on: ftp://africagrids.net/Other/
#http://www.inside-r.org/packages/cran/raster/docs/terrain
library(raster)
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

# SH61 - Soil Physical Indicator

#load DEM South east Africa (not available for Tanzania only):
# NOTE: Looks like it is now. Changed to AfSIS_HySRTM_DEM_Tanzania.tif
# - Tom
dem <- raster(vstables$tables[["AfSIS/AfSIS_HySRTM_DEM_Tanzania/AfSIS_HySRTM_DEM_Tanzania.tif"]]$getData())

#load polygon boundary data (sagcot area)

unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")
sagcot

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(sagcot)
dem.sagcot <- crop(dem, e, snap ="out")
rsagcot <- rasterize(sagcot, dem.sagcot) # ~30-40 min? 9a16-
dem <- mask(x = dem.sagcot, mask = rsagcot)

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject SAGCOT boundary shapefile )
sagcot <- spTransform(sagcot, crs(aea))
# reproject DEM: 
dem <- projectRaster(dem, crs = aea) #6min

#generate a slope raster (in degrees) 
dem_slope <- terrain(dem, opt='slope', unit='degrees', neighbors=8) 

#generate a slop raster for flow direction 
dem_flowdir <- terrain(dem, opt='flowdir', unit='degrees', neighbors=8)

#write outputs
setwd(dir_out1)
writeRaster(dem_slope, "sagcot_dem_slope.tif", format='GTiff', overwrite=T)
writeRaster(dem_flowdir, "sagcot_dem_flowdir.tif", format='GTiff', overwrite=T)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_dem_slope.tif", target_path = "Soil_Health/sagcot_dem_slope.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_dem_flowdir.tif", target_path = "Soil_Health/sagcot_dem_flowdir.tif")

#checks
pdf(file='sagcot_slope_flow_direction.pdf')
plot(dem_slope, main='Slope (degree)')
hist(dem_slope, main='Slope distribution (degree)')
plot(dem_flowdir, main='Flow direction')
hist(dem_flowdir, main='Flow direction distribution')
dev.off()

######################################
# generate slope and flow direction at a  250m resolution

# use the pH layer to create a reference for resampling:
sagcot_ph <- raster("sagcot_ph.tif")
# reproject reference raster pH raster file for resampling
sagcot_aea <- projectRaster(sagcot_ph, crs = aea)
sagcot_aea
#dimensions  : 2177, 4083, 8888691  (nrow, ncol, ncell)
#resolution  : 235, 266  (x, y)
#extent      : 554167.7, 1513673, -1254644, -675562  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
sagcot_ref <- raster(nrow=2177, ncol=4083, crs=aea, res=c(235, 266), extent(c(554167.7, 1513673, -1254644, -675562)))
sagcot_ref <- setValues(sagcot_ref, NA)

# Note: same extent, same projection, but different resolution
dem
sagcot_aea

plot(sagcot_aea)
plot(dem, add=T)

# Set up higher resolution (apply a grid with LESS pixels within the same extent) and resample:
dem2 <- raster(nrow=2177, ncol=4083)
dem2<- resample(dem, sagcot_ref, method='bilinear')

#generate a slope raster (in degrees) 
dem_slope2 <- terrain(dem2, opt='slope', unit='degrees', neighbors=8) 

#generate a slop raster for flow direction 
dem_flowdir2 <- terrain(dem2, opt='flowdir', unit='degrees', neighbors=8)

#write outputs
setwd(dir_out1)
writeRaster(dem_slope2, "sagcot_dem_slope_250m.tif", format='GTiff', overwrite=T)
writeRaster(dem_flowdir2, "sagcot_dem_flowdir_250m.tif", format='GTiff', overwrite=T)

s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_dem_slope_250m.tif", target_path = "Soil_Health/sagcot_dem_slope_250m.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_dem_flowdir_250m.tif", target_path = "Soil_Health/sagcot_dem_flowdir_250m.tif")

pdf(file='sagcot_slope_flow_direction_250m.pdf')
plot(dem_slope2, main='Slope (degree)')
hist(dem_slope2, main='Slope distribution (degree)')
plot(dem_flowdir2, main='Flow direction')
hist(dem_flowdir2, main='Flow direction distribution')
dev.off()

# display processing time:
proc.time() - ptm
#   user  system elapsed 
# 4467.54  152.79 4857.06 