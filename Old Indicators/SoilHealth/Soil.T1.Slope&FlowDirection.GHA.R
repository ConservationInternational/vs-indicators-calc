####################################################
#Slope and Flow direction rasters
#Date - August 12, 2015
#GHA
#Run time: ~1h30 min
####################################################

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

#load DEM North west Africa (not available for Ghana only):
# NOTE: Looks like it is now. Changed to AfSIS_HySRTM_DEM_Tanzania.tif
# - Tom
dem <- raster("AfSIS/AfSIS_HySRTM_DEM_Ghana/AfSIS_HySRTM_DEM_Ghana.tif")

#load polygon boundary data (gha area)
unzip(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()))
gha <- readOGR(dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()), "GHA_adm1")
gha

# Crop using extent, rasterize polygon and finally, create poly-raster
e <- extent(gha)
dem.gha <- dem# crop(dem, e, snap ="out")
rgha <- rasterize(gha, dem.gha) # 70min
dem <- mask(x = dem.gha, mask = rgha)

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject GHA boundary shapefile )
gha <- spTransform(gha, crs(aea))
# reproject DEM: 
dem <- projectRaster(dem, crs = aea) #6min

#generate a slope raster (in degrees) 
dem_slope <- terrain(dem, opt='slope', unit='degrees', neighbors=8) 

#generate a slop raster for flow direction 
dem_flowdir <- terrain(dem, opt='flowdir', unit='degrees', neighbors=8)

#write outputs
writeRaster(dem_slope, "gha_dem_slope.tif", format='GTiff', overwrite=T)
writeRaster(dem_flowdir, "gha_dem_flowdir.tif", format='GTiff', overwrite=T)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "gha_dem_slope.tif", target_path = "Soil_Health/gha_dem_slope.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "gha_dem_flowdir.tif", target_path = "Soil_Health/gha_dem_flowdir.tif")

#checks
pdf(file='gha_slope_flow_direction.pdf')
plot(dem_slope, main='Slope (degree)')
hist(dem_slope, main='Slope distribution (degree)')
plot(dem_flowdir, main='Flow direction')
hist(dem_flowdir, main='Flow direction distribution')
dev.off()

######################################
# generate slope and flow direction at a  250m resolution

# use the pH layer to create a reference for resampling:
gha_ph <- raster("gha_ph.tif")
# reproject reference raster pH raster file for resampling
gha_aea <- projectRaster(gha_ph, crs = aea)
gha_aea
#dimensions  : 2966, 2102, 6234532  (nrow, ncol, ncell)
#resolution  : 240, 260  (x, y)
#extent      : -2972559, -2468079, 538097, 1309257  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
gha_ref <- raster(nrow=2966, ncol=2102, crs=aea, res=c(240, 260), extent(c(-2972559, -2468079, 538097.3, 1309257)))
gha_ref <- setValues(gha_ref, NA)

# Note: same extent, same projection, but different resolution
dem
gha_aea

plot(gha_aea)
plot(dem, add=T)

# Set up higher resolution (apply a grid with LESS pixels within the same extent) and resample:
dem2 <- raster(nrow=2966, ncol=2102)
dem2<- resample(dem, gha_ref, method='bilinear')

#generate a slope raster (in degrees) 
dem_slope2 <- terrain(dem2, opt='slope', unit='degrees', neighbors=8) 

#generate a slop raster for flow direction 
dem_flowdir2 <- terrain(dem2, opt='flowdir', unit='degrees', neighbors=8)

#write outputs
writeRaster(dem_slope2, "gha_dem_slope_250m.tif", format='GTiff', overwrite=T)
writeRaster(dem_flowdir2, "gha_dem_flowdir_250m.tif", format='GTiff', overwrite=T)
s3$writeS3(bucket = "ci-vsindicators", source_path = "gha_dem_slope_250m.tif", target_path = "Soil_Health/gha_dem_slope_250m.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "gha_dem_flowdir_250m.tif", target_path = "Soil_Health/gha_dem_flowdir_250m.tif")

pdf(file='gha_slope_flow_direction_250m.pdf')
plot(dem_slope2, main='Slope (degree)')
hist(dem_slope2, main='Slope distribution (degree)')
plot(dem_flowdir2, main='Flow direction')
hist(dem_flowdir2, main='Flow direction distribution')
dev.off()
