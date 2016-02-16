########################################
#Tier 1 - C Factor - Cover factor for soil erosion modeling- TZA
########################################
#issue with resampling command

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

# <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2002.tif")
fapar_2003_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2003.tif"]]$getData())
fapar_2004_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2004.tif"]]$getData())
fapar_2005_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2005.tif"]]$getData())
fapar_2006_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2006.tif"]]$getData())
fapar_2007_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2007.tif"]]$getData())
fapar_2008_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2008.tif"]]$getData())
fapar_2009_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2009.tif"]]$getData())
fapar_2010_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2010.tif"]]$getData())
fapar_2011_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2011.tif"]]$getData())
fapar_2012_gha <- raster(vstables$tables[["fAPAR/GHA/cumulative_annual_fpar_1km_2012.tif"]]$getData())

# Reproject all files to Africa Albers Equal Area Conic (ESRI:102022)

aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
# reproject GHANA boundary shapefile )
unzip(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()))
gha <- readOGR(dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()), "GHA_adm1")
gha_aea <- spTransform(gha, crs(aea))

# reproject reference raster pH raster file for resampling

gha_ph <- raster("gha_ph.tif")
gha_aea <- projectRaster(gha_ph, crs = aea)


#build stack of normalized cumulative fapar
cumfaparstack <- stack(fapar_2003_gha)
cumfaparstack <- addLayer(cumfaparstack, c(fapar_2004_gha, fapar_2005_gha, 
                                           fapar_2006_gha, fapar_2007_gha, fapar_2008_gha, 
                                           fapar_2009_gha, fapar_2010_gha, fapar_2011_gha, 
                                           fapar_2012_gha))

#normalize function
normalize <- function(x) {(x - min(x, na.rm=TRUE)*1)/(max(x,na.rm=TRUE) -
                                                        min(x, na.rm=TRUE)-0)}

norm_cum_fapar <- normalize(cumfaparstack)

#take average value for the time series
mean_norm_cum_fapar <- mean(norm_cum_fapar)
#convert to C factor
c_factor <- 1 - mean_norm_cum_fapar

# use the pH layer to create a reference for resampling:
gha_aea
#dimensions  : 2966, 2102, 6234532  (nrow, ncol, ncell)
#resolution  : 240, 260  (x, y)
#extent      : -2972559, -2468079, 538097.3, 1309257  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 
#+ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
gha_ref <- raster(nrow=2966, ncol=2102, crs=aea, res=c(240, 260), extent(c(-2972559, -2468079, 538097.3, 1309257)))
gha_ref <- setValues(gha_ref, NA)

# Set up higher resolution for erosivity (apply a grid with MORE pixels within the same extent) and resample:
#ero2 <- raster(nrow=2177, ncol=4083)
c_factor2 <- resample(c_factor, gha_ref, method='bilinear')

#write ouput
writeRaster(mean_norm_cum_fapar, filename="cumfAPAR_norm_gha.tif", format='GTiff', overwrite=TRUE)
writeRaster(c_factor2, filename="cfactor_gha.tif", format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "cumfAPAR_norm_gha.tif", target_path = "Soil_Health/cumfAPAR_norm_gha.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "cfactor_gha.tif", target_path = "Soil_Health/cfactor_gha.tif")

plot(mean_norm_cum_fapar, main="fapar")