####################################################
#Calculates 3 Indicators from the Soil Health Thread
#Topsoil carbon (0-20cm)(%)
####################################################
#load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)
library(VitalSignsUtilities)

#set directory
vs_tempdir <- tempdir()
setwd(vs_tempdir)

arguments <- commandArgs(trailingOnly = FALSE)
service_credentials <- gsub("[^=]+={1}", "",
                            arguments[grep("credentials", arguments)])
#credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T, cache = T)

# Query for all vital signs datasets in Google Drive
vstables$getExternalData()
vstables$getInternalData()

#load raster data (accessing via an external drive rather than web)
#ftp://ftp.soilgrids.org/data/AF/recent/  
#username: soilgrids / password: soilgrids
org_c_0_5 <- raster(vstables$tables[["AfSIS/af_ORCDRC_T__M_sd1_250m/af_ORCDRC_T__M_sd1_250m.tif"]]$getData())
org_c_5_15 <- raster(vstables$tables[["AfSIS/af_ORCDRC_T__M_sd2_250m/af_ORCDRC_T__M_sd2_250m.tif"]]$getData())
org_c_15_30 <- raster(vstables$tables[["AfSIS/af_ORCDRC_T__M_sd3_250m/af_ORCDRC_T__M_sd3_250m.tif"]]$getData())

#load polygon boundary data
gha <- readRDS(vstables$tables[["Boundaries/GHA_adm1.rds"]]$getData())

######################################
#Step 1. clip rasters to polygon boundary data
#reproject boundary shapefile to lambert equal area (using specification from AfSIS raster files)
#CRS information from ISRIC/AFSIS layers
laea <- '+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
gha_laea <- spTransform(gha, crs(laea))

#get extent of boundary shapefile
e <- extent(gha_laea)

#crop raster to shapefile extent (rectangular)
org_c_0_5_crop <- crop(org_c_0_5, e, snap="out")
org_c_5_15_crop <- crop(org_c_5_15, e, snap="out")
org_c_15_30_crop <- crop(org_c_15_30, e, snap="out")

#drop unneccessary dataframes
rm(org_c_0_5, org_c_5_15, org_c_15_30)

#create NA dummy raster with spatial extent equal to cropped raster
gha_dummy <- setValues(org_c_0_5_crop, NA)

#rasterize the boundary data
gha_r <- rasterize(gha_laea, gha_dummy)

#Put NA values in all raster cells outside shapefile boundaries
org_c_0_5_mask <- mask(x=org_c_0_5_crop, mask=gha_r)
org_c_5_15_mask <- mask(x=org_c_5_15_crop, mask=gha_r)
org_c_15_30_mask <- mask(x=org_c_15_30_crop, mask=gha_r)

#Weight the three separate layers to represent 0-20cm, and divide to convert units from g kg-1 to percent
org_c <- ((5*org_c_0_5_mask + 10*org_c_5_15_mask + 5*org_c_15_30_mask)/20)/10

#drop unneccessary dataframes
rm(gha_dummy, org_c_0_5_crop, org_c_5_15_crop, org_c_15_30_crop, org_c_0_5_mask, org_c_5_15_mask, org_c_15_30_mask, gha_r)

#check on data - 
plot(org_c, main="Soil organic carbon content (%) (0-20cm)")
org_c
hist(org_c, main="Soil organic carbon content (%) (0-20cm)", 
     col= "purple", 
     maxpixels=5517001)

#need to specify output location

writeRaster(org_c,"gha_org_c.tif",format='GTiff')
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "gha_org_c.tif", target_path = "Soil_Health/gha_org_c.tif")
