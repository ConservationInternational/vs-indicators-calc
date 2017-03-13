####################################################
#Calculates 3 Indicators from the Soil Health Thread
#Topsoil carbon (0-20cm)(%)
####################################################
#load the raster, sp, and rgdal packages
library(vitalsigns)
library(sp)

#Create a temp dir and set it as the working direcotry
vs_tempdir <- tempdir()
setwd(vs_tempdir)

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(client_id = "885296351610-59nnamcfrb05kdbjnev27g9t0hsbc53i.apps.googleusercontent.com",
                               client_secret = "0u9nM0ccDsr9ONp7otH7Ag7O")

vstables$getExternalData()

#load raster data (accessing via an external drive rather than web)
#ftp://ftp.soilgrids.org/data/AF/recent/  
#username: soilgrids / password: soilgrids
writeBin(vstables$tables[["af_ORCDRC_T__M_sd1_250m/af_ORCDRC_T__M_sd1_250m.tif"]]$getData(returnData = TRUE), "af_ORCDRC_T__M_sd1_250m.tif")
org_c_0_5 <- raster("af_ORCDRC_T__M_sd1_250m.tif")
writeBin(vstables$tables[["af_ORCDRC_T__M_sd2_250m/af_ORCDRC_T__M_sd2_250m.tif"]]$getData(returnData = TRUE), "af_ORCDRC_T__M_sd2_250m.tif")
org_c_5_15 <- raster("af_ORCDRC_T__M_sd2_250m/af_ORCDRC_T__M_sd2_250m.tif")
writeBin(vstables$tables[["af_ORCDRC_T__M_sd3_250m/af_ORCDRC_T__M_sd3_250m.tif"]]$getData(returnData = TRUE), "af_ORCDRC_T__M_sd3_250m.tif")
org_c_15_30 <- raster("af_ORCDRC_T__M_sd3_250m/af_ORCDRC_T__M_sd3_250m.tif")

#load polygon boundary data
writeBin(vstables$tables[["Boundaries/GHA_adm1.rds"]]$getData(returnData = TRUE), "GHA_adm1.rds")
gha <- readRDS("GHA_adm1.rds")

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
