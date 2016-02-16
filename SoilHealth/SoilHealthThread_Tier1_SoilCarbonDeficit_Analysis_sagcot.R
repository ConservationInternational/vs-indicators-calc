####################################################
#Processes Soil Carbon Reference value and Soil Carbon Deficit for the Soil Health Thread
#Date - August 8, 2015 
####################################################
#currently only runs for Ghana
#deficit itself looks good but indicator logic produces some values less than 0, not sure why
#load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)
library(VitalSignsUtilities)

#arguments <- commandArgs(trailingOnly = FALSE)
#service_credentials <- gsub("[^=]+={1}", "",
#                            arguments[grep("credentials", arguments)])
credentials_file <- "~/Downloads/Vital Signs Data-bc27b8bfb478.json"

# Create instance of vital signs tables collector
vstables <- vital_signs_tables(creds_file = credentials_file, service = T)
s3 <- newS3()

#load raster data
#assumes that we would not download directly from web and I don't know how to abbreviate file paths on a PC, is it "..."?
#SH60 - Soil Carbon Deficit Indicator: Inputs: silt, clay and ph (depth: 0-5cm, 5-15cm, 15-30cm)
ph_0_5 <- raster(vstables$tables[["AfSIS/af_PHIHOX_T__M_sd1_250m/af_PHIHOX_T__M_sd1_250m.tif"]]$getData())
ph_5_15 <- raster(vstables$tables[["AfSIS/af_PHIHOX_T__M_sd2_250m/af_PHIHOX_T__M_sd2_250m.tif"]]$getData())
ph_15_30 <- raster(vstables$tables[["AfSIS/af_PHIHOX_T__M_sd3_250m/af_PHIHOX_T__M_sd3_250m.tif"]]$getData())
silt_0_5 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd1_250m/af_SLTPPT_T__M_sd1_250m.tif"]]$getData())
silt_5_15 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd2_250m/af_SLTPPT_T__M_sd2_250m.tif"]]$getData())
silt_15_30 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd3_250m/af_SLTPPT_T__M_sd3_250m.tif"]]$getData())
clay_0_5 <- raster(vstables$tables[["AfSIS/af_CLYPPT_T__M_sd1_250m/af_CLYPPT_T__M_sd1_250m.tif"]]$getData())
clay_5_15 <- raster(vstables$tables[["AfSIS/af_CLYPPT_T__M_sd2_250m/af_CLYPPT_T__M_sd2_250m.tif"]]$getData())
clay_15_30 <- raster(vstables$tables[["AfSIS/af_CLYPPT_T__M_sd3_250m/af_CLYPPT_T__M_sd3_250m.tif"]]$getData())
silt_0_5 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd1_250m/af_SLTPPT_T__M_sd1_250m.tif"]]$getData())
silt_5_15 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd2_250m/af_SLTPPT_T__M_sd2_250m.tif"]]$getData())
silt_15_30 <- raster(vstables$tables[["AfSIS/af_SLTPPT_T__M_sd3_250m/af_SLTPPT_T__M_sd3_250m.tif"]]$getData())
sand_0_5 <- raster(vstables$tables[["AfSIS/af_SNDPPT_T__M_sd1_250m/af_SNDPPT_T__M_sd1_250m.tif"]]$getData())
sand_5_15 <- raster(vstables$tables[["AfSIS/af_SNDPPT_T__M_sd2_250m/af_SNDPPT_T__M_sd2_250m.tif"]]$getData())
sand_15_30 <- raster(vstables$tables[["AfSIS/af_SNDPPT_T__M_sd3_250m/af_SNDPPT_T__M_sd3_250m.tif"]]$getData())
org_c <- raster("sagcot_org_c.tif")

#load polygon boundary data
unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")

######################################
#Staging
#Step 1. Clip all rasters (reprojecting and resampling as needed)
#Step 2. Weight the three separate layers to represent 0-20cm
#Step 3. Process data to correct units
######################################

######################################
#Step 1. clip rasters to polygon boundary data

#reproject boundary shapefile to lambert equal area (using specification from AfSIS/ raster files)
#CRS information from ISRIC/AfSIS/ layers
laea <- '+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

#sagcot_aeac <- spTransform(sagcot, crs(aeac))
sagcot_laea <- spTransform(sagcot, crs(laea))

#get extent of boundary shapefile
e <- extent(sagcot_laea)

#crop raster to shapefile extent (rectangular)
ph_0_5_crop <- crop(ph_0_5, e, snap="out")
ph_5_15_crop <- crop(ph_5_15, e, snap="out")
ph_15_30_crop <- crop(ph_15_30, e, snap="out")
silt_0_5_crop <- crop(silt_0_5, e, snap="out")
silt_5_15_crop <- crop(silt_5_15, e, snap="out")
silt_15_30_crop <- crop(silt_15_30, e, snap="out")
clay_0_5_crop <- crop(clay_0_5, e, snap="out")
clay_5_15_crop <- crop(clay_5_15, e, snap="out")
clay_15_30_crop <- crop(clay_15_30, e, snap="out")
sand_0_5_crop <- crop(sand_0_5, e, snap="out")
sand_5_15_crop <- crop(sand_5_15, e, snap="out")
sand_15_30_crop <- crop(sand_15_30, e, snap="out")

#create NA dummy raster with spatial extent equal to cropped raster
sagcot_dummy <- setValues(ph_0_5_crop, NA)

#rasterize the boundary data
sagcot_r <- rasterize(sagcot_laea, sagcot_dummy)

#Put NA values in all raster cells outside shapefile boundaries
ph_0_5_mask <- mask(x=ph_0_5_crop, mask=sagcot_r)
ph_5_15_mask <- mask(x=ph_5_15_crop, mask=sagcot_r)
ph_15_30_mask <- mask(x=ph_15_30_crop, mask=sagcot_r)
silt_0_5_mask <- mask(x=silt_0_5_crop, mask=sagcot_r)
silt_5_15_mask <- mask(x=silt_5_15_crop, mask=sagcot_r)
silt_15_30_mask <- mask(x=silt_15_30_crop, mask=sagcot_r)
clay_0_5_mask <- mask(x=clay_0_5_crop, mask=sagcot_r)
clay_5_15_mask <- mask(x=clay_5_15_crop, mask=sagcot_r)
clay_15_30_mask <- mask(x=clay_15_30_crop, mask=sagcot_r)
sand_0_5_mask <- mask(x=sand_0_5_crop, mask=sagcot_r)
sand_5_15_mask <- mask(x=sand_5_15_crop, mask=sagcot_r)
sand_15_30_mask <- mask(x=sand_15_30_crop, mask=sagcot_r)

#drop unneccessary dataframes
rm(ph_0_5_crop, ph_5_15_crop, ph_15_30_crop, clay_0_5_crop, clay_5_15_crop, clay_15_30_crop, silt_0_5_crop, silt_5_15_crop, silt_15_30_crop, sand_0_5, sand_5_15, sand_15_30, sand_0_5_crop, sand_5_15_crop, sand_15_30_crop)
rm(ph_0_5, ph_5_15, ph_15_30, clay_0_5, clay_5_15, clay_15_30, silt_0_5, silt_5_15, silt_15_30, sand_0_5, sand_5_15, sand_15_30)

#Weight the three separate layers to represent 0-20cm, ph appears to be multiplied by "10"
ph <- ((5*ph_0_5_mask + 10*ph_5_15_mask + 5*ph_15_30_mask)/20)/10
clay <- (5*clay_0_5_mask + 10*clay_5_15_mask + 5*clay_15_30_mask)/20
silt <- (5*silt_0_5_mask + 10*silt_5_15_mask + 5*silt_15_30_mask)/20
sand <- (5*sand_0_5_mask + 10*sand_5_15_mask + 5*sand_15_30_mask)/20

#check on data - 
plot(ph, main="ph")
ph
hist(ph, main="pH in H20 (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

plot(clay, main="Clay (%)")
clay
hist(clay, main="Clay (%) (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

plot(silt, main="Silt (%)")
silt
hist(silt, main="Silt (%) (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

plot(sand, main="sand (%)")
sand
hist(sand, main="sand (%) (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

#write outputs
writeRaster(ph,"sagcot_ph.tif",format='GTiff', overwrite=TRUE)
writeRaster(silt,"sagcot_silt.tif",format='GTiff', overwrite=TRUE)
writeRaster(clay,"sagcot_clay.tif",format='GTiff', overwrite=TRUE)
writeRaster(sand,"sagcot_sand.tif",format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_ph.tif", target_path = "Soil_Health/sagcot_ph.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_silt.tif", target_path = "Soil_Health/sagcot_silt.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_clay.tif", target_path = "Soil_Health/sagcot_clay.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "sagcot_sand.tif", target_path = "Soil_Health/sagcot_sand.tif")

#drop unneccessary dataframes
rm(ph_0_5_mask, ph_5_15_mask, ph_15_30_mask, clay_0_5_mask, clay_5_15_mask, clay_15_30_mask, silt_0_5_mask, silt_5_15_mask, silt_15_30_mask, sand_0_5_mask, sand_5_15_mask, sand_15_30_mask)

#Soil Carbon defecit Indicator (modif Cheryl; August 10)
Cref <- exp(1.333 + .00994*clay + .00699*silt-.156*(.923*ph-.52))
Cref_adj <- Cref*((10)/7)^-.58
sh42<-(org_c/Cref_adj)*100
sh42
hist(sh42, main="Soil Carbon Defecit (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

#SH60 -Soil carbon deficit indicator
SH60 <- .03333333*sh42 -1.66666667
SH60 [sh42 < 50] <- 0
SH60 [sh42 > 80] <- 1

hist(SH60, main="Soil Carbon Deficit Indicator (0-20cm)", 
     col= "purple", 
     maxpixels=8554200)

#write outputs
writeRaster(sh42, filename="soil_carbon_deficit_sagcot.tif", format='GTiff', overwrite=TRUE)
writeRaster(SH60, filename="soil_carbon_deficit_indicator_sagcot.tif", format='GTiff', overwrite=TRUE)
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_carbon_deficit_sagcot.tif", target_path = "Soil_Health/soil_carbon_deficit_sagcot.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_carbon_deficit_indicator_sagcot.tif", target_path = "Soil_Health/soil_carbon_deficit_indicator_sagcot.tif")

#display 
plot(SH60, main="SAGCOT - Soil Carbon Defecit Indicator (0-20cm)")
hist(SH60, main="SAGCOT - Soil Carbon Defecit Indicator (0-20cm)", col="blue")

plot(sh42, main="SAGCOT - Soil Carbon Defecit (0-20cm)")
hist(sh42, main="SAGCOT - Soil Carbon Defecit (0-20cm)", col="blue")

plot(Cref, main="SAGCOT - Carbon Reference (0-20cm)")
hist(Cref, main="SAGCOT - Carbon Reference  (0-20cm)", col="blue")

plot(org_c, main="SAGCOT - Organic Carbon (0-20cm)")
hist(org_c, main="SAGCOT - Organic Carbon (0-20cm)", col="blue")

