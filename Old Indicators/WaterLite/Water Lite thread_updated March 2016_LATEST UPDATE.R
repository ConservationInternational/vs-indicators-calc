################################# CI   VITAL SIGNS WATER LITE THREAD #######################################
########################  Calculation of indicators and water security index ###############################
#This script is to calculate the required indicators and water security index per catchment or sub-catcment within a country.
#It follows the data processing framework developed for the Woodfuel thread by Marc Pienaar (CSIR, SA).

#The Water Lite thread workflow document (RJ Scholes & MR van der Merwe, version 12 October 2015) accompanies this script.
#Script was developed for R version 3.2.2 (2015-08-14) -- "Fire Safety"
#Script was tested again in Feb 2016 BUT for R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
#Script was only tested for the Rufiji basin and its four subbasins in TZA
#It has not been applied to and tested for Ghana and Uganda.
#The main Rufiji overlaps to varying extents with the SAGCOT region.
#Rapid roadside assessments, water quality and flow measurements taken as part of the VS project are needed for validation purposes. This data are not yet available for usage.
#Vital Signs generated or collected data should be used as input to calculate the indicators and to validate model results where required. 
#Currently external datasets are used as inputs for the indicator calculations except for rainfall, reference evaporation and population data (i.e. Vital Signs datasets).

#Technical inputs wrt models were contributed by multiple persons (Bob Scholes (Wits Uni); Marna vd Merwe, Marc Pienaar, Graham von Maltitz, Michele Walters (CSIR)).

#This is part of the Vital Signs project started at CI.

#AUTHORS: Marna van der Merwe                                                                   #
#DATE CREATED: 14/08/2015 
#LAST DATE MODIFIED:28/10/2015
#LAST DATE UPDATED: 21/03/2016  (code was rerun, changes made to original script and intermediate and final results regenerated)

#The water security index per main/sub-river basin is the difference between the total water yield adjusted for fitness for use and total demand (ratio, [%])
#It depends on the calculation of the following indicators:
# (1) Total surface water Yield indicator[km3/year] per main or sub-river basin
# (2) Total water demand indicator [km3/year] per main or sub-river basin
# (2a) Human demand
# (2b) Livestock demand (not included in workflow document but provision is made for it in the calculations)
# (2c) Irrigation demand
# (3) Water quality indicator [MPN and mg/100ml] mean values per year per main or sub-river basin
# (3a) Suspended sediment [mg/100ml]
# (3b) E.coli [MPN/100ml]
# (3c) Nitrogen fertiliser [mg-N/100ml]

#The following thread outputs are generated & visualised
# 1) Annual Water Supply (available)
# 2) Water Quality Index
# 3) Water Security

#Validation is done but not in detail due to the lack of appropriate data sets

#The appoach followed starts with generic calculations per pixel at the country scale and then detailed basin scale calculations.

####################################

# ------------------------------------------------------------------------------
# 1. INSTALLING AND LOADING LIBRARIES
# Function to install and load multiple R packages (Author: Steven Worthington: ipak.R function, 2013)
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  #Check if packages are already installed
  #If not, install and then load into worksession
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
packages <- c("raster","ff","mapproj", "geosphere","rgdal","gdalUtils", "maptools","rgeos","RColorBrewer","R.utils","pryr");#list of packages that are required
ipak(packages); # check that all packages are installed and loaded, i.e. "TRUE".

#Note: some packages (rgeos, gdalUtils, R.utils) first need to be installed using one of the following 2x options before running:
# > packages <- c("raster","ff","mapproj", "geosphere","rgdal","gdalUtils", "maptools","rgeos","RColorBrewer","R.utils","pryr")
# > ipak(packages)
# 1. >install.packages('rgeos', type="source"); #example: rgeos version: 0.3-13 needs to be installed from source in R3.2.2 if it gives error message "not available for R3.2.2"
# OR
# 2. > Download the zip binary from Cran-r and all the dependencies, use R-menu to install all of these from local zip files. Only needed to be done once, then run the function in full.

#Note: to get a list of objects in memory in the currect session after some calculations are done, type ls()
#Note: to check the size of the objects in memory individually using the 'pryr' package, i.e. use object_size(objectName); total memory in use in current session mem_used(); memory change in memory used by the object (i.e. mem_change(objectName) '-' value means decrease)
#Note: to remove an object(s) that is/are not needed anymore and uses/use a lot of memory use rm(objectName1, objectName2,....). Garbage collection (gc()) should happen automatically BUT is NOT always the case depending on the OS
#Note: The code below was all run in one worksession on Windows7 64B machine. It may be necessary to close R and restart or even shutdown the machine to release memory for use by R.
#Note: The code is written so that the memory intensive functions are first executed and the results saved in an intermediate results folder for future use, i.e. only necessary to open the intermediate results files in a new R session.
#Note: To zoom in on a graph of a raster object use the 'raster' package:: zoom(x, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE) (where x=object)
#    : To zoom in on a graph of a spatial object use the 'raster' package:: zoom(x, ext=drawExtent(), new=TRUE,  ...)
#Note: Use 'rgdal' to work with shapefiles readOGR::rgdal as readOGR also reads the CRS.
#Note: Use setMinMax('objectName') when the raster object (replace 'objectName' with appropriate name) does not have values assigned to it (ie when typing 'objectName' no min and max values appear in the object summary)
#Note: Use 'objectName'['objectName'==0]<-NA for the human population count data when calculating cellStats function in the 'raster' package, eg mean. Replace 'objectName' with appliable object created
#    : Use freq('objectName', digits=0, value=0, useNA='no') where value can be  any value (i.e. zero used as an example) to check number of occurrences of the specific value using the 'raster' package

#Note: use rgdal::readOGR to read in the projection information of a shapefile by default, unlike maptools::readShapePoly
# ------------------------------------------------------------------------------
# 2. DEFINE GEOGRAPHICAL PROJECTION
# For descriptions of the proj4 strings or coordinate reference system see www.spatialreference.org
# For parameter options & projection names see http://www.remotesensing.org/geotiff/proj_list/
crs.geo <-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs");#all inputs without an assigned CRS (e.g. points data) are first assigned a lat/lon before reprojection
crs.aea<-CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ");#the albers equal area projection are used for the datasets
# Notes:
# Albers equal area projection is used as its assumed that the effect of area is usually considered more serious than the effect of shape.
# The Albers Equal Area projection has the property that the area bounded by any pair of parallels and meridians is exactly reproduced
# between the image of those parallels and meridians in the projected domain.
# THUS the projection preserves the correct area of the earth although it distorts to some extent the direction, distance and shape.  

# ------------------------------------------------------------------------------
# 3. CREATE EMPTY 1000m RESOLUTION GRID THAT FITS THE EXTENT OF THE COUNTRY (RECTANGULAR FORMAT)
# This consists of several steps
# This grid is then used to:
# 1) Clip (if data are for more than one country) to the country extent;
# 2) Aggregate (fine-> coarser) and disaggregate (coarse->finer) input layers to all have a 1000x1000m resolution;
# 3) Resampled data at the country level in raster format are then clipped/cropped to fit the basin (or subbasin) extent and shape.
# The country and basin outlines are in vector format and in different to Albers Equal Area projection.

# COUNTRY OUTLINE
# Import country outline data: SpatialPolygonsDataFrame
datadir<-choose.dir(getwd()); # choose directory where country outline shapefile is stored
setwd(datadir); # then set working directory to user selected directory
TZA<-readOGR(choose.files(),layer="Tanz"); # choose the shapefile; layer is the name of the shapefile without its extension '~.shp'.
TZA; # check if it has a CRS assigned, IF NOT THEN
proj4string(TZA) <- crs.geo; # assign a CRS (i.e.crs.geo) before transforming it to the crs.aea projection ELSE reproject directly
TZA.equal <- spTransform(TZA,crs.aea); # reproject to albers equal area using crs.aea
TZA.equal; # Check that it was reprojected
# Save reprojected country outline as shapefile in intermediate results folder for future use, i.e. to create a raster grid with 1x1km resolution, aea CRS that fits the extent of the country outline
dir<-choose.dir(getwd()); # choose intermediate results folder
setwd(dir); # then set working directory to user selected directory
writeOGR(TZA.equal, dsn=getwd(), layer="TZA_aea_CRS", driver="ESRI Shapefile", overwrite_layer=T); # this file is used to create a raster grid fitting the extent of the country outline (1x1km resolution)

# Create an empty rectangular grid in raster format with ~1000x~1000m resolution (resolution will not be exact with method presented below). Method below was the least memory consuming and 'fastest' on Windows7 64-B machine 
# and in aea projection that fits exactly the country outline extent
# Get grid extent
Xmin<-xmin(extent(TZA.equal))
Xmax<-xmax(extent(TZA.equal))
Ymax<-ymax(extent(TZA.equal))
Ymin<-ymin(extent(TZA.equal))
# Round to km as TZA.equal is in meters
Xmin<-round(Xmin/1000)
Xmax<-round(Xmax/1000)
Ymin<-round(Ymin/1000)
Ymax<-round(Ymax/1000)
# Calculate number of rows (lat=y) and number of columns (lon=x) that are needed to create the raster
nrows<-length(Ymin:Ymax)
ncols<-length(Xmin:Xmax)
# Make extent object with dimensions rounded to km but extent and resolution expressed in decimal meters
ext<-extent(TZA.equal)
# Create a raster with ~1000m resolution and in crs.aea projection
r <- raster(ext,ncol=ncols-1,nrow=nrows-1,crs=crs.aea)
r[]<-0; # fill grid cells with zero values
# Save as raster file in geotiff format. This file is used in the calculations
c(getwd()); # choose the intermediate results folder
setwd(dir)
writeRaster(r,filename="TZA_grid_1km.tif",format="GTiff", overwrite=TRUE); # for use later

# BASIN OUTLINES
# It is recommended to use the Hydrosheds 30s beta version vector file for the main river basin outlines and Hydrobasins subbasins vector file for the subbasin outlines.
# Main reason for using the hydrosheds and hydrobasins files is that these are the most accurate ito delineating the catchment areas based on topography and river flow directions.
# Rufiji basin in Tanzania is used as an example.
# The Hydrosheds main basin file does not contain subbasin data e.g. subbasin outlines,areas and names which are needed for doing sub basin calculations.
# The hydrobasins file with the subbasins info first needs to be cropped with the main basin file as the subbasins may not overlap with the main basin outlines 100%.
# For example only the rivers in the western side of the Bahi Swamps basin drain into the Rufiji basin and it would be wrong to use the hydrobasins file as a substitute for the Rufiji main basin outline. 

# Import main basin outline data: SpatialPolygonsDataFrame
datadir<-choose.dir(getwd()); # choose directory where hydrosheds 30s beta basin outlines shapefile is stored
setwd(datadir); # then set working directory to user selected directory
Main_Basins<-readOGR(choose.files(),layer="af_bas_30s_beta"); # open hydrosheds basins outline shapefile which is in 30 arc sec or 0.008333 degrees resolution
Main_Basins; # to get feature names, e.g. "BASIN_ID" is the one needed
Rufiji_Main <- subset(Main_Basins, BASIN_ID=="55230"); # used QGIS and TZA Ministry of Water shapefiles to first determine which BASIN_ID in hydrosheds file is applicable to Rufiji
Rufiji.main.equal <-spTransform(Rufiji_Main,crs.aea); # project to albers equal area using crs.aea
# Save main basin outline object as shapefile
dir<-choose.dir(getwd()); # save file in the intermediate results directory
setwd(dir)
writeOGR(Rufiji.main.equal, dsn=getwd(), layer="Rufiji_Main_CRS_aea", driver="ESRI Shapefile", overwrite_layer=T); # save as shapefile with latlon, albers equal area projection for future use

# Import sub basins outline data: SpatialPolygonsDataFrame. 
datadir<-choose.dir(getwd()); # choose directory where basin outlines shapefile from hydrobasins is stored
setwd(datadir); # then set working directory to user selected directory
Subbasins<-readOGR(choose.files(),layer="hydrobasins_africa"); # open hydrobasins africa sub basins outline shapefile stored in the Hydrobasins and NOT Hydrosheds folder
projection(Subbasins) <- projection(Main_Basins); # first set the projections the same in order to subset
# Subset hydrobasins_africa shapefile with desired subbasins that fall within Rufiji main basin
# Used QGIS to get the sub basin names that overlap with the hydrosheds 30sec basin shapefile, BASIN_ID="55230"
Rufiji_sub <- c("Great Ruaha 1","Great Ruaha 2","Rufiji 1","Rufiji 2", "Njombe", "Bahi swamps")
Rufiji.subbasins <- Subbasins[as.character(Subbasins@data$SUB_NAME) %in% Rufiji_sub, ]; # create object with only the sub basins located in Rufiji main basin
Rufiji.subbasins.equal <- spTransform(Rufiji.subbasins,crs.aea); # project to albers equal area using crs.aea
# Plot the Rufiji main basin extracted from Hydrosheds and reprojected to aea CRS together with the Rufiji subbasins extracted from the Hydrobasins file.
# Note: The plots show that the subbasin outlines from Hydrobasins need to be clipped to fit the Hydroshed main basin outline.
# Crop the sub basins object with the subsetted Rufiji main basin spatial polygons data frame as this is the correct main basin outline.
Rufiji.subbasins_crop <- crop(Rufiji.subbasins.equal, Rufiji.main.equal); # the subbasins outline is now the main basin outline and its extent and shape are used to create the empty raster grid below
# Check if it now has same extent as the Rufiji main basin, both are in the same projection. Will not be exact match ito extent as can also be seen when plotting the two objects.
extent(Rufiji.main.equal)
extent(Rufiji.subbasins_crop)
plot(Rufiji, border="red"); # plot main basin outline extracted from Hydrosheds
plot(Rufiji.subbasins_crop,add=T, border="black",lty=2); # plot extracted Hydrobasins subbasins outline clipped to the Hydrosheds main basin outline
text(Rufiji.subbasins_crop, label="SUB_NAME", cex=0.7); # to add the subbasin names to the plot for inspection using the basics plot function in R
# > zoom(Rufiji.subbasins_crop,ext=drawExtent(), new=TRUE); # not run. To zoom in on the overlayed graphs to inspect differences if necessary
# Note: Although there are differences ito of the latitudinal extents, these are considered insignificant and the cropped subbasin outlines object is kept as is.
# Save cropped subbasins object with aea CRS as shapefile
dir<-choose.dir(getwd()); # save file in the intermediate results directory
setwd(dir)
writeOGR(Rufiji.subbasins_crop, dsn=getwd(), layer="Rufiji_subbasins_CRS_aea", driver="ESRI Shapefile", overwrite_layer=T); # for re-use, i.e. to clip and mask country level files in raster format

# Create an empty grid in raster format with ~1000x~1000m resolution (resolution will not be exact) and in aea projection that fits exactly the extent of the basin of interest's outline.
# Note: To create an empty basin grid is ONLY necessary to do when input data are only available at the basin level and not the country level;
# VS generated input data sets used are all available at the country level.
# Code between ****** and ****** only included for Section 7 purposes (to delineate the basin area that contributes runoff to specific point in the river)

# **************
# Get grid extent
Xmin<-xmin(extent(Rufiji.subbasins_crop))
Xmax<-xmax(extent(Rufiji.subbasins_crop))
Ymax<-ymax(extent(Rufiji.subbasins_crop))
Ymin<-ymin(extent(Rufiji.subbasins_crop))
# Round to km as TZA.equal is in meters
Xmin<-round(Xmin/1000)
Xmax<-round(Xmax/1000)
Ymin<-round(Ymin/1000)
Ymax<-round(Ymax/1000)
# Calculate number of rows (lat=y) and number of columns (lon=x) that are needed to create the raster
nrows<-length(Ymin:Ymax)
ncols<-length(Xmin:Xmax)
# Make extent object with dimensions rounded to km but extent and resolution expressed in decimal meters
ext<-extent(Rufiji.subbasins_crop)
# Create a raster with ~1km resolution and in crs.aea projection
basinr <- raster(ext,ncol=ncols-1,nrow=nrows-1,crs=crs.aea)
basinr[]<-0; # fill grid cells with zero values
# Save as raster file in geotiff format. This file is used in the calculations
dir<-choose.dir(getwd()); # choose the intermediate results folder
setwd(dir)
writeRaster(basinr,filename="Rufiji_grid_1km.tif",format="GTiff", overwrite=TRUE); # for use later
# Crop country level data to fit the basin outline and shape using the basin outlines file(s) generated as described above.
# It is then masked with the basin outline shapefile file to get the same shape and extent as the subbasins outline shapefile but now in grid format.
# Crop and mask the basin grid to fit the shape and extent of the basin outline shapefile.
basinr_crop<-crop(basinr, extent(Rufiji.subbasins_crop)); # if the subbasins outline cropped to fit the main basin outline is still in memory, else read in using readOGR ""Rufiji_subbasins_CRS_aea.shp" 
basinr_mask<-mask(basinr_crop,Rufiji.subbasins_crop): # to fit shape of the cropped basin outline object
# Plot to see if it is correct
plot(basinr_mask)
plot(Rufiji.subbasins_crop,add=T,lwd=2);# plot outline with a thick line
# **************
 
# ------------------------------------------------------------------------------
# 4. RASTERIZE ALL INPUT DATA THAT ARE IN POLYGON, SHAPE OR POINT FORMAT
# For Tanzania all data inputs are at the country level.
# The empty country grid object is already in raster format. If this object is not still in memory read in the created and saved grid geotiff file(s)
# > country_grid<-raster(choose.files()); # run only if necessary
# > basin_grid<-raster(choose.files()); # run only if necessary when data are only available on the basin level, i.e. not country level

# WATER YIELD: INPUT FILES
# The VS generated Precipitation and Evapo'transpi'ration files are already in raster format with a 3000x3000m resolution and clipped to the country extent and outline.
# The daily files first need to be summed to yearly totals and the final yearly result file needs to be disaggregated to a 1000x1000m resolution and clipped to the basin extent grid files (see Section 5)

# WATER NEED/CONSUMPTION: INPUT FILES
# HUMAN WATER NEEDS
# Household survey data are not available to estimate the average household and per capita water consumption per year.
# VS-human population data are already in raster format with 3000x3000m resolution.Disaggregate population count data to 1000x1000m resolution and clip and mask to fit the basin extent and shape grid files (see Section 5)
# Mean water consumption per person per year are assumed based on the WHO guidelines of a minimum of 20 litre/capita per day independent of social conditions and urban or rural settings.
# The VS generated Human population count per pixel (NOT density as pixel size varies across latitude) Landscan data per year (2000-2013) files are already in raster format with a 923.3468 x 923.3468m resolution (i.e. 30 arc-seconds).
# Notes wrt Landscan information (http://web.ornl.gov/sci/landscan/landscan_documentation.shtml):
# Population counts per pixel values have already been normalized to sum to each sub-national administrative unit estimate.
# For this reason, projecting the data in a raster format to a different coordinate system (including on-the-fly projections) will result in a re-sampling of the data and the integrity of normalized population counts will be compromised.
# Also prior to all spatial analysis, users should ensure that extents are set to an exact multiple of the cell size (for example 35.25, 35.50, 35.0) to avoid "shifting" of the dataset.
# It was assumed that the VS population data set was generated with consideration of the above.
# When reading in the population geotiff tiles it is necessary to set the min and max values using setMinMax('objectName') (See Section 5)

# ANIMAL WATER NEEDS
# Animal water needs are considered insignificant in VS but are included for completion purposes.
# Mean animal water consumption per animal type per day is based on Peden etal (2007). Assumed worst case scenario - dry hot temperature values Cattle: 50 litres/animal per day; Sheep: 50 L/animal per day; goats: 50 L/animal per day.
# Global animal population density (number of animals for a given type/km2 per year) files for 2006 (Robinson etal, 2014; available from the livestock geowiki) are already in raster format with a 0.008333 dec degree resolution and need to be clipped to the country grid, aggregated to 1000x1000m resolution and then cropped and masked with the cropped subbasins outline (see Section 5&6)

# IRRIGATION WATER NEEDS
# Africa country irrigated areas files for 2005 are already in raster format with a 0.0833333 dec degree and need to be reprojected, clipped to the country grid, disaggregated to 1000x1000m resolution and then cropped and masked with the cropped subbasins outline (see Section 5&6)
# A rough estimate of the irrigation water needs is provided below.
# 1) The water use by crops is automatically included in the river flow calculations described above, since the actual evaporation by cropped landscapes is detected in the same way that natural vegetation is detected.
# 2) However, the demand by irrigated crops needs to be factored into the calculation of water demand in the basin.
# 3) Irrigation needs (m3/period) = Ai *(( ???Ep*Ci)-???P) * 10; # period=year as crop seasons data are needed to identify months and number of months per year
# Where Ai is the area (ha) in the basin planted to irrigated crop i
# 4) ???Ep is the sum of potential evaporation over the period for which the water need is being calculated (monthly, or perhaps the period for which the crop is planted) [mm]
# 5) Ci is the 'crop factor', from FAO, which related the water needs of particular crops to  the potential evaporation (a value between 0 and 1, typically around 0.8) [dimensionless ratio]
# Ci of 0.8 is assumed in the absence of measured data; 
# 6) ???P is the rainfall over the evaluation period [mm]
# 7) The constant 10 is to convert mm*ha to m3

# WATER QUALITY: INPUT FILES
# N & P LOADS
# Amount of N- and P-fertilizer runoff (i.e. excess) into water sources are estimated as part of the Agricultural Intensification thread.
# However, in the absence of data on the amount of nitrogen and phosphorous that are not utilised by the crops and soil biota or emitted into the atmosphere and become part of the runoff, N and P application data layers, i.e. Global Fertilizer and Manure dataset, created by Potter, Navin Ramankutty et al. (2010) are used.
# N-application rates reference: Potter, P., N. Ramankutty, E.M. Bennett, and S.D. Donner. 2011. Global Fertilizer and Manure, Version 1: Nitrogen Fertilizer Application. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). http://dx.doi.org/10.7927/H4Q81B0R.
# It is assumed that 50% and 65% of the applied N and P respectively are not recovered (Tanzania fertilizer assessment done by IFDC (2012)), i.e. are lost and ultimately land in water courses.
# The N and P loss percentages are assumed the same for all crop types and to be independent of farming system used, soil and local climate and weather conditions.
# Africa continent fertilizer application files are already in raster format with a 0.5 dec degree resolution and need to be clipped to the country grid, disaggregated to 1000x1000m resolution and then cropped and masked with the cropped subbasins outline (see Section 5)
# Fertilizer application is assumed the same for all hectares planted with crops in the grid cell. Use the FAO crop area maps (ie no differentiation between crop types and no N application differentiation between N-application rates)
# Use the ArcGIS Land use shapefile for TZA to extract the cultivated areas [ha/pixel]
# Read in the land cover shapefile for the country and extract the cropland pixels
dir<-choose.dir()
setwd(dir)
list.files();# to get list of file names
LU<-readOGR("Tanzania_landuseR.shp" ,layer="Tanzania_landuseR"); # external data set available from http://www.arcgis.com/features/. VS-data set should be used if available
LU;# to get summary of the file and the column/variable names (e.g GENVEG)
LU$GENVEG;# to get names of the different land cover classes used, e.g. 'Cultivated Land'
crops <- subset(LU, GENVEG=="Cultivated Land")
# Reproject the subsetted shapefile to fit the country raster grid, resolution and extent
crops.equal <- spTransform(crops,crs.aea)
# Rasterize the crops shapefile to fit country raster grid, extent and resolution
crops_raster<- rasterize(crops.equal, r);# default bilinear method is used, use the country grid raster object (if not in memory then create an object with name 'r' and read in the already created country grid aea 1km file )
# Check that the object is in raster format
class(crops_raster);# should retun "Rasterlayer"
# Save rasterized crops object
dir<-choose.dir()
setwd(dir);# select intermediate results folder
writeRaster(crops_raster,filename="'countryName'_crops_aea_1km.tif",format="GTiff", overwrite=TRUE); # replace 'countryName' with applicable country name
# Multiply the crops [ha] raster with N-washoff [kg/ha] to estimate the total N washoff per pixel assuming the N-washoff is the same independent of crop type, farming practices and local soil and weather conditions (Section 6)
# This is done only to create an input layer for modelling purposes and should be replaced with VS-generated data when this become available.

# E.COLI LOAD
# Biological water quality data are not yet available from the VS field protocols.
# E.coli load needs to be calculated using an assumed E.coli load per person/year that finally reaches the river network in a year.
# Assume each person produces on average 128g feaces/day which contains 1x10^6 E.coli cells/g feaces.
# Based on Pegram (2001) E.coli wash-off rates can vary between 1-500 x 10^6 number of E.coli cells/ha per day during a rainy event and shortly thereafter.
# The range in values relates to rural, urban or informal settlements.A wash-off value of 100x10^6 cells/ha/day (equivalent to 1x10^10 cells/km2 per day) is assumed which takes into consideration the differences in soil and rainfall conditions over time and space.
# This equates to a wash-off rate per person of about 196x10^6 cells/person/year based on a mean population number of 51 persons/km2 for Tanzania.
# If it is assumed that on average a person produces 128g feaces/day which contains 1x10^6 E.coli cells/g of feaces, each person excretes 1.28x10^8 E.coli cells/day
# equivalent to 1.28x10^8 x 365.25 (days per year) = 4.675 x 10^10 cells/person per year, only about 0.004456 of all the cells produced/person in a year will ultimately reach a water course in a viable condition.
# Thus for VS purposes: the number of people per pixel [persons/year] x 4.675 x 10^10 [cells/person per year] x 0.004194 [wash-off ratio] = E.coli load per person/year per pixel with a resolution of ~1000m based on the Pegram (2001) reported E.coli wash-off rates per unit area.
# Ideally the E.coli runoff load should be differentiated between rural and urban areas. Currently not possible due to the lack of VS data.

# SEDIMENT LOAD
# Sediment load is available as a point file, needs to be turned into points spatial data frame, and the locations and measured/estimated sediment load values need to be extracted for the basin (e.g. Rufiji) of interest.
# The external sediment load data are not rasterized as no modelling be done with these. Code is only to extract the required lat/lon, catchment area contributing to the observed load at the point of interest and sediment yield values.
# NO sediment load modeling is done as part of the water thread, it is assumed that such data will become available as part of VS project and that the data will be available in a raster format for country/basin of interest.
dir<-choose.dir(getwd())
setwd(dir)
# Read in comma separated file with points data
points<-read.csv("Sediment load.csv",stringsAsFactors = FALSE)
# View column names
names(points): #to get the column names and indices where lat ('Latitude',y) and lon ('Longitude', x) are stored; i.e. columns 5&6, attribute names to be used for filtering, e.g. "Country"; "ID"; River.Catchment.Name"
coordinates(points) <- ~ Longitude + Latitude; # to make points a spatialpointsdataframe (SPDF)
class(points);# check that the points object is now a SPDF
# Assign projection to the spatial points and reproject to albers equal area
proj4string(points) <- crs.geo
points_equal<-spTransform(points,crs.aea)
crs(points_equal); # to check object was assigned the aea projection
# Extract only the stations within Tanzania and save the object
points_subset_country <- points[points_equal$Country=="Tanzania",]; # filter SPDF to find only stations in Tanzania
points_subset_country; # to check which stations were extracted and if they are all in Tanzania
# Save the extracted dataset with all its attributes as a shapefile for later use
dir<-choose.dir(getwd())
setwd(dir)
writeOGR(points_subset_country, dsn=getwd(), layer="Sediment.Stations_'countryName'", driver="ESRI Shapefile", overwrite_layer=T); # replace _'countryName' with the correct name, e.g. Tanzania
# Do the same but now for the basin
points_subset_basin <- points_equal[points_equal$River.Catchment.Name=="Rufiji",]; # filter SPDF to find only Rufiji stations
points_subset_basin; # to check which stations were extracted and if they are all in Rufiji basin
# Save the extracted dataset with all its attributes as a shapefile for later use
dir<-choose.dir(getwd())
setwd(dir)
writeOGR(points_subset_basin, dsn=getwd(), layer="Sediment.Stations_'basinName'", driver="ESRI Shapefile", overwrite_layer=T); # replace _'basinName' with basin name of interes, e.g. _Rufiji"
# Plot to see if the stations are located correctly within Rufiji basin
plot(Rufiji)
plot(points_subset,add=T,col="red")
text(points_subset, label="ID", cex=0.7); # add station IDs to the points in the plot
# Save stations data for the country and the basin of interest. Lat and lon values and sediment yield will be used in Section 6 in the code.
TZA_stations_data<-head(points_subset_country)
Rufiji_stations_data<-head(points_subset_basin)


# ------------------------------------------------------------------------------
# 5. AGGREGATE(fine to coarser)/DISAGGREGATE (coarse to finer) ALL INPUT DATA TO GRID RESOLUTION AND CLIP ('MASK') THE DIS(AGGREGATED) DATA TO THE BASIN OUTLINE EXTENT AND SHAPE

# First do all data reformatting, generation of input layers to be used e.g. rainfall; evapotranspiration; human, animal and irrigation water needs; fertilizer application & runoff; sediment load; E.coli load at country level.
# Then apply water supply (available: Yield-Need), water quality and security models at the basin level by clipping (masking) the country level results to fit the basin extent and shape.

# Clipping of country level input data involves the following:
# 1) First crop the country raster grid to the extent of basin/subbasin  using the applicable shapefile (e.g. Rufiji)
# 2) Then mask the object to fit the extent and shape of basin/subbasin shape file

# WATER YIELD:
# Daily precipitation values: 
# Assume all applicable (365/366) daily files are stored in the applicable year folder
# Sum to yearly totals for year(s) of interest. The year 2013 is used as an example below
dir<-choose.dir(getwd()); # select data folder for the year of interest and with only the daily files for that year in geotif format
setwd(dir)
# Create a stack of the daily files provided they all have the same extent and resolution
list<-list.files(pattern="*.tif")
rain_'year'<-stack(list); # replace _'year' with the applicable year, e.g. 2013
# Check that number of missing days are less than 10 (user needs to decide how to treat missing values, e.g. (1) interpolate; or (2) ignore based on a certain criterium(a)a e.g. if less than 10 days of data (i.e. worst case 10 consecutive days within the same month) no interpolation and accept data as is else leave a particular year out if interpolation is not an option
# Set min max values
setMinMax(rain_'year'); # this takes ~ 1 minute on Windows7 64-B machine and only necessary to do if min max values not displayed when typing rain_'year' in command line
# Sum the daily values
rain_'year'_sum_3km <- sum(rain_'year');# create object which is the sum per pixel of all the daily values in the stack of layers
# Save summed object as a raster in geotiff format for later use
# Note: the rainfall and ETP files are already in the aea CRS that will be used for all other input files
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(rain_'year'_sum_3km,filename="TZA_rain_'year'_aea_3km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif
# Read in .tif file with summed values and reproject and disaggregate to fit country grid in aea CRS and ~1000x1000m resolution in case the object is not in memory anymore
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
rain_'year'_sum_3km<-raster("TZA_rain_'year'_aea_3km.tif"); # replace 'year' with actual year, e.g. 2013 ELSE work with the rain_'year'_sum_3km object that is still in memory

# Note: The method below was not used for files generated and available in the intermediate results folder on google drive; see 'Alternative method'
# Disaggregate the summed yearly rainfall object to the same resolution as the country raster grid file (~1000x1000m resolution)
rain_'year'_disaggr<-disaggregate(rain_'year'_sum_3km,fact=c(round(dim(r)[1]/dim(rain_'year'_sum_3km)[1]),round(dim(r)[2]/dim(rain_'year'_sum_3km)[2])), method='ngb'))
# Then resample to match the spatial characteristics of the country's ~1000x1000m resolution grid exactly using the 'ngb' method.
# Rainfall must NOT be interpolated using bilinear because the same values in the original coarse resolution file need to be transferred to the individual 1km cells that overlap with the 3km grid.
# 'ngb' is used for categorical data. Rainfall and ET data are treated as 'categorical' in order to avoid an increase in cell values when sampling from coarse to finer resolution.
rain_'year'_sum_1km=resample(rain_'year'_disaggr, r,  method="ngb"); # 'different methods were not tested; 'bilinear' is the default and must be replaced with 'ngb' for 'categorical' data, the raster object will have same CRS (aea) as the country raster grid
# Note the above can be achieved by using raster::projectRaster() (Alternative method below) using the country raster grid (r) for the extent, projection and resolution changes. 'Bilinear' method is used the default and MUST BE replaced with 'ngb' for 'categorical' data.
# Save summed object as a raster in geotiff format for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(rain_'year'_sum_1km,filename="TZA_rain_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif

#***************************
# Alternative approach (this approach was used for the files created in the intermediate results folder on the google drive)
# Project summed per pixel and disaggregated raster object to the same crs as the country raster grid with a ~1000x1000m resolution
rain_'year'_sum_1km <- projectRaster(from=rain_'year'_sum_3km,to=r,method='ngb'); # project to country 1000x1000m grid in albers equal area, replace _'year' with the applicable year.
# This methods includes disaggregating to a finer scale and resampling using 'bilinear' method as default. Therefore replace bilinear method with 'ngb'
# Note: 'bilinear' takes about 65 minutes compared to a few seconds when using 'ngb' on Windows 7 64-B machine
# By projecting to the country raster grid it assures that the resulting Raster object lines up with other Raster objects (i.e. input data sets eg ET in case these were not generated in the same manner) also projected to fit the country raster extent.
# Save summed object as a raster in geotiff format for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(rain_'year'sum_1km,filename="'countryName'_rain_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif and 'countryName' with specific country, e.g TZA
# Note: Check to see that the 3km and 1km resolution rainfall files have the same mean or overall values by either:
# (1) calculate at the country scale the mean rainfall value per year [mm/year] for the 3km versus 1km resolution layers and compare 
cellStats(rain_'year'_sum_3km,mean)
CellStats(rain_'year'_sum_1km,mean); # should be close to the 3km mean cell value
## (2) OR check the min-max values for the 3km and 1km resolution files
rain_'year'_sum_3km; # summary with min, max values, resolution and CRS
rain_'year'_sum_1km; # summary with min, max values, resolution and CRS

#***************************

# Do the same for reference evapotranspiration (ETp) using the 'Alternative approach'
# ETp: Sum the daily reference evapotranspiration (ETp) values to yearly totals and do for yearly folder(s) of interest.
dir<-choose.dir(); # select data folder for the year of interest and with only the daily files for that year in geotif format
setwd(dir)
# Create a stack of the daily files provided they all have the same extent and resolution
list<-list.files(pattern="*.tif")
ETp_'year'<-stack(list); # replace _'year' with the applicable year, e.g. 2013
# Set min max values IF not displayed when typing ETp_'year'
setMinMax(ETp_'year')
# Sum the daily values
ETp_'year'_sum_3km <- sum(ETp_'year');# create object which is the sum per pixel of all the daily values in the stack of layers
# Save summed object as a raster in geotiff format for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(ETp_'year'_sum_3km,filename="'countryName'_ETp_'year'_Wm2_aea_3km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. ETp_2013.tif and 'countryName' with specific country, e.g TZA
# Read in .tif file with summed values and reproject and disaggregate to fit country grid in aea CRS and ~1000x1000m resolution in case the object is not in memory anymore
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
ETp_'year'_sum_3km<-raster("countryName'_ETp_'year'_Wm2_aea_3km.tif"); # replace 'year' with actual year, e.g. 2013 ELSE work with the ETp_'year'_sum object that is still in memory
# Reproject, disaggregate and resample ETp using 'alternative approach'
ETp_'year'_sum_1km <- projectRaster(from=ETp_'year'_sum_3km, to=r, method='ngb'); # resample the data file to fit the country raster in crs.aea and ~1000m resolution; use method='ngb'
# ETp values are reported in [W/m2] and need to be converted into [mm/day] and then per year for each pixel.
# Only convert the summed per year ETp raster files by dividing the summed [W/m2] by 28.4. This value is based on:
# the amount of energy to evaporate one unit weight of water= 2 454 000 [J/kg] ; avg. water density (rho) of 999.9 [kg/m3];number of seconds in a day = 60*60*24 and in a year = 60*60*24*365.25; 1mm=0.001m 
# Convert Units using ET [mm/day] = (ET[W m-2=j.s-1.m-2] / ?? [j.kg-1])/ rho [kg.m-3] ? 60 ? 60 ? 24 ? 365.25  ? sec.minute-1 ? minute.hour-1 ? hours.day-1 ? days.year-1)
ETp_'year'_mm_sum_1km<-(ETp_'year'_sum_1km /28.9); # convert summed [W/m2.year-1] to [mm/year]
# Save summed object in aea CRS and ~1000m resolution and with units converted to [mm/year] as a raster in geotiff format for later use. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(ETp_'year'_mm_sum_1km,filename="'countryName'_ETp_'year'_mm_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. 2013 and 'countryName' with for example TZA

# Crop and mask the daily summed reprojected with finer resolution  rainfall and ETp raster objects to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution rainfall and ETp files both in [mm/year] to the basin extent
rain_'year'_Rufiji <- crop(rain_'year'_sum_1km, extent(Rufiji.subbasins_crop)); # replace 'year' with applicable year, e.g. 2013
ETp_'year'_Rufiji<-crop(ETp_'year'_mm_sum_1km,extent(Rufiji.subbasins_crop)); # replace 'year' with applicable year, e.g. 2013
# Mask the cropped object with the basin's outline and shape
Rufiji_rain_'year'_masked <- mask(rain_'year'_Rufiji, Rufiji.subbasins_crop); # replace 'year' with applicable year, e.g. 2013
Rufiji_ETp_'year'_masked <- mask(ETp_'year'_Rufiji, Rufiji.subbasins_crop); # replace 'year' with applicable year, e.g. 2013
# Save cropped and masked objects in aea CRS and ~1000m resolution for the Rufiji basin in geotiff format for later use. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_rain_'year'_masked,filename="Rufiji_rain_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif
writeRaster(Rufiji_ETp_'year'_masked,filename="Rufiji_ETp_'year'_mm_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif

#Check that it worked, i.e. that the clipped rain and ETp files fit the basin extent and shape; it wont be an exact match as can be seen when using raster::zoom()
plot(Rufiji_rain_'year'_masked)
plot(Rufiji.subbasins_crop, add=TRUE, lwd=2)
dev.off();# to close the graphics window
# Can also use extent() to see the size of the difference
extent(Rufiji.subbasins_crop)
extent(Rufiji_rain_'year'_masked)
# ditto for ETp

# Note: WATER SUPPLY CALCULATIONS PER BASIN ARE DONE IN SECTION 6.

# WATER NEEDS:
# HUMAN WATER NEED
# Read in the human count per pixel per year data
dir<-choose.dir(); # select population data folder
setwd(dir)
list.files(); # to have the names of the different files (this and the following steps could be automated to do for each individual file but is done here for the year 2013)
TZA_'year'_pop<-raster(choose.files()); # read in the pop count data for 2013; replace 'year' with 2013 and 'choose"lspop2013.tif" for example
TZA_'year'_pop; # to see file info.
# Set min and max values if necessary
# Note: the population files show inconsistent min and max values when looking at the summaries of the objects used for reading in the files. 
setMinMax(TZA_'year'_pop)
TZA_'year'_pop; # see if the min and max values are now between 0 and a 'realistic' large value
# Note: Some pixels have >70 000 person/pixel (on the coast) - this was not validated with other data sources
# Calculate mean number of people/pixel excluding the zero values, i.e. only the mean of pixels with people (>=1 person). Also used to interpolate the data when resampling.
TZA_'year'_pop[TZA_'year'_pop==0]<-NA
TZA_mean_pop<-ceiling(cellStats(TZA_'year'_pop,'mean')); # rounded up (to integer) e.g. mean of 50.28 persons/populated pixel will be rounded up to 51 persons per populated pixel for the country. 
# This value is used to calculate the mean possible E.coli load that reaches the water courses in a year (See Section 6) using the results from the Pegram (2001) study (See Section 4).
# The Pegram (2001) study does not take into account the possible fluctuations in the number of persons/unit area or fluctuations over time.

# Resample the population raster file to fit the extent, resolution and projection of the country grid file.
# Use raster::projectRaster() as the population and country grid are already in the same projection (aea CRS) 
TZA_pop_'year'_resample <- projectRaster(from=TZA_2013_pop,to=r, method='bilinear'); # this uses the default method of 'bilinear'OR as an alternative the following can be run

#*****************************
# Alternative approach (Not used to generate files in intermediate results folder on the google drive)
# > TZA_pop_'year'_resample<- raster::resample(TZA_2013_pop,r, method='bilinear');# Alternative, use 'raster::resample, indicate that 'resample'function of the "raster" package is used
# Check to see if projection, number of columns and rows, resolution and min and max values are the same for the 2 methods (resample vs reproject)
# Note this is most likely introducing errors: see the comments on Landscan project (Section 4 : HUMAN WATER NEEDS).
# Check: max value of the original population raster object (x) is not the same as the max value of the resampled raster object (x) when using cellStats(x,max)
# Based on the decision and approach to do all calculations on a ~1km grid, errors are unfortunately introduced as the code below does not compensate for the shifting and resampling errors of the data
#*****************************

# Save object as a raster in geotiff format and crs.aea projection for later use; the clipping to the basin outline and shape only done in Section 6 after human water need calculations are done
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(TZA_pop_'year'_resample,filename="TZA_pop_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif

# Crop and mask the country 1km resolution population raster object raster objects to fit the extent and shape of the basin outline, e.g. Rufiji
Rufiji_pop_'year' <- crop(TZA_pop_'year'_resample, extent(Rufiji.subbasins_crop)); # replace 'year' with applicable year, e.g. 2013
# Mask the cropped object with the basin's outline and shape
Rufiji_pop_'year'_masked <- mask(Rufiji_pop_'year', Rufiji.subbasins_crop); # replace 'year' with applicable year, e.g. 2013
# Save cropped and masked object in aea CRS and ~1000m resolution for the Rufiji basin in geotiff format for later use. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_pop_'year'_masked,filename="Rufiji_pop_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; replace 'year' with applicable year, e.g. Rain_2013.tif

# HUMAN WATER NEED AT THE COUNTRY SCALE
# Calculate the total human water need per pixel per year [m3/year per pixel]
TZA_humanH2O_'year'<-TZA_pop_'year'_resample*35.8*365.25/1000;# [m3/year] assume minimum of 20 liter/person per day (WHO guidelines) BUT used 35.8 liters/person per day based on published national stats for TZA for 2002/2004; convert liters to m3 by dividing by 1000 [1000liter/m3]; 365.25 days in a year
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(TZA_humanH2O_'year',filename="'countryName'_humanH2O_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # replace 'year', e.g. 2013 and 'countryName' eg TZA

# HUMAN WATER NEED AT THE BASIN SCALE
# Crop and mask the human yearly water need object to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution human water need [m3/year per pixel] to the basin extent
humanH2O_'year'_Rufiji <- crop('countryName'_humanH2O_'year', extent(Rufiji.subbasins_crop)); # replace 'year' with applicable year, e.g. 2013 and 'countryName' with TZA
# Mask the cropped object with the basin's outline and shape
Rufiji_humanH2O_'year'_masked <- mask(humanH2O_'year'_Rufiji, Rufiji.subbasins_crop); # replace 'year' with applicable year, e.g. 2013
#Check that it worked, i.e. that the clipped human water needs file fits the basin extent and shape; it wont be an exact match as can be seen when using raster::zoom()
plot(Rufiji_humanH2O_'year'_masked)
plot(Rufiji.subbasins_crop, add=TRUE, lwd=2)
dev.off();# to close the graphics window
# Can also use extent() to see the size of the difference
extent(Rufiji.subbasins_crop)
extent(Rufiji_humanH2O_'year'_masked)
# Calculate total yearly Human water need demand in the basin
Rufiji_'year'_human water demand<-cellStats(Rufiji_humanH2O_'year'_masked,sum); # sum the pixel human water demands in the basin [m3/year]
# Save raster object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_humanH2O_'year'_masked ,filename="Rufiji_humanH2O_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE)

# Repeat for all years and basins of interest

# ANIMAL WATER NEEDS
# Read in the animal density per pixel per year data
cattle<-raster(choose.files()); # file with global data
sheep<-raster(choose.files()); # file with global data
goats<-raster(choose.files()); # file with global data
# set Min and Max values
setMinMax(cattle)
setMinMax(sheep)
setMinMax(goats)
# Project raster objects to albers equal area CRS in order to crop the global data set to fit the country grid extent
# Code between ****** can not be run on Windows7 64-B machine; >8GB memory problem; only included for completeness

#****************************
cattle_equal<-projectRaster(cattle,crs=crs.aea)
sheep_equal<-projectRaster(sheep,crs=crs.aea)
goats_equal<-projectRaster(goats,crs=crs.aea)
# Crop global data sets to fit country grid
# First determine the TZA grid extent
# ANIMAL WATER NEED AT COUNTRY SCALE
TZA_ext<-extent(r)
# Crop global raster object in aea CRS to fit country grid extent
TZA_cattle<-crop(cattle_equal,TZA_ext,snap='near')
TZA_sheep<-crop(sheep_equal,TZA_ext,snap='near')
TZA_goats<-crop(goats_equal,TZA_ext,snap='near')
# Note: Not necessary to 'mask' country objects with country grid in order to fit country shape as the grid is rectangular
# Save object as a raster in geotiff format and crs.aea projection for later use; the clipping to the basin outline and shape only done in Section 6
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(TZA_cattle,filename="TZA_cattle_aea.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; 
writeRaster(TZA_sheep,filename="TZA_sheep_aea.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file; 
writeRaster(TZA_goats,filename="TZA_goats_aea.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file;

# Project raster object to the same crs as the country raster grid with a ~1000x~1000m resolution
cattle_resample <- projectRaster(from=TZA_cattle,to=r, method='ngb'); # project to country 1000x1000m grid in albers equal area using the 'ngb' interpolation method 
sheep_resample <- projectRaster(from=TZA_sheep,to=r); # project to country 1000x1000m grid in albers equal area using the default 'bilinear' interpolation method as animals are expressed as # animals per total cell size and not # animals/unit area eg ha
goats_resample <- projectRaster(from=TZA_goats,to=r); # project to country 1000x1000m grid in albers equal area using the default 'bilinear' interpolation method as animals are expressed as # animals per total cell size and not # animals/unit area eg ha
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(cattle_resample,filename="TZA_cattle_aea_1km.tif",format="GTiff", overwrite=TRUE); # do the same for goats and sheep
writeRaster(sheep_resample,filename="TZA_sheep_aea_1km.tif",format="GTiff", overwrite=TRUE)
writeRaster(goats_resample,filename="TZA_goats_aea_1km.tif",format="GTiff", overwrite=TRUE)
#**************************

# ANIMAL WATER NEEDS: Alternative approach used to create files in intermediate results folder on google drive:

# ANIMAL WATER NEEDS AT THE COUNTRY SCALE
dir<-choose.dir(); # select folder with the country shapefile
setwd(dir)
TZA<-readOGR("Tanz.shp",layer="Tanz")
# First check the crs of each animal raster object
crs(cattle)
crs(sheep)
crs(goats)
# Then set country outline CRS to be the same as the CRS of the very large animal raster objects; they are all in the same projection BUT in case not 
proj.string<-crs(cattle)
# > proj.string<-crs(sheep); # only run if crs differs from cattle CRS
# > proj.string<-crs(goats); # only run if crs differs from cattle CRS
# Then reproject using spTransform()::sp the spatial polygons data object (country shapefile) to the same CRS as the cattle object for example.
# The country shape file is smaller and therefore easier to reproject than the other way round
TZA_reprojected<-spTransform(TZA,CRS=proj.string)
# Plot country shapefile on top of the graph of the global cattle object to check that it is correctly located
# Not run
# > plot(cattle)
# > plot(TZA_crs.cattle,add=T,col="red")
# Use extent of TZA reprojected spatial polygons data frame object to crop the global cattle raster file
TZA_ext<-extent(TZA_reprojected)
# Crop the global raster file to fit the country shapefile that now has the same CRS as the global animal file
TZA_cattle<-crop(cattle,TZA_ext,snap='near')
# Project cropped animal raster objects to the same crs as the country raster grid with a ~1000x~1000m resolution
cattle_resample <- projectRaster(from=TZA_cattle,to=r,method='ngb'); # project to country 1000x1000m grid in albers equal area; use 'ngb' method as cattle numbers=counts (considered categorical and not continuous); if 'bilinear' is used negative non-integers will be produced
sheep_resample <- projectRaster(from=TZA_sheep,to=r,method='ngb'); # project to country 1000x1000m grid in albers equal area 
goats_resample <- projectRaster(from=TZA_goats,to=r,method='ngb'); # project to country 1000x1000m grid in albers equal area
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(cattle_resample,filename="TZA_cattle_aea_1km.tif",format="GTiff", overwrite=TRUE); # do the same for goats and sheep
writeRaster(sheep_resample,filename="TZA_sheep_aea_1km.tif",format="GTiff", overwrite=TRUE)
writeRaster(goats_resample,filename="TZA_goats_aea_1km.tif",format="GTiff", overwrite=TRUE)
cattle_H2Oneed<-cattle_resample*((50*365.25)/1000); # convert mean daily water intake [litres/day/animal] to yearly [litres/year/animal] and from litres to [m3] by dividing by 1000 [litres/m3]
sheep_H2Oneed<-sheep_resample*((50*365.25)/1000)
goats_H2Oneed<-goats_resample*((50*365.25)/1000)
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(cattle_H2Oneed,filename="TZA_cattle_H2O_aea_1km.tif",format="GTiff", overwrite=TRUE); # do the same for goats and sheep
writeRaster(sheep_H2Oneed,filename="TZA_sheep_H2O_aea_1km.tif",format="GTiff", overwrite=TRUE)
writeRaster(goats_H2Oneed,filename="TZA_goats_H2O_aea_1km.tif",format="GTiff", overwrite=TRUE)
# Note: The above only done for cattle for illustration purposes as animal water needs are considered insignificant in the VS-project.

#ANIMAL WATER NEEDS AT THE BASIN SCALE
# Crop and mask the animal yearly water need object to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution animal water need [m3/year per pixel] to the basin extent
cattle_H2Oneed <- crop(cattle_H2Oneed, extent(Rufiji.subbasins_crop))
sheep_H2Oneed <- crop(sheep_H2Oneed, extent(Rufiji.subbasins_crop))
goats_H2Oneed <- crop(goats_H2Oneed, extent(Rufiji.subbasins_crop))
# Mask the cropped object with the basin's outline and shape
Rufiji_cattle_H2O_masked <- mask(cattle_H2Oneed, Rufiji.subbasins_crop)
Rufiji_sheep_H2O_masked <- mask(sheep_H2Oneed, Rufiji.subbasins_crop)
Rufiji_goats_H2O_masked <- mask(goats_H2Oneed, Rufiji.subbasins_crop)
#Check that it worked, i.e. that the clipped human water needs file fits the basin extent and shape; it wont be an exact match as can be seen when using raster::zoom()
plot(Rufiji_cattle_H2O_masked); # example
plot(Rufiji.subbasins_crop, add=TRUE, lwd=2)
dev.off();# to close the graphics window
# Can also use extent() to see the size of the difference
extent(Rufiji.subbasins_crop)
extent(Rufiji_cattle_H2O_masked)
# Calculate total yearly animal water need demand in the basin
Rufiji_cattle_H2O<-cellStats(Rufiji_cattle_H2O_masked,sum); # sum the pixel cattle water demands in the basin [m3/year]
Rufiji_sheep_H2O<-cellStats(Rufiji_sheep_H2O_masked,sum); # sum the pixel sheep water demands in the basin [m3/year]
Rufiji_goats_H2O<-cellStats(Rufiji_goats_H2O_masked,sum); # sum the pixel goats water demands in the basin [m3/year]
Rufiji_animal_H2O<Rufiji_cattle_H2O+Rufiji_sheep_H2O+Rufiji_goats_H2O; # [m3/year]
# Save raster objects as rasters in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_cattle_H2O_masked,filename="Rufiji_cattleH2O_aea_1km.tif",format="GTiff", overwrite=TRUE)
writeRaster(Rufiji_sheep_H2O_masked,filename="Rufiji_sheepH2O_aea_1km.tif",format="GTiff", overwrite=TRUE)
writeRaster(Rufiji_goats_H2O_masked,filename="Rufiji_goatsH2O_aea_1km.tif",format="GTiff", overwrite=TRUE)

#*******************************
# IRRIGATION WATER NEEDS
# Note: The code below is only to create a data set for illustration purposes till VS-generated data become available. It must be replaced with such country specific data.

# IRRIGATION WATER NEEDS AT THE COUNTRY SCALE (only the areas irrigated and not the irrigation water applied determined at country scale)
# Read in the area actually irrigated [ha] data per crop type (SPAM data set)
dir<-choose.dir(); # select Irrigation data folder
setwd(dir)
list.files()
# Outputs from the SPAM model is used. These include the physical areas planted and irrigated per crop type. Data need to be replaced with VS-data when these become available.
# Major  crops  grown  in Rufiji include  cassava,  maize,  rice, millet, sesame, coconut and cashew nuts. Only rice's irrigation amounts are calculated for testing purposes.
# Shape Files were downloaded per country from http://mapspam.info/data/ for the actual area planted with a specific crop (i.e. physical area) as the geotiff files did not contain values for the area planted variable.
# Read in the raster file in shapefile format, reproject and rasterize
rice<-readOGR("spam2005v2r0_physical_area_rice_i_TZA.shp",layer="spam2005v2r0_physical_area_rice_i_TZA"): # only rice's water needs ito irrigation was calculated
rice_equal<-spTransform(rice,crs.aea);# transform to have the albers equal projection
rice_raster<-rasterize(rice_equal, r,method='ngb');# rasterize the grid file in shapefile format to fit extent and resolution of the country grid and use nearest neighbour method as the values are in [ha] per cell
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(rice_raster,filename="TZA_rice_aea_1km.tif",format="GTiff", overwrite=TRUE)

# IRRIGATION WATER NEEDS AT THE BASIN SCALE
# Crop and mask the irrigation raster object to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution raster with physical area irrigated [ha] per pixel to the basin extent
Rufiji_rice_irr<- crop(rice_raster, extent(Rufiji.subbasins_crop)); # example
# Do the same for other crops
# Mask the cropped object with the basin's outline and shape
Rufiji_rice_irr_masked <- mask(Rufiji_rice_irr, Rufiji.subbasins_crop)
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_rice_irr_masked,filename="Rufiji_rice_irr_aea_1km.tif",format="GTiff", overwrite=TRUE)
# The irrigation amount required can be estimated (See Section 4). A yearly value of 1000m3/ha was assumed irrespective of crop type, weather conditions, irrigation practices.
Rufiji_rice_irrH2O<-Rufiji_rice_irr_masked*0.01*1000; # in [m3/km2]; convert [ha] to [km2] (x0.01, [km2/ha]); multiply with assumed 1000 [m3/ha].
# Sum physical area irrigated in the basin [ha],  and multiply with the irriga
Rufiji_rice_irrH2O_sum<-cellStats(Rufiji_rice_irrH2O,sum); # [m3/year] for the basin
# The suggested model (Section 4) should ideally be implemented but this was not done. The input files required are however available in the intermediate results folder to run the suggested model.

# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_rice_irr,filename="Rufiji_riceH2O_aea_1km.tif",format="GTiff", overwrite=TRUE)

# Do for all the crops and add the total [m3/year] irrigation need for the whole basin if the suggested model is used else
# multiply total area actually irrigated using the FAO AQUASTATS data set independent of crop type with 1000[m3/year]

# WATER QUALITY:
# SEDIMENT LOAD AT THE COUNTRY AND BASIN SCALE
# Only station data for select few stations within Tanzania are available in the literature. VS-generated sediment layer is required and should be used when this becomes available.
# The Stiegler Gorge measured mean annual sediment load per year is used to distribute the amount over the area of the catchment that contributes to the runoff at the station
# The amount is distributed according the relative contribution of each pixel to the total runoff calculated for the whole of the basin (See Section 6)
# If VS data set is in raster format the same approach should be followed, namely:
# project it to the country grid (in 1000m resolution), crop and mask it with the basin outline ELSE if in point format follow the approach suggested in Section 4

# NITROGEN LOAD AT THE COUNTRY SCALE
# Read in the Nitrogen application per pixel per year data
dir<-choose.dir(); # select fertiliser data folder
setwd(dir)
list.files()
nitrogen<-raster(choose.files()); # N applied [kg/ha] per grid cell data layer
setMinMax(nitrogen); # do only if necessary, i.e. no min max values are shown in summary of 'nitrogen'
# Project pixel raster object to the same crs as the country raster grid with a ~1000x~1000m resolution, disaggregate from 0.5 dec.degrees to 1000m resolution
nitrogen.equal <- projectRaster(from=nitrogen,to=r,method='ngb'); # project to country 1000x1000m grid in albers equal area, replace _'year' with the applicable year. This takes about 20 minutes on Windows 7 64-B machine
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(nitrogen.equal,filename="TZA_N applied_aea_1km.tif",format="GTiff", overwrite=TRUE)
# Calculate N-runoff per pixel
N_runoff<-nitrogen.equal*0.5; # 0.5 is the published ratio of assumed fertilizer-N to be washed-off (i.e. not used and/or oxidised); units [kg/ha]
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(N_runoff,filename="TZA_N runoff_aea_1km.tif",format="GTiff", overwrite=TRUE)
# Calculate the total N-washoff per cultivated pixel
total_N<-N_runoff*crops_raster;# calculate total N-washoff [kg/pixel]
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(total_N,filename="TZA_N runoff_kgPerPixel_aea_1km.tif",format="GTiff", overwrite=TRUE)

# NITROGEN LOAD AT THE BASIN SCALE
# Crop and mask the N-runoff raster object to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution raster with N-runoff per pixel to the basin extent
Rufiji_N_runoff_crop<- crop(total_N, extent(Rufiji.subbasins_crop))
# Mask the cropped object with the basin's outline and shape
Rufiji_N_runoff_mask <- mask(Rufiji_N_runoff_crop, Rufiji.subbasins_crop)
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_N_runoff_mask,filename="Rufiji_N runoff_kgPerPixelaea_1km.tif",format="GTiff", overwrite=TRUE)
# Sum total N-washoff for the basin (to be used for Water Quality index)
Rufiji_N_kg<-cellStats(Rufiji_N_runoff_mask,sum)

# E.COLI LOAD AT THE COUNTRY SCALE
# Population data are already in country grid format and projection (see Section 4)
'countryName'_ecoli_runoff_'year'<-'countryName'_pop_2013_resample*4.675*10^10*0.004456; # [number of persons per pixel] x [# bacteria cells/person per year per pixel] x [wash-off ratio] per pixel; replace 'year' and 'countryName'
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster('countryName'_ecoli_runoff_'year',filename="'countryName'_ecoli runoffPerPixel_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE)

# E.COLI LOAD AT THE BASIN SCALE
# Crop and mask the E.coli load raster object to fit the extent and shape of the basin outline, e.g. Rufiji
# If these raster objects are not in memory anymore read in the geotiff files using the same raster object names
# Use the Rufiji main basin outline shapefile (as a spatial polygons data frame) that contains the subbasins for the cropping and masking
# Crop the country 1km resolution raster with E.coli runoff load per pixel to the basin extent
Rufiji_Ecoli_runoff_crop<- crop('countryName'_ecoli_runoff_'year', extent(Rufiji.subbasins_crop)); # replace 'year' with applicable year
# Mask the cropped object with the basin's outline and shape
Rufiji_Ecoli_runoff_mask <- mask(Rufiji_Ecoli_runoff_crop, Rufiji.subbasins_crop)
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_Ecoli_runoff_mask,filename="Rufiji_ecoli_runoffPerPixel_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # replace 'year' with applicable year
# Sum total Ecoli-washoff for the basin (to be used for Water Quality index)
Rufiji_ecoli_cells<-cellStats(Rufiji_Ecoli_runoff_mask,sum)

# ------------------------------------------------------------------------------
# 6. THREAD OUTPUTS: INDICATOR CALCULATIONS PER PIXEL AND/OR FOR THE TOTAL BASIN AT THE BASIN SCALE

# 6.1 WATER YIELD OUTPUT AT THE BASIN SCALE
# (1) Calculate dryness index (sigma) per pixel per year
# (2) Calculate mean and total (sum) sigma for the total basin
# (3) calculate the sigma ratio per pixel as geotiff file
# (4) Calculate the Actual Evapo'transpi'ration at the basin scale per pixel
# (5) Calculate total runoff (R,[mm/year]) at the basin scale
# (6) Distribute total runoff per pixel based on the pixel sigma value ratios (Q, [m3/year])
# (7) Plot the water supply for the basin : ANNUAL WATER YIELD INDEX (THREAD OUTPUT)

# Note: ETp original files do not fit country extent and shape (file obtained from VS) accurately, e.g. the bottom part of the Rufiji basin entering the ocean is exxluded in these files.

# (1) CALCULATE DRYNESS INDEX PER PIXEL AT THE BASIN SCALE.
# sigma (dryness index, [ratio]) =  ETp (reference ET, [mm/year])/P (precipitation, [mm/year])
# For each summed yearly rainfall and ETp file clipped and resampled to 1000m resolution calculate the sigma. Repeat the following steps for each year.
# Read in the yearly summed daily values in raster format that are already in the correct projection and resampled to 1000m resolution and converted to correct units, i.e. [mm/day]
ETp<-raster(choose.files()); # For the basin of interest (e.g. Rufiji) select and load the yearly summed daily values per pixel, resampled and units converted ETp raster file, i.e. the file with the [mm/year] units for a specific year.
rain<-raster(choose.files()); # For the basin of interest (e.g. Rufiji) select and load the yearly summed daily values per pixel resampled rain raster file
# Calculate the dryness ratio per pixel for the basin.
sigma<-ETp/rain
# Save dryness per pixel object as a raster in geotiff format and crs.aea projection for re-use later. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(sigma,filename="Rufiji_sigmaPerPixel_'year'_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file in applicable folder; replace 'year' with applicable year, e.g. Rain_2010.tif

# (2) SUM and MEAN PIXEL SIGMA FOR THE WHOLE BASIN
sigma_mean.'year'<-cellStats(sigma,mean); # calculate a single dryness ratio per basin and keep in memory, i.e. mean dryness values of all pixels for the whole of the basin for each 'year' separately. Replace 'year' with the actual year e.g. 2013
sigma_sum.'year'<-cellStats(sigma,sum); # sum all the pxiel  per basin and keep in memory, i.e. sum dryness values of all pixels for the whole of the basin for each 'year' separately. Replace 'year' with the actual year e.g. 013

# (3) SIGMA RATIO PER PIXEL
sigma_ratio<-sigma/sigma_sum.'year'
# Save dryness (sigma) ratio per pixel object as a raster in geotiff format and crs.aea projection for re-use later. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(sigma_ratio,filename="Rufiji_sigma_ratioPerPixel_'year'_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file in applicable folder; replace 'year' with applicable year, e.g. Rain_2010.tif

# (4) CALCULATE THE ACTUAL ET (ETa) AT THE BASIN SCALE PER PIXEL
# Use the Fu (1981) formula, i.e. E/P = 1+sigma-(1+sigma^alpha)^(1/alpha) rewritten as E = P x (1+sigma-(1+sigma^alpha )^(1/alpha)) for the whole of the basin
# where E is the actual Evapotranspiration [mm/year]; P is the Precipitation [mm/year]; # sigma is the dryness ratio calculated as ETp/P at basin scale;and
# alpha = 2.8 as this closely approximates the original Budyko formulation. Alpha is typically in the range 2.0 to 3.2 (Teng et al 2013) and assumed to be similar for African systems.
# It should be estimated wherever possible from a prior year's calculated runoff data in relation to the annual accumulated gauge flow for that year; and then
# interpolated to adjacent ungauged catchments. IF Not possible assume a value for alpha of 2.8. Not possible in VS project-need input data
# Note: The Fu equation is a generalized form of the original Budyko equation, namely the ratio (E/P) = {sigma ? tanh(1/sigma) ? [1-exp(-sigma)]}^(1/2)
# Set alpha 
alpha = 2.8; # default value BUT needs to be tweaked (see below: for Rufiji it was adjusted to be 1.6). See Workflow document in Word how to do the tweeking provided data are available : currently not possible as part of VS and alternative data was used
# Read in the yearly files with the summed daily values for P and ETp IF not still in memory.These files are already in the reprojected, resampled, reformatted ito units and clipped to basin extent format.
# > rain<-raster(choose.files()); # select and load the yearly file for a specific year that contains the summed pixel values, reprojected, clipped to basin outline, and projected, resampled to 1000m resolution
# > ETp<-raster(choose.files()); 

# Calculate the basin summed rainfall values per year. Repeat for every year if necessary.
rain.'year'<-cellStats(rain,sum); # calculate a single rainfall value for the whole of the basin and keep in memory, i.e. sum the yearly value per pixel for all the pixels that cover the basin.
# replace .'year' with the year, e.g. rain.2013.
# Calculate the basin pixel mean rainfall values per year. Repeat for every year if necessary.
rain.'year'.mean<-cellStats(rain,mean);
# Calculate ETa (actual evapotranspiration) per pixel for the whole of the basin per year.
ETa.'year'<-rain*(1+sigma-(1+sigma^alpha)^(1/alpha)); # calculate actual evapotranspiration (ETa) per pixel per year using the per pixel sigma values.
# Calculate the basin pixel mean ETa values per year. Repeat for every year if necessary.
ETa.'year'.mean<-cellStats(ETa.'year',mean);

# Save ETa per pixel object as a raster in geotiff format and crs.aea projection for re-use later. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(ETa.'year',filename="Rufiji_ETa_mm_PerPixel_'year'_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file in applicable folder; replace 'year' with applicable year, e.g. Rain_2010.tif

# (5) CALCULATE BASIN SCALE TOTAL YEARLY RUNOFF (R) [mm/year] AND THEN FLOW (Q) [m3/year] 
# R[m/year] = (P (rain,[mm/year]) - E (ETa,[mm/year]))/1000 # /1000 to convert mm to m using the mean pixel rainfall and ETa values
# Q (flow) is necessary to calculate for validation purposes as well as to work out pollutant (sediment; N; E.coli) concentrations at the basin scale
# First calculate the mean pixel basin runoff (R, [m/year]) 
R.mean<-(rain.'year'.mean-ETa.'year'.mean)/1000; # convert [mm] to [m] by /1000 replace 'year' with applicable year , e.g. 2013
# Then calculate the equivalent flow per year at the basin exit point [m3/year] using the total basin/catchment surface area (A, [m2]) 
# Q [m3/year] = R [m/year] x A (catchment area, [m2])
# Requires to first calculate the total area of the basin/subbasin, e.g. for Rufiji [m2]
A<-gArea(Rufiji.subbasins_crop); # [m2] the value is 175 997.4 [km2] which is < 177 429 (wikipedia) or 183 791 [km2] (from www.maji.go.tz/?q=en/content/rufiji-basin) or 180 000 [km2] (van Maercke et al, 2014 Sediment yield article)
Q.basinTotal_m3<-R.mean*A; # flow [m3/year]
Q.basinTotal_km3<-Q.basinTotal/10^9; # convert [m3] to [km3]
# Note: This Q value is compared with the measured Q value and the alpha value to calculate the ETa (actual ET) is adjusted to fit published measured Q.
# Note: Caveats: 
# (1) Only flow data for 1958-1978 (SAGE data set) is available, i.e. totally out of date. The SAGE data set is mean flow rates [m3/s] per month.
# Note: This is first converted to total mean monthly flow volume (Qmeasured[m3/s]*60[s/minute]*60[minutes/hour]*24[hours/day]*days per month to get a mean monthly Q [m3/month])
# Note: Divided by 10^9 to convert to [km3/month], then summed to get a mean total yearly flow volume [km3/year]
# From the SAGE data set: Stieglers Gorge: 24.89 [km3/year] based on the 1958-1978 data.
# (2) It is assumed that rainfall and ET values are correct, correctly converted and resampled and that it is ONLY the alpha value that contributes to over/under estimates of the ET actual values
# Based on the above, alpha was adjusted to be 1.6 to produce a total Q [km3/year] for 2013 for Rufiji of 24.12 [km3/year]. This alpha value is lower than the accepted range (see Workhlow thread word document)

# (6) DISTRIBUTE RUNOFF (R) FOR THE BASIN TO A PER PIXEL VALUE AT THE BASIN SCALE (the amount used for agric, human needs are not included in this R amount)
# Assumption: the per pixel dryness (sigma) value can be used to distribute the total Q
runoffPerPixel<-Q.basinTotal*sigma_ratio; # runoff [mm/year] pixel
# Output: Annual Water Supply
plot(runoffPerPixel)
# Save Runoff [m3/year] per pixel object as a raster in geotiff format and crs.aea projection for re-use later. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(runoffPerPixel,filename="Rufiji_runoff_PerPixel_'year'_1km.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file in applicable folder; replace 'year' with applicable year, e.g. Rain_2010.tif
# Plot water yield
plot(runoffPerPixel,main="Rufiji Total water yield per pixel for 2013 [m3/year]"); # replace year with the applicable year

# Do for every year/years of interest.

# 6.2 CALCULATE TOTAL WATER NEED AT THE BASIN SCALE AND PER PIXEL
# 6.2.1 Output: Total water need
H2Oneed_'year'<-Rufiji_cattleH2O_masked+Rufiji_humanH2O_'year'_masked+irr_rice_masked
# Save total water need [m3/year] per pixel object as a raster in geotiff format and crs.aea projection for re-use later. Repeat for each year.
dir<-choose.dir(); # select the intermediate results folder
setwd(dir)
writeRaster(H2Oneed_'year',filename="Rufiji_TotalH2O need_m3Peryear_aea_1km_2013.tif",format="GTiff", overwrite=TRUE); # save object as geotiff file in applicable folder; replace 'year' with applicable year, e.g. Rain_2010.tif
# plot to check
plot(H2Oneed_'year',main="Rufiji total water need for 2013 [m3/year]")
# Calculate total water need for the whole basin
H2Oneed.basinTotal_km3<-cellStats(H2Oneed_'year',sum)/10^9; # sum the total water need based on irrigation, human and animal water use/needs per pixel to calculate an estimate for the whole basin in [km3/year] (divide by 10^9 to convert m3 to km3)
# 6.2.2 Output: Total water difference per pixel based on difference between the calculated Runoff per pixel based on the sigma ratio per pixel and water need/used
# Two calculations are done:
# (1) the deficit/surplus water available at the bottom of the basin (one estimate value per year)
# (2) the deficit/surplus water available per pixel by deducting the the amount of water used for irrigation, animals and humans from the total water yield per pixel
# Note: The latter does not take into account pixel transfers, i.e. people located within a specific pixel do in reality access the surplus water in neighbouring pixels

# (1) Deduct from the basin Q (km3/year) the total amount of water estimated to be 'used'/needed to determine the amount of water that flows out of the basin at the end point
Q_km3_available<-Q.basinTotal_km3-H2Oneed.basinTotal_km3
# (2) Calculate deficit/surplus per pixel
Runoff_m_perPixel<-rain-ETa

# 6.3 CALCULATE WATER QUALITY OUTPUT AT THE BASIN SCALE AND ITS FIT FOR USE (COMPARE CONCENTRATIONS WITH WHO GUIDELINES) 
# E.coli load
# Crop and mask the country level data to fit the basin extent and shape
# If not in memory anymore read in the calculated raster file for the country (Section 5) using the same object name
# Crop to basin extent
Rufiji_ecoli_load_'year'<-crop(ecoli_load_'year',extent(Rufiji.subbasins_crop)); # replace 'year' with the applicable year
# Mask to fit basin shape
Rufiji_ecoli_load_'year'_masked <- mask(Rufiji_ecoli_load_'year', Rufiji.subbasins_crop); # replace 'year' with the applicable year
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_ecoli_load_'year'_masked ,filename="Rufiji_ecoli_load_'year'_aea_1km.tif",format="GTiff", overwrite=TRUE); # replace 'year' with the applicable year

# Nitrogen load
# Crop and mask the country level data to fit the basin extent and shape
# If not in memory anymore read in the calculated raster file for the country (Section 5) using the same object name
# Crop to basin extent
Rufiji_N_load<-crop(N_runoff,extent(Rufiji.subbasins_crop))
# Mask to fit basin shape
Rufiji_N_load_masked <- mask(Rufiji_N_load, Rufiji.subbasins_crop)
# Save object as a raster in geotiff format and crs.aea projection for later use
dir<-choose.dir(getwd()); # select the intermediate results folder
setwd(dir)
writeRaster(Rufiji_N_load_masked ,filename="Rufiji_N_load_aea_1km.tif",format="GTiff", overwrite=TRUE)

# Sediment load
# Used a total basin estimate (Stiegler's Gorge (van Maercke et.al, 2014)) from the literature as no VS data available yet (See Section 7. Validation)

# WATER FIT FOR USE AT THE BASIN SCALE
# Use WHO guidelines for sediment, N, E.coli to determine if suitable or not
# Use Water yield [km3/year] to work out river load of pollutants
ecoli_'year'_conc<-ecoli_load/


# 7. VALIDATION OF RIVER FLOW AT A SPECIFIC MEASURING STATION WITHIN THE CATCHMENT: 
# Use RSAGA (requires that SAGA GIS must be installed in order for R to find the SAGA .cmd and methods library
# CALCULATE CATCHMENT AREA THAT CONTRIBUTES TO THE RUNOFF/DISCHARGE/SEDIMENT YIELD AT A SPECIFIC STATION LOCATION
# This is done to determine the total area estimated to have contributed to runoff/discharge/sediment yield measured at a point (station), in this case the Stiegler's Gorge station close to the river mouth.
# Long term mean based on historical data are available for Stiegler's Gorge for the yearly sediment yield and discharge data measured at the station.
# Data quality is unknown for the sediment yield and the discharge data is out of date.
# An estimated catchment area for Stiegler's Gorge is available from Vanmaercke M, Poesen J, Broeckx J, Nyssen J.(2014) Sediment Yield in Africa. Earth-Science Reviews 136:  350-368 (+ Appendix).DOI: 10.1016/j.earscirev.2014.06.004
# to compare estimated cathcment area for the catchment area they have calculated to be associated with Stiegler's Gorge station.
# Use VS DEM30.tif for countries, e.g. TZA
# Resampling 30m resolution file for the country to 1km resolution takes too long on Windows7 64-Bit machine and could not produce results
# SAGA-GIS software (V2.2.2) (GUI and/or command line version) must be installed for the RSAGA library to work. Download it from the sourgeforge link available http://www.saga-gis.org/en/index.html. 
# The code can be replicated for any station provided DEM data file is available and in surfer grid format (.sgrd) in order to run the RSAGA module.
# If file is in geotiff format eg country DEM 30m files, then open SAG GIS GUI and import the file uing the gdal tool and save it as a .sgrd with its associated .sprm (parameters) file. This should be able to be done from within the RSAGA module. 
# Install and load RSAGA after setting the cran mirror
# Function to install and load multiple R packages (Author: Steven Worthington: ipak.R function, 2013) as RSAGA and some of its dependencies have not yet been installed/loaded.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  #Check if packages are already installed
  #If not, install and then load into worksession
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
packages <- c("raster","maptools","rgeos","magrittr","RSAGA");#list of packages that are required
ipak(packages); # check that all packages are installed and loaded, i.e. "TRUE".
# Code adapted from Joel (jwtrubil@gmail.com),10/01/2015: Delineate a watershed with SAGA-GIS and R - Part 1&2 (http://www.headwateranalytics.com/blog/delineate-a-watershed-with-saga-gis-and-r4)
# Set up the environment where 'path' is where the 'saga.cmd' file is stored and 'modules' is the directory where the 'modules' folder of the installed SAGA GIS V2.2.2 is stored.
# On Windows machines: Make sure SAGA is installed in C:/Program Files otherwise RSAGA can not find the saga.cmd file or the modules even when setting the path and moduless path in the myenv object.
# Establish which SAGA version the RSAGA package links to:
rsaga.env()
# Note: The RSAGA library was developed to work with SAGA GIS V2.2.2 and not the latest version V2.2.4.
myenv = rsaga.env(workspace = getwd(), path = 'C:/Program Files/SAGA-GIS', modules = 'C:/Program Files/SAGA-GIS/modules', version = '2.2.2')
# The following data are needed:
# DEM in surfer grid (.sgrd), i.e. the SAGA raster format and the GPS coordinates of the station(s) of interest. The .sgrd file will always be accompanied by .sdat and .mgrd files. 
# Set the working directory
dir<-choose.dir(getwd()); # select the DEM folder in the inputs data folder
setwd(dir)
# Create a dem file using a specific method to fill the sinks in the TZA_dem.sgrd file. This filled dem file is to enable easier calculations of catchment area.
rsaga.fill.sinks('TZA_dem.sgrd', 'TZA_demfilled.sgrd', method = "xxl.wang.liu.2006", env = myenv); # TZA_dem.sgrd is an existing file and 'TZAdemfilled.sgrd' will be created.
# Create catchment grid from the dem file.
rsaga.topdown.processing('TZA_demfilled.sgrd', out.carea = 'catchment_area.sgrd')
# Make the GPS coordinates for the station(s) of interest into a spatial point(s) and project it/them to albers equal area.
crs.geo <-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs");#all inputs are first assigned a geographic coordinate system WGS84) before reprojection
crs.aea<-CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ");#albers equal area is the final projection
# Make the base data frame, x is longitude and y is latitude
Stiegler_stn <- data.frame(y = -7.7989, x = 37.18632)
# Turn data frame into a spatial object
coordinates(Stiegler_stn) <- ~ x + y
# Assign the geographic coordinate reference ystem (WGS84)
proj4string(Stiegler_stn) <- crs.geo
Stiegler_stn; # check that is was assigned a CRS
# Reproject to albers equal area
Stiegler_stn <- spTransform(Stiegler_stn, crs.aea)
Stiegler_stn; # check that is was reprojected
# Check that station is in the same projection as the DEM file and located correctly by plotting it using the raster package
fill_dem <- raster('TZA_demfilled.sdat')
proj4string(fill_dem) <- crs.aea
plot(fill_dem)
plot(Stiegler_stn, add=T); # this could not be checked
# Read in the catchment area grid object (i.e. "basinr_mask" saved as raster file). First convert the geotiff file to .srgd with associated 'Rufiji_catchment_area'.sdat file using SAGA GIS using the rgdal tools)
Stiegler_stn_area <- raster('Rufiji_catchment_area.sdat')
# Extract a window around around the gauge point, eg get the maximum value within 500 m of the gauge
buffer <- extract(Stiegler_stn_area, Stiegler_stn, buffer = 500, cellnumbers = T)[[1]] %>% as.data.frame
# Get location of the maximum catchment area on the grid, given as the id from the raster
snap_loc <- buffer$cell[which.max(buffer$value)]
# Get the xy coordinates at that max location, which is now going to be the location of the gauge.
snap_loc <- xyFromCell(Stiegler_stn_area, snap_loc)
# Use snapped point and filled DEM to com;pute uplose area using function in SAGA-GIS called by RSAGA
rsaga.geoprocessor(lib = 'ta_hydrology', 4,param = list(TARGET_PT_X = snap_loc[1,1],TARGET_PT_Y = snap_loc[1,2],ELEVATION = 'TZA_demfilled.sgrd',AREA = 'Stiegler_stn.sgrd',METHOD = 0))
# Make Steigle_stn.sgrd a polygon
rsaga.geoprocessor(lib = 'shapes_grid', 6,param = list(GRID = 'Stiegler_stn.sgrd',POLYGONS = 'Stiegler_stn_boundary.shp',CLASS_ALL = 0,CLASS_ID = 100,SPLIT = 0))
Stiegler_stn_basin<- readShapeSpatial('Stiegler_stn_boundary.shp', proj4string = crs.aea)
# Plot
plot(fill_dem)
plot(Stiegler_stn_basin, add = T)

# Note: If errors occur then the first troubleshooting step is to change the buffer size in the extract() function used above
                                
                                
                                
                            
                                
                                