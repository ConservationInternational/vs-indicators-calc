################################# CI   VITAL SIGNS PROCESSING  #######################################
##################################### Woodfuel indices   #############################################
#This work is part of the Vital Signs project started at Conservation International.
#This script is a modified workflow for the woodfuel thread. It is loosely based on code developed by CSIR.
#The inputs and outputs are clearly separated and uses standard tools for spatial processing (resampling etc.)
#Base code was contributed by mulitple persons at CSIR South Africa.
#
#AUTHORS: Marc Piennar, Marna van der Werve, Benoit Parmentier                                                                   #
#DATE CREATED: 09/12/2015 
#DATE MODIFIED: 11/02/2015
#
#COMMENTS: 
# - Code from the initial CSIR work relevant to the indices was extracted.
# - Java resampling code is not used here, instead standard tools from raster/rgdal are used.
#TO DO:
# - Use the same logic to extract code for the water table and climate forcing indices.
# - Identify inputs for water table and climate forcing in a clear manner.
# - Identify inputs that are randomly generated from actual data.
# - Introduce more functions to reduce the inputs and have only parameters defined in the main script and short
# - Move all libraries call and sourcing to the main script.
#
#PROJECT: CI VS, CSIR, EI 
#
##################################################################################################
#
###Loading R library and packages

library(raster)                             # loading the raster package
library(gtools)                             # loading R helper programming tools/functions
library(sp)                                 # spatial objects in R
library(gplots)                             # plotting functions such as plotCI
library(rgdal)                              # gdal driver for R
library(RColorBrewer)                       # color scheme, palettes used for plotting
library(gdata)                              # read different format (including .xlsx)
library(plotrix)                            # plot options and functions 
library(rasterVis)                          # raster visualization
library(colorRamps)                         # contains matlab.like palette
library(zoo)                                # time series objects and methods
library(xts)                                # extension of time series objects
library(gfcanalysis)                        # Hansen dataset...
library(geosphere)                          # curved surface and geodetic trigonometric

### INPUTS FOR WOODFUEL AS DEFINED FROM CSIR

#Variable	staging_protocol	file_name	Aux_file	raw_input
#eplotmass	/Users/private/Vital_signs/woodfuel/Tanzania/WF12/	eplot_v20.tif	Tanzania_eplots_summary.txt	eplot_v20.csv
#treeheight	/Users/private/Vital_signs/woodfuel/Tanzania/WF13/	treeheight.txt	NA	Simard_Pinto_3DGlobalVeg_L3C.tif
#treecover	/Users/private/Vital_signs/woodfuel/Tanzania/WF14/	treecover.txt	NA	treecover_raw.tif
#rainfall	/Users/private/Vital_signs/woodfuel/Tanzania/WF15/	rainfall2010.txt	NA	/2010/
#meanrainfall50year	/Users/private/Vital_signs/woodfuel/Tanzania/WF15/	meanrain.txt	NA	/worldclim/
#meanstemdiameter	/Users/private/Vital_signs/woodfuel/Tanzania/WF15/	meanstemdiameter2010.txt	NA	meanstemdiameter_raw2010.tif
#woodconsumption	/Users/private/Vital_signs/woodfuel/Tanzania/WF16/	woodfuel_cunsumption.txt	NA	NA
#population	/Users/private/Vital_signs/woodfuel/Tanzania/WF17/	population.txt	NA	population_raw.tif
#processedgrid_paraters	/Users/private/Vital_signs/woodfuel/Tanzania/processedgrid_paraters/	X.txt	X.txt	X.txt

### OUTPUTS: how many are created?

# Threads and indices...
# Index 1: Woodfuel
# Index 2: watertable
# Index 3: climateforcing

#######################

#CSIR: " The notes below accompany the R-code developed and tested by the CSIR.  
#      " Description, purpose, assumptions and where applicable,  alternative  
#      " approaches or steps to derive input data required are provided.

#################################################
###### Functions  used in the script  ##########

create_dir_fun <- function(outDir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

###################### INPUTS AND PARAMETERS #####################

########################## PART 1: Inputs arguments ###############################

#Step one: set the initial working directory and other directories, this will have to correspond to a directories on the implementing 
#computer. The following files should be put into these directores:
#load the functions script stored in this directory. 
#This script contains all the functions or model parameters necessary to solve the threads

in_dir <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs" #CI Amazon server
#in_dir <- "/home/bparmentier/google_drive/Google Drive/VS Indicators/External Datasets/Woodfuel_CSIR_temp" #Local laptop
#in_dir <- "/Users/private/Vital_signs" #in previous CSIR code
function_script1 <-"VS_processing_functions_11022015.R" #Script functions from Benoit Parmentier
function_script2<- "functions_v1.4.R" #Origininal CSIR developed functions script
in_dir_script <-"/home/benoit/scripts_vs_csir"  #Directory devoted to script on CI Amazon server
source(file.path(in_dir_script,function_script1))
source(file.path(in_dir_script,function_script2))

initial.directory <- in_dir #Varible Defined by CSIR in original script
#Sys.setenv(http_proxy="http://sanren-proxy.meraka.csir.co.za:3128") #? what is this input? Is this running on a separate server
#initial.directory="/Users/private/Vital_signs";
#source(scriptdirectory);#load functions

#AOIdirectory="/Users/private/Vital_signs/raw_data/shapefiles/tanzania";
AOIdirectory <- file.path(in_dir,"/raw_data/shapefiles/tanzania")
AOIfile="tanzania"; #processing region

#ref_fname <- "vs_tanzania_10_v2.tif"
out_suffix <-"_vs_tza_workflow_11022015" #output suffix used in the production of files and output dir
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" # This is EPSG4326, we are processing using geographic coordinates from WGS84

CRS_reg <- CRS_WGS84 #not in use yet, proj4 string for the region of interest (aoi/roi)

file_format <- ".tif" #raster format used ihe outputs
NA_value <- -9999 #No data value/flag value
NA_flag_val <- NA_value

CRS_reg <- CRS_WGS84 #not in use yet, proj4 string for the region of interest (aoi/roi)

setwd(in_dir) 
out_dir <- "/home/benoit/output_data_vs_csir" #base directory used to create the new output dir

create_out_dir_param <- TRUE #If True, a new output directory is created if none exists at the time of running

#Create output directory

if(create_out_dir_param==TRUE){  
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

#Input files and dir for woodfuel from CSIR

#infile_eplotmass <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF12/eplot_v20.csv" #Not processed at this stage.
infile_treeheight <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF13/Simard_Pinto_3DGlobalVeg_L3C.tif" #Height from Lidar derived global product, Simard paper
infile_treecover  <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF14/treecover_raw.tif" ##WF14 percent cover from Hansen 2000 layers.
infile_meanrainfall <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/test_meanrainfal.tif" #Mean
#infile_meanrainfall <- NULL #if null compute
infile_rainfall <- NULL #if null compute
infile_rainfall2010 <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF15/2010/"
infile_meanrainfall50year <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF15/worldclim/"
infile_meanstemdiameter <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF15/meanstemdiameter_raw2010.tif"

infile_woodconsumption <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF16/woodconsumption/"
infile_population <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF17/population_raw.tif"

########################## PART 2: PROCESS INITIAL DATA  ###############################

####################################################

#First get the AOI for each thread (returns a polygon representing the extent of the shapefile).
#here all calculations are done on 1km^2 grid representing the AOI and only at the very end can be clipped with different polygons or shapefiles.
#For these threads we are using Tanzania (the whole country) for all calculations.
reg_ref_spdf <- aoi(AOIdirectory,AOIfile) # this is a function...move loading out of it
#aoi <- create_polygon_from_extent(reg_ref_spdf,outDir=NULL,outSuffix=NULL)
ref_e <- extent(reg_ref_spdf) #get the extent
#reg_ref_rast
#create raster from extent in EPSG4326, may want to use Alberts Equal Area projection here instead?
## S4 method for signature 'missing'
res_deg <- 1/111 #about 111km for one degree at the equator
r_ref <- raster(ext=ref_e, resolution=res_deg, vals=NULL)
r_ref <- setValues(r_ref,rnorm(1:ncell(r_ref)))

#note that we could use the following proj.
#proj_str <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

######################################################
### Match to the study region and aggregate/resample if necessary...

treeheight <- raster(infile_treeheight)
treecover <- raster(infile_treecover)
r_rainfall2010 <- stack(mixedsort(list.files(infile_rainfall2010,full.names=T)))
r_meanrainfall50year <- stack(mixedsort(list.files(infile_meanrainfall50year,"prec_.*.bil$",full.names=T)))
r_population <- raster(infile_population)
#r_meanstemdiameter <- try(raster(infile_meanstemdiameter)) #not recognized as tif file, needs to be computed

#out_rast_name <- c("test.tif")
out_rast_name <- NULL
agg_param <- c(FALSE,33,"mean") #if FALSE not aggregation takes place

list_param_create_region <- list(list(infile_treeheight),r_ref, out_rast_name,agg_param,
                                 file_format,NA_flag_val,
                                 input_proj_str=NULL,out_suffix,out_dir)
names(list_param_create_region) <- c("raster_name","reg_ref_rast", "out_rast_name","agg_param",
                                 "file_format","NA_flag_val",
                                 "input_proj_str","out_suffix","out_dir")

#debug(create__m_raster_region)
treeheight_reg_raster_name <- create__m_raster_region(1,list_param=list_param_create_region)
plot(raster(treeheight_reg_raster_name))

agg_param <- c(TRUE,33,"mean")
list_param_create_region <- list(list(infile_treecover),r_ref, out_rast_name,agg_param,
                                 file_format,NA_flag_val,
                                 input_proj_str=NULL,out_suffix,out_dir)
names(list_param_create_region) <- c("raster_name","reg_ref_rast", "out_rast_name","agg_param",
                                     "file_format","NA_flag_val",
                                     "input_proj_str","out_suffix","out_dir")
#debug(create__m_raster_region)
treecover_reg_raster_name <- create__m_raster_region(1,list_param=list_param_create_region)
plot(raster(treecover_reg_raster_name))

########################## PART 3: RUN MODEL ###############################

#Run woodfuel model # what is the model?

#run models,make this a simple function
treeheight <- raster(treeheight_reg_raster_name)
treecover <- raster(treecover_reg_raster_name)
BCFbiome <- 23.50 #t biomass/ha. Note how was this number determined for Tanzania ! We need a reference for these parameters!!!
AGBwoody <- ((treeheight * (treecover/100)) * BCFbiome)  *100#t/km^2. WF21 

raster_name <- file.path(out_dir,paste("AGBwoody","_",out_suffix,".tif",sep=""))
writeRaster(AGBwoody, NAflag=NA_flag_val,filename=raster_name,overwrite=TRUE)  #save to disk

plot_to_file(raster_name) #quick test
plot(AGBwoody)

################################
#WF22 Tree Production
#we first need to derive max tree cover (using nature paper formula
#derive the maxtreecover from mean50rainfall (Sankaran et al, 2005 (nature) regression)

#meanrainfall <- calc(r_meanrainfall50year
if(is.null(infile_meanrainfall)){
  meanrainfall_world  <- calc(r_meanrainfall50year,fun=mean,na.rm=T,filename="test_meanrainfal.tif") #first crop?
}else{
  meanrainfall_world <- raster(infile_meanrainfall)
}

## Match to the study area...

agg_param <- c(FALSE,NULL,"mean")
#use r_ref as reference...
list_param_create_region <- list(list(infile_meanrainfall),
                                 r_ref, out_rast_name,agg_param,
                                 file_format,NA_flag_val,
                                 input_proj_str=NULL,out_suffix,out_dir)
names(list_param_create_region) <- c("raster_name",
                                     "reg_ref_rast", "out_rast_name","agg_param",
                                     "file_format","NA_flag_val",
                                     "input_proj_str","out_suffix","out_dir")
#debug(create__m_raster_region)
meanrainfall_raster_name <- create__m_raster_region(1,list_param=list_param_create_region)
meanrainfall <- raster(meanrainfall_raster_name)
plot(meanrainfall)

## Now compute the max treeccover from rainfall using the reference litterature

maxtreecover <- meanrainfall
#get rid of NA values
maxtreecover[is.na(maxtreecover)] <- 0
meanrainfall[is.na(meanrainfall)] <- 0
#set maxtreecover
maxtreecover[meanrainfall[] < 100] <- 1;
#mid range max treecover
midrange <- (0.1436*(maxtreecover[meanrainfall[] >= 100 & meanrainfall[] <= 650])-13.364)
maxtreecover[meanrainfall[] >= 100 & meanrainfall[] <= 650] = midrange
#max treecover (set to 80%)
maxtreecover[meanrainfall[] > 650] <- 80

raster_name <- file.path(out_dir,paste("maxtreecover","_",out_suffix,".tif",sep=""))
writeRaster(maxtreecover, NAflag=NA_flag_val,filename=raster_name,overwrite=TRUE)  #save to disk

plot_to_file(raster_name) #quick test
plot(maxtreecover)

################################
#calculate woodproduction
#summed eg daily rainfall to yearly rainfall)
#WF15 Rainfall 
#rainfall=checkandprocess_rainfall(rainfall,tableinputs[4,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);

## Process meanstemdiamter and rainfall data
#Need meanstemdiamter for this part and need to read eplot
# @TODO: this will be changed !!, fake data and should be set at the beginning of the script
meanstemdiameter <- maxtreecover*0.2
#rainfall ??
#meanrainfall <- calc(r_meanrainfall50year
if(is.null(infile_rainfall)){
  rainfall_world  <- calc(r_rainfall2010,fun=mean,na.rm=T,filename="test_mean_rainfall2010.tif",overwrite=T) #first crop?
  infile_rainfall <- "test_rainfall2010.tif"
  # @TODO: Combine these functions together
  rainfall_world_disagg3x3 <- disaggregate(rainfall_world, fact=c(3,3), method='bilinear',
                                           filename=infile_rainfall,overwrite=T)
}else{
  rainfall_world <- raster(infile_rainfall)
}

## Match to the study area...
agg_param <- c(FALSE,NULL,"mean")
#use r_ref as reference...
list_param_create_region <- list(list(infile_rainfall),
                                 r_ref, out_rast_name,agg_param,
                                 file_format,NA_flag_val,
                                 input_proj_str=NULL,out_suffix,out_dir)
names(list_param_create_region) <- c("raster_name",
                                     "reg_ref_rast", "out_rast_name","agg_param",
                                     "file_format","NA_flag_val",
                                     "input_proj_str","out_suffix","out_dir")
#debug(create__m_raster_region)
meanrainfall2010_raster_name <- create__m_raster_region(1,list_param=list_param_create_region)
rainfall <- raster(meanrainfall2010_raster_name)
plot(rainfall)

woodproduction <- ((AGBwoody)*(0.01+(1-treecover/maxtreecover)*(0.42*exp(-0.29*meanstemdiameter))*((1+0.26*(rainfall-meanrainfall)/maxtreecover))));
woodproduction[woodproduction < 0] <- 0
#woodproduction is in t/km^2
percentyield <- (woodproduction/AGBwoody*100)

raster_name <- file.path(out_dir,paste("woodproduction","_",out_suffix,".tif",sep=""))
writeRaster(woodproduction, NAflag=NA_flag_val,filename=raster_name,overwrite=TRUE)  #save to disk

plot_to_file(raster_name) #quick test
plot(woodproduction)

###############################################
#calculate woodconsumption: this is not finished and currently does not work
# @TODO: Gather needed inputs to calculate actual wood fuel results
infile_woodconsumption <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF16/woodconsumption/"
#Note that this folders does not exist at this stage!!
#woodconsumption=populateresamplegrid(resamplematrix,1,1);#WF16 Total tons/capita/year (2010 for now)
#this is currently based on random values!!

infile_population <- "/home/benoit/input_data_vs_csir/Woodfuel_CSIR_temp/Vital_signs/woodfuel/Tanzania/WF17/population_raw.tif"
population <- raster(infile_population)
woodconsumption <- raster(infile_woodconsumption)
#raster(list.files(infile_woodconsumption))

plot(r_population) 

#distribute wood consumption / capita using population
woodconsumption <- population*woodconsumption;#0.6 t/capita/km^2*population/km^2;
#bring in difference between wood purchased and sold when a value becomes available
#woodpurchasedsold=populateresamplegrid(resamplematrix,5000,5000);#number of tons/year a single value of exported wood + (other purposes eg construction) - wood bought (imported from outside country) chacoal production production and consumption not accounted for
#woodpurchasedsold=woodpurchasedsold[-nrow(woodpurchasedsold),-ncol(woodpurchasedsold)]
#Woodsecurity=(woodproduction +woodpurchasedsold) -(woodconsumption);

Woodsecurity <- woodproduction -(woodconsumption);#the index is only based on wood produced vs wood consumed
hist(Woodsecurity);
sum(Woodsecurity,na.rm=T);

plot(raster(Woodsecurity),zlim=c(-700,7000))#
setwd(initial.directory)

raster_name <- file.path(out_dir,paste("woodsecurity","_",out_suffix,".tif",sep=""))
writeRaster(woodsecurity, NAflag=NA_flag_val,filename=raster_name,overwrite=TRUE)  #save to disk

#################################################################
## End of woodfuel

########################## PART 4: CLIMATE FORCING ###############################

# #################################################################
# #Climate forcing
# 
# tableinputs=read.table(tablelocclimate,header=T,sep=",");
# tableinputs=as.data.frame(tableinputs);
# tableinputs=initiate_table(tableinputs);
# #Step 3. process the raw data or replace dummy variables with processed data 

# 
# ################## 09302015
# creategridparamters(tableinputs[length(tableinputs[,1]),]); # ERROR MESSAGE HERE
# 
# #resamplegrid=createresamplegrid(Tanzania,1000,90,180) #note we use 180 instead of -180 for r funtion
# #Create all required inputs to match the resample grid and populate with random numbers for now 
# #(representing the assumed ranges for each variable, we swap these out later if the actual data exists.)
# #The following variables are for woodfuel:
# AGBwoody=populateresamplegrid(resamplematrix,1,4000);# t/km^2 from VS woodfuel
# AGBwoodycarbon=populateresamplegrid(resamplematrix,1,2000);# t/km^2 from VS woodfuel
# soilcarbon1m=populateresamplegrid(resamplematrix,50,300);#t/km^2 to a depth of 1m (multiply code below by 100)
# soilcarbon30cm=populateresamplegrid(resamplematrix,50,300);#t/km^2 to a depth of 1m (multiply code below by 100)
# builtenvironcarbonstock=populateresamplegrid(resamplematrix,50,300);#default value from either landcover map or literature
# cultivatedarea=populateresamplegrid(resamplematrix,50,300);#proporton of area under cultivation per pixel. derive total carbon in 0-30cm horizon for pixel 
# carbonloss=populateresamplegrid(resamplematrix,50,300);
# #run model
# #get the above ground woody biomass
# #checkorprocess_grid is the most common processing function and can be used for a majority of inputs.
# #the parameters are:
# #1 grid - the empty grid to process
# #2 tableline e.g. woodfuelinputs[2,] represents the second row from the woodfuelinputs table and holds all the information to find raw inputs.
# #3 java (boolean) - tells the function to use java resampling (fast) or if set to F to use native R code.
# #4 AOI - a shape file representing the AOI
# #5 agg - used to determine whether the resampling into the 1km^2 grids should be aggregated/disaggregated 
# #(depending on the resolution of the raw data in relation to the resmaple grid - this is determine automatically
# #by the function) or if set to "false" will result in a direct fractional weighting. 
# #Note, that we use the terms "true" or "false" (for java purposes rather than TRUE or FALSE as in R.
# #6 agg2 - this is done prior to resmapling using the aggregate function to scale up finer resolution dataset (for speed).
# # if T, then aggamount and aggfunction will be used, else they are ignored and no aggregation is done prior to resampling.
# #7 aggamount - the agg amount (see point above)
# #8 the aggregation function ("mean" or "sum").
# #9 resolution - the resolution of the resample grid in meters
# # parseraster - only used in other functions (leave F) not in the checkorprocess_grid function
# #WF13 Tree height 
# treeheight=checkorprocess_grid(treeheight,tableinputs[1,],java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=F);
# #WF14 treecover 
# treecover=checkorprocess_grid(treecover,tableinputs[2,],java=T,Tanzania,agg="false",agg2=T,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# BCFbiome = 23.50 #t biomass/ha.
# AGBwoody = ((treeheight * (treecover/100)) * BCFbiome)  *100#t/km^2. WF21 
# AGBwoodycarbon=(AGBwoody+(AGBwoody*.3))*0.47;#this is to get the total above and below woody carbon (about 30% is below ground). The carbon factor is about 47%
# #soilcarbon30cm
# soilcarbon30cm=checkorprocess_soilgrid30cm(soilcarbon30cm,tableinputs,java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=T);
# soilcarbon1m=checkorprocess_soilgrid1m(soilcarbon1m,tableinputs,java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=T);
# carbonloss=soilcarbon30cm * cultivatedarea * 0.5;
# Totalsoilcarbon= (soilcarbon1m)-carbonloss;
# Totalcarbon=AGBwoodycarbon+Totalsoilcarbon;
# ####still need to finish and figure out how to get a flux value??
# 
# 
# 
# ########################## PART 5: WATER THREAD ###############################
# 
# #################################################################
# #water thread
# 
# #Step 2 create the  tables describing the inputs for  specific thread using the function generate_WF_table (for wood fuel)
# #and generate_WL_table() for water.
# 
# tableinputs=read.table(tablelocwater,header=T,sep=",");
# tableinputs=as.data.frame(tableinputs);
# tableinputs=initiate_table(tableinputs);
# #transfer the grid parameters #defined by the woodfuel thread above
# creategridparamters(tableinputs[length(tableinputs[,1]),]);
# #process all the water thread inputs - the following variables are created and processed in the same way as the woodfuel ones above.
#process 2001 to 2010 rainfall

# population=populateresamplegrid(resamplematrix,1,200);# This has to be filled with proper pop disaggreated as the random numers are equally distributed.
# livestock_cattle=populateresamplegrid(resamplematrix,1,300);# animals 2006
# livestock_sheep=populateresamplegrid(resamplematrix,1,150);# animals 2006
# livestock_goats=populateresamplegrid(resamplematrix,1,200);# animals 2006
# livestock_pigs=populateresamplegrid(resamplematrix,1,80);# animals 2006
# livestock_chickens=populateresamplegrid(resamplematrix,1,2000);# animals 2006; this needs to be divided by 100 to calculate water consumtpion [m3/100 animals per year] in water demand model
# Irrigation_area_equiped=populateresamplegrid(resamplematrix,0,1);#area/km^2 2008 used to calculate with [%] the actual area irrigated
# Irrigation_area_actual_equiped=populateresamplegrid(resamplematrix,0,100);#% 
# cropland_area=populateresamplegrid(resamplematrix,0,1);#area/km^2
# sediment_yield=populateresamplegrid(resamplematrix,0,20);#t/km^2
# #process the files
# rainfall2001=checkandprocess_rainfall(rainfall2001,tableinputs[1,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2002=checkandprocess_rainfall(rainfall2002,tableinputs[2,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2003=checkandprocess_rainfall(rainfall2003,tableinputs[3,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2004=checkandprocess_rainfall(rainfall2004,tableinputs[4,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2005=checkandprocess_rainfall(rainfall2005,tableinputs[5,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2006=checkandprocess_rainfall(rainfall2006,tableinputs[6,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2007=checkandprocess_rainfall(rainfall2007,tableinputs[7,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2008=checkandprocess_rainfall(rainfall2008,tableinputs[8,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2009=checkandprocess_rainfall(rainfall2009,tableinputs[9,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# rainfall2010=checkandprocess_rainfall(rainfall2010,tableinputs[10,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2001=checkandprocess_rainfall(Ep2001,tableinputs[11,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2002=checkandprocess_rainfall(Ep2002,tableinputs[12,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2003=checkandprocess_rainfall(Ep2003,tableinputs[13,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2004=checkandprocess_rainfall(Ep2004,tableinputs[14,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2005=checkandprocess_rainfall(Ep2005,tableinputs[15,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2006=checkandprocess_rainfall(Ep2006,tableinputs[16,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2007=checkandprocess_rainfall(Ep2007,tableinputs[17,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2008=checkandprocess_rainfall(Ep2008,tableinputs[18,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2009=checkandprocess_rainfall(Ep2009,tableinputs[19,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# Ep2010=checkandprocess_rainfall(Ep2010,tableinputs[20,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
# 
# population=checkorprocess_grid(population,tableinputs[21,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# livestock_cattle=checkorprocess_grid(population,tableinputs[22,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# livestock_sheep=checkorprocess_grid(population,tableinputs[23,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# livestock_goats=checkorprocess_grid(population,tableinputs[24,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# livestock_pigs=checkorprocess_grid(population,tableinputs[25,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# livestock_chickens=checkorprocess_grid(population,tableinputs[25,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# Irrigation_area_equiped=checkorprocess_grid(Irrigation_area_equiped,tableinputs[26,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# Irrigation_area_actual_equiped=checkorprocess_grid(Irrigation_area_actual_equiped,tableinputs[27,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# cropland_area=checkorprocess_grid(cropland_area,tableinputs[28,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# sediment_yield=checkorprocess_grid(cropland_area,tableinputs[28,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
# ########################
# #update table
# tableinputs=initiate_table(tableinputs);
# updatetable(tablelocwater,tableinputs);#water thread
# 
# #run water model
# #1. first we calculate discharge (supply) in km^3
# alpha_tune=2.8;
# discharge2001=discharge(rainfall2001,Ep2001,alpha_tune);
# discharge2002=discharge(rainfall2002,Ep2002,alpha_tune);
# discharge2003=discharge(rainfall2003,Ep2003,alpha_tune);
# discharge2004=discharge(rainfall2004,Ep2004,alpha_tune);
# discharge2005=discharge(rainfall2005,Ep2005,alpha_tune);
# discharge2006=discharge(rainfall2006,Ep2006,alpha_tune);
# discharge2007=discharge(rainfall2007,Ep2007,alpha_tune);
# discharge2008=discharge(rainfall2008,Ep2008,alpha_tune);
# discharge2009=discharge(rainfall2009,Ep2009,alpha_tune);
# discharge2010=discharge(rainfall2010,Ep2010,alpha_tune);
# 
# #demand
# #human demand
# human_consump=7.3;#l/m^2
# human_consump=12.5;#l/m^2 (however this is a range between 5.5 to 20.1)
# population*(human_consum/1000000000);#l/km^3/year/person
# livestock_cattle*(7.3/1000000000);#l/km^3/year/person
# #etc... then add them all wrtie a function to do this.
# 
# #Irrigation.
# 
# #......
# 
# ### Added code from email:
# 
# #################################################################
# #Climate forcing
# 
# #step 1 CO2
# #get AFSIS soil variables
# BLD_sd1_M=populateresamplegrid(resamplematrix,1,2.5);#soil density 0-5cm kg / m3  (AFSIS)
# BLD_sd2_M=populateresamplegrid(resamplematrix,1,2.5);#soil density 5-15cm kg / m3  (AFSIS)
# BLD_sd3_M=populateresamplegrid(resamplematrix,1,2.5);#soil density 15-30cm kg / m3  (AFSIS)
# BLD_sd4_M=populateresamplegrid(resamplematrix,1,2.5);#soil density 30-60cm kg / m3  (AFSIS)
# BLD_sd5_M=populateresamplegrid(resamplematrix,1,2.5);#soil density 60-100cm kg / m3  (AFSIS)
# ORCDRC_sd1_M=populateresamplegrid(resamplematrix,1,5);#mean carbon % 0-5cm (AFSIS)
# ORCDRC_sd2_M=populateresamplegrid(resamplematrix,1,5);#mean carbon % 5-15cm (AFSIS)
# ORCDRC_sd3_M=populateresamplegrid(resamplematrix,1,5);#mean carbon % 15-30cm (AFSIS)
# ORCDRC_sd4_M=populateresamplegrid(resamplematrix,1,5);#mean carbon % 30-60cm (AFSIS)
# ORCDRC_sd5_M=populateresamplegrid(resamplematrix,1,5);#mean carbon % 60-100cm (AFSIS)
# #get landcover for a specific year (re-write this as a fuction) e.g. (these steps would need to be repeated for alternative years)
# Irrigation_area_percent2010=populateresamplegrid(resamplematrix,0,100);#% / km^2
# cropland_area2010=populateresamplegrid(resamplematrix,0,1);#% area/km^2
# village_area2010=populateresamplegrid(resamplematrix,0,1);#% area/km^2
# #get the top 30cm in t/km^2
# top30=((BLD_sd1_M * ORCDRC_sd1_M*.05*10)+(BLD_sd2_M * ORCDRC_sd2_M*.1*10)+(BLD_sd3_M * ORCDRC_sd3_M * 0.15*10))*100;
# #get the bottom 70cm in t/km^2
# bottom70=((BLD_sd4_M * ORCDRC_sd4_M * 0.3 * 10) + (BLD_sd5_M * ORCDRC_sd5_M * 0.4 * 10))*100;
# #correct for landcover
# top30cm_irrigated=(top30*Irrigation_area_percent2010)*0.9;#0.9 is the correction factor
# top30cm_agriculture=(top30*cropland_area2010)*0.4;
# top30cm_villages=(top30*village_area2010)*0.5;#correction factor for villages
# top30_balance=100-(Irrigation_area_percent2010+cropland_area2010+village_area2010)
# #check to see that the balance is not < 0, if > 100, set balance to 0;
# if(top30_balance<0)
# {
#   top30_balance=0;
# }
# top30_corrected_total=top30cm_irrigated+top30cm_villages+top30cm_agriculture+top30_balance;
# #get the total corrected soil carbon for the first 1m
# Total_soil_corrected2010=top30_corrected_total+bottom70;
# #get the AG woody biomass (from wood thread) for a specific year eg
# AGBwoody_2010=populateresamplegrid(resamplematrix,1,50000);#t/km^2
# AGBwoodycarbon2010=(AGBwoody2010+(AGBwoody2010*.3))*0.47;#this is to get the total above and below woody carbon (about 30% is below ground). The carbon factor is about 47%
# Total_land_cover_and_soil_carbon2010=(AGBwoodycarbon2010+Total_soil_corrected2010)*(44/12);#to get CO2  
# #repeat above for additional years in this example we assume this has been done for the following line
# Total_land_cover_and_soil_carbon2015=(AGBwoodycarbon2015+Total_soil_corrected2010)*(44/12);
# #get the flux as a national figure
# NetCO2_flux=sum(Total_land_cover_and_soil_carbon2015-Total_land_cover_and_soil_carbon2010);
# 
# #step 2 methane
# #get livestock densities (if new data can be found it would need to be updated)
# livestock_cattle=populateresamplegrid(resamplematrix,1,300);# animal densities 2006
# livestock_sheep=populateresamplegrid(resamplematrix,1,150);# animals 2006
# livestock_goats=populateresamplegrid(resamplematrix,1,200);# animals 2006
# livestock_pigs=populateresamplegrid(resamplematrix,1,80);# animals 2006
# livestock_chickens=populateresamplegrid(resamplematrix,1,2000);#
# 
# #calcualtion of emissions per type (IPCC 2006 factors) kgCH4/head/year
# livestock_cattle*39.65;
# livestock_sheep*11.10;
# livestock_goats*14.27;
# #pigs and chicken factors would need to be found;
# CH4_emissions=sum(livestock_cattle+livestock_sheep+livestock_goats);#plus chicken and pigs when corrected
# #get modis burnt area % area/km^2
# modis_burnt_area=populateresamplegrid(resamplematrix,0,1);
# #get forage production and consumption form livestock
# forrage_consumption=populateresamplegrid(resamplematrix,0,1);
# forrage_production=populateresamplegrid(resamplematrix,0,1);
# #get the fuel load combustion efficiency *2.3;
# #check that fuel load is in kg or convert ot kg
# fuel_load=((orrage_production-forrage_consumption)*.95)*2.3;#g/CH4/kg fuel
# #get area rice planted (FAO stats) eg. 2010
# rice_area==1136290*100;#to get km^2
# rice_area=rice_area*1300;#kgCH4/km^2
# rice_area=rice_area*90 #90 is  the number of days the crop has grown for
# #total CH4
# CH4_emissions=(CH4_emissions+rice_area+fuel_load); #Still need to convert this mtCO2 equivalent
# 
# #step 3 N2O emissions
# #get area legumes planted (FAO stats) eg. 2010
# legume_area==1994880;#=1 kg N / ha / year
# legume_area=legume_area*(44/28)#to convert to N2O
# #get Tons_N_fertilizer (consumption) from FAO e.g 2010
# Tons_N_fertilizer=58341;#assume 0.01 kg N2O is emmited / kg fertilizer
# #convert fertilizer to net N2O emissions in t
# Tons_N_fertilizer=(Tons_N_fertilizer*0.01)*1000;
# 
# #step 4 Albedo
# #get solution from BIRD et al paper.
# #The first thing to do is work out the forcing in ppm equivalent without albedo
# #radiative forcing is:
# B=3.7;#w/m^2
# #where  B is the doubling factor derived from hundreds of climate models. Taken as 3.7 w/m^2 in bird paper - meaning if CO2 is #383 ppm now and becomes 766 ppm in the future the temp will be raised by 3.7 degrees.
# #e.g. B * ln(new_co2_value/reference_co2_value)/ln(2)
# #which becomes: tempnow + 3.7*(ln(766/383)/ln(2))=tempnow + 3.7 or
# #20 + 3.7*(ln(766/383)/ln(2))=23.7
# #useful conversion,
# #2.3 GtC of emissions = 1 ppm carbon
# #i.e if emissions are 8.51  gtc then 8.51/2.3 =  3.7 ppm  - so to convert to gtc we use ppm*2.3
# #Now for forcing changes due to albedo...
# #1) R =radiation per location-  solar constant  = 1360 /m2, but  will have to recalibrate this amount by lat and long position
# R=1360;
# A = 1000000#1km^2
# delta_albedo=#Difference between two albedo measurements over time
#   area_of_earth=5.1*1014 #m2
# 
# forcing =(B * ln(new_co2_value/reference_co2_value)/ln(2))+((R*A*delta_albedo)/area_of_earth)
# co2=forcing * 2.3 #  equivalence in gtc. divide by a billion to get tonnes


################### END OF SCRIPT ############################



