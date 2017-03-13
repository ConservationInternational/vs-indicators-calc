################################# CI   VITAL SIGNS PROCESSING  #######################################
##################################  HANSEN DATA  #############################################
#This script downloads Hansen datasets for a given area.
#Crop the data to the are of interest and aggregate to the 1km resolution (for now).
#This is part of the Vital Signs project started at CI.
#
#AUTHOR: Benoit Parmentier                                                                       #
#DATE CREATED: 09/01/2015 
#DATE MODIFIED: 09/01/2015
#
#COMMENTS: Needs the input ref used for the target area
#TO DO:
# - add handling of any reference system (it assumes same for now)
# - not sure which hansen data input is needed, change accordingly.
# - assumes square grid cells/pixels
# - add log file of processing
# - add compression for raster if needed
# - deal with NA value if needed
#
#PROJECT: CI VS, CSIR 
##################################################################################################
#
###Loading r library and packages

library(raster)                             # loading the raster package
library(gtools)                             # loading ...
library(sp)                                 # spatial objects in R
library(gplots)                             # 
library(rgdal)                              # gdal driver for R
library(RColorBrewer)                       # color scheme, palettes used for plotting
library(gdata)                              # read different format (including .xlsx)
library(plotrix)                            # plot options and functions including plotCI
library(rasterVis)                          # raster visualization
library(colorRamps)                         # contains matlab.like palette
library(zoo)                                # time series objects and methods
library(xts)                                # extension of time series objects
library(gfcanalysis)                        # Hansen dataset...

#################################################
###### Functions  used in the script  ##########

#This function is very very slow not be used most likely
create_polygon_from_extent<-function(reg_ref_rast,outDir=NULL,outSuffix=NULL){
  #This functions returns polygon sp from input rast
  #Arguments: 
  #reg_ref_rast: input ref rast
  #outDir : output directory, if NULL then the current dir in used
  #outSuffix: output suffix used for the naming of the shapefile
  #Output: 
  #reg_outline_poly: spatial polygon data.frame
  #
  if(is.null(outDir)){
    outDir=getwd()
  }
  if(is.null(outSuffix)){
    outSuffix=""
  }
  #CP <- as(extent(130, 180, 40, 70), "SpatialPolygons")
  #proj4string(CP) <- CRS(proj4string(wrld_simpl))
  ref_e <- extent(reg_ref_rast)
  reg_outline_poly <- as(ref_e, "SpatialPolygons")
  reg_outline_poly <- as(reg_outline_poly, "SpatialPolygonsDataFrame")
  proj4string(reg_outline_poly) <- projection(reg_ref_rast)
  infile_reg_outline <- paste("reg_out_line_",out_suffix,".shp",sep="")
  writeOGR(reg_outline_poly,dsn= outDir,layer= sub(".shp","",infile_reg_outline), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
  
  return(reg_outline_poly)
}

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

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#############################################
######## Parameters and arguments  ########

in_dir <- "/home/bparmentier/Google\ Drive/CI_Vital_signs"
ref_fname <- "vs_tanzania_10_v2.tif"
out_suffix <-"_vs_tza_09012015"
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
agg_fact <- 33 #aggregation factor, set to NULL if computed on the fly.
CRS_reg <- CRS_WGS84 #not in use yet, proj4 string for the region of interest (aoi/roi)

file_format <- ".tif" #raster format used
NA_value <- -9999 #No data value/flag value
NA_flag_val <- NA_value

setwd(in_dir)
out_dir <- in_dir
                   
create_out_dir_param <- TRUE
                   
#Create output directory
                   
if(create_out_dir_param==TRUE){  
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

########################################################
##############  BEGIN script  ##############

### PART 0: READ IN DATASETS DOWNLOADINGS

reg_ref_rast <- raster(file.path(in_dir,ref_fname))

ref_e <- extent(reg_ref_rast)
#r_pol <- polygonFromExtent(ref_e)

#this function is inefficient but quick and dirty for now
aoi <- create_polygon_from_extent(reg_ref_rast,outDir=NULL,outSuffix=NULL)
#debug(create_polygon_from_extent)

tiles <- calc_gfc_tiles(aoi)
print(length(tiles)) # Number of tiles needed to cover AOI

download_tiles(tiles, output_folder= out_dir, first_and_last=TRUE)

lf_Hansen <- list.files(out_dir,"Hansen.*.tif",full.names = T)

#use first year...
r_first <- raster(lf_Hansen[2]) #res 0.0002777778 in decimial degrees (i.e. 30m)
dim(r_first)
r_first_w <- crop(r_first,reg_ref_rast,filename=paste("r_first_w",out_suffix,".tif",sep=""))
#r_first_w <- crop(r_first,ref_e,filename=paste("r_first_w",out_suffix,".tif",sep=""))
plot(r_first_w)

#now aggregate to 1km or 990m, this will change accordingly once the input raster ref is given..
#
if(is.null(agg_fact)){
  res_ref <- res(reg_ref_rast)[1] #assumes square cells, and decimal degrees from WGS84 for now...
  res_in <- res(r_first_w)[1] #input resolution, assumes decimal degrees
  agg_fact <-round(res_ref/res_in) #find the factor needed..
  #
}

#1km or 0.009009009 at the equator (roughly)
r_agg33 <- aggregate(r_first_w, fact=agg_fact,FUN=mean)

raster_name <- file.path(out_dir,paste("r_agg_",agg_fact,out_suffix,file_format,sep=""))
writeRaster(r_agg33,filename=file.path(out_dir,raster_name))

################ End of script ################
