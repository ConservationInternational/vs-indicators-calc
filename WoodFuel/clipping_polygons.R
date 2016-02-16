################################# CI   VITAL SIGNS PROCESSING  #######################################
##################################  Clipping Polygons #############################################
#This script explores different clipping options and functions in R. 
#The goal is to clip two polygons layers and retain attributes.
#Contributed by mulitple persons.
#This is part of the Vital Signs project started at CI.
#
#AUTHORS: Benoit Parmentier, Kevin Tschirh , Clare Sullivan                                                                       #
#DATE CREATED: 09/12/2015 
#DATE MODIFIED: 09/12/2015
#
#COMMENTS: 
# - Modified gClip from discussion http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
# - Modified code from Kevin Tschirh
#TO DO:
# - Check if gClip works for mulitple polygons defining the area of interest?
#
#PROJECT: CI VS, CSIR, EI 
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
library(geosphere)                          # curved surface and geodetic trigonometric

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

  ref_e <- extent(reg_ref_rast)
  reg_outline_poly <- as(ref_e, "SpatialPolygons")
  reg_outline_poly <- as(reg_outline_poly, "SpatialPolygonsDataFrame")
  proj4string(reg_outline_poly) <- projection(reg_ref_rast)
  infile_reg_outline <- paste("reg_out_line_",outSuffix,".shp",sep="")
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

gClip <- function(shp, bb, keep.attribs=TRUE,outDir=NULL,outSuffix=NULL){
  #Purpose: clipping SpatialPolygonsDataFrame using another SpatialPolygonsDataFrame 
  #shp: input shapefile that we would like to clip
  #bb: input shapefile used for clipping, can be and extent raster object, matrix of coordinates 
  #keep.attribs: join attributes to spatial feature
  #outDir: output directory
  #outSuffix: output suffix attached to the name of new file
  
  #Authors: Benoit Parmentier, Modified code originating at: 
  #http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  
  #Comments: 
  #- Note that some attribute should be updated: e.g. areas, length etc. of original polygons
  #- Add bbox option for spdf
  #- Send error if different carthographic projection used
  
  ### BEGIN ####
  
  #First check inputs used from clipping, output dir and suffix
  
  if(class(bb) == "matrix"){
    b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons") #if matrix of coordinates
  }
  if(class(bb)=="SpatialPolygonsDataFrame"){
    b_poly <- bb #if polygon object, keep the same
  } 
  if(class(bb)=="exent"){
    b_poly <- as(extent(bb), "SpatialPolygons") #make a Spatial Polygon from raster extent
  }
  rm(bb) #remove from memory in case the polygon file is a giant dataset
  
  #If no output dir present, use the current dir
  if(is.null(outDir)){
    outDir=getwd()
  }
  #if no output suffix provided, use empty string for now
  if(is.null(outSuffix)){
    outSuffix=""
  }
  
  #Second, clip using rgeos library
  new.shape <- gIntersection(shp, b_poly, byid = T)
  
  #Third, join the atrribute back to the newly created object
  if(keep.attribs){
    #create a data.frame based on the spatial polygon object
    new.attribs <- data.frame(do.call(rbind,strsplit(row.names(new.shape)," ")),stringsAsFactors = FALSE)
    #test <-over(shp,bb)
    
    #new.attrib.data <- shp[new.attribs$X1,]@data #original data slot? #not a good way to extract...
    new.attrib.data <- as.data.frame(shp[new.attribs$X1,])
    row.names(new.shape) <- row.names(new.attrib.data)
    new.shape <-SpatialPolygonsDataFrame(new.shape, new.attrib.data) #Associate Polygons and attributes

  }
  
  #Writeout shapefile (default format for now)
  infile_new.shape <- paste("clipped_spdf",outSuffix,".shp",sep="")
  writeOGR(new.shape,dsn= outDir,layer= sub(".shp","",infile_new.shape), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
  
  return(new.shape)
}

#############################################
######## Parameters and arguments  ########

in_dir <- "/home/bparmentier/Google Drive/CI_Vital_signs"
#dir_data1 <-"C:/Users/Gary/Google Drive/VS Indicators/External Datasets/Boundaries"

infile_poly1 <- "TZA_adm1.rds" #copied in local input dir "in_dir"
infile_poly_ref <-"SAGCOT.shp"  #copied in local input dir "in_dir", reference used to clip
  
out_suffix <-"_vs_tza_09122015"
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" # this is EPSG4326

CRS_reg <- CRS_WGS84 #not in use yet, proj4 string for the region of interest (aoi/roi)

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

#Code from Kevin...below
# load data

adm1 <-  readRDS(file.path(in_dir,infile_poly1)) #file to be clipped, Tanzania administrative 
#sagcot <- readOGR(in_dir, "SAGCOT") #rgdal command
sagcot <- readOGR(dsn=in_dir,sub(".shp","",infile_poly_ref))

# check for identical projection
identicalCRS(adm1, sagcot)

# plot
plot(adm1)
plot(sagcot, border="red", pch=16, add=T)

# create a SAGCOT boundary files with regions from the adm1 layer
# preserve the attribute with "byid = TRUE"
sagcot.reg <- gIntersection(adm1, sagcot, byid=TRUE)
test <- over(adm1, sagcot) #quick check
sum(test$OBJECTID,na.rm=T) #12 features

# the 2 SpatialPolygonsDataFrames, sagcot and adm1, are transform into a polygon
# correctly but loose their attributes
plot(sagcot.reg)
class(sagcot.reg)
class(sagcot)
class(adm1)
head(sagcot.reg)

## Now use modified gClip function

sagcot_clipped <- gClip(adm1, sagcot, keep.attribs=TRUE)
#run with out_suffix and out_dir options
sagcot_clipped <- gClip(adm1, sagcot, keep.attribs=TRUE,out_dir,out_suffix)

#Check the output
length(sagcot_clipped) #12 features
length(sagcot.reg)# clipping from intersect, 12 features

plot(adm1)
plot(sagcot_clipped,border="blue",add=T)
plot(sagcot,border="red",add=T)

################ END OF SCRIPT #########################

