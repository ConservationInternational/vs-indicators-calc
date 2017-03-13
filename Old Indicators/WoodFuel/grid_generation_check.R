################################# CI   VITAL SIGNS PROCESSING  #######################################
##################################  GRID GENERATION CHECK #############################################
#This script examines grid generation from the VS (Marc Pienaar) function. 
#The main issue is to ascertain that the projection of the grid can be set to make the data usable.
#This is part of the Vital Signs project started at CI.
#
#AUTHOR: Benoit Parmentier                                                                       #
#DATE CREATED: 09/05/2015 
#DATE MODIFIED: 09/06/2015
#
#COMMENTS: 
#TO DO:

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

##function from Marc Pienaar CSIR
createresamplegrid=function(AOI,resolution)
{
  #resolution = meters
  if(!require(geosphere)) install.packages('geosphere');
  if(!require(sp)) install.packages('sp');
  if(!require(raster)) install.packages('raster');
  
  #get grid extent for the area of interest
  Xmin=xmin(extent(AOI));
  Xmax=xmax(extent(AOI));
  Ymax=ymax(extent(AOI));
  Ymin=ymin(extent(AOI));
  #define a meter-based resample grid based on resolution
  #initial point
  p=c(Xmin,Ymax);
  #set an offset to get cente value
  p=destPoint(p,180, resolution/2); #this is overwritten??
  
  p=destPoint(p,90, resolution/2);
  rows=p[2];cols=p[1];#initialise the first row and column
  while(p[2]>=Ymin)#
  {
    newloc=destPoint(p,180, resolution);
    rows=rbind(rows,newloc[2]);
    p=newloc;
  }
  #add one more for a buffer
  newloc=destPoint(p,180, resolution);
  rows=rbind(rows,newloc[2]);
  #reset p
  p=c(Xmin,Ymax);
  #set an offset to get cente value
  p=destPoint(p,180, resolution/2);
  p=destPoint(p,90, resolution/2);
  #initial point
  
  while(p[1]<=Xmax)#cols
  {
    newloc=destPoint(p,90, resolution);
    cols=rbind(cols,newloc[1]);
    p=newloc;
  }
  #add one more for a buffer : It is unclear why this buffer is added (benoit)
  newloc=destPoint(p,90, resolution);
  cols=rbind(cols,newloc[1]);
  cols=cols[1:(length(cols)-1)]
  rows=rows[1:(length(rows)-1)]
  #return the grid
  grd <- expand.grid(x = cols, y = rows);  # expand points to grid
  coordinates(grd) <- ~x + y;
  #projection(grd) <- CRS("+proj=longlat +datum=WGS84");
  #create a raster of the temp grid
  #rast=raster(grd);
  #rast[]=NA;
  return(grd)
}

#############################################
######## Parameters and arguments  ########

in_dir <- "/home/bparmentier/Google\ Drive/CI_Vital_signs"
ref_fname <- "vs_tanzania_10_v2.tif"
out_suffix <-"_vs_tza_09012015"
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # this is EPSG4326

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

resolution <- 1000 #1000m is 1km, this is the input in the despoint function from the package gesophere
grid1 <- createresamplegrid(aoi,resolution)
df <- data.frame(x=coordinates(grid1)[,1],y=coordinates(grid1)[,2],z=1:length(grid1))
r1 <- rasterFromXYZ(df, crs=NA, digits=5)
#r1 <- raster(grid1) #this will not place the values at the center of pixels, use rasterFromXYZ function
#r1 <- setValues(r1,rnorm(1:ncell(r1)))

class(grid1)
plot(grid1)
extent(r1)
all.equal(ref_e,extent(r1))
plot(r1)
plot(grid1,add=T)
plot(aoi,add=T) #half a pixel shift from the original extent...? 
projection(r1) <- CRS("+init=epsg:4326")

#create raster from extent in EPSG4326
## S4 method for signature 'missing'
res_deg <- 1/111 #about 111km for one degree at the equator
r2 <- raster(ext=ref_e, resolution=res_deg, vals=NULL)
r2 <- setValues(r2,rnorm(1:ncell(r2)))
r1 <- setValues(r1,rnorm(1:ncell(r1)))
projection(r1) <-projection(r2)

all.equal(extent(r1),extent(r2)) #some differences...,xmin is the same...
plot(r2)
plot(grid1, add=T)
#r1[,11] <- NA_flag_val
#NAvalue(r1) <- NA_flag_val
#r_test <- trim(r1)

#Force the operation...
#res(r_test)<-res(r2)
#r_sum <- r_test + r2 #resolution and extent do not match...
#Warning message:
#  In r_test + r2 :
#  Raster objects have different extents. Result for their intersection is returned


res_pix <- 480

col_mfrow <- 1
row_mfrow <- 1

png(filename=paste("grid_comparison","_",out_suffix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)


plot(r1)
plot(grid1, add=T)
grid2 <- rasterToPoints(r2,spatial=T)
plot(grid2,col="red",add=T)
title("red:raster function grid, black:createresamplegrid ")

dev.off()

## If starting point is at higher latitude and grid larger/coarser, there might be larger differences...
##

################ END OF SCRIPT #########################

