####################################################
#  Watershed Definition for Soil Erosion modelling
#  Clare Sullivan
#  Date - August 30, 2015
#  Date - Sept. 12, 2015 modified code with Benoit Parmentier
####################################################

library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(GISTools)
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

#load water basin polygons (30 arcsec resolution)
#source: http://hydrosheds.org/page/hydrobasins
unzip(vstables$tables[["HydroBasins/af_bas_30s_beta.zip"]]$getData(),
      exdir = dirname(vstables$tables[["HydroBasins/af_bas_30s_beta.zip"]]$getData()))
watershed <- readOGR(paste0(dirname(vstables$tables[["HydroBasins/af_bas_30s_beta.zip"]]$getData()), "/af_bas_30s_beta"), "af_bas_30s_beta")

#load boundary file polygons 
unzip(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()))
gha <- readOGR(dirname(vstables$tables[["Boundaries/GHA_adm1.zip"]]$getData()), "GHA_adm1")

unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")


#load raster files for templates
gha_ph <- raster("gha_ph.tif")
sagcot_ph <- raster("sagcot_ph.tif")

#project the polygon layer into AEA:
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
sagcot <- spTransform(sagcot, crs(aea))
gha <- spTransform(gha, crs(aea))
watershed <- spTransform(watershed, crs(aea))

# reproject reference raster pH raster file
gha_aea <- projectRaster(gha_ph, crs = aea)
sagcot_aea <- projectRaster(sagcot_ph, crs = aea)

#Purpose: clipping SpatialPolygonsDataFrame using another SpatialPolygonsDataFrame 
#shp: input shapefile that we would like to clip
#bb: input shapefile used for clipping, can be and extent raster object, matrix of coordinates 
#keep.attribs: join attributes to spatial feature
#outDir: output directory
#outSuffix: output suffix attached to the name of new file
#Authors: Benoit Parmentier, Modified code originating at: 
#http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
gClip <- function(shp, bb, keep.attribs=TRUE,outDir=NULL,outSuffix=NULL){
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

#note - this function splits whole polygons somehow
#it does not appear to effect the output needed for the LS factor
#but could create issues if applied to other purposes

######## Parameters and arguments  ########

out_suffix <-"09162015"

#clip watershed to appropriate boundaries
watershed_sagcot <- gClip(watershed, sagcot, keep.attribs=TRUE,getwd(),out_suffix)
watershed_gha <- gClip(watershed, gha, keep.attribs=TRUE,getwd(),out_suffix)

plot(watershed_gha,col="lightblue")
plot(watershed_sagcot,col="lightblue")

#rasterize shapefiles
watershed_gha_r<-rasterize(watershed_gha, gha_aea, field="AREA_SQKM")
watershed_sagcot_r<-rasterize(watershed_sagcot, sagcot_aea, field="AREA_SQKM")

s3 <- newS3()
#output shapefiles
writeOGR(watershed_gha, ".", "Hydrobasins_GHA", driver="ESRI Shapefile", overwrite=TRUE)
writeOGR(watershed_sagcot, ".", "Hydrobasins_SAGCOT", driver="ESRI Shapefile", overwrite=TRUE)
sapply(list.files(pattern = "Hydrobasins_"), function(ogrfile) {
  s3$writeS3(bucket = "ci-vsindicators", source_path = ogrfile, target_path = paste0("Soil_Health/", ogrfile), overwrite = TRUE)
})
#output rasters
writeRaster(watershed_gha_r, filename="watershed_gha_r.tif", format='GTiff', overwrite=TRUE)
writeRaster(watershed_sagcot_r, filename="watershed_sagcot_r.tif", format='GTiff', overwrite=TRUE)
s3$writeS3(bucket = "ci-vsindicators", source_path = "watershed_gha_r.tif", target_path = "Soil_Health/watershed_gha_r.tif")
s3$writeS3(bucket = "ci-vsindicators", source_path = "watershed_sagcot_r.tif", target_path = "Soil_Health/watershed_sagcot_r.tif")


#remove large files
rm(gha, gha_ph, sagcot, sagcot_ph, watershed, watershed_gha, watershed_sagcot, watershed_gha_r, watershed_sagcot_r)
