####################################################
# Potential yields - GHA
# Date - August 25, 2015
####################################################

# load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)

# data sources
dir_data1 <-"C:/Data/Vital Signs/Soil Health Thread/Inputs/IISA Total production capacity/res03crav6190hylhrmze_package"
dir_data2 <-"C:/Data/Vital Signs/Soil Health Thread/Inputs/Boundaries"

# data output
dir_out1 <-"C:/Data/Vital Signs/Ag Intensification/Outputs"

# Load data: IISA Yield Potential (yp) raster and boundaries
setwd(dir_data1)
yp <- raster("res03_crav6190h_ylhr_mze.tif")
gha <- readOGR(dir_data2, "GHA_adm1")
landscapes <- readOGR(dir_data2,"GHA_VS_Landscapes")

## 1. calculate yield potential for Ghana

# crop using extent, rasterize polygon and finally, create poly-raster
c.gha <- crop(yp, extent(gha))
r.gha <- rasterize(gha, c.gha)
yp <- mask(x=c.gha, mask=r.gha)

# potential yields 
yp.gha <- cellStats(yp, mean, na.rm=T)  #5,762 kg/ha
yp.gha.med <- cellStats(yp, median, na.rm=T) #6,256 kg/ha
yp.gha.sd <- cellStats(yp, sd, na.rm=T) #2,094 kg/ha

rm(c.gha, r.gha)

## 2. calculate yield potential for each landscape

head(data.frame(landscapes))
# crop landcover raster by other landscapes
L01 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALSS',]))
L02 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALGS',]))
L03 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALTZ',]))
L04 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALSF',]))
L05 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALRF',]))
L06 <- crop(yp, extent(landscapes[landscapes$New_ID == 'GHALCS',]))

# potential yields by landscapes
yp.L01 <- cellStats(L01, mean, na.rm=T) #2,637 kg/ha
yp.L02 <- cellStats(L02, mean, na.rm=T) #3,044 kg/ha
yp.L03 <- cellStats(L03, mean, na.rm=T) #1,001 kg/ha
yp.L04 <- cellStats(L04, mean, na.rm=T) #10,400 kg/ha
yp.L05 <- cellStats(L05, mean, na.rm=T) #8,603 kg/ha
yp.L06 <- cellStats(L06, mean, na.rm=T) #8,451 kg/ha

# End