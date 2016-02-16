####################################################
# Potential yields - TZA
# Date - August 10, 2015
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
sagcot <- readOGR(dir_data2, "SAGCOT")
landscapes <- readOGR(dir_data2,"TZA_VS_Landscapes")

## 1. calculate yield potential for SAGCOT

# crop using extent, rasterize polygon and finally, create poly-raster
c.sagcot <- crop(yp, extent(sagcot))
r.sagcot <- rasterize(sagcot, c.sagcot)
yp <- mask(x=c.sagcot, mask=r.sagcot)

# potential yields 
yp.sagcot <- cellStats(yp, mean, na.rm=T)  #5,932 kg/ha
yp.sagcot.med <- cellStats(yp, median, na.rm=T) #6,452 kg/ha
yp.sagcot.sd <- cellStats(yp, sd, na.rm=T) #2,960 kg/ha

rm(c.sagcot, r.sagcot)

## 2. calculate yield potential for each landscape

head(data.frame(landscapes))
# crop landcover raster by other landscapes
L03 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL03',]))
L10 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL10',]))
L11 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL11',]))
L18 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL18',]))
L20 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL20',]))
L21 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL21',]))
L19 <- crop(yp, extent(landscapes[landscapes$New_ID == 'TZAL19',]))

# potential yields by landscapes
yp.L03 <- cellStats(L03, mean, na.rm=T) #2,637 kg/ha
yp.L10 <- cellStats(L10, mean, na.rm=T) #3,044 kg/ha
yp.L11 <- cellStats(L11, mean, na.rm=T) #1,001 kg/ha
yp.L18 <- cellStats(L18, mean, na.rm=T) #10,400 kg/ha
yp.L20 <- cellStats(L20, mean, na.rm=T) #8,603 kg/ha
yp.L21 <- cellStats(L21, mean, na.rm=T) #8,451 kg/ha
yp.L19 <- cellStats(L19, mean, na.rm=T) #10,403 kg/ha

# End