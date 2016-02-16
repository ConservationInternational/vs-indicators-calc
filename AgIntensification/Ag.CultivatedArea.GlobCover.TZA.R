#########################
# Ag Intensification - Cultivated Area Calculations 
# Globcover2009 (low resolution)
# TZA: Sagcot and landscape levels
# August 20, 2015
#########################
# Utility package for CI Vital Signs project
library(VitalSignsUtilities)
library(raster)
library(sp)
library(rgdal)
library(rasterVis)
library(ggplot2)

# define function to calculate area (ha) for cells with a certain value
area.calc <- function(x,value){
  y <- x
  y[y != value] <- NA
  return(100*cellStats(area(y,na.rm=T),stat='sum',na.rm=T))
}


# read in area shape files
unzip(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()))
sagcot <- readOGR(dirname(vstables$tables[["Boundaries/SAGCOT.zip"]]$getData()), "SAGCOT")
unzip(vstables$tables[["Boundaries/TZA_VS_Landscapes.zip"]]$getData(),
      exdir = dirname(vstables$tables[["Boundaries/TZA_VS_Landscapes.zip"]]$getData()))
landscapes <- readOGR(dirname(vstables$tables[["Boundaries/TZA_VS_Landscapes.zip"]]$getData()), "TZA_VS_Landscapes")


# read in land cover files
landcover <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\GLOBCOVER_SAGCOTv2\\GLOBCOVER_SAGCOT_CI_L4_200901_200912_V2.3.tif")

# landcover values range from 11 to 210
# each value corresponds to LULC type
# types of interest are:
# 11 - Post-flooding or irrigated croplands (or aquatic)
# 14 - Rainfed croplands
# 20 - Mosaic cropland (50-70%) / vegetation (grassland/shrubland/forest) (20-50%)
# 30 - Mosaic vegetation (grassland/shrubland/forest) (50-70%) / cropland (20-50%)

## 1. calculate area by category for entire SAGCOT
i.area <- area.calc(landcover,11)  # 9
r.area <- area.calc(landcover,14)  # 941,754
mc.area <- area.calc(landcover,20) # 72,251
mv.area <- area.calc(landcover,30) # 6,730,314

cult.area.sagcot <- i.area + r.area + .6*mc.area + .35*mv.area
# 3,340,724
area.sagcot <- 100*cellStats(area(landcover,na.rm=T),stat='sum')
# 29,080,180
pctcult.sagcot <- 100*(cult.area.sagcot/area.sagcot)  # 11.49% of total area

## 2. calculate area for each category for landscapes 

# crop landcover raster by other landscapes
L03 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL03',]))
L10 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL10',]))
L11 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL11',]))
L18 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL18',]))
L19 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL19',]))
L20 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL20',]))
L21 <- crop(landcover,extent(landscapes[landscapes$New_ID == 'TZAL21',]))

# calculate area for L03 (sumbawanga)
i.area.L03 <- area.calc(L03,11)  # 0
r.area.L03 <- area.calc(L03,14)  # 85
mc.area.L03 <- area.calc(L03,20) # 0
mv.area.L03 <- area.calc(L03,30) # 6,105

cult.area.L03 <- i.area.L03 + r.area.L03 + .6*mc.area.L03 + .35*mv.area.L03
# 2,222
area.L03 <- 100*cellStats(area(L03,na.rm=T),stat='sum')
pctcult.L03 <- 100*(cult.area.L03/area.L03) 
# 21.68% of total area

# calculate area for L10 
i.area.L10 <- area.calc(L10,11)  # 0
r.area.L10 <- area.calc(L10,14)  # 0
mc.area.L10 <- area.calc(L10,20) # 0
mv.area.L10 <- area.calc(L10,30) # 7,947

cult.area.L10 <- i.area.L10 + r.area.L10 + .6*mc.area.L10 + .35*mv.area.L10
# 2,781
area.L10 <- 100*cellStats(area(L10,na.rm=T),stat='sum')
pctcult.L10 <- 100*(cult.area.L10/area.L10) 
# 28.04% of total area

# calculate area for L11 (ludewa)
i.area.L11 <- area.calc(L11,11)  # 0
r.area.L11 <- area.calc(L11,14)  # 0
mc.area.L11 <- area.calc(L11,20) # 0
mv.area.L11 <- area.calc(L11,30) # 617

cult.area.L11 <- i.area.L11 + r.area.L11 + .6*mc.area.L11 + .35*mv.area.L11
# 216
area.L11 <- 100*cellStats(area(L11,na.rm=T),stat='sum')
pctcult.L11 <- 100*(cult.area.L11/area.L11) 
# 2.19% of total area

# calculate area for L18
i.area.L18 <- area.calc(L18,11)  # 0
r.area.L18 <- area.calc(L18,14)  # 0
mc.area.L18 <- area.calc(L18,20) # 0
mv.area.L18 <- area.calc(L18,30) # 6,460

cult.area.L18 <- i.area.L18 + r.area.L18 + .6*mc.area.L18 + .35*mv.area.L18
# 2,261
area.L18 <- 100*cellStats(area(L18,na.rm=T),stat='sum')
pctcult.L18 <- 100*(cult.area.L18/area.L18) 
# 23.45% of total area

# calculate area for L19 (kilombero)
i.area.L19 <- area.calc(L19,11)  # 0
r.area.L19 <- area.calc(L19,14)  # 536
mc.area.L19 <- area.calc(L19,20) # 0
mv.area.L19 <- area.calc(L19,30) # 3,207

cult.area.L19 <- i.area.L19 + r.area.L19 + .6*mc.area.L19 + .35*mv.area.L19
# 1,658
area.L19 <- 100*cellStats(area(L19,na.rm=T),stat='sum')
pctcult.L19 <- 100*(cult.area.L19/area.L19) 
# 17.22% of total area

# calculate area for L20 (mbarali)
i.area.L20 <- area.calc(L20,11)  # 0
r.area.L20 <- area.calc(L20,14)  # 0
mc.area.L20 <- area.calc(L20,20) # 0
mv.area.L20 <- area.calc(L20,30) # 59.73

cult.area.L20 <- i.area.L20 + r.area.L20 + .6*mc.area.L20 + .35*mv.area.L20
# 2,091
area.L20 <- 100*cellStats(area(L20,na.rm=T),stat='sum',na.rm=T)
pctcult.L20 <- 100*(cult.area.L20/area.L20) 
# 21.08% of total area

# calculate area for L21 (rufiji)
i.area.L21 <- area.calc(L21,11)  # 0
r.area.L21 <- area.calc(L21,14)  # 0.94
mc.area.L21 <- area.calc(L21,20) # 0
mv.area.L21 <- area.calc(L21,30) # 51.07

cult.area.L21 <- i.area.L21 + r.area.L21 + .6*mc.area.L21 + .35*mv.area.L21
# 1,882
area.L21 <- 100*cellStats(area(L21,na.rm=T),stat='sum',na.rm=T)
pctcult.L21 <-100*(cult.area.L21/area.L21) 
# 18.37% of total area

#End