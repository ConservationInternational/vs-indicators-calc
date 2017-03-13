#########################
# Ag Intensification - Cultivated Area Calculations 
# Family5 (high resolution)
# TZA: landscape (L10, L18 & L19)
# September 24, 2015
# Running time:  ~ 60 min 
#########################

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

#L10 <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\Family_5\\vs_tanzania_10_v2.tif")
L10 <- raster("C:\\Users\\Gary\\Desktop\\Inputs\\vs_tanzania_10_v2\\vs_tanzania_10_v2.tif")
#L18 <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\Family_5\\vs_tanzania_18_v2.tif")
L18 <- raster("C:\\Users\\Gary\\Desktop\\Inputs\\vs_tanzania_18_v2\\vs_tanzania_18_v2.tif")
#L19 <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\Family_5\\vs_tanzania_19.tif")
L19 <- raster("C:\\Users\\Gary\\Desktop\\Inputs\\vs_tanzania_19\\vs_tanzania_19.tif")

# data outputs
#out_dir1 <- "C:\\Data\\Vital Signs\\Ag Intensification\\Outputs"
out_dir1 <- "C:\\Users\\Gary\\Desktop\\Outputs"

##calculate area for each high resolution landscape image in TZ

# L10 (mufindi) 
cult.area.L10 <- area.calc(L10,1) # 2,339
area.L10 <- 100*cellStats(area(L10,na.rm=T),stat='sum') # 9,779
pctcult.L10 <- 100*cult.area.L10/area.L10 # 23.92%

# L18 (kilolo) 
cult.area.L18 <- area.calc(L18,0) # 953
area.L18 <- 100*cellStats(area(L18,na.rm=T),stat='sum') # 9,828
pctcult.L18 <- 100*cult.area.L18/area.L18 # 9.70%

# L19 (kilombero) 
cult.area.L19 <- area.calc(L19,1) # 5,156
area.L19 <- 100*cellStats(area(L19,na.rm=T),stat='sum') # 9,799
pctcult.L19 <- 100*cult.area.L19/area.L19 # 52.62%

# write outputs
setwd(out_dir1)

Country <- ("TZA")
Scale <- "Landscape"
Data <- ("Family5")
Name <- c("Mufundi", "Kilolo","Kilombero" )
Code <- c("L10", "L18", "L19")
Cult.area <- c(cult.area.L10, cult.area.L18, cult.area.L19)
Area <- c(area.L10, area.L18, area.L19)
Pct.cult <- c(pctcult.L10, pctcult.L18, pctcult.L19)
output <- data.frame(Country, Scale, Data, Name, Code, Cult.area, Area, Pct.cult)
output$Cult.area <- round(output$Cult.area, 0)
output$Area <- round(output$Area, 0)
output$Pct.cult <- round(output$Pct.cult, 3)

write.csv(output, file = "TZA.AreaCultivated.Family5.csv")

# End
