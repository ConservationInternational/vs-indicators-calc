#########################
# Ag Intensification - Cultivated Area Calculations 
# Family5 (high resolution)
# GHA: landscape (L04 and L06)
# August 20, 2015
# Running time:  40 min
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

L04 <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\Family_5\\vs_ghana_sdf.tif")
L06 <- raster("C:\\Data\\Vital Signs\\UMd Deliverables\\Family_5\\vs_ghana_cs.tif")

##calculate area for each high resolution landscape image in TZ

# L04 (semi-deciduous forest)
cult.area.L04 <- area.calc(L04,0) # 460.87
area.L04 <- 100*cellStats(area(L04,na.rm=T),stat='sum') # 9,997
pctcult.L04 <- 100*cult.area.L04/area.L04 # 4.61%

# L06 (coastal savannah)
cult.area.L06 <- area.calc(L06,1) # 5,160
area.L06 <- 100*cellStats(area(L06,na.rm=T),stat='sum') # 9,996
pctcult.L06 <- 100*cult.area.L06/area.L06 # 51.62%

# write outputs
setwd("C:\\Users\\Gary\\Desktop\\Outputs")

Country <- ("GHA")
Scale <- "Landscape"
Data <- ("Family5")
Name <- c("Semi-deciduous Forest", "Coastal Savannah")
Code <- c("L04", "L06")
Cult.area <- c(cult.area.L04, cult.area.L06)
Area <- c(area.L04, area.L06)
Pct.cult <- c(pctcult.L04, pctcult.L06)
output <- data.frame(Country, Scale, Data, Name, Code, Cult.area, Area, Pct.cult)
output$Cult.area <- round(output$Cult.area, 0)
output$Area <- round(output$Area, 0)
output$Pct.cult <- round(output$Pct.cult, 3)

write.csv(output, file = "GHA.CultivatedArea.Family5.csv")