read.csv('eplo


silt <- raster("gha_silt.tif")
clay <- raster("gha_clay.tif")
sand <- raster("gha_sand.tif")

# Mean soil particle diameter (mm)
#ICRAF lab texture size ranges
d1.clay <- 0.002 
d2.clay <- 0.0005
d1.sand <- 2
d2.sand <- 0.05
d1.silt <- 0.05
d2.silt <- 0.002
#mean particle size (Renard 1997)
dgclay <- clay*log((d1.clay+d2.clay)/2)
dgsand <- sand*log((d1.sand+d2.sand)/2)
dgsilt <- silt*log((d1.silt+d2.silt)/2)
dg <- exp(.01 * (dgclay + dgsand + dgsilt))

# Erodibility
erodibility <- 7.594 * (0.0034 + 0.0405 * exp((-.5) * (((log10(dg) + 1.659)/.7101)^2)))
summary(erodibility)
plot(erodibility, main="Soil erodibility - Ghana")

# Project into AEA: 
#     Reproject to Africa Albers Equal Area Conic (ESRI:102022) 
aea <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
erodibility <- projectRaster(erodibility, crs = aea)

# Write output
writeRaster(erodibility, filename="soil_erodibility_gha.tif", format='GTiff', overwrite=TRUE)
s3 <- newS3()
s3$writeS3(bucket = "ci-vsindicators", source_path = "soil_erodibility_gha.tif", target_path = "Soil_Health/soil_erodibility_gha.tif", overwrite = TRUE)

