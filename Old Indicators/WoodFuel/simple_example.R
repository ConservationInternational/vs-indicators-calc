library(raster)

# Make base raster: Tanzania rainfall
rain <- raster('tanzania_rainfall.tif')
tza_poly <- readOGR('some folder', 'name of file with shapefile of Tanzania not including the .shp extension')
rain_tza <- crop(in_rast, tza_poly, filename="outputfilename.tif")

# Now reproject an input raster to match the above
tree_h <- raster('tree_height.tif')
tree_h_tza <- projectRaster(tree_h, rain_tza, method='bilinear', 
                            filename="outputfilename.tif")

# For Hansen dataset, he will want to use "aggregate" from the raster package 
# to calculate whatever metric he needs (the percentage forest loss per 1 km 
# grid cell, or the percentage forest cover, or whatever it is)

# Note: if you have GDAL installed you could use gdalwarp from the gdalUtils 
# package to do the above much more quickly - but lets just go with the above 
# unless performance is really an issue.
