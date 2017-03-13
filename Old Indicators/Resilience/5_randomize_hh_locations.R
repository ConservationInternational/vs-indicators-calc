# Following the DHS protocol, add positional error to LSMS location data to 
# ensure respondent confidentiality. Adds:
#
#   a minimum of 0 and a maximum of 5 kilometers of positional error with a 
#   further 1% of the rural clusters displaced a minimum of 0 and a maximum of 
#   10 kilometers

source('0_settings.R')

library(rgdal)
library(teamlucc)
library(dplyr)

set.seed(as.numeric(read.table("random_seed.txt")))

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

lsms_hhs <- readOGR(lsms_pt_folder, paste0(ISO3, "_HH_pt_locations"))
lsms_hhs <- spTransform(lsms_hhs, CRS(s_srs))

displace_points <- function(coords, max_displacement=5) {
    x <- coords[, 1]
    y <- coords[, 2]
    # Select a set of random angles and radii to calculate x, y displacements 
    # for each point
    angles <- runif(length(x), min=0, max=2*pi)
    radii <- runif(length(x), min=0, max=max_displacement)
    x_displacement <- radii * cos(angles)
    y_displacement <- radii * sin(angles)
    return(cbind(x + x_displacement, y + y_displacement))
}

# Need a planar coordinate system
utm_proj4string <- utm_zone(lsms_hhs, proj4string=TRUE)
lsms_hhs_utm <- spTransform(lsms_hhs, utm_proj4string)
lsms_hhs_utm <- as.data.frame(lsms_hhs_utm)
lsms_hhs_utm <- select(lsms_hhs_utm, -LAT, -LONG, -DATUM)

# Split hhs into two groups. 90% will be displaced by up to 5km, and 10% by up 
# to 10 km.
displacement <- rep(5000, nrow(lsms_hhs_utm))
displacement[sample(nrow(lsms_hhs_utm), nrow(lsms_hhs)*.1)] <- 10000

new_coords <- displace_points(cbind(lsms_hhs_utm$coords.x1, 
                                    lsms_hhs_utm$coords.x2), 
                              displacement)
lsms_hhs_utm <- select(lsms_hhs_utm, -coords.x1, -coords.x2, -ST1GPSLONG, 
                       -ST1GPSLAT, -ST1FORMAT)

lsms_hhs_utm <- SpatialPointsDataFrame(new_coords, lsms_hhs_utm, 
                                       proj4string=CRS(utm_proj4string))
lsms_hhs_randomized <- spTransform(lsms_hhs_utm, proj4string(lsms_hhs))

write.csv(rename(as.data.frame(lsms_hhs_randomized), x=coords.x1, y=coords.x2), 
          file=file.path(intermediate_folder,
                         paste0(ISO3, '_LSMS_hhs_anonomized.csv')), row.names=FALSE)
