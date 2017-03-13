# Set location of CHIRPS files. These files need to be downloaded from:  
# ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/
chirps_raw_folder <- 'O:/Data/CHIRPS-2.0/global-monthly'
stopifnot(file_test('-d', chirps_raw_folder))

# Drive folder root location
drive_root <- 'C:/Users/azvol/Google Drive/VS Indicators'

# Location of external datasets folder
external_datasets_folder <- file.path(drive_root, 'External Datasets')
stopifnot(file_test('-d', external_datasets_folder))

# Resilience indicator intermediate output folder
intermediate_folder <- file.path(drive_root, 'External Datasets', 'Resilience')
stopifnot(file_test('-d', intermediate_folder))

# Resilience indicator final output folder
indicator_output_folder <- file.path(drive_root, 'Indicator Dataset Outputs', 'Resilience')
stopifnot(file_test('-d', indicator_output_folder))


# Which country to process
ISO3 <- 'TZA'

# LSMS data folder (public data)
lsms_folder <- file.path(external_datasets_folder, 'TZA LSMS/NPS 2010')
stopifnot(file_test('-d', lsms_folder))

# LSMS point location data (only present on Alex Zvoleff's machine, cannot be 
# saved to server as it is confidential information)
lsms_pt_folder <- file.path('O:/Data/Vital_Signs/LSMS', ISO3)
if (!file_test('-d', lsms_pt_folder)) {
    warning('LSMS point location data not present - cannot produce final results')
}

# Location for processed output from CHIRPS dataset
chirps_proc_folder <- file.path(external_datasets_folder, 'CHIRPS Precipitation')
stopifnot(file_test('-d', chirps_proc_folder))

# Time period to include in CHIRPS SPI calculation (shouldn't need to alter 
# this). Note the day is ignored, but required to convert to a date object.  
# Period is inclusive of the below limits.
chirps_start_date <- as.Date('1981/1/1')
chirps_end_date <- as.Date('2014/12/1')
date_limits_string <- paste0(format(chirps_start_date, '%Y%m'), '-', format(chirps_end_date, '%Y%m'))
