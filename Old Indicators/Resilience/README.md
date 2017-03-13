# README: Rcode/resilience/misc

This folder contains various scripts needed to calculate the Vital Signs 
"Resilience of Crop Yield" indicator.

## Notes on input datasets:
* Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS): 
    precipitation dataset, produced by UCSB Climate Hazards Group, available 
    at: 
    ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/
* Living Standards Measurement Survey (LSMS): a household survey dataset. 
    Available from http://microdata.worldbank.org/index.php/catalog/lsms. Note 
    that this dataset is confidential survey data. The household location data 
    cannot be publicly released. The location information included in the 
    dataset in the external datasets folder has been obfuscated (a random 
    displacement of 0-10km has been added to each point location). Only CI 
    Vital Signs staff have access to the original location information and it 
    cannot be shared outside of CI.
