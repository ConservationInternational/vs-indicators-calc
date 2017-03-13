# READ ME – SOIL HEALTH THREAD 
08/31/2015 
Authors: Clare Sullivan, Isha Shah, and Cheryl Palm

# Topsoil Carbon (%)

## Tier 1
Two scripts are required to run this indicator (SH_T1_TopsoilC_gha.R & SH_T1_TopsoilC_tza.R). These scripts each produce an output file (gha_org_c.tif & sagcot_org_c.tif).

The input data is provided by AfSIS (http://www.isric.org/content/african-soilgrids-250m-geotiffs) for the depths, 0-5, 5-15, 15-30cm. This data is weighted so that the VS indicator is produced for topsoil (0-20cm) and units are converted to percent (%). Resolution is 250m, and projection is lambert equal area. 

## Tier 2/4
Two scripts are required to run this indicator (SH_T4_MergingEplotGPSwithLabdata.R & SH_T2_SH12_SH59_SH60.R) for eplots. A slightly modified separate script (SH_T2_SH59&SH60_FFS.R) is required to run this indicator for farm fields that pulls farm field soil lab data* once these data are available. The eplot script produces the output file (SH_T2_SH12_SH59_SH60.csv) and the farm field script produces the output file (SH_T4_FF_SH12_SH59_SH60.csv). In each file, the indicator is variable (SH12) and is associated with the required ID fields and GPS coordinates (applicable to eplots only).
*Note that there is no farm field data yet, so there is not an example of the second output file

# Soil Carbon Deficit Indicator (Unitless, 0-1)

## Tier 1
Two scripts are required to run this indicator (SoilHealthThread_Tier1_SoilCarbonDeficit_Analysis_sagcot.R & SoilHealthThread_Tier1_SoilCarbonDeficit_Analysis_gha.R). These each produce an output file (soil_carbon_deficit_gha.tif & soil_carbon_deficit_sagcot.tif)

These scripts depend on the output of the above topsoil carbon scripts that produce clipped and weighted topsoil carbon geotiffs in the correct units (gha_org_c.tif & sagcot_org_c.tif), as well as additional input data (pH, silt, and clay) provided by AfSIS (http://www.isric.org/content/african-soilgrids-250m-geotiffs) for the depths, 0-5, 5-15, 15-30cm. Each of these datasets are weighted and combined so that the VS indicator is produced for topsoil (0-20cm). Resolution is 250m, and projection is lambert equal area.

The carbon reference equation is based on Palm 2005, adjusted for depth of sample and method of pH analysis.

## Tier 2/4
Two scripts are required to run this indicator (SH_T4_MergingEplotGPSwithLabdata.R & SH_T2_SH12_SH59_SH60.R) for eplots. A slightly modified separate script (SH_T2_SH59&SH60_FFS.R) is required to run this indicator for farm fields that pulls farm field lab data*. The eplot script produces the output file (SH_T2_SH12_SH59_SH60.csv) and the farm field script produces the output file (SH_T4_FF_SH12_SH59_SH60.csv). In each file, the indicator is variable (SH60) and is associated with the required ID fields and GPS coordinates (applicable for eplots only).

The carbon reference equation is based on Palm 2005, adjusted for depth of sample and method of pH analysis.

*Note that there is no farm field soil lab data yet, so there is not an example of the second output file

# Soil Fertility Indicator (Unitless, 0-1)

## Tier 1
This indicator can not be run at the Tier 1 level until AfSIS releases P and S layers, likely to be available later this year (2015).

## Tier 2/4**
Two scripts are required to run this indicator (SH_T4_MergingEplotGPSwithLabdata.R & SH_T2_SH12_SH59_SH60.R) for eplots. A slightly modified separate script (SH_T2_SH59&SH60_FFS.R) is required to run this indicator for farm fields that pulls farm field soil lab data*. The eplot script produces the output file (SH_T2_SH12_SH59_SH60.csv) and the farm field script produces the output file (SH_T4_FF_SH12_SH59_SH60.csv). In each file, the indicator is variable (SH59) and is associated with the required ID fields and GPS coordinates (applicable for eplots only)**.

The critical values used to set the nutrient or property specific thresholds for the indicator come from Sanchez et al. 2003 and AfSIS 2011. Soil texture data from the ICRAF lab is used rather than the soil texture data from the CROPNUTS lab due to inconsistencies in the latter.

*Note that there is no farm field soil lab data yet, so there is not an example of the second output file

# Soil Physical Indicator (Unitless, 0-1)

## Soil Surface Indicator (Unitless, 0-.1)

### Tier 1
This indicator is not available at the Tier 1 level.

### Tier 2/4*
Two scripts are required to run this indicator (SH45) at the eplot and farm field scale (SH_T4_MergingEplotGPSwithLabdata.R & Soil_T2_SoilPhysicalIndicators.R).  For the eplot data, measurements are collected at the subplot scale and the mean value from these subplots is generated for the eplot. For farm fields, only one measurement is taken per field. The algorithm used is (Rooted Plants + Litter Cover +Downed Wood + Disturbed Soil + Microfloral Crust)/(
(Rooted Plants + Litter Cover +Downed Wood + Disturbed Soil + Microfloral Crust+ Undisturbed, Bare & Porous Soil + Undisturbed, bare but sealed soil+Sodic Soil+ Termite Mound). This script outputs two data files (SH_T2_T4_SH27_SH45_FFS.csv & SH_T2_T4_SH27_SH45_Eplot.csv). In the files, the indicator is variable (SH45) and is associated with the required ID fields and GPS coordinates (applicable only to eplots). 

## Soil Depth Indicator (Unitless, 0-.1)

### Tier 1
Two scripts are required to produce this VS Indicator (SH46) (Soil.T1.DepthIndicator.GHA.R & Soil.T1.DepthIndicator.TZA.R). The input data is provided by AfSIS (http://www.isric.org/content/african-soilgrids-250m-geotiffs) for the depths, 0-5, 5-15, 15-30. This data is weighted so that the VS indicator is produced for topsoil (0-20cm). Resolution is 250m, and projection is lambert equal area. The critical thresholds come from Sanchez et al. 2003. The scripts clip and resample the dataset, and produce two outputs (soil_depth_indicator_gha & soil_depth_indicator_sagcot.tif).

### Tier 2/4
Two scripts are required to run this indicator (SH46) at the eplot level (SH_T4_MergingEplotGPSwithLabdata.R & Soil_T2_SoilPhysicalIndicators.R).  Depth restriction is measured to 100cm at the SE corner of each eplot and to 50cm at the other three corners. This relies only on the data at the SE corner. This script outputs one data file (SH_T2_T4_SH46_Eplot.csv). In the file, the indicator is variable (SH46) and is associated with the required ID fields and GPS coordinates.

## Soil Erosion Indicator (Unitless, 0-.8)
The below factors are combined following the RUSLE model : R x K x LS x C x P in scripts (RUSLE_JoinTZA.R and RUSLE_JoinGHA.R).

### Tier 1
1. R factor
Erosivity (MJ mm ha-1h-1y-1) utilizes an Africa wide raster dataset (~25km res) by Vrieling et al. 2014 (http://www.sciencedirect.com/science/article/pii/S0921818114000265).
Two scripts are required to generate this component (Soil.T1.RainfallErosivity.GHA.R & Soil.T1.RainfallErosivity.TZA.R). They clip and resample the dataset, and produce two outputs (erosivity_gha & erosivity_sagcot.tif).

2. K factor
The erodibility component is generated following SWALIM/ FAO 2009, using the arithmetic mean particle size diameter (based on texture size classes from the ICRAF lab and an erodibility equation from Renard 1997. Two scripts are required to generate this component (Soil.T1.Erodibility.TZA.R & Soil.T1.Erodibility.GHA.R) These scripts depend on the clipped and weighted sand, silt and clay geotiffs that are outputted by the above topsoil carbon deficit scripts with the input data provided by AfSIS (http://www.isric.org/content/african-soilgrids-250m-geotiffs) for the depths, 0-5, 5-15, 15-30. Again, each of these datasets are weighted and combined so that the texture data is produced for topsoil (0-20cm). Resolution is 250m, and projection is lambert equal area. The scripts resample the dataset, process the above algorithms and produce two outputs (soil_erodibility_sagcot.tif & soil_erodibility_gha.tif).

3. LS factor
Four scripts are required to generate the LS factor (Soil.T1.Hydrobasins.R, Soil.T1.Slope&FlowDirection.TZA, Soil.T1.Slope&FlowDirection.GHA, Soil.T1.LSFactor.R) following the method in SWALIM/ FAO 2009. Input data is from Hydrobasins (Africa - Basin outlines, 30sec resolution (http://www.hydrosheds.org/download)) for catchment area, and SRTM DEM 90m resolution (http://www.ciesin.columbia.edu/afsis/bafsis_fullmap.htm) for slope and flow direction. The scripts clip and resample the datasets, generate slope (in degrees) and flow direction, and process the above algorithms and produce two outputs (Soil_LS_Factor_GHA.tif & Soil_LS_Factor_TZA.tif).

4. C factor
Two scripts are required to generate the C factor (Soil.T1.CFactor.TZA.R &  Soil.T1.CFactor.GHA.R). The data input is the available time series of cumulative fAPAR data (2003-2012) from the University of Maryland remote sensing products. The scripts clip the Tanzania data, resample and normalize both SAGCOT and Ghana and produce two outputs (C_factor_GHA.tif & C_factor_TZA.tif).  

5. P factor
XX scripts are required to generate the P factor.
This factor is based on national agriculture survey data and thus, is not available for Ghana.

### Tier 2/4
*Note Tier 2/4 Soil erosion indicator will be calculated to be used later for validation purposes. 

# Partial Nutrient Budget (Unitless, 0-1)

## Tier 1
One script is required to run this indicator (SH_TZALSMS_PartialNutrientIndicator.R). The script will use responses in Sections A, 2, 3, 4, and 5 of the Tanzania 2012-2013 Living Standards Measurement Survey, and will produce one output file (LSMS_TZAPartialNutrientIndicator.csv) that contains the country, household ID, plot number, cultivated crop, crop yield, overall nutrient budget indicator, and partial nutrient budget indicators for N, P, and K for SAGCOT region of Tanzania. 

LSMS observations are identified as being in SAGCOT using the latitude and longitude of enumeration areas is under development (which are offset for anonymization purposes and could possibly fall slightly outside of SAGCOT). 

There are currently a few cleaning steps (recoding plot numbers in section 2B and correcting misspellings in crop names from section 4A and 4B) included in the script that are identified by comments and allow for the retention of 50-100 observations that would otherwise be dropped. 

There are 4 reference tables used in this script from different sources; currently, they are called from the excel file (SoilHealthMatrix_12172014.xlsx). These tables include: 
1.) A table from with the nutrient removal factors for each crop from a 2004 FAO report (FAO - Food and Agriculture Organization of the UN. 2004. Scaling Soil Nutrient Balances: Enabling mesolevel applications for African realities. Publishing Management Service, Information Division, FAO, Rome, Italy. http://www.fao.org/docrep/008/y5749e/y5749e09.htm); 
2.) A table from TEAM with the nutrients added by different fertilizers (TEAM. Monitoring Protocol Soil Nutrient Balance (2011), pg 18); 
3.) A table from V. Smil 1999 (http://bioscience.oxfordjournals.org/content/49/4/299.full) with the harvest indices of several crop categories;
4.) A table that matches the crops listed in the Agriculture survey with the crop categories in tables (1) and (3).

This factor is based on national agriculture survey data and thus, is not available for Ghana.

## Tier 2/4

One script is required to run this indicator (SH_VS_PartialNutrientIndicator.R). The script will use responses in Sections 2,3,4 and 5 of the Vital Signs Agriculture Survey and will produce two output files (VS_GHAPartialNutrientIndicator.csv) and (VS_TZAPartialNutrientIndicator.csv) that contain the country, landscape, and mean and standard deviation for the overall nutrient budget indicator (SH58) for each landscape in Ghana and Tanzania respectively.

There are a few cleaning steps to remove duplicate observations in the current data snapshots that currently in the script; they are marked by comments and will not be necessary once the duplicated observations are removed from the data snapshots. There is also a step to drop those observations whose estimate of total plot area is listed as 0. When the data snapshots are updated to allow more precise values in these columns, this step may still be retained to exclude any observations which have a total plot area of 0 listed in error.

Observations lacking sufficient plot area information (i.e., do not have an estimate of field area size, the proportion of the field devoted to each crop, or both) are excluded. Observations that do not have a quantity of crop harvested listed are also excluded (this includes non-responses, as well as those plots which may be cultivated, but whose crops have not yet been harvested, plots that are in fallow, and plots where crops were cultivated, but destroyed before harvest). Observations with non-responses for certain questions (including use of crop residue and whether or not fertilizer was used) are also excluded. Under these criteria, roughly 219 (insufficient area information) + 217 (no crop harvest quantity) + 52 (incomplete management questions) for a total of 488 of the 1355 observations from the agricultural survey (for Ghana and Tanzania) are dropped.

The same four reference tables that are called to calculate the partial nutrient budget indicator for Tier 1 data are used here.
