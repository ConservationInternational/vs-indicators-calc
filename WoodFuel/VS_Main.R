#Generic framework for processing data, doing interpolations and resmapling various VS thread data.
#Here, The transformation and reformatting of required input data is the most time and computing 
#intensive component of all the VS threads. The actual execution of the models to produce the final 
#indices for each thread requires the least amount of code and computing time.
#The resampling, interpolation, reprojection and calculation of mean and total values from satellite 
#and other available datasets in different formats, resolutions and projections are applicable to all 
#threads.

#The notes below accompany the R-code developed and tested by the CSIR. Description, purpose, assumptions 
#and where applicable, alternative approaches or steps to derive input data required are provided.
#Water thread specifics are included where applicable. Although the Water thread lite is a simplified 
#and reduced water balance model the implementation of R-code to automate the generation of certain
#required input datasets is more complex. Where possible Vital Signs collected data sets are used but
#when such data are not yet available alternative external data sets are used. 
#The data and modeling framework is generic and accomodates the use of either Vital Signs or external data sets. 
#code and testing by M. Pienaar and M. van der Merwe. However, and small errors / logic defaults or bugs could still be updated in the future.
#The steps are largely in chronological order and should be implemented accordingly.

#***The code may still need optimisation and results need to be validated for all catchments. 
#Validation was only done for the Rufiji basin which overlaps with the SACGOT region in Tanzania.


#Step one: set the initial working directory and other directories, this will have to correspond to a directories on the implementing 
#computer. The following files should be put into these directores:
Sys.setenv(http_proxy="http://sanren-proxy.meraka.csir.co.za:3128")
initial.directory="/Users/private/Vital_signs";
scriptdirectory=paste(initial.directory,"/scripts/functions_v1.4.R",sep="");
AOIdirectory="/Users/private/Vital_signs/raw_data/shapefiles/tanzania";
AOIfile="tanzania";
java_resample_grid_function=paste(initial.directory,"/resamplegrid_java1.7",sep="");
java_resample_function=paste(initial.directory,"/resample_java1.7",sep="")
tableloc=paste(initial.directory,"/woodfueltable.csv",sep="");#woodfuel
tablelocwater=paste(initial.directory,"/watertable.csv",sep="");#water
tablelocclimate=paste(initial.directory,"/climateforcing.csv",sep="");#climate forcing
setwd(initial.directory);
#load the functions script stored in this directory. 
#This script contains all the functions or model parameters necessary to solve the threads
source(scriptdirectory);#load functions
#Step 1 is to create tables that list all the input variables needed and associated data files 
# The status of files being in their raw state (=0) or processed using this code (=1) is included.
#Processed files include being resampled, measurement units converted, summed values or mean values 
#for a specific time period. In any event, empty directories  associated with each variable in the 
#table are created when no raw or processed data files are available.
#The purpose of the table is to indicate the availibility of data and the status of it being processed 
# A data file for example total yearly rainfall which is used in multiple threads and is created from 
#the raw daily input files per year only needs to be processed once in one thread and the output can 
#then be used in other threads.

#note that after creating the tables (which will automatically check or create the associated directories) 
#All data files need to be placed in the correct directory associated with the thread of interest. 
#This implies that if a processed datafile for example yearly rainfall is used in multiple threads 
#the user should duplicate the processed data file(s) in the applicable thread directory(ies) or point to its location.


#Step 2 create the  tables describing the inputs for  specific thread using the function generate_WF_table (for wood fuel)
#and generate_WL_table() for water.

tableinputs=read.table(tableloc,header=T,sep=",");
tableinputs=as.data.frame(tableinputs);
tableinputs=initiate_table(tableinputs);

#Step 3. process the raw data or replace dummy variables with processed data 
#First get the AOI for each thread (returns a polygon representing the extent of the shapefile).
#here all calculations are done on 1km^2 grid representing the AOI and only at the very end can be clipped with different polygons or shapefiles.
#For these threads we are using Tanzania (the whole country) for all calculations.
Tanzania=aoi(AOIdirectory,AOIfile);
#create a  resample grid (1km^2) for the threads based on the AOI.
require(geosphere);#package to do geodesic operations
resamplegridparameter="meters";#replace with "degrees" to create a degree grid if desired
meterres=1000;#meters. This parameter tells us we will be creating a grid in meters (i.e 1km^2) using meterres = 10000
degreeres=destPoint(c(0,0),90,meterres)[1];#equivalent of meterres in degrees
resolution=meterres;#the default resolution of all grids (variables) in meters, swap with degreeres if wanting to create a grid in degrees
#the createresamplegrid function starts at the top left of the extent defined by AOI
#It then "walks" half the desired resolution at 90 degrees and half the resolution at 180 degrees to get the mid point of the first pixel returned latlon degrees
#It creates the grid by moving (in meters defeined by the resolution) at 90 degrees till the end of the entent
#then moves 180 down and repeats the process until the entire extent is covered (at each point returning the latlon coordinates at exactly 1000 meters apart) to a table called X and a table called Y
#Note that an extra row and col is generated for a buffer but is removed after processing
#There are 2 options to generate this grid. 1) use the java function (fast, except for read and write times between R and Java) or 2) the native R code (slow)
#here we run command through java (requires Java 1.7)
setwd(java_resample_grid_function)#this is where the .jar file is stored to create the grids.
#create a string command to parse to the system console.
command=paste("java -jar Create_resampling_grid.jar",ymax(extent(Tanzania)),xmin(extent(Tanzania)),ymin(extent(Tanzania)),xmax(extent(Tanzania)),'-180','90', resolution,resamplegridparameter);
#run the command (this will write out two tables X.txt and Y.txt) in the current working directory (set 2 lines above)
system(command);
#read in the outputs (X.txt and Y.txt from the java program) and attach them to the variable resamplegrid
resamplegridx=as.matrix(read.table("X.txt",sep=",",header=F));
resamplegridy=as.matrix(read.table("Y.txt",sep=",",header=F));
#read in  the other parameters as well in case needed
x=resamplegridx;
y=resamplegridy;
xbl =as.matrix(read.table("Xbl.txt",sep=",",header=F));
xbr =as.matrix(read.table("Xbr.txt",sep=",",header=F));
xtl =as.matrix(read.table("Xtl.txt",sep=",",header=F));
xtr =as.matrix(read.table("Xtr.txt",sep=",",header=F));
ybl =as.matrix(read.table("Ybl.txt",sep=",",header=F));
ybr =as.matrix(read.table("Ybr.txt",sep=",",header=F));
ytl =as.matrix(read.table("Ytl.txt",sep=",",header=F));
ytr =as.matrix(read.table("Ytr.txt",sep=",",header=F));
resamplegrid=list(X= resamplegridx,Y= resamplegridy);
resamplematrix=resamplegrid$X;#generate a matrix to use for a dummy variable later on.
#transfer the grid parameters 
creategridparamters(tableinputs[length(tableinputs[,1]),]);
#resamplegrid=createresamplegrid(Tanzania,1000,90,180) #note we use 180 instead of -180 for r funtion
#Create all required inputs to match the resample grid and populate with random numbers for now 
#(representing the assumed ranges for each variable, we swap these out later if the actual data exists.)
#The following variables are for woodfuel:
eplotmass=populateresamplegrid(resamplematrix,1,300);#WF12 t/ha from VS Eplot measurements
treeheight=populateresamplegrid(resamplematrix,1,40);#WF13 meters (ICESAT)
treecover=populateresamplegrid(resamplematrix,1,75);#WF14 percent cover from Hansen 2000 layers.
rainfall=populateresamplegrid(resamplematrix,10,2000);#WF15 mm (VS RFE data)
meanrainfall=populateresamplegrid(resamplematrix,1,1200);#WF15 mean 50 year rainfall (worldclim)
meanstemdiameter=populateresamplegrid(resamplematrix,10,10);#WF15 mean tree diameters in cm (can try reverse engineer this from tree height later on)
woodconsumption=populateresamplegrid(resamplematrix,1,1);#WF16 Total tons/capita/year (2010 for now)
population=populateresamplegrid(resamplematrix,1,200);#WF17 derived from Afripop
#Run through each variable using a check and process function to swap out data if it exists, 
#or to process raw inputs, or else to leave default values in place. if the raw data is not available.
#for each function we parse the appropriate line from the tables to tell it where to find raw or processed inputs. 
eplotmass=checkorcreate_eplotmass(eplotmass,tableinputs,1);#eplotmass



#checkorprocess_grid is the most common processing function and can be used for a majority of inputs.
#the parameters are:
#1 grid - the empty grid to process
#2 tableline e.g. woodfuelinputs[2,] represents the second row from the woodfuelinputs table and holds all the information to find raw inputs.
#3 java (boolean) - tells the function to use java resampling (fast) or if set to F to use native R code.
#4 AOI - a shape file representing the AOI
#5 agg - used to determine whether the resampling into the 1km^2 grids should be aggregated/disaggregated 
#(depending on the resolution of the raw data in relation to the resmaple grid - this is determine automatically
#by the function) or if set to "false" will result in a direct fractional weighting. 
#Note, that we use the terms "true" or "false" (for java purposes rather than TRUE or FALSE as in R.
#6 agg2 - this is done prior to resmapling using the aggregate function to scale up finer resolution dataset (for speed).
# if T, then aggamount and aggfunction will be used, else they are ignored and no aggregation is done prior to resampling.
#7 aggamount - the agg amount (see point above)
#8 the aggregation function ("mean" or "sum").
#9 resolution - the resolution of the resample grid in meters
# parseraster - only used in other functions (leave F) not in the checkorprocess_grid function
#WF13 Tree height 
treeheight=checkorprocess_grid(treeheight,tableinputs[2,],java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=F);
#WF14 treecover 
treecover=checkorprocess_grid(treecover,tableinputs[3,],java=T,Tanzania,agg="false",agg2=T,aggamount=33,aggfunction="mean",resolution,parseraster=F);
#checkandprocess_rainfall function.
#all the parameters are the same as in checkorprocess_grid described above, but parseraster must be set to true
#so that internal function calls can be made. (this is for inputs that comprise multiple rasters and need to be 
#summed eg daily rainfall to yearly rainfall)
#WF15 Rainfall 
rainfall=checkandprocess_rainfall(rainfall,tableinputs[4,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
#checkorprocess_worldclimgrid function.
#all the parameters are the same as in checkandprocess_rainfall described above,but used specifically for worldclim data (monthly to yearly)
#WF15 meanrainfall (need to do some pre-processing first)
meanrainfall=checkorprocess_worldclimgrid(meanrainfall,tableinputs[5,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=T);
meanstemdiameter=checkorprocess_grid(meanstemdiameter,tableinputs[6,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
woodconsumption=checkorprocess_grid(woodconsumption,tableinputs[7,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
#WF17 population 
population=checkorprocess_grid(population,tableinputs[8,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
#update table
tableinputs=initiate_table(tableinputs);
updatetable(tableloc,tableinputs);
#Run woodfuel model
#run models
#WF20 (we skip this as the processing has already been done above)
#WF21 (area based allometry)
#treeheight=treeheight[-nrow(treeheight),-ncol(treeheight)];
#treecover=treecover[-nrow(treecover),-ncol(treecover)];
#reset all rasters to matrices for calculation purposes
#nrow=1187 ncol=1237
eplotmass=as.matrix(eplotmass);#
treeheight=as.matrix(treeheight);#
treecover=as.matrix(treecover);#
rainfall=as.matrix(rainfall);#
meanrainfall=as.matrix(meanrainfall);#
meanstemdiameter=as.matrix(meanstemdiameter);#
woodconsumption=as.matrix(woodconsumption);#
population=as.matrix(population);#

BCFbiome = 23.50 #t biomass/ha.
AGBwoody = ((treeheight * (treecover/100)) * BCFbiome)  *100#t/km^2. WF21 


#WF22 Tree Production
#we first need to derive max tree cover (using nature paper formula
#derive the maxtreecover from mean50rainfall (Sankaran et al, 2005 (nature) regression)
maxtreecover=meanrainfall;
#get rid of NA values
maxtreecover[is.na(maxtreecover)]=0
meanrainfall[is.na(meanrainfall)]=0
#set maxtreecover
maxtreecover[meanrainfall[] < 100] <- 1;
#mid range max treecover
midrange=(0.1436*(maxtreecover[meanrainfall[] >= 100 & meanrainfall[] <= 650])-13.364);
maxtreecover[meanrainfall[] >= 100 & meanrainfall[] <= 650] = midrange;
#max treecover (set to 80%)
maxtreecover[meanrainfall[] > 650] <- 80;
plot(raster(maxtreecover));
#calculate woodproduction
woodproduction=((AGBwoody)*(0.01+(1-treecover/maxtreecover)*(0.42*exp(-0.29*meanstemdiameter))*((1+0.26*(rainfall-meanrainfall)/maxtreecover))));
woodproduction[woodproduction < 0] <- 0;
#woodproduction is in t/km^2
percentyield=(woodproduction/AGBwoody*100);
plot(raster(woodproduction));
#distribute wood consumption / capita using population
woodconsumption=population*woodconsumption;#0.6 t/capita/km^2*population/km^2;
#bring in difference between wood purchased and sold when a value becomes available
#woodpurchasedsold=populateresamplegrid(resamplematrix,5000,5000);#number of tons/year a single value of exported wood + (other purposes eg construction) - wood bought (imported from outside country) chacoal production production and consumption not accounted for
#woodpurchasedsold=woodpurchasedsold[-nrow(woodpurchasedsold),-ncol(woodpurchasedsold)]
#Woodsecurity=(woodproduction +woodpurchasedsold) -(woodconsumption);
Woodsecurity=woodproduction -(woodconsumption);#the index is only based on wood produced vs wood consumed
hist(Woodsecurity);
sum(Woodsecurity,na.rm=T);

plot(raster(Woodsecurity),zlim=c(-700,7000))#
setwd(initial.directory)
if(file.exists("woodsecuritylatlong.txt")){
  Woodsecuritylatlong=as.matrix(read.table("woodsecuritylatlong.txt",header=F,sep=","));
  Woodsecuritylatlong=raster(Woodsecuritylatlong);
  }else
{
#resmaple woodsecurity back to lat long coords (we use java 1.8 - to run in parallel)
Woodsecuritylatlong = resampleback(Woodsecurity,tableinputs[length(tableinputs[,1]),],resolution,Tanzania);
#crop out lakes and oceans with a shapefile if desired
setwd(initial.directory)
write.table(as.matrix(Woodsecuritylatlong),file="woodsecuritylatlong.txt",row.names=F,col.names=F,sep=",");
#try write out the raster
writeRaster(Woodsecuritylatlong, filename=dir, format="GTiff", overwrite=TRUE);

}
aoi <- readOGR(AOIdirectory, AOIfile);
#crop out lakes and oceans with a shapefile if desired
extent(Woodsecuritylatlong)=extent(aoi);
#Woodsecuritylatlong=crop(Woodsecuritylatlong,aoi);
plot(Woodsecuritylatlong);


#################################################################
#End of woodfuel


#################################################################
#Climate forcing

tableinputs=read.table(tablelocclimate,header=T,sep=",");
tableinputs=as.data.frame(tableinputs);
tableinputs=initiate_table(tableinputs);
#Step 3. process the raw data or replace dummy variables with processed data 
#First get the AOI for each thread (returns a polygon representing the extent of the shapefile).
#here all calculations are done on 1km^2 grid representing the AOI and only at the very end can be clipped with different polygons or shapefiles.
#For these threads we are using Tanzania (the whole country) for all calculations.
Tanzania=aoi(AOIdirectory,AOIfile);
#create a  resample grid (1km^2) for the threads based on the AOI.
require(geosphere);#package to do geodesic operations
resamplegridparameter="meters";#replace with "degrees" to create a degree grid if desired
meterres=1000;#meters. This parameter tells us we will be creating a grid in meters (i.e 1km^2) using meterres = 10000
degreeres=destPoint(c(0,0),90,meterres)[1];#equivalent of meterres in degrees
resolution=meterres;#the default resolution of all grids (variables) in meters, swap with degreeres if wanting to create a grid in degrees
#the createresamplegrid function starts at the top left of the extent defined by AOI
#It then "walks" half the desired resolution at 90 degrees and half the resolution at 180 degrees to get the mid point of the first pixel returned latlon degrees
#It creates the grid by moving (in meters defeined by the resolution) at 90 degrees till the end of the entent
#then moves 180 down and repeats the process until the entire extent is covered (at each point returning the latlon coordinates at exactly 1000 meters apart) to a table called X and a table called Y
#Note that an extra row and col is generated for a buffer but is removed after processing
#There are 2 options to generate this grid. 1) use the java function (fast, except for read and write times between R and Java) or 2) the native R code (slow)
#here we run command through java (requires Java 1.7)
setwd(java_resample_grid_function)#this is where the .jar file is stored to create the grids.
#create a string command to parse to the system console.
command=paste("java -jar Create_resampling_grid.jar",ymax(extent(Tanzania)),xmin(extent(Tanzania)),ymin(extent(Tanzania)),xmax(extent(Tanzania)),'-180','90', resolution,resamplegridparameter);
#run the command (this will write out two tables X.txt and Y.txt) in the current working directory (set 2 lines above)
system(command);
#read in the outputs (X.txt and Y.txt from the java program) and attach them to the variable resamplegrid
resamplegridx=as.matrix(read.table("X.txt",sep=",",header=F));
resamplegridy=as.matrix(read.table("Y.txt",sep=",",header=F));
#read in  the other parameters as well
x=resamplegridx;
y=resamplegridy;
xbl =as.matrix(read.table("Xbl.txt",sep=",",header=F));
xbr =as.matrix(read.table("Xbr.txt",sep=",",header=F));
xtl =as.matrix(read.table("Xtl.txt",sep=",",header=F));
xtr =as.matrix(read.table("Xtr.txt",sep=",",header=F));
ybl =as.matrix(read.table("Ybl.txt",sep=",",header=F));
ybr =as.matrix(read.table("Ybr.txt",sep=",",header=F));
ytl =as.matrix(read.table("Ytl.txt",sep=",",header=F));
ytr =as.matrix(read.table("Ytr.txt",sep=",",header=F));
resamplegrid=list(X= resamplegridx,Y= resamplegridy);
resamplematrix=resamplegrid$X;#generate a matrix to use for a dummy variable later on.
#transfer the grid parameters 
creategridparamters(tableinputs[length(tableinputs[,1]),]);

#resamplegrid=createresamplegrid(Tanzania,1000,90,180) #note we use 180 instead of -180 for r funtion
#Create all required inputs to match the resample grid and populate with random numbers for now 
#(representing the assumed ranges for each variable, we swap these out later if the actual data exists.)
#The following variables are for woodfuel:
AGBwoody=populateresamplegrid(resamplematrix,1,4000);# t/km^2 from VS woodfuel
AGBwoodycarbon=populateresamplegrid(resamplematrix,1,2000);# t/km^2 from VS woodfuel
soilcarbon1m=populateresamplegrid(resamplematrix,50,300);#t/km^2 to a depth of 1m (multiply code below by 100)
soilcarbon30cm=populateresamplegrid(resamplematrix,50,300);#t/km^2 to a depth of 1m (multiply code below by 100)
builtenvironcarbonstock=populateresamplegrid(resamplematrix,50,300);#default value from either landcover map or literature
cultivatedarea=populateresamplegrid(resamplematrix,50,300);#proporton of area under cultivation per pixel. derive total carbon in 0-30cm horizon for pixel 
carbonloss=populateresamplegrid(resamplematrix,50,300);
#run model
#get the above ground woody biomass
#checkorprocess_grid is the most common processing function and can be used for a majority of inputs.
#the parameters are:
#1 grid - the empty grid to process
#2 tableline e.g. woodfuelinputs[2,] represents the second row from the woodfuelinputs table and holds all the information to find raw inputs.
#3 java (boolean) - tells the function to use java resampling (fast) or if set to F to use native R code.
#4 AOI - a shape file representing the AOI
#5 agg - used to determine whether the resampling into the 1km^2 grids should be aggregated/disaggregated 
#(depending on the resolution of the raw data in relation to the resmaple grid - this is determine automatically
#by the function) or if set to "false" will result in a direct fractional weighting. 
#Note, that we use the terms "true" or "false" (for java purposes rather than TRUE or FALSE as in R.
#6 agg2 - this is done prior to resmapling using the aggregate function to scale up finer resolution dataset (for speed).
# if T, then aggamount and aggfunction will be used, else they are ignored and no aggregation is done prior to resampling.
#7 aggamount - the agg amount (see point above)
#8 the aggregation function ("mean" or "sum").
#9 resolution - the resolution of the resample grid in meters
# parseraster - only used in other functions (leave F) not in the checkorprocess_grid function
#WF13 Tree height 
treeheight=checkorprocess_grid(treeheight,tableinputs[1,],java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=F);
#WF14 treecover 
treecover=checkorprocess_grid(treecover,tableinputs[2,],java=T,Tanzania,agg="false",agg2=T,aggamount=33,aggfunction="mean",resolution,parseraster=F);
BCFbiome = 23.50 #t biomass/ha.
AGBwoody = ((treeheight * (treecover/100)) * BCFbiome)  *100#t/km^2. WF21 
AGBwoodycarbon=(AGBwoody+(AGBwoody*.3))*0.47;#this is to get the total above and below woody carbon (about 30% is below ground). The carbon factor is about 47%
#soilcarbon30cm
soilcarbon30cm=checkorprocess_soilgrid30cm(soilcarbon30cm,tableinputs,java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=T);
soilcarbon1m=checkorprocess_soilgrid1m(soilcarbon1m,tableinputs,java=T,Tanzania,agg="false",agg2=F,aggamount=1,aggfunction="mean",resolution,parseraster=T);
carbonloss=soilcarbon30cm * cultivatedarea * 0.5;
Totalsoilcarbon= (soilcarbon1m)-carbonloss;
Totalcarbon=AGBwoodycarbon+Totalsoilcarbon;
####still need to finish and figure out how to get a flux value??

#################################################################
#water thread

#Step 2 create the  tables describing the inputs for  specific thread using the function generate_WF_table (for wood fuel)
#and generate_WL_table() for water.

tableinputs=read.table(tablelocwater,header=T,sep=",");
tableinputs=as.data.frame(tableinputs);
tableinputs=initiate_table(tableinputs);
#transfer the grid parameters #defined by the woodfuel thread above
creategridparamters(tableinputs[length(tableinputs[,1]),]);
#process all the water thread inputs - the following variables are created and processed in the same way as the woodfuel ones above.
rainfall2001=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2002=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2003=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2004=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2005=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2006=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2007=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2008=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2009=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
rainfall2010=populateresamplegrid(resamplematrix,10,2000);#mm/year, daily rainfall summed to yearly value
Ep2001=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2002=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2003=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2004=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2005=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2006=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2007=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2008=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2009=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
Ep2010=populateresamplegrid(resamplematrix,0,1000);#w/m^2/year, daily reference evaporation summed to yearly value
population=populateresamplegrid(resamplematrix,1,200);# This has to be filled with proper pop disaggreated as the random numers are equally distributed.
livestock_cattle=populateresamplegrid(resamplematrix,1,300);# animals 2006
livestock_sheep=populateresamplegrid(resamplematrix,1,150);# animals 2006
livestock_goats=populateresamplegrid(resamplematrix,1,200);# animals 2006
livestock_pigs=populateresamplegrid(resamplematrix,1,80);# animals 2006
livestock_chickens=populateresamplegrid(resamplematrix,1,2000);# animals 2006; this needs to be divided by 100 to calculate water consumtpion [m3/100 animals per year] in water demand model
Irrigation_area_equiped=populateresamplegrid(resamplematrix,0,1);#area/km^2 2008 used to calculate with [%] the actual area irrigated
Irrigation_area_actual_equiped=populateresamplegrid(resamplematrix,0,100);#% 
cropland_area=populateresamplegrid(resamplematrix,0,1);#area/km^2
sediment_yield=populateresamplegrid(resamplematrix,0,20);#t/km^2
#process the files
rainfall2001=checkandprocess_rainfall(rainfall2001,tableinputs[1,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2002=checkandprocess_rainfall(rainfall2002,tableinputs[2,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2003=checkandprocess_rainfall(rainfall2003,tableinputs[3,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2004=checkandprocess_rainfall(rainfall2004,tableinputs[4,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2005=checkandprocess_rainfall(rainfall2005,tableinputs[5,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2006=checkandprocess_rainfall(rainfall2006,tableinputs[6,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2007=checkandprocess_rainfall(rainfall2007,tableinputs[7,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2008=checkandprocess_rainfall(rainfall2008,tableinputs[8,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2009=checkandprocess_rainfall(rainfall2009,tableinputs[9,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
rainfall2010=checkandprocess_rainfall(rainfall2010,tableinputs[10,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2001=checkandprocess_rainfall(Ep2001,tableinputs[11,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2002=checkandprocess_rainfall(Ep2002,tableinputs[12,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2003=checkandprocess_rainfall(Ep2003,tableinputs[13,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2004=checkandprocess_rainfall(Ep2004,tableinputs[14,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2005=checkandprocess_rainfall(Ep2005,tableinputs[15,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2006=checkandprocess_rainfall(Ep2006,tableinputs[16,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2007=checkandprocess_rainfall(Ep2007,tableinputs[17,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2008=checkandprocess_rainfall(Ep2008,tableinputs[18,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2009=checkandprocess_rainfall(Ep2009,tableinputs[19,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);
Ep2010=checkandprocess_rainfall(Ep2010,tableinputs[20,],java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T);

population=checkorprocess_grid(population,tableinputs[21,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
livestock_cattle=checkorprocess_grid(population,tableinputs[22,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
livestock_sheep=checkorprocess_grid(population,tableinputs[23,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
livestock_goats=checkorprocess_grid(population,tableinputs[24,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
livestock_pigs=checkorprocess_grid(population,tableinputs[25,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
livestock_chickens=checkorprocess_grid(population,tableinputs[25,],java=T,Tanzania,agg="true",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
Irrigation_area_equiped=checkorprocess_grid(Irrigation_area_equiped,tableinputs[26,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
Irrigation_area_actual_equiped=checkorprocess_grid(Irrigation_area_actual_equiped,tableinputs[27,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
cropland_area=checkorprocess_grid(cropland_area,tableinputs[28,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
sediment_yield=checkorprocess_grid(cropland_area,tableinputs[28,],java=T,Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution,parseraster=F);
########################
#update table
tableinputs=initiate_table(tableinputs);
updatetable(tablelocwater,tableinputs);#water thread

#run water model
#1. first we calculate discharge (supply) in km^3
alpha_tune=2.8;
discharge2001=discharge(rainfall2001,Ep2001,alpha_tune);
discharge2002=discharge(rainfall2002,Ep2002,alpha_tune);
discharge2003=discharge(rainfall2003,Ep2003,alpha_tune);
discharge2004=discharge(rainfall2004,Ep2004,alpha_tune);
discharge2005=discharge(rainfall2005,Ep2005,alpha_tune);
discharge2006=discharge(rainfall2006,Ep2006,alpha_tune);
discharge2007=discharge(rainfall2007,Ep2007,alpha_tune);
discharge2008=discharge(rainfall2008,Ep2008,alpha_tune);
discharge2009=discharge(rainfall2009,Ep2009,alpha_tune);
discharge2010=discharge(rainfall2010,Ep2010,alpha_tune);

#demand
#human demand
human_consump=7.3;#l/m^2
human_consump=12.5;#l/m^2 (however this is a range between 5.5 to 20.1)
population*(human_consum/1000000000);#l/km^3/year/person
livestock_cattle*(7.3/1000000000);#l/km^3/year/person
#etc... then add them all wrtie a function to do this.

#Irrigation.

#......








