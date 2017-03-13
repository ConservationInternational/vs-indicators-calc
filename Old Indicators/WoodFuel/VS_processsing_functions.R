################################# CI   VITAL SIGNS PROCESSING  #######################################
##################################  GRID GENERATION CHECK #############################################
#This script collects functions for processing of data in the CI Vital Signs project.
#This was new created to simplify to workflow from CSIR by Benoit Parmentier.
#
#AUTHORS: Marc Pienaar, Marna van der werve,Benoit Parmentier                                                                      #
#DATE CREATED: 10/05/2015 
#DATE MODIFIED: 11/02/2015
#
#COMMENTS: Tested but can be improved!!
#TO DO: - Add functions to process eplot and separate functions script used for processing spatial data inputs from
#       the actual computation of indices. This is important to allow clear identification of parameters for validation
#       and allow for subsequent input changes later on in the production!
#
#PROJECT: CI VS, CSIR 
#
##################################################################################################
#
###Loading r library and packages

library(raster)                             # loading the raster package
library(gtools)                             # loading ...
library(sp)                                 # spatial objects in R
library(gplots)                             # 
library(rgdal)                              # gdal driver for R
library(RColorBrewer)                       # color scheme, palettes used for plotting
library(gdata)                              # read different format (including .xlsx)
library(plotrix)                            # plot options and functions including plotCI
library(rasterVis)                          # raster visualization
library(colorRamps)                         # contains matlab.like palette
library(zoo)                                # time series objects and methods
library(xts)                                # extension of time series objects
library(gfcanalysis)                        # Hansen dataset...
library(geosphere)                          # curved surface and geodetic trigonometric

#################################################
###### Functions  used in the script  ##########

#This function creates a spatial polygon data frame object for the extent matching a raster input
create_polygon_from_extent<-function(reg_ref_rast,outDir=NULL,outSuffix=NULL){
  #This functions returns polygon sp from input rast
  #Input Arguments: 
  #reg_ref_rast: input ref rast
  #outDir : output directory, if NULL then the current dir in used
  #outSuffix: output suffix used for the naming of the shapefile
  #Output: 
  #reg_outline_poly: spatial polygon data.frame
  #
  if(is.null(outDir)){
    outDir=getwd()
  }
  if(is.null(outSuffix)){
    outSuffix=""
  }
  ref_e <- extent(reg_ref_rast) #extract extent from raster object
  reg_outline_poly <- as(ref_e, "SpatialPolygons") #coerce raster extent object to SpatialPolygons from sp package 
  reg_outline_poly <- as(reg_outline_poly, "SpatialPolygonsDataFrame") #promote to spdf
  proj4string(reg_outline_poly) <- projection(reg_ref_rast) #Assign projection to spdf
  infile_reg_outline <- paste("reg_out_line_",out_suffix,".shp",sep="") #name of newly crated shapefile with the extent
  writeOGR(reg_outline_poly,dsn= outDir,layer= sub(".shp","",infile_reg_outline), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
  
  return(reg_outline_poly) #return spdf
}


#Function to aggregate from fine to coarse resolution, this will change accordingly once the input raster ref is given..
#
aggregate_raster <- function(reg_ref_rast,agg_fact,r_in,agg_fun="mean",out_suffix=NULL,file_format=".tif",out_dir=NULL){
  #Aggregate raster from raster input and reference file
  #INPUT arguments:
  #agg_fact: factor to aggregate
  #agg_fun: default is mean
  #out_suffix: output suffix
  #file_Format: raster format used e.g. .tif
  #out_dir: output directory
  #OUTPUT:
  # raster_name: name of the file containing the aggregated raster
  #
  # Authors: Benoit Parmentier
  # Created: 10/15/2015
  # Modified: 11/01/2015
  # To Do: 
  # - Add option to disaggregate
  #
  ################################
  
  if(is.null(agg_fact)){
    res_ref <- res(reg_ref_rast)[1] #assumes square cells, and decimal degrees from WGS84 for now...
    res_in <- res(r_in)[1] #input resolution, assumes decimal degrees
    agg_fact <-round(res_ref/res_in) #find the factor needed..
    #fix this to add other otpions e.g. aggregating down
  }
  
  #Default values...
  if(is.null(out_suffix)){
    out_suffix <- ""
  }
  
  if(is.null(out_dir)){
    out_dir <- "."
  }
  
  raster_name <- file.path(out_dir,paste("r_agg_",agg_fact,out_suffix,file_format,sep="")) #output name for aster file
  r_agg <- aggregate(r_in, fact=agg_fact,FUN=agg_fun,filename=raster_name,overwrite=TRUE)

  return(raster_name)
  
}

### This is a very general function to process raster to match a raster of reference,
create__m_raster_region <-function(j,list_param){
  #This processes a list of raster to match to a region of interest defined by a reference raster
  #INPUT Arguments: raster name of the file,reference file with
  # j: file to be processed with input parameters
  # raster_name: list of raster to process i.e. match the region of interest
  # reg_ref_rast: reference raster used to defined the region of interest and spatial parameters
  # out_rast_name: output raster name, if NULL then use out_suffix to the input name to generate output name
  # agg_param: aggregation parameters: this is a vector used in in the aggregate function. It has three items:
  #                                     -TRUE/FALSE: if true then aggregate
  #                                     -agg_fact: aggregation factor, if NULL compute on the fly
  #                                     -agg_fun: aggregation function to use, the default is mean
  # file_format: output format used in the raster e.g. .tif, .rst
  # NA_flag_val: flag value used for no data
  # input_proj_str: defined projection,default null in which case it is extract from the input raster
  # out_suffix : output suffix added to output names if no output raster name is given
  # out_dir:  <- list_param$out_dir
  # Output: spatial grid data frame of the subset of tiles
  #
  # Authors: Benoit Parmentier
  # Created: 10/01/2015
  # Modified: 11/01/2015
  #TODO:
  # - Add option to disaggregate...
  # - Modify agg param to be able to use different ones by file j for the mcapply function
  #
  ################################################
  ## Parse input arguments
  raster_name <- list_param$raster_name[[j]] #list of raster ot project and crop, this is a list!!
  reg_ref_rast <- list_param$reg_ref_rast #This must have a coordinate system defined!!
  out_rast_name <- list_param$out_rast_name[j] #if NULL then use out_suffix to add to output name
  agg_param <- list_param$agg_param #TRUE,agg_fact,agg_fun
  file_format <- list_param$file_format #.tif, .rst
  NA_flag_val <- list_param$NA_flag_val #flag value used for no data
  input_proj_str <- list_param$input_proj_str #default null?
  out_suffix <- list_param$out_suffix
  out_dir <- list_param$out_dir
  
  ## Start #
  
  ## Create raster object if not already present
  if(class(raster_name)!="RasterLayer"){
    layer_rast<-raster(raster_name)
  }else{
    layer_rast <- raster_name
    raster_name <- filename(layer_rast)
  }
  
  ## Create output raster name if out_rast_name is null
  if(is.null(out_rast_name)){
    extension_str <- extension(raster_name)
    raster_name_tmp <- gsub(extension_str,"",basename(raster_name))
    out_rast_name <- file.path(out_dir,paste(raster_name_tmp,"_crop_proj_reg_",out_suffix,file_format,sep="")) #for use in function later...
  }
  
  ## Get the input raster projection information if needed
  if(is.null(input_proj_str)){
    input_proj_str <-projection(layer_rast)   #Extract current coordinates reference system in PROJ4 format
  }else{
    projection(layer_rast) <- input_proj_str #assign projection info
  }
  region_temp_projected <- projectExtent(reg_ref_rast,CRS(input_proj_str))     #Project from ref to current region coord. system
  
  layer_crop_rast <- crop(layer_rast, region_temp_projected) #crop using the extent from the region tile
  #layer_projected_rast<-projectRaster(from=layer_crop_rast,crs=proj4string(reg_outline),method="ngb")
  if(agg_param[1]==TRUE){
    agg_fact <- as.numeric(agg_param[2]) #in case we have a string/char type
    agg_fun <- agg_param[3]
    #debug(aggregate_raster)
    r_agg_raster_name <- aggregate_raster(reg_ref_rast, #reference raster with the desired resolution
                             agg_fact=agg_fact, #given aggregation factor
                             r_in=layer_crop_rast, #raster to be aggregated
                             agg_fun="mean", #aggregation function
                             out_suffix=out_suffix,
                             file_format=".tif",
                             out_dir=out_dir)
    layer_crop_rast <- raster(r_agg_raster_name)
  }
  
  layer_projected_rast <- projectRaster(from=layer_crop_rast,to=reg_ref_rast,method="ngb",
                                        filename=out_rast_name,overwrite=TRUE)
  
  #Need cleanup of tmp files here!!! building up to 19gb!
  removeTmpFiles(h=0)

  return(out_rast_name)
}

plot_to_file <- function(raster_name,res_pix=480,out_suffix=NULL,out_dir=NULL){
  #Quick utility function to plot raster to png file for the workflow
  #This is useful to visually check the outputs from the workflow.
  #INPUT arguments:
  #raster_name: input raster object or file name of the raster object
  #out_suffix: output suffix
  #out_dir: output directory
  #OUTPUT:
  # png_file: name of the file containing the figure/plot
  #
  # Authors: Benoit Parmentier
  # Created: 11/01/2015
  # Modified: 11/02/2015
  # To Do: 
  # - Add option to choose plot format e.g. jpeg, tif etc.
  #
  ################################
  ## Create raster object if not already present
  if(class(raster_name)!="RasterLayer"){
    layer_rast<-raster(raster_name)
  }else{
    layer_rast <- raster_name
    raster_name <- filename(layer_rast)
  }
  
  if(is.null(out_suffix)){
    out_suffix <- ""
  }
  if(is.null(out_dir)){
    out_dir <- getwd()
  }
  
  #Extract name
  extension_str <- extension(raster_name)
  raster_name_tmp <- gsub(extension_str,"",basename(raster_name))
  
  res_pix <- 480
  col_mfrow <- 1
  row_mfrow <- 1
  #Might change file format later...
  png_file <- file.path(out_dir,paste("figure_",raster_name_tmp,"_",out_suffix,".png",sep="")) #may be changed later to other format
  png(filename=png_file,
      width=col_mfrow*res_pix,height=row_mfrow*res_pix)
  plot(layer_rast)
  title(raster_name_tmp)
  dev.off()
  return(png_file)
}

#### Functions from earlier workflow by Marc Pienaar..still to modify!!

#function to randomly populate a resample grid using a range for testing purposes until actual values can be processed
populateresamplegrid=function(grid, minval, maxval)
{
  output=grid;
  output[]=runif(length(grid),minval,maxval);
  return(output);
}

### Functions to process eplot

#######################
#######################
#the allometry is taken from Chave et al. 2005 and Nickless et al (e=-2.235, and c=0.916)
stem_allometry=function(Diam,height,bro,exponent=-2.235,coeff=0.916 )
{
  #bro is the stem density (specific to tree species)
  #Height is the tree height in m
  #Diam = tree diamter in cm
  StemMass=exp(exponent+(coeff*log(Diam^2*height*bro)));
  return(StemMass);
}


#######################
#Function to process eplot data 
processEplots=function(eplotmass, rawinput,AUXfile,upperthreshold,lowerthreshold)
{
  x<- read.csv(file=rawinput, header=TRUE,stringsAsFactors=FALSE);
  #Do some basic data filtering
  #1) Rename / fix landscape names
  index=grep("landscape_real",colnames(x));
  x[grep("3",x[,index]), index]="L03";
  x[grep("6",x[,index]), index]="L06";
  x[grep("10",x[,index]), index]="L10";
  x[grep("11",x[,index]), index]="L11";
  x[grep("18",x[,index]), index]="L18";
  x[grep("19",x[,index]), index]="L19";
  x[grep("20",x[,index]), index]="L20";
  #2. get long coords to correct country names
  x[,grep("metadata.meta_display.country",colnames(x))][which(x[,grep("long",colnames(x))[1]]>20)]="TZA";
  x[,grep("metadata.meta_display.country",colnames(x))][which(x[,grep("long",colnames(x))[1]]>20)]="TZA";
  x[,grep("metadata.meta_display.country",colnames(x))][which(x[,grep("long",colnames(x))[1]]<20)]="GHA";
  #Create an output table
  OUTPUT=matrix(data=NA,ncol=27);
  # x[,grep("lat",colnames(x))[1]] #lat coords for eplot
  # x[,grep("long",colnames(x))[1]] #long coords for eplot
  
  for(i in 1:length(unique(x[, grep("landscape_real",colnames(x))])))
  {
    #get all eplot rows within i
    TEMP= x[which(x[,index]==unique(x[, grep("landscape_real",colnames(x))])[i]),];
    n1=0;
    #get n samples (eplots) per landscape
    N=length(TEMP[,1]);
    for(j in 1:N)#loop for each eplot
    {
      #Get subplot radius
      radius = as.numeric(TEMP[,grep("radius", colnames(TEMP))[1]][j])#radius
      #subplot area
      subplot_area=pi*radius^2;
      long=TEMP[,grep("long", colnames(TEMP))[1]][j];
      lat=TEMP[,grep("lat", colnames(TEMP))[1]][j]
      #loop within the subplots (36) for each eplot
      for(k in 1:36) 
      {
        #show progress on screen
        print(paste("subplot", k,"of eplot",j , "of landscape" , TEMP[,grep("landscape_real", colnames(TEMP))][j]));
        #count number of trees in each subplot
        no_tree=length(grep("position",colnames(TEMP[,grep(paste("subplot_group.",k,"..subplot_holder.tree",sep=""), colnames(TEMP))])));
        #define an index position
        a=grep(paste("subplot_group.",k,"..subplot_holder.tree" ,sep=""), colnames(TEMP))
        treeindex=grep("position",colnames(TEMP[,a]));
        #define a temp matrix to hold data called trees
        trees=matrix(data=NA,ncol=27,nrow=length(treeindex));
        #give the matrix col names
        colnames(trees)=c(
          "country",
          "landscape",
          "eplot",
          "subplot",
          "subplot_radius(m)",
          "tree_number",
          "singlestem_height(m)",
          "singlestem_circumference(cm)",
          "singlestem_canopy_width(m)",
          "singlestem_diameter(m)",
          "singlestem_basal_area(m^2)",
          "multistem_samesize_height(m)",
          "multistem_samesize_circumference(cm)",
          "multistem_samesize_nstems",
          "multistem_samesize_canopy_width(m)",
          "multistem_samesize_diameter(m)",
          "multistem_samesize_basal_area(m^2)",
          "species",
          "genus",
          "sg(bro)",
          "sNstems",
          "msNstems",
          "singlestem_Stemmass(kg)",
          "multistem_samesize_Stemmass(kg)",
          "Total trees(including_NAs)","LAT","LONG");
        #fill the tree matrix
        trees[1:length(as.numeric(TEMP[j,a[grep("singlestem_height",colnames(TEMP[,a]))]])),7]=as.numeric(TEMP[j,a[grep("singlestem_height",colnames(TEMP[,a]))]]);
        trees[1:length(as.numeric(TEMP[j,a[grep("singlestem_circ",colnames(TEMP[,a]))]])),8]=as.numeric(TEMP[j,a[grep("singlestem_circ",colnames(TEMP[,a]))]]);
        trees[1:length(as.numeric(TEMP[j,a[grep("singlestem_canopy",colnames(TEMP[,a]))]])),9]=as.numeric(TEMP[j,a[grep("singlestem_canopy",colnames(TEMP[,a]))]]);
        trees[,10]=((as.numeric(trees[,8])/100/pi));
        trees[,11]=((as.numeric(trees[,8])/100/pi)/2)^2*pi;
        trees[1:length(as.numeric(TEMP[j,a[grep("multistem_samesize_height",colnames(TEMP[,a]))]])),12]=as.numeric(TEMP[j,a[grep("multistem_samesize_height",colnames(TEMP[,a]))]]);
        trees[1:length(as.numeric(TEMP[j,a[grep("multistem_samesize_circumference",colnames(TEMP[,a]))]])),13]=as.numeric(TEMP[j,a[grep("multistem_samesize_circumference",colnames(TEMP[,a]))]]);
        trees[1:length(as.numeric(TEMP[j,a[grep("multistem_samesize_numstems",colnames(TEMP[,a]))]])),14]=as.numeric(TEMP[j,a[grep("multistem_samesize_numstems",colnames(TEMP[,a]))]]);
        trees[1:length(as.numeric(TEMP[j,a[grep("multistem_samesize_canopy",colnames(TEMP[,a]))]])),15]=as.numeric(TEMP[j,a[grep("multistem_samesize_canopy",colnames(TEMP[,a]))]]);
        trees[,16]=((as.numeric(trees[,13])/100/pi));
        trees[,17]=((as.numeric(trees[,13])/100/pi)/2)^2*pi;
        
        #get missing values index
        row.has.na1 <- apply(trees[,7:8], 1, function(x){any(is.na(x))})#singlestem height and circ
        row.has.na2 <- apply(trees[,12:14], 1, function(x){any(is.na(x))})#multistem values
        aa=which(!row.has.na1)
        ab=which(!row.has.na2)
        ac=c(aa,ab);
        ac=unique(sort(ac));#remove duplicate rows
        trees[ac,18]=as.character(TEMP[j,a[grep("species", colnames(TEMP[,a]))]])[ac];
        trees[ac,19]=as.character(TEMP[j,a[grep("genus", colnames(TEMP[,a]))]])[ac];
        trees[ac,20]=800;
        trees[which(!row.has.na1),21]=1;
        trees[which(!row.has.na2),22]=as.numeric(trees[which(!row.has.na2),14]);
        trees[,23]=stem_allometry(as.numeric(trees[,10]),as.numeric(trees[,7]),as.numeric(trees[,20]))*as.numeric(trees[,21]);
        trees[,24]=stem_allometry(as.numeric(trees[,16]),as.numeric(trees[,12]),as.numeric(trees[,20]))*as.numeric(trees[,22]);
        trees[ac,25]= no_tree;
        #fill metadata
        trees[ac,1]=TEMP[,grep("country", colnames(TEMP))[1]][j];
        trees[ac,2]=TEMP[,grep("landscape_real", colnames(TEMP))[1]][j];
        trees[ac,3]=TEMP[,grep("plot", colnames(TEMP))[1]][j];
        trees[ac,4]=as.character(k);
        trees[ac,5]=as.numeric(TEMP[,grep("radius", colnames(TEMP))[1]][j]);
        trees[ac,6]=1:length(ac);
        trees[ac,26]=lat;
        trees[ac,27]=long;
        #x[,grep("lat",colnames(x))[1]]
        #populate OUTPUT matrix
        OUTPUT=rbind(OUTPUT,trees[ac,]);
        #close loops
      }}}
  
  OUTPUT=OUTPUT[-1,];
  #write out the values to the aux file
  write.table(OUTPUT, file = paste(AUXfile,"temp.txt",sep=""), append = FALSE, quote = TRUE, sep = ",",row.names = F, col.names = TRUE);
  # some code to do basic calculations on the summary table
  out=matrix(data=NA,nrow=1,ncol=18)
  colnames(out)=c("Country","N_landscapes","landscape_id","N_eplots","eplot_id","N-subplots","subplot_id","N_trees","N_duplicate_trees","Tree_id","SP_radius","MS_mass","SS_mass","SP_treemass","SP_areamass","mean_eplotmass","Lat","Long")
  #country index
  countries=unique(OUTPUT[,1]);
  
  for(i in 1:length(countries))
  {
    
    temp=OUTPUT[which(OUTPUT[,1]==countries[i]),];
    landscapes=unique(temp[,2]);
    for(j in 1:length(landscapes))#foreach landscape
    {
      temp1=temp[which(temp[,2]== landscapes[j]),,drop=FALSE];
      eplots=unique(temp1[,3]);
      for(k in 1:length(eplots))#for each eplot
      {
        temp2=temp1[which(temp1[,3]== eplots[k]),,drop=FALSE];
        subplot=unique(temp2[,4]);
        for(l in 1:length(subplot))#foreach suplot
        {
          temp3=temp2[which(temp2[,4]== subplot[l]),,drop=F];
          trees=unique(temp3[,6]);
          subplotmass= 0;
          temp4=temp3[which(temp3[,6]==unique(temp3[,6])),,drop=F];
          
          if(nrow(temp4)>0)
          {
            for(p in 1:nrow(temp4))
            {
              
              subplotmass=sum(subplotmass,as.numeric(temp4[p,24]),as.numeric(temp4[p,23]),na.rm=T);
              sparea=as.numeric(temp4[p,5])^2*pi
              sp2= (subplotmass/1000)/(sparea/10000);
              ttt=c(countries[i], length(landscapes),landscapes[j] ,length(eplots),eplots[k],length(subplot), subplot[l],length(trees),length(which(duplicated(trees))),temp4[p,6],temp4[p,5],temp4[p,24],temp4[p,23], subplotmass, sp2,"NA",temp4[p,26],temp4[p,27]);
              out= rbind(out, ttt);
              
              
            }
          }
          out[1:nrow(temp4),14:15]="NA";
        }
        out[ which(out[,5]== eplots[k])[length(which(out[,5]== eplots[k]))],16]=sum(as.numeric(out[which(out[,5]== eplots[k]),15]),na.rm=T)/36;#per 36 subplots
      }
    }
  }
  #out=out[-1,]
  # out=out[complete.cases(as.numeric(out[,16])),]
  #out=out[-which(as.numeric(out[,16])>upperthreshold),]
  #out=out[-which(as.numeric(out[,16])<lowerthreshold),]
  #write summary to AUX file
  write.table(out, file =AUXfile, append = FALSE, quote = TRUE, sep = ",",row.names = F, col.names = TRUE);
  
  #boxplot(as.numeric(out[,16]), ylab="t/ha",main="Woody biomass (zoomed in)",ylim=c(0,50))
  return(out);
  
}
