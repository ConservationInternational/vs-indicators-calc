#function to initiate a VS table
#This funtion runs through -or creates the directories specified by the table and sets 
#the raw and processed data flags accodingly.
initiate_table=function(inputs)
{
  #inputs is the table (represented by a data.frame)
  for(i in 1:(length(inputs[,1])))
  {
    dir=as.character(inputs[i,]$staging_protocol);
    if(file.exists(dir)){}else{dir.create(dir,recursive = T)};
    if(is.na(inputs[i,]$raw_input)){}else
    {
      rawinputs=paste(inputs[i,]$staging_protocol,inputs[i,]$raw_input,sep="");
      if(file.exists(rawinputs)){inputs[i,]$data_flag_raw=1;}else{}; 
    }
    if(is.na(inputs[i,]$file_name)){}else
    {
      processedinputs=paste(inputs[i,]$staging_protocol,inputs[i,]$file_name,sep="");
      if(file.exists(processedinputs)){inputs[i,]$data_flag_processed=1;}else{}; 
    }
  }
  return(inputs);
}
#######################
#######################
#function to return the area of interest (AOI) used for cropping etc., from a shapefile
aoi=function(shapefilepath, shapfilename)
{
  #get required packages
  if(!require(rgdal)) install.packages('rgdal'); require(rgdal)
  if(!require(raster)) install.packages('raster'); require(raster)
  #load the area of interest from a shapefile
  aoi <- readOGR(shapefilepath, shapfilename);
  #get grid extent
  Xmin=xmin(extent(aoi));
  Xmax=xmax(extent(aoi));
  Ymax=ymax(extent(aoi));
  Ymin=ymin(extent(aoi));
  #create a new shapefile representing the extent (rectangle)
  Xcoord=c(Xmin,Xmax,Xmax,Xmin);
  Ycoord=c(Ymin,Ymin,Ymax,Ymax);
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(Xcoord,Ycoord))), ID=1)),proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  #return a spatial dataframe of the extent
  return(SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1)))
}
#######################
#function to create a resample grid based on area of interest
createresamplegrid=function(AOI,resolution,bX,bY)
{
  #resolution = meters
  #AOI = area of interest
  #bX is the bearing in the X direction
  #bY is the bearing in the y direction
  #this function walks along the geodisic, and at each 1000m distance records the lat long in degrees.
  if(!require(geosphere)) install.packages('geosphere'); require(geosphere);
  if(!require(sp)) install.packages('sp'); require(sp);
  if(!require(raster)) install.packages('raster'); require(raster);
  
  #get grid extent for the area of interest
  Xleft=xmin(extent(AOI));
  Xright=xmax(extent(AOI));
  Ytop=ymax(extent(AOI));
  Ybottom=ymin(extent(AOI));
  #set the initial starting point (top left of the AOI)
  p=c(Xleft,Ytop);
  #set an offset to get center value
  p=destPoint(p,bY, resolution/2);
  p=destPoint(p,bX, resolution/2);
  #get the number of rows and columns that will make up the grid
  nrows=0;ncols=0;
  while(p[2]>=Ybottom)
  {
    nrows=nrows+1;
    p=destPoint(p,bY, resolution);
  }
  nrows=nrows+1
  #reset p
  p=c(Xleft,Ytop);
  #set an offset to get cente value
  p=destPoint(p,bY, resolution/2);
  p=destPoint(p,bX, resolution/2);
  while(p[1]<=Xright)#cols
  {
    ncols=ncols+1;
    p=destPoint(p,bX, resolution);
  }
  ncols=ncols+1;
  #create the X and Y grids to hold coordinates (centre values)
  resamplegridX=matrix(data=NA,nrow=nrows,ncol=ncols);
  resamplegridY=matrix(data=NA,nrow=nrows,ncol=ncols);
  
  #reset the starting point
  p=c(Xleft,Ytop);
  #set an offset to get center value
  p=destPoint(p,bY, resolution/2);
  p=destPoint(p,bX, resolution/2);
  a=sqrt((resolution/2)^2+(resolution/2)^2);
  newloc1=p#store the start location
  for(i in 1:nrows)#rows
  {
    for(j in 1:ncols)#cols
    {
      resamplegridX[i,j]=p[1];
      resamplegridY[i,j]=p[2];
      #walk in direction bX (bearing X [90]) by resolution (m) and store lat long location
      p=destPoint(p,bX, resolution);
      
    }
    #adjust the new start location for each row by walking down 
    newloc=destPoint(newloc1,bY, resolution*i);
    #reset p according to the new location on the left hand side
    p=newloc;#
    print(paste(i,"of",nrows,"complete"))
  }
  #create the output and return it as a list
  out=list(X=resamplegridX,Y=resamplegridY)
  return(out);
}
#######################
#######################
#function to randomly populate a resample grid using a range for testing purposes until actual values can be processed
populateresamplegrid=function(grid, minval, maxval)
{
  output=grid;
  output[]=runif(length(grid),minval,maxval);
  return(output);
}
#######################
#function to do Eplot processing
checkorcreate_eplotmass=function(grid,table,rownumber)
{
  
  dir=paste(table[rownumber,]$staging_protocol,table[rownumber,]$file_name,sep="");
  if(file.exists(dir)){#swap out values
    #Note that we only use raster to store values in tabular format, and make no use of the CRS information as our grids are in km^2
    grid=checkandpopulategridtif(grid,dir);
    #if exsists set the flag to true
    table[rownumber,]$data_flag_processed=1;
  }else{
    #check raw inputs and process if necessary
    rawdir=paste(table[rownumber,]$staging_protocol,table[rownumber,]$raw_input,sep="");
    if(file.exists(rawdir))
    {
      #set the raw flag
      table[rownumber,]$data_flag_raw=1;
      #first see if the AUX file exists which we can use, otherwise generate it
      AUXfile=paste(table[rownumber,]$staging_protocol,table[rownumber,]$Aux_file,sep="");
      if(file.exists(AUXfile))
      {
        temp=read.table(AUXfile,header=T,sep=",");
        #do some processing
        cnames=colnames(temp);
        temp=as.data.frame(temp);
        colnames(temp)=cnames;
        temptanzania=temp[which(temp$Country=="TZA"),];
        temptanzania=temptanzania[-which(is.na(temptanzania[,16])),];
        #filter for unwanted values
        eplotmass=as.numeric(as.vector(temptanzania$mean_eplotmass))
        #check output
        plot(eplotmass[eplotmass<500]);
        temptanzania=temptanzania[eplotmass<500,];
        #create a raster of the values
        tlat=-as.numeric(as.vector(temptanzania$Lat))
        tlong=as.numeric(as.vector(temptanzania$Long))
        tvalues=as.numeric(as.vector(temptanzania$mean_eplotmass))
        output=cbind(tlong,tlat,tvalues);
        output=output[!is.na(output)[,1],];
        #create a raster to store values
        points=SpatialPoints(output[,1:2]);
        crs(points)='+proj=longlat +datum=WGS84';
        resampltest=resamplegridy[-nrow(resamplegridy),-ncol(resamplegridy)];
        #create a raster roughly 1km^2
        r= raster(nrows=nrow(resampltest), ncols=ncol(resampltest),  
                  crs='+proj=longlat +datum=WGS84', ext=extent(Tanzania));
        rast=rasterize(points,r,output[,3]);
        #small test to see if we can return values
        values(rast)[!is.na(values(rast))];
        writeRaster(rast, filename=dir, format="GTiff", overwrite=TRUE);
        grid=rast;
      }else
      {
        #record start time of calculation
        starttime=Sys.time();
        #eplotmass=paste(woodfuelinputs[1,]$staging_protocolwoodfuelinputs[1,]$Aux_file,sep="");
        temp=processEplots(grid,rawdir,AUXfile,400,0);
        #do some processing
        cnames=colnames(temp);
        temp=as.data.frame(temp);
        colnames(temp)=cnames;
        temptanzania=temp[which(temp$Country=="TZA"),];
        temptanzania=temptanzania[-1,];
        temptanzania=temptanzania[-which(temptanzania[,16]=="NA"),];
        temptanzania=droplevels(temptanzania);
        #filter for unwanted values
        eplotmass=as.numeric(as.vector(temptanzania$mean_eplotmass))
        #check output
        plot(eplotmass[eplotmass<500]);
        temptanzania=temptanzania[eplotmass<500,];
        #create a raster of the values
        tlat=-as.numeric(as.vector(temptanzania$Lat))
        tlong=as.numeric(as.vector(temptanzania$Long))
        tvalues=as.numeric(as.vector(temptanzania$mean_eplotmass))
        output=cbind(tlong,tlat,tvalues);
        output=output[!is.na(output)[,1],];
        #create a raster to store values
        points=SpatialPoints(output[,1:2]);
        crs(points)='+proj=longlat +datum=WGS84';
        #create a raster roughly 1km^2
        r= raster(nrows=nrow(resamplegridy), ncols=ncol(resamplegridy),  
                  crs='+proj=longlat +datum=WGS84', ext=extent(Tanzania));
        rast=rasterize(points,r,output[,3]);
        #small test to see if we can return values
        values(rast)[!is.na(values(rast))];
        writeRaster(rast, filename=dir, format="GTiff", overwrite=TRUE);
        grid=rast;
        Sys.time()-starttime
      }
      table[rownumer,]$data_flag_processed=1;
    }else
    {#leave the original grid in place with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
      grid=raster(grid);
      table[rownumber,]$data_flag_raw=0;
      table[rownumber,]$data_flag_processed=0;
    }
    
  }
  return(grid)
}
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
#small function to replace a grid with a processed file (txt.
checkandpopulategrid=function(input,inputfilename)
{
  if(file.exists(inputfilename))
  {
    testfile=read.table(inputfilename,header=F,sep=",");
    testfile=as.matrix(testfile);
  }
  return(testfile);
}
#small function to replace a grid with a processed file (tif).
checkandpopulategridtif=function(input,inputfilename)
{
  if(file.exists(inputfilename))
  {
    testfile=raster(inputfilename)
  }
  return(as.matrix(testfile));
}
#######################
#Function to do processing and resampling from a regular WGS84 latlong tif file to a metric grid (km^2)
checkorprocess_grid=function(grid,table,java,AOI,agg,agg2,aggamount,aggfunction,resolution,parseraster)
{
  dir=paste(table$staging_protocol,table$file_name,sep="");
  #print(dir)
  if(file.exists(dir)){#swap out values
    grid=checkandpopulategrid(grid,dir);
    #grid=raster(grid);
    table$data_flag_processed=1
  }else{
    #check raw inputs and process if necessary
    rawdir=paste(table$staging_protocol,table$raw_input,sep="");
    if(file.exists(rawdir))
    {
      table$data_flag_raw=1;
      #record start time of calculation
      starttime=Sys.time();
      #the 2nd last input in the function determines whether to aggregate of or not. If True it will
      #aggregate, else it will do a direct mapping using a fractional weighting based on area intersections
      
      grid=checkandprocessgrid(java,grid,initial.directory,rawdir,AOI,agg,dir,agg2,aggamount,aggfunction,resolution,parseraster)
      Sys.time()-starttime
      table$data_flag_processed=1;
    }else
    {#leave the original grid inplace with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
      #grid=raster(grid);
      table$data_flag_raw=0;
      table$data_flag_processed=0;
    }
  }
  return(grid);
}
#######################
#function to resample Geotiffs that are in latlong WGS84 format into a resample grid in km^2 (but expressed by lat long coords)
checkandprocessgrid=function(java,resamplematrix,initialdir,rawdir,AOI,agg,output,agg2,aggamount,aggfunction,resolution,passraster)
{
  #java is a boolean value to use java or native R to resample
  #resamplematrix is the blank resample grid to be populated
  if(!require(raster)) install.packages('raster'); require(raster);
  if(!require(rgdal)) install.packages('rgdal'); require(rgdal);
  if(passraster)
  {
    input=rawdir
  }else
  {
    input=raster(rawdir);
  }
  #we assum the input is already in WGS84 lat lon projection
  print("Cropping to AOI");
  input=crop(input,AOI);
  
  if(agg2)
  {
    print("aggregating the raster");
    if(aggfunction=="mean"){
      input=aggregate(input,aggamount,mean);
    }else
    {
      input=aggregate(input,aggamount,sum);
    }
  }
  
  ##first interpolate missing values if any
  print("checking and/or interpolating missing values");
  input=interpolategrid(input,6);#(larger p values will weight closer points more)
  print("finished interpolation");
  #cat("\014")
  print("creating data frame (unwrapping raster into array lists)");
  #create a data.frame from the raster (this is to allow fast resampling (provided its not too big))
  data=as.data.frame(cbind(coordinates(input)[,1],coordinates(input)[,2],values(input)));
  xmininput=data[,1]-(xres(input)/2);
  xmaxinput=data[,1]+(xres(input)/2);
  ymininput=data[,2]-(yres(input)/2);
  ymaxinput=data[,2]+(yres(input)/2);
  data=cbind(data, xmininput, xmaxinput, ymininput, ymaxinput);
  colnames(data)=c("x","y","values","xmin","xmax","ymin","ymax");
  if(java)
  {
    #write out the data to use in java and resample using java call (fastest)
    setwd(java_resample_function);
    #alternatively use java 1.7
    #setwd(paste(initial.directory,"/resample_java1.7",sep=""));
    print("writing data frame to file");
    write.table(data,file="data.txt", col.names=F,row.names=F,sep=",");
    command=paste("java -jar Resample.jar",ymax(extent(AOI)),xmin(extent(AOI)),ymin(extent(AOI)),xmax(extent(AOI)),'-180','90', resolution,paste(getwd(),"/data.txt",sep=""),length(data[,1]),ncol(input),nrow(input),agg);
    #run the command
    print("running java resampling");
    system(command);
    resampleoutput=as.matrix(read.table("resampleoutput.txt",sep=",",header=F));
    resamplematrix2=resampleoutput[-nrow(resampleoutput),-ncol(resampleoutput)];
    r=resamplematrix2;
    print("writing output to file");
    write.table(r,file=output,col.names=F,row.names=F,sep=",");
    
    
    
    
    #no crs is written (coords are stored in the resamplegrid) 
   # writeRaster(r, filename=output, format="GTiff", overwrite=TRUE);
    #cat("\014")
    print("Done!");
  }else
  {
    print("running R resampling (will be slow)");
    resampleoutput=resample(data,bX,bY,resamplegrid$Y, resamplegrid$X, resolution,agg)
    resamplematrix2=resampleoutput[-nrow(resampleoutput),-ncol(resampleoutput)];
    r=resamplematrix2;
    write.table(r,file=output,col.names=F,row.names=F,sep=",");
    #r=raster(resmaplematrix2);
    #writeRaster(r, filename=output, format="GTiff", overwrite=TRUE);
    #cat("\014")
    print("Done!");
  }
  return(r);
  ##we dont set any crs as our grid is in km^2, but positions represented by lat long coords
}
#######################
#resample function (slow - use java version rather about 1000 faster)
resample=function(data,bX,bY,resamplegridY,resamplegridX,resolution,agg)
{
  if(!require(geosphere)) install.packages('geosphere'); require(geosphere);
  if(!require(rgeos)) install.packages('rgeos');require(rgeos);
  #rewrap the values into arrays of ytop,ybot,xleft,xright
  xr1 <- matrix(unlist(data[,5]), ncol = ncol(input), byrow = TRUE)[1,];
  xl1 <- matrix(unlist(data[,4]), ncol = ncol(input), byrow = TRUE)[1,];
  yt1 <- matrix(unlist(data[,7]), ncol = ncol(input), byrow = TRUE)[,1];
  yb1 <- matrix(unlist(data[,6]), ncol = ncol(input), byrow = TRUE)[,1];
  #start a counter for the loop
  counter=0;
  resamplegridvals=matrix(data=NA,nrow=nrow(resamplegridY),ncol=ncol(resamplegridY));
  for( k in 1:nrow(resamplegridY))#rows
  {
    for(l in 1:ncol(resamplegridY))#cols
    {
      counter=counter+1;
      #get the first points
      p.first=resamplegridY[k,l];#lat
      p.second=resamplegridX[k,l];#long
      #//top left
      temp=destPoint(c(p.second,p.first), 360-bX, resolution);
      p.first=temp[2];#lat
      p.second=temp[1];#long
      #bearing of 0 is hard coded saying we go directly up
      temp=destPoint(c( p.second, p.first),0,resolution/2);
      p.first=temp[2];#lat
      p.second=temp[1];#long
      #set xtopleft and ytopleft
      xtl=p.second;
      ytl=p.first;
      #bottom left
      temp=destPoint(c( p.second,p.first), bY,resolution);
      p.first=temp[2];#lat
      p.second=temp[1];#long
      xbl=p.second;
      ybl=p.first;
      #bottom right
      temp=destPoint( c( p.second,p.first), bX,resolution);
      p.first=temp[2];#lat
      p.second=temp[1];#long
      xbr=p.second;
      ybr=p.first;
      #top right corner
      temp=destPoint( c( p.second,p.first),0,resolution);
      p.first=temp[2];#lat
      p.second=temp[1];#long
      xtr=p.second;
      ytr=p.first;
      #populate the coordinate vectors
      xcoords=c(xtl,xbl,xbr,xtr);
      ycoords=c(ytl,ybl,ybr,ytr);
      #create resample polygon
      sp <- SpatialPolygons(list(Polygons(list(Polygon(cbind(xcoords,ycoords))), ID=1)),proj4string=CRS("+proj=longlat +datum=WGS84"));
      # SimplePolygon2D sp=new SimplePolygon2D(xcoords, ycoords);
      #define the minimum points
      xlmin=min(xtl, xbl);
      xrmax=max(xtr, xbr);
      ytmax=max(ytl, ytr);
      ybmin=min(ybl, ybr);
      #Create arrays to get the indices from the data table
      xindex=which(xr1> xlmin&xl1< xrmax);
      yindex =which(yb1< ytmax & yt1> ybmin)
      datatablevalues=(yindex*length(xr1)+ xindex)-length(xr1);
      data5=data[datatablevalues,]
      Ao=0;
      Ai=0;
      Ar=0;
      newvalue=0;
      newvalueweights=0;
      newvalueagg=0;
      aggcounter=0;
      nval2=NA;
      tryCatch({
        for(i in 1:length(data5[,1]))
        {
          xcoords =c(data5$xmin[i],data5$xmax[i],data5$xmax[i],data5$xmin[i]);
          ycoords =c(data5$ymin[i],data5$ymin[i],data5$ymax[i],data5$ymax[i]);
          #create polygon from input
          sp2 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(xcoords,ycoords))), ID=1)),proj4string=CRS("+proj=longlat +datum=WGS84"));
          #the area of the polygons
          Ao=area(sp2);
          #the intersecting area
          a2=gIntersection(sp2, sp);
          Ai=area(a2);
          Ao_Ai_min=min(Ao/Ai, Ai/Ao);
          # perform fractional weighting
          newvalue=+(Ao_Ai_min*data5[i,]$values);
          newvalueweights=+(Ao_Ai_min);
          if(agg)
          {
            Ar=area(sp);
            newvalueagg=newvalueagg+(Ao_Ai_min*data5[i,]$values);
            if(Ao>Ar)
            {
              aggcounter=aggcounter+1;
            }else
            {
              if(Ao<Ar)
              {
                aggcounter=1;
              }
            } 
          }
        }
        if(agg)
        {
          nval2=newvalueagg/aggcounter;
        }else
        {
          nval2=newvalue/newvalueweights;
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      resamplegridvals[k,l]=nval2;
      print(paste( counter/(length(yb1)*length(xr1))*100, " % resampling complete"));
    }
  }
  return(resamplegridvals);
  
}
#######################
#funtion to interpolate missing values using IDW (run in parallel), but will need to check
#if this will work with all file types
interpolategrid=function(input,p)
{
  #get values as a data frame from the input file
  data=as.data.frame(cbind(coordinates(input)[,1],coordinates(input)[,2],values(input)));
  colnames(data)=c("lon","lat","values");
  #seperate out known and unknown values
  x=data[!complete.cases(data),][,1]#x to interpolate
  y=data[!complete.cases(data),][,2]#y to interpolate
  vals=data[complete.cases(data),][,3];#known values
  xv=data[complete.cases(data),][,1];#known x
  yv=data[complete.cases(data),][,2];#known y
  #load packages for parallel computation
  #if(!require(foreach)) install.packages('foreach');
  #if(!require(doSNOW)) install.packages('doSNOW');
  #if(!require(doMC)) install.packages('doMC');
  #spawn clusters - we use DoSNOW to print progress (don't know if it will work on windows machines)
  #cluster = makeCluster(detectCores(), type = "SOCK",outfile="");
  #registerDoSNOW(cluster);
  #record the start time
  starttime=Sys.time();
  if(length(x)>0)
  {
    n=length(x);
    output=matrix(data=NA,nrow=n,ncol=1);
    #set length 
    
    for(i in 1:n)
    {
      percent=i/n*100;
      print(percent);
      output[i]=IDW(i,x,y,xv,yv,vals,p);
    }
    #run code. Remeber to put the function call last as these are the outputs we require
    #output=foreach(i = icount(n),.combine=c) %dopar% {
    # percent=i/n*100; 
    # print(percent);
    # IDW(i,x,y,xv,yv,vals,p);
    #}
    endtime=Sys.time()-starttime;
    #stopCluster(cluster);
    output2=cbind(x,y,output);
    output3=cbind(xv,yv,vals);
    output4=rbind(output2,output3);
    rast=rasterFromXYZ(output4,crs=crs(input))
    return(rast)
  }
  else
  {
    return(input);
  }
}
#######################
#Nearest neighbour weighted interpolation ( using all exsitng values for weighting)
IDW=function(i,x,y,known_x,known_y,known_vals,p)
{
  # i is the index for unknown points
  # x and y are the locations for the unknown values
  # known_x, known_y, and known_vals are the known locations and values
  # p is the power parameter (degree of smoothness for closest locations)
  nd=(1/((sqrt((x[i]-known_x)^2+(y[i]-known_y)^2))^p));
  return(sum(nd*known_vals)/sum(nd));
}
#######################
#function to process multiple tiff files (diffferent projection)
checkandprocess_rainfall=function(grid,table,java=T,AOI=Tanzania,agg="false",agg2=F,aggamount=33,aggfunction="mean",resolution=1000,parseraster=T)
{
  if(!require(raster)) install.packages('raster'); require(raster);
  if(!require(rgdal)) install.packages('rgdal'); require(rgdal);
  dir=paste(table$staging_protocol,table$file_name,sep="");
  if(file.exists(dir)){#swap out values
    grid=checkandpopulategrid(grid,dir);
    table$data_flag_processed=1
  }else{
    rawdir=paste(table$staging_protocol,table$raw_input,sep="");
    if(file.exists(rawdir))
    {
      table$data_flag_raw=1;
      #record start time of calculation
      starttime=Sys.time();
      setwd( rawdir);
      files=list.files();
      input=as.matrix(raster(files[1]));
      for(i in 2:length(files))
      {
        input2=as.matrix(raster(files[i]));
        input=input+input2;
        print(paste(i,"of",length(files),"complete"));
      }
      temp=raster(input);
      #interpolate grid
      print(paste("checking and interpolating"));
      input=interpolategrid(temp,6);#(larger p values will weight closer points more)
      #get the original file again to check coordinates
      e2=readGDAL(files[1]);
      #get coords in WGS84 latlong 
      e3=spTransform(e2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"));
      #determine the equivalent resolution in degrees
      res=destPoint(c(0,0),90,e2@grid@cellsize[1])[1];
      #save the current dir
      currentdir=getwd();
      #create a new grid in degrees roughly the same size as the input files
      setwd(java_resample_grid_function);
      command=paste("java -jar Create_resampling_grid.jar",ymax(extent(AOI)),xmin(extent(AOI)),ymin(extent(AOI)),xmax(extent(AOI)),'-180','90', res,"degrees")
      #run the command (this will write out two tables X.txt and Y.txt)
      system(command);
      resx=as.matrix(read.table("X.txt",sep=",",header=F));
      resy=as.matrix(read.table("Y.txt",sep=",",header=F));
      #get parameters to do a nearest neighbour reprojection
      x=c(resx);
      y=c(resy)
      vals=values(input);
      xv=coordinates(e3)[,1]
      yv=coordinates(e3)[,2]
      #perform nn reprojection
      n=length(x);
      output=matrix(data=NA,nrow=n,ncol=1);
      for(i in 1:n)
      {
        percent=i/n*100;
        print(paste(percent, " % nn reproj complete"));
        output[i]=vals[which.min(abs(x[i]-xv)+abs(y[i]-yv))];
      }
      Sys.time()-starttime;
      #save the output as a raster
      d=matrix(unlist(output[,1]), nrow = nrow(resx), byrow = F);
      d2=raster(d);
      extent(d2)=extent(AOI);
      #do normal checkandprocessgrid function
      grid=checkandprocessgrid(java,grid,initial.directory,d2,AOI,agg,dir,agg2,aggamount,aggfunction,resolution,parseraster)
      
      
    }else
    {#leave the original grid inplace with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
     # grid=raster(grid);
      table$data_flag_raw=0;
      table$data_flag_processed=0;
    }
  }
  return(grid);
}
#######################
#function to handle worldclim data for woodfuel
checkorprocess_worldclimgrid=function(grid,table,java,AOI,agg,agg2,aggamount,aggfunction,resolution,parseraster)
{
  dir=paste(table$staging_protocol,table$file_name,sep="");
  if(file.exists(dir)){#swap out values
    grid=checkandpopulategrid(grid,dir);
    #grid=raster(grid);
    table$data_flag_processed=1
  }else{
    #check raw inputs and process if necessary
    rawdir=paste(table$staging_protocol,table$raw_input,sep="");
    
    if(file.exists(rawdir))
    {
      table$data_flag_raw=1;
      #record start time of calculation
      starttime=Sys.time();
      setwd( rawdir);
      files=list.files();
      
      setwd( paste(rawdir,"/",files[1],sep=""));
      files2=list.files();
      input=raster(files2[1]);
      input=crop(input,Tanzania);
      setwd( rawdir);
      files=list.files();
      setwd( paste(rawdir,"/",files[2],sep=""));
      files3=list.files();
      input2=raster(files3[1]);
      input2=crop(input2,AOI);
      input3=merge(input,input2);
      s=stack(input3);
      # rest of months
      for(k in 2:12)
      {
        setwd( rawdir);
        files=list.files();
        i=1;j=2;
        setwd( paste(rawdir,"/",files[1],sep=""));
        files2=list.files();
        input=raster(files2[k]);
        input=crop(input,Tanzania);
        setwd( rawdir);
        files=list.files();
        setwd( paste(rawdir,"/",files[2],sep=""));
        files3=list.files();
        input2=raster(files3[k]);
        input2=crop(input2,AOI);
        input3=merge(input,input2);
        s=stack(s,input3);
        print(k);
        
      }
      #get the sum
      rast <- calc(s, sum);
      #getrid of ocean by giving it a 0 value
      rast[is.na(rast)]=0
      #the 2nd last input in the function determines whether to aggregate of or not. If True it will
      #aggregate, else it will do a direct mapping using a fractional weighting based on area intersections
      
      grid=checkandprocessgrid(java,grid,initial.directory,rast,AOI,agg,dir,agg2,aggamount,aggfunction,resolution,parseraster)
      Sys.time()-starttime
      table$data_flag_processed=1;
    }else
    {#leave the original grid inplace with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
     # grid=raster(grid);
      table$data_flag_raw=0;
      table$data_flag_processed=0;
    }
  }
  return(grid);
}
#######################
#Function to transfer grid parameter file created using java create grid function
creategridparamters=function(table)
{
  dir=paste(table$staging_protocol,sep="");
  setwd(java_resample_grid_function)
  print("Creating grid parameters");
  file.copy("X.txt", paste(dir,"/X.txt",sep=""));
  file.copy("Y.txt", paste(dir,"/Y.txt",sep=""));
  file.copy("Xbl.txt", paste(dir,"/Xbl.txt",sep=""));
  file.copy("Xbr.txt", paste(dir,"/Xbr.txt",sep=""));
  file.copy("Xtl.txt", paste(dir,"/Xtl.txt",sep=""));
  file.copy("Xtr.txt", paste(dir,"/Xtr.txt",sep=""));
  file.copy("Ybl.txt", paste(dir,"/Ybl.txt",sep=""));
  file.copy("Ybr.txt", paste(dir,"/Ybr.txt",sep=""));
  file.copy("Ytl.txt", paste(dir,"/Ytl.txt",sep=""));
  file.copy("Ytr.txt", paste(dir,"/Ytr.txt",sep=""));
  print("Done");
}
#######################
#Basic function to genrate the water lite table
#This function generates a table which flags the availability of raw and processed data required for the water thread.
generate_WL_table=function()
{
  #this table has the following colnames
  #1. Variable
  #2. staging_protocol (the directory for processed and raw inputs)
  #3. file_name (the processed file)
  #4. Aux_file (any AUx. outputs)
  #5. raw_input (the raw input data or data directory)
  #6. data_flag_raw (0=data missing, 1=exists)
  #7. data_flag_processed (0=data missing, 1=processed)
  
  #Annual rainfall (2001)
  waterliteinputs <- data.frame(
    Variable = "rainfall2001", 
    staging_protocol = paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2001.tif",
    Aux_file=NA,
    raw_input = "/2001/",data_flag_raw=0,data_flag_processed=0);
  #Annual rainfall (2002)
  temp <- data.frame(
    Variable = "rainfall2002", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2002.tif",Aux_file=NA,
    raw_input = "/2002/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  
  #Annual rainfall (2003)
  temp <- data.frame(
    Variable = "rainfall2003", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2003.tif",Aux_file=NA,
    raw_input = "/2003/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2004)
  temp <- data.frame(
    Variable = "rainfall2004", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2004.tif",Aux_file=NA,
    raw_input = "/2004/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2005)
  temp <- data.frame(
    Variable = "rainfall2005", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2005.tif",Aux_file=NA,
    raw_input = "/2005/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2006)
  temp <- data.frame(
    Variable = "rainfall2006", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2006.tif",Aux_file=NA,
    raw_input = "/2006/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2007)
  temp <- data.frame(
    Variable = "rainfall2007", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2007.tif",Aux_file=NA,
    raw_input = "/2007/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2008)
  temp <- data.frame(
    Variable = "rainfall2008", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2008.tif",Aux_file=NA,
    raw_input = "/2008/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2009)
  temp <- data.frame(
    Variable = "rainfall2009", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2009.tif",Aux_file=NA,
    raw_input = "/2009/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Annual rainfall (2010)
  temp <- data.frame(
    Variable = "rainfall2010", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/rainfall/",sep=""),
    file_name = "rainfall2010.tif",Aux_file=NA,
    raw_input = "/2010/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  
  #Ep (2001) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2001", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2001.tif",Aux_file=NA,
    raw_input = "/2001/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2002) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2002", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2002.tif",Aux_file=NA,
    raw_input = "/2002/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2003) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2003", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2003.tif",Aux_file=NA,
    raw_input = "/2003/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2004) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2004", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2004.tif",Aux_file=NA,
    raw_input = "/2004/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2005) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2005", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2005.tif",Aux_file=NA,
    raw_input = "/2005/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2006) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2006", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2006.tif",Aux_file=NA,
    raw_input = "/2006/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2007) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2007", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2007.tif",Aux_file=NA,
    raw_input = "/2007/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2008) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2008", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2008.tif",Aux_file=NA,
    raw_input = "/2008/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2009) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2009", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2009.tif",Aux_file=NA,
    raw_input = "/2009/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Ep (2010) : Ep stands for a reference Evapotranspiration, also called potential ET 
  temp <- data.frame(
    Variable = "Ep2010", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Ep/",sep=""),
    file_name = "Ep2010.tif",Aux_file=NA,
    raw_input = "/2010/",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  
  #Population density (Afripop 2010 numbers per pixel)
  temp <- data.frame(
    Variable = "population", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/population/",sep=""),
    file_name = "population.tif",Aux_file=NA,
    raw_input = "population_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  
  #Livestock density cattle
  temp <- data.frame(
    Variable = "livestock_cattle", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/livestock/",sep=""),
    file_name = "livestock_cattle.tif",Aux_file=NA,
    raw_input = "livestock_cattle_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Livestock density sheep
  temp <- data.frame(
    Variable = "livestock_sheep", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/livestock/",sep=""),
    file_name = "livestock_sheep.tif",Aux_file=NA,
    raw_input = "livestock_sheep_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Livestock density goats
  temp <- data.frame(
    Variable = "livestock_goats", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/livestock/",sep=""),
    file_name = "livestock_goats.tif",Aux_file=NA,
    raw_input = "livestock_goats_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Livestock density pigs
  temp <- data.frame(
    Variable = "livestock_pigs", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/livestock/",sep=""),
    file_name = "livestock_pigs.tif",Aux_file=NA,
    raw_input = "livestock_pigs_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Livestock density chickens
  temp <- data.frame(
    Variable = "livestock_chickens", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/livestock/",sep=""),
    file_name = "livestock_chickens.tif",Aux_file=NA,
    raw_input = "livestock_chickens_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #FAO Irrigated area per pixel (depends on the total area equiped * % area irrigated)
  temp <- data.frame(
    Variable = "Irrigation_area_equiped", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Irrigated_area/",sep=""),
    file_name = "Irrigation_area_equiped.tif",Aux_file=NA,
    raw_input = "Irrigation_area_equiped_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #FAO Irrigated area % per pixel
  temp <- data.frame(
    Variable = "Irrigation_area_actual", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/Irrigated_area/",sep=""),
    file_name = "Irrigation_area_actual.tif",Aux_file=NA,
    raw_input = "Irrigation_area_actual_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Cropland area 500m resolution (VS product)
  temp <- data.frame(
    Variable = "cropland_area", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/cropland_area/",sep=""),
    file_name = "cropland_area.tif",Aux_file=NA,
    raw_input = "cropland_area_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  #Sediment_yield 250m resolution (VS product) 2010
  temp <- data.frame(
    Variable = "sediment_yield", 
    staging_protocol =paste(initial.directory,"/waterthread/Tanzania/sediment_yield/",sep=""),
    file_name = "sediment_yield.tif",Aux_file=NA,
    raw_input = "sediment_yield_raw.tif",data_flag_raw=0,data_flag_processed=0);
  waterliteinputs=rbind(waterliteinputs,temp);
  
  
  return(waterliteinputs);
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
#######################
discharge=function(rainfall,Ep,alpha)
{
  #first convert Ep
  Ep=conversiontomm_y(Ep);
  #drought index
  Drought_index=Ep/rainfall;
  Ea=rainfall*(1+Drought_index-(1+Drought_index^alpha)^1/alpha);
  Q=(rainfall-Ea)/1000000;
  return(Q);
  
}
#######################
#function to write out the updated inputs-table
updatetable=function(location,tableinputs)
{
  write.table(tableinputs,file=location,row.names=F,sep=",");
}
#######################
#function to resmaple meter grids back to latlong
resampleback=function(input,table,resolution,AOI)
{
  vals=c(input);
  dir=paste(table$staging_protocol);
  setwd(dir);
  print("reading in coordinate parameters for grids");
  Xbl=as.matrix(read.table("Xbl.txt",sep=",",header=F));
  Xbr=as.matrix(read.table("Xbr.txt",sep=",",header=F));
  Xtr=as.matrix(read.table("Xtr.txt",sep=",",header=F));
  Xtl=as.matrix(read.table("Xtl.txt",sep=",",header=F));
  Ybl=as.matrix(read.table("Ybl.txt",sep=",",header=F));
  Ybr=as.matrix(read.table("Ybr.txt",sep=",",header=F));
  Ytr=as.matrix(read.table("Ytr.txt",sep=",",header=F));
  Ytl=as.matrix(read.table("Ytl.txt",sep=",",header=F));
  print("Unwrapping coordinates")
  Xbl= c(Xbl[-nrow(Xbl),-ncol(Xbl)]);
  Xbr = c(Xbr[-nrow(Xbr),-ncol(Xbr)]);
  Xtr = c(Xtr[-nrow(Xtr),-ncol(Xtr)]);
  Xtl = c(Xtl[-nrow(Xtl),-ncol(Xtl)]);
  Ybl= c(Ybl[-nrow(Ybl),-ncol(Ybl)]);
  Ybr = c(Ybr[-nrow(Ybr),-ncol(Ybr)]);
  Ytr = c(Ytr[-nrow(Ytr),-ncol(Ytr)]);
  Ytl = c(Ytl[-nrow(Ytl),-ncol(Ytl)]);
  data=cbind(vals, Xbl, Xbr, Xtr, Xtl, Ybl, Ybr, Ytr, Ytl);
  colnames(data)=c("values","Xbl","Xbr","Xtr","Xtl","Ybl","Ybr","Ytr","Ytl");
  meterres=resolution;
  degreeres=destPoint(c(0,0),90,meterres)[1]
  setwd(java_resample_grid_function)#this is where the .jar file is stored to create the grids.
  resolution= degreeres;
  #Creating degree grid parameters
  command=paste("java -jar Create_resampling_grid.jar",ymax(extent(AOI)),xmin(extent(AOI)),ymin(extent(AOI)),xmax(extent(AOI)),'-180','90', resolution,'degrees');
  system(command);
  print("reading in latlong parameters for resamplegrid");
  Xr=as.matrix(read.table("X.txt",sep=",",header=F));
  Yr=as.matrix(read.table("Y.txt",sep=",",header=F));
  
  Xr=c(Xr[-nrow(Xr),-ncol(Xr)]);
  Yr=c(Yr[-nrow(Yr),-ncol(Yr)]);
  
  Xrbl=as.matrix(read.table("Xbl.txt",sep=",",header=F));
  Xrbr=as.matrix(read.table("Xbr.txt",sep=",",header=F));
  Xrtr=as.matrix(read.table("Xtr.txt",sep=",",header=F));
  Xrtl=as.matrix(read.table("Xtl.txt",sep=",",header=F));
  
  Yrbl=as.matrix(read.table("Ybl.txt",sep=",",header=F));
  Yrbr=as.matrix(read.table("Ybr.txt",sep=",",header=F));
  Yrtr=as.matrix(read.table("Ytr.txt",sep=",",header=F));
  Yrtl=as.matrix(read.table("Ytl.txt",sep=",",header=F));
  print("Unwrapping latlong coordinates");
  Xrbl= c(Xrbl[-nrow(Xrbl),-ncol(Xrbl)]);
  Xrbr = c(Xrbr[-nrow(Xrbr),-ncol(Xrbr)]);
  Xrtr = c(Xrtr[-nrow(Xrtr),-ncol(Xrtr)]);
  Xrtl = c(Xrtl[-nrow(Xrtl),-ncol(Xrtl)]);
  
  Yrbl= c(Yrbl[-nrow(Yrbl),-ncol(Yrbl)]);
  Yrbr = c(Yrbr[-nrow(Yrbr),-ncol(Yrbr)]);
  Yrtr = c(Yrtr[-nrow(Yrtr),-ncol(Yrtr)]);
  Yrtl = c(Yrtl[-nrow(Yrtl),-ncol(Yrtl)]);
  data2=cbind(Xrbl, Xrbr, Xrtr, Xrtl, Yrbl, Yrbr, Yrtr, Yrtl);
  colnames(data2)=c("Xbl","Xbr","Xtr","Xtl","Ybl","Ybr","Ytr","Ytl");
  print("writing parameters to file");
  setwd("/Users/private/Vital_signs/");
  write.table(data,file="data.txt", col.names=F,row.names=F,sep=",");
  write.table(data2,file="data2.txt", col.names=F,row.names=F,sep=",");
  #running java resampling (shuold take +- 3 hours);
  setwd("/Users/private/Vital_signs/resampleback_java1.8");
  command=paste("java -jar resampleback.jar","/Users/private/Vital_signs/data.txt","/Users/private/Vital_signs/data2.txt",length(data[,1]),length(data[1,]),length(data2[,1]),length(data2[1,]),"false");
  system(command);
  s=read.table("resampleoutput.txt",header=F);
  sout=cbind(Xr,Yr,s);#this should be the resampled data
  rast=rasterFromXYZ(sout,crs=crs(AOI));
  #re-interpolate to get rid of any na's
  rast=interpolategrid(rast,6);
  #plot output
  plot(rast);
  return(rast);
  
}

polygonextent=function(p,bX,bY,resolution,id)
{
  #returns a ploygon representing the extent of the point p
  #bX and bY and the bearings in the X and Y directions to get the extent
  #resolution is the size of the pixel in meters
  #id is the id you wish to attach to the poygon
  
  #get the extent to create a spatial polygon
  #top left corner
  tl= destPoint(p,360-bX, resolution/2);
  tl= destPoint(tl,180-bY, resolution/2);
  xtl=tl[1];
  ytl=tl[2];
  #top right corner
  tr=destPoint(tl,bX, resolution);
  xtr=tr[1];
  ytr=tr[2];
  #bottom left
  bl=destPoint(tl,bY, resolution);
  xbl=bl[1];
  ybl=bl[2];
  #bottom right
  br=destPoint(bl,bX, resolution);
  xbr=br[1];
  ybr=br[2];
  
  Xcoord =c(xtl,xtr,xbr,xbl);
  Ycoord =c(ytl,ytr,ybr,ybl);
  
  return(Polygons(list(Polygon(cbind(Xcoord,Ycoord))), ID=id));
  
}
#####################
#function for top 30cm soil
checkorprocess_soilgrid30cm=function(grid,table,java,AOI,agg,agg2,aggamount,aggfunction,resolution,parseraster)
{
  if(!require(raster)) install.packages('raster'); require(raster);
  if(!require(rgdal)) install.packages('rgdal'); require(rgdal);
  dir=paste(table$staging_protocol,table$file_name,sep="");
  if(file.exists(dir[14])){#swap out values
    grid=checkandpopulategrid(grid,dir);
    #grid=raster(grid);
    table$data_flag_processed[14]=1
  }else{
    #check raw inputs and process if necessary
    rawdir=paste(table$staging_protocol,table$raw_input,sep="");
    if(file.exists(rawdir[14]))
    {
      table$data_flag_raw[14]=1;
      #record start time of calculation
      starttime=Sys.time();
      #first do the calculation in raw coordinate form i.e lea from afsis
      #get all the soil layers and crop them to our AOI
      e2=raster(rawdir[3]);
      e4=spTransform(AOI, projection(e2));
      BLD_sd1_M=crop(e2,e4);
      e2=raster(rawdir[4]);
      BLD_sd2_M=crop(e2,e4);
      e2=raster(rawdir[5]);
      BLD_sd3_M=crop(e2,e4);
     
      e2=raster(rawdir[8]);
      ORCDRC_sd1_M=crop(e2,e4);
      e2=raster(rawdir[9]);
      ORCDRC_sd2_M=crop(e2,e4);
      e2=raster(rawdir[10]);
      ORCDRC_sd3_M=crop(e2,e4);
     
      
      #Do calculation for the top30cm
      top30=(BLD_sd1_M * ORCDRC_sd1_M*.05*10)+(BLD_sd2_M * ORCDRC_sd2_M*.1*10)+(BLD_sd3_M * ORCDRC_sd3_M * 0.15*10);
      writeRaster(top30,"temp.tif")
      e2=readGDAL("temp.tif")
      #get coords in WGS84 latlong 
      e3=spTransform(e2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"));
      #determine the equivalent resolution in degrees
      res=destPoint(c(0,0),90,e2@grid@cellsize[1])[1];
      command=paste("java -jar Create_resampling_grid.jar",ymax(extent(AOI)),xmin(extent(AOI)),ymin(extent(AOI)),xmax(extent(AOI)),'-180','90', res,"degrees")
      setwd(java_resample_grid_function);
      system(command);
      resx=as.matrix(read.table("X.txt",sep=",",header=F));
      resy=as.matrix(read.table("Y.txt",sep=",",header=F));
      #get parameters to do a nearest neighbour reprojection
      x=c(resx);
      y=c(resy)
      vals=values(soil);
      xv=coordinates(e3)[,1]
      yv=coordinates(e3)[,2]
      #perform nn reprojection
      n=length(x);
      starttime=Sys.time();
      output=matrix(data=NA,nrow=n,ncol=1);
      for(i in 1:n)
      {
        percent=i/n*100;
        print(paste(percent, " % nn reproj complete"));
        output[i]=vals[which.min(abs(x[i]-xv)+abs(y[i]-yv))];
      }
      Sys.time()-starttime;
      #save the output as a raster
      d=matrix(unlist(output[,1]), nrow = nrow(resx), byrow = F);
      d2=raster(d);
      extent(d2)=extent(AOI);
      #do normal checkandprocessgrid function
      starttime=Sys.time();
      grid=checkandprocessgrid(java,grid,initial.directory,d2,AOI,agg,dir[14],agg2,aggamount,aggfunction,resolution,parseraster)
      Sys.time()-starttime
      table$data_flag_processed[14]=1;
    }else
    {#leave the original grid inplace with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
      # grid=raster(grid);
      table$data_flag_raw[14]=0;
      table$data_flag_processed[14]=0;
    }
    
  }
}

checkorprocess_soilgrid1m=function(grid,table,java,AOI,agg,agg2,aggamount,aggfunction,resolution,parseraster)
{
  if(!require(raster)) install.packages('raster'); require(raster);
  if(!require(rgdal)) install.packages('rgdal'); require(rgdal);
  dir=paste(table$staging_protocol,table$file_name,sep="");
  if(file.exists(dir[13])){#swap out values
    grid=checkandpopulategrid(grid,dir);
    #grid=raster(grid);
    table$data_flag_processed[13]=1
  }else{
    #check raw inputs and process if necessary
    rawdir=paste(table$staging_protocol,table$raw_input,sep="");
    if(file.exists(rawdir[13]))
    {
      table$data_flag_raw[13]=1;
      #record start time of calculation
      starttime=Sys.time();
      #first do the calculation in raw coordinate form i.e lea from afsis
      #get all the soil layers and crop them to our AOI
      e2=raster(rawdir[3]);
      e4=spTransform(AOI, projection(e2));
      BLD_sd1_M=crop(e2,e4);
      e2=raster(rawdir[4]);
      BLD_sd2_M=crop(e2,e4);
      e2=raster(rawdir[5]);
      BLD_sd3_M=crop(e2,e4);
      e2=raster(rawdir[6]);
      BLD_sd4_M=crop(e2,e4);
      e2=raster(rawdir[7]);
      BLD_sd5_M=crop(e2,e4);
      e2=raster(rawdir[8]);
      ORCDRC_sd1_M=crop(e2,e4);
      e2=raster(rawdir[9]);
      ORCDRC_sd2_M=crop(e2,e4);
      e2=raster(rawdir[10]);
      ORCDRC_sd3_M=crop(e2,e4);
      e2=raster(rawdir[11]);
      ORCDRC_sd4_M=crop(e2,e4);
      e2=raster(rawdir[12]);
      ORCDRC_sd5_M=crop(e2,e4);
      
      #Do calculation for the top30cm
      top30=(BLD_sd1_M * ORCDRC_sd1_M*.05*10)+(BLD_sd2_M * ORCDRC_sd2_M*.1*10)+(BLD_sd3_M * ORCDRC_sd3_M * 0.15*10);
      bottom70=(BLD_sd4_M * ORCDRC_sd4_M * 0.3 * 10) + (BLD_sd5_M * ORCDRC_sd5_M * 0.4 * 10);
      total1m=top30+bottom70;
      
      writeRaster(total1m,"temp.tif")
      e2=readGDAL("temp.tif")
      #get coords in WGS84 latlong 
      e3=spTransform(e2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"));
      #determine the equivalent resolution in degrees
      res=destPoint(c(0,0),90,e2@grid@cellsize[1])[1];
      command=paste("java -jar Create_resampling_grid.jar",ymax(extent(AOI)),xmin(extent(AOI)),ymin(extent(AOI)),xmax(extent(AOI)),'-180','90', res,"degrees")
      setwd(java_resample_grid_function);
      system(command);
      resx=as.matrix(read.table("X.txt",sep=",",header=F));
      resy=as.matrix(read.table("Y.txt",sep=",",header=F));
      #get parameters to do a nearest neighbour reprojection
      x=c(resx);
      y=c(resy)
      vals=values(soil);
      xv=coordinates(e3)[,1]
      yv=coordinates(e3)[,2]
      #perform nn reprojection
      n=length(x);
      starttime=Sys.time();
      output=matrix(data=NA,nrow=n,ncol=1);
      for(i in 1:n)
      {
        percent=i/n*100;
        print(paste(percent, " % nn reproj complete"));
        output[i]=vals[which.min(abs(x[i]-xv)+abs(y[i]-yv))];
      }
      Sys.time()-starttime;
      #save the output as a raster
      d=matrix(unlist(output[,1]), nrow = nrow(resx), byrow = F);
      d2=raster(d);
      extent(d2)=extent(AOI);
      #do normal checkandprocessgrid function
      starttime=Sys.time();
      grid=checkandprocessgrid(java,grid,initial.directory,d2,AOI,agg,dir[13],agg2,aggamount,aggfunction,resolution,parseraster)
      Sys.time()-starttime
      table$data_flag_processed[13]=1;
    }else
    {#leave the original grid inplace with pre-defined vals, but remove buffer col and row 
      grid=grid[-nrow(grid),-ncol(grid)];
      # grid=raster(grid);
      table$data_flag_raw[13]=0;
      table$data_flag_processed[13]=0;
    }
    
  }
}



#######################