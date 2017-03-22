library(dplyr)

setwd('D:/Documents and Settings/mcooper/GitHub/vs-indicators-calc/Biophysical/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

con <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

shannon_entropy <- function(vect, base=exp(1)){
  vect <- vect[!is.na(vect)]
  tb <- table(vect)
  p <- tb/sum(tb)
  H <- -sum(p*log(p, base=base))
  return(H)
}

random_round <- function(vect, freq){
  n <- length(vect)*freq
  remainder <- n%%1
  true <- rep(TRUE, remainder*1000)
  false <- rep(FALSE, (1-remainder)*1000)
  value <- sample(c(true, false), 1)
  
  if (value){
    n <- ceiling(n)
  }else{
    n <- floor(n)
  }
  
  return(sample(vect, n))
}

bootstrap_diversity <- function(vector, freq, iter){
  freq <- unique(freq)
  
  ents <- NULL
  
  for (j in 1:iter){
    samp <- random_round(vector, freq)
    ents <- c(ents, shannon_entropy(samp))
  }
  
  return(mean(ents))
}

####################
#Biodiversity
####################

#The area of the plots are not the same, so we have to somehow adjust that.  Restrict to smallest plot size?
eplot <- tbl(con, 'flagging__eplot') %>% select(survey_uuid, Country, latitude, longitude, `Subplot Radius`, flag) %>% data.frame %>%
  filter(!grepl('Subplot Radius', flag) & !is.na(Subplot.Radius))

radii <- unique(eplot$Subplot.Radius)
area <- pi*unique(eplot$Subplot.Radius)^2
ratio <- pi*2^2/area

subplot <- data.frame(Subplot.Radius=radii, area, ratio)

eplot <- merge(eplot, subplot)

species <- tbl(con, 'flagging__eplot_woody_plant') %>% select(survey_uuid, Country, Genus, Species) %>% data.frame

species <- merge(eplot, species)

species <- species %>% filter(Genus != 'Musa')

species_sum <- species %>% group_by(survey_uuid, latitude, longitude, Country) %>% 
  summarize(biodiversity=bootstrap_diversity(vector=paste0(Genus, Species), freq=ratio, iter=500))

######################
#Tree Cover
######################

treecov <- tbl(con, 'flagging__eplot_subplot_woody_canopy') %>%
  select(survey_uuid, Country, latitude, longitude, 
         `Densiometer N`, `Densiometer S`, `Densiometer E`, `Densiometer W`) %>%
  data.frame %>%
  group_by(survey_uuid, Country, latitude, longitude) %>%
  summarize(Canopy = (mean((Densiometer.N + Densiometer.S + Densiometer.E + Densiometer.W)/4)))

######################
#LandscapeFN
######################

landscapefn <- tbl(con, 'flagging__eplot_subplot_landscapefn') %>%
  data.frame %>%
  group_by(survey_uuid, Country, latitude, longitude) %>%
  summarize(Rooted.Plants = mean(Rooted.Plants, na.rm=T),
            Litter.Cover = mean(Litter.Cover, na.rm=T),
            Downed.Wood = mean(Downed.Wood, na.rm=T),
            Stone.or.Gravel = mean(Stone.or.Gravel, na.rm=T),
            Dung = mean(Dung, na.rm=T),
            Disturbed.Soil = mean(Disturbed.Soil, na.rm=T),
            Undisturbed.Bare...Porous.Soil = mean(Undisturbed.Bare...Porous.Soil, na.rm=T),
            Undisturbed.but.Sealed.Soil = mean(Undisturbed.but.Sealed.Soil, na.rm=T),
            Sodic.Soil = mean(Sodic.Soil, na.rm=T),
            Microfloral.Crust = mean(Microfloral.Crust, na.rm=T),
            Termite.Mound = mean(Termite.Mound, na.rm=T),
            Herbaceous.Crown.Cover.Score = mean(Herbaceous.Crown.Cover.Score, na.rm=T),
            Soil.Erosion.Score = mean(Soil.Erosion.Score, na.rm=T))

#####################
#Soil
#####################

soils <- tbl(con, 'eplotsoils_processed') %>%
  select(Country=country, Landscape..=landscape_no, Eplot..=eplot_no, total_carbon, 
         total_nitrogen, ph, k, p) %>% data.frame %>%
  group_by(Country, Landscape.., Eplot..) %>%
  summarize(C=mean(total_carbon, na.rm=T),
            N=mean(total_nitrogen, na.rm=T),
            P=mean(p, na.rm=T),
            K=mean(k, na.rm=T),
            pH=mean(ph, na.rm=T))

###################
#All Together
###################

eplot <- tbl(con, 'flagging__eplot') %>% 
  select(latitude, longitude, Country, survey_uuid, `Landscape #`, `Eplot #`, flag) %>%
  data.frame %>%
  filter(!grepl("GPS NEpoint not within landscape boundary", flag))

eplot <- merge(eplot, species_sum, by=c('latitude', 'longitude', 'Country', 'survey_uuid'), all.x=T, all.y=F)

eplot <- merge(eplot, treecov, by=c('latitude', 'longitude', 'Country', 'survey_uuid'), all.x=T, all.y=F)

eplot <- merge(eplot, landscapefn, by=c('latitude', 'longitude', 'Country', 'survey_uuid'), all.x=T, all.y=F)

eplot <- merge(eplot, soils, by=c('Country', 'Landscape..', 'Eplot..'), all.x=T, all.y=F)

#########################################
#Write
#################################

library(aws.s3)
aws.signature::use_credentials()

writeS3 <- function(df, name){
  names(df) <- gsub('.', '_', names(df), fixed=T)
  names(df)[names(df)=='Landscape__'] <- 'Landscape'
  names(df)[names(df)=='Eplot__'] <- 'Eplot'
  
  zz <- rawConnection(raw(0), "r+")
  write.csv(df, zz, row.names=F)
  aws.s3::put_object(file = rawConnectionValue(zz),
                     bucket = "vs-cdb-indicators", object = name)
  close(zz)
}

writeS3(eplot, 'Biophysical.csv')


