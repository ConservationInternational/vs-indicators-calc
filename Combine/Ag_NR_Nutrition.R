library(ggplot2)
library(dplyr)
library(sp)
library(raster)

setwd('../Combine')

nr <- read.csv('../NaturalResources/NaturalResources.HH.csv')
nut <- read.csv('../Nutrition/Nutrition.Individual.csv')
ag <- read.csv('../Ag/AgValue.HH.csv')

df <- Reduce(merge, list(nr, nut, ag))

df <- df %>% filter(Country != 'GHA')

df$NR_Value <- df$total_fuelwood_value + df$hh_annual_nonfuel_nr_value

df$Ag_Value <- rowSums(df[ , c("Crops", "Permanent.Crops", 
                              "Livestock.Byproducts", "Livestock", "Crop.Byproducts")])

df <- merge(df, data.frame(Country=c("RWA", "UGA", "TZA"), USD=c(824.3686, 3594.0071, 2232.2166)))

df$NR_Value <- df$NR_Value/df$USD
df$Ag_Value <- df$Ag_Value/df$USD

df$Ag_Value[df$Ag_Value < 0] <- 10

df$mzlen <- df$zlen < -2

ggplot(df) + geom_point(aes(x=log(Ag_Value), y=log(NR_Value), color=zlen))# + facet_wrap(~Country)

######################
##Try Binning
#####################

sigmean <- function(x, na.rm=T){
  if(na.rm){
    x <- na.omit(x)
  }
  if (length(x) > 1){
    return(mean(x))
  }
  else{
    return(NA)
  }
}


sp <- df
sp$y <- log(sp$NR_Value)
sp$x <- log(sp$Ag_Value)
coordinates(sp) <- ~ x + y

r <- raster(nrows=12, ncols=14, xmn=.5, xmx=12, ymn=0.5, ymx=9)
out <- rasterize(sp, r, field='mzlen', fun=sigmean)

outdf <- out %>% SpatialPoints %>% as.data.frame()
outdf$stun <- out %>% as.data.frame
outdf$Percent.Stunted <- outdf$stun * 100

ggplot() +
  geom_tile(data=outdf,aes(x=x,y=y,fill=Percent.Stunted), alpha = ifelse(is.na(outdf$stun), 0, 1)) + 
  scale_fill_gradient(low='#2aaf2a', high='#efef2f') + theme_bw() + 
  #geom_point(data=df, aes(x=log(Ag_Value), y=log(NR_Value), color=as.numeric(mzlen)), alpha = ifelse(is.na(df$zlen), 0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits=c(2.142856,10.35715), breaks = seq(2.142856,10.35715,by=11.5/14),
                     labels = signif(exp(seq(2.142856,10.35715,by=11.5/14)), 2)) +
  scale_y_continuous(expand = c(0, 0), limits=c(2.624999,7.583334), breaks = seq(2.624999,7.583334,by=8.5/12),
                     labels = signif(exp(seq(2.624999,7.583334,by=8.5/12)), 2)) +
  ggtitle("Agriculture, Natural Resources, and Stunting") + 
  xlab('Annual Value of Agricultural Production (USD)') + 
  ylab('Annual Value of Natural Resource Gathering (USD)')
