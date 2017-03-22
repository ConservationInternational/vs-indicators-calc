library(ggplot2)
library(dplyr)
library(reshape2)

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

df <- df %>% group_by(Country, Landscape..) %>% summarize(Stunting=mean(stunting, na.rm=T), 
                                                          Natural.Resource.Use=mean(hh_annual_nonfuel_nr_value, na.rm=T),
                                                          Agricultural.Production=mean(Ag_Value, na.rm=T)) %>%
  filter(Country == 'UGA')

df$Stunting <- df$Stunting/max(df$Stunting)
df$Natural.Resource.Use <- df$Natural.Resource.Use/max(df$Natural.Resource.Use)
df$Agricultural.Production <- df$Agricultural.Production/max(df$Agricultural.Production)

df <- melt(df, id.vars=c('Country', 'Landscape..'))
df$variable <- gsub('.', ' ', df$variable, fixed=T)

ggplot(df %>% filter(Landscape..=='L01'), aes(x=variable, y=value^2)) +
  geom_bar(aes(fill=variable), stat='identity') + theme_bw() + 
  scale_fill_manual(values=c('#FFCC33', '#669933', '#c04420')) + 
  ylab('') + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),legend.position="none",
        panel.grid.minor=element_blank(),plot.background=element_blank()) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1.05))
ggsave('D://Documents and Settings/mcooper/Desktop/Images For Presentation/NR.png')

ggplot(df %>% filter(Landscape..=='L03'), aes(x=variable, y=value^2)) +
  geom_bar(aes(fill=variable), stat='identity') + theme_bw() + 
  scale_fill_manual(values=c('#FFCC33', '#669933', '#c04420')) + 
  ylab('') + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),legend.position="none",
        panel.grid.minor=element_blank(),plot.background=element_blank()) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1.05))
ggsave('D://Documents and Settings/mcooper/Desktop/Images For Presentation/Ag.png')

ggplot(df %>% filter(Landscape..=='L04'), aes(x=variable, y=value^2)) +
  geom_bar(aes(fill=variable), stat='identity') + theme_bw() + 
  scale_fill_manual(values=c('#FFCC33', '#669933', '#c04420')) + 
  ylab('') + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),legend.position="none",
        panel.grid.minor=element_blank(),plot.background=element_blank()) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1.05))
ggsave('D://Documents and Settings/mcooper/Desktop/Images For Presentation/Stunting.png')


ggplot(df %>% filter(Landscape..=='L01'), aes(x=variable, y=value^2)) +
  geom_bar(aes(fill=variable), stat='identity') + theme_bw() + 
  scale_fill_manual(values=c('#FFCC33', '#669933', '#c04420')) + 
  #ylab('High Natural Resource Use')
ggsave('D://Documents and Settings/mcooper/Desktop/Images For Presentation/ForLegend.png')

library(maps)
library(mapdata)

ls <- tbl(con, 'flagging__household_secE') %>% 
  filter(Country == 'UGA') %>% 
  #select(x=longitude, y=latitude, Country, `Landscape #`) %>%
  data.frame %>% unique

ls <- ls[ , c('Country', 'Landscape..', 'latitude', 'longitude')] %>% unique

map('worldHires', "Uganda", fill=T, col='#A4D65E')
points(ls$longitude, ls$latitude, pch=0, col='#FF6C2F', cex=1.2)
