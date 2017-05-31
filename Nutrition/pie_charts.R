library(ggplot2)
library(reshape2)
library(dplyr)
library(png)
library(grid)
library(maps)
library(mapdata)
library(fields)

nut <- read.csv('D:/Documents and Settings/mcooper/GitHub/vs-indicators-calc/Nutrition/Nutrition.Individual.csv', stringsAsFactors=F)

setwd('D:/Documents and Settings/mcooper/Documents/Nutrition Pie Charts/')

nut$no_stunting[!is.na(nut$stunting) & !is.na(nut$severe_stunting)] <- 0
nut$no_stunting[nut$stunting==0] <- 1
nut$stunting[nut$severe_stunting==1] <- 0
                
nut$no_wasting[!is.na(nut$wasting) & !is.na(nut$severe_wasting)] <- 0       
nut$no_wasting[nut$wasting==0] <- 1
nut$wasting[nut$severe_wasting==1] <- 0

nut$no_underweight[!is.na(nut$underweight) & !is.na(nut$severe_underweight)] <- 0
nut$no_underweight[nut$underweight==0] <- 1
nut$underweight[nut$severe_underweight==1] <- 0

nut$any[!is.na(nut$stunting) & !is.na(nut$wasting) & !is.na(nut$underweight)] <- 0
nut$severe_any[!is.na(nut$stunting) & !is.na(nut$wasting) & !is.na(nut$underweight)] <- 0
nut$no_any[!is.na(nut$stunting) & !is.na(nut$wasting) & !is.na(nut$underweight)] <- 0

nut$severe_any[nut$severe_stunting==1 | nut$severe_underweight==1 | nut$severe_wasting] <- 1
nut$any[(nut$stunting==1 | nut$underweight==1 | nut$wasting) & nut$severe_any==0] <- 1
nut$no_any[(nut$no_stunting==1 | nut$no_underweight==1 | nut$no_wasting) & nut$any==0] <- 1

generateIndividuals <- function(c, l, v, nut){
  cols <- c('Individual.ID', names(nut)[grepl(v, names(nut))])
  
  sel <- nut %>% filter(country==c, landscape_no==l) %>%
    select_(.dots=cols) %>%
    melt(id.vars=c('Individual.ID')) %>%
    filter(value==1)
  
  if(length(unique(sel$Individual.ID)) < nrow(sel)){
    cat('There are duplicate Individuals in ', c, ' ', l, ' ', v, '\n')
  }
  
  ggplot(sel, aes(x='', y=value, fill=factor(variable))) + 
    geom_bar(width=10, stat="identity") + 
    coord_polar("y", start=0) + xlab('') + ylab('') + 
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank()) + 
    scale_fill_manual(values = c("severe_stunting"='#f6ff00', "stunting"="#61ff00", "no_stunting"='#2b8e12',
                                 "severe_underweight"='#f6ff00', "underweight"="#61ff00", "no_underweight"='#2b8e12',
                                 "severe_wasting"='#f6ff00', "wasting"="#61ff00", "no_wasting"='#2b8e12',
                                 "severe_any"='#f6ff00', "any"="#61ff00", "no_any"='#2b8e12')) + 
    scale_y_continuous(expand=c(0,0))
  
  ggsave(paste0(v, '-', c, '-', l, '.png'), bg = "transparent", width=6, height=6, units="in")
}

generateGroups <- function(c, l){
  any_img <- readPNG(paste0("any-", c, '-', l, ".png")) %>% rasterGrob(interpolate=T)
  stu_img <- readPNG(paste0("stunting-", c, "-", l, ".png")) %>% rasterGrob(interpolate=T)
  was_img <- readPNG(paste0("wasting-", c, "-", l, ".png")) %>% rasterGrob(interpolate=T)
  und_img <- readPNG(paste0("underweight-", c, "-", l, ".png")) %>% rasterGrob(interpolate=T)
  
  qplot(0:12, 0:12, geom='blank') +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank()) +
    xlab('') + 
    ylab('') +
    annotation_custom(any_img, xmin=2, xmax=10, ymin=4, ymax=12) + 
    annotation_custom(stu_img, xmin=0, xmax=4, ymin=1, ymax=5) + 
    annotation_custom(was_img, xmin=4, xmax=8, ymin=1, ymax=5) + 
    annotation_custom(und_img, xmin=8, xmax=12, ymin=1, ymax=5)
  
  ggsave(paste0(c, '-', l, '.png'), bg = "transparent", width=6, height=6, units="in")
}

generateMaps <- function(c, nut){

png(paste0('nutrition_', c, '.png'), width = 6000, height = 6000)

sel <- unique(nut[nut$country==c, c('landscape_no', 'latitude', 'longitude')])

cty <- switch(c, "UGA"=map('worldHires', "Uganda", fill=T, col='#ccbbab'), 
                 "TZA"=map('worldHires', "Tanzania", ylim=c(-11.8504064, -6), fill=T, col='#ccbbab'), 
                 "KEN"=map('worldHires', "Kenya", fill=T, col='#ccbbab'), 
                 "RWA"=map('worldHires', "Rwanda", fill=T, col='#ccbbab'), 
                 "GHA"=map('worldHires', "Ghana", fill=T, col='#ccbbab'))

pr <- par()$usr

len <- abs(pr[2]-pr[1])/15

if (c == 'TZA'){
  len <- 0.35 
}

for (i in 1:nrow(sel)){
  img <- readPNG(paste0(c, '-', sel$landscape_no[i], ".png"))
  rasterImage(img, xleft=sel$longitude[i]-len, ybottom=sel$latitude[i]-len, xright=sel$longitude[i]+len, ytop=sel$latitude[i]+len)
}

if (c == 'TZA'){
  len <- 0.7 
}else if (c == 'GHA'){
  len <- 0.4
}

leg <- readPNG('legend4.png')
rasterImage(leg, xleft=pr[2]-len*3, xright=pr[2], ybottom=pr[3], ytop=pr[3]+len*3)

dev.off()
}

for (c in unique(nut$country)){
  ls <- unique(nut[nut$country==c, 'landscape_no'])
  for (l in ls){
    for (v in c('stunting', 'wasting', 'underweight', 'any')){
      generateIndividuals(c, l, v, nut)
    }
    generateGroups(c, l)
  }
}

for (c in unique(nut$country)){
  generateMaps(c, nut)
}
