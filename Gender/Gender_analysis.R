library(ggplot2)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Combine/')

hh <- read.csv('hh_level.csv')

hh$Land <- paste0(hh$Country, '-', hh$Landscape..)
hh <- hh %>% select(-Male.Gender, -Female.Gender)

df <- data.frame()
for(i in names(hh)[c(6:84,86:127)]){
  print(i)
  mod <- aov(as.formula(paste0(i, '~HH.Head.Gender + Country + Land')), data=hh) %>% summary
  
  df2 <- data.frame(Category = i,
                    Male_mean = mean(hh[hh$HH.Head.Gender=='Male' , i], na.rm=T),
                    Male_sd = sd(hh[hh$HH.Head.Gender=='Male' , i], na.rm=T),
                    Female_mean = mean(hh[hh$HH.Head.Gender=='Female' , i], na.rm=T),
                    Female_sd = sd(hh[hh$HH.Head.Gender=='Female' , i], na.rm=T),
                    P_score = mod[[1]]['Pr(>F)'][[1]][1],
                    F_value = mod[[1]]['F value'][[1]][1])
  
  df <- bind_rows(df, df2)
}

write.csv(df, '../Gender/Gender_Correlates.csv', row.names=F)



