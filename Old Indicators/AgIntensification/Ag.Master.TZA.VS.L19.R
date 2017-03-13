#########################
# Ag Intensification join file
# Area: TZA - L19
# September 24, 2015
#########################
# data sources
data_dir1 <- "C:/Users/Administrator/Dropbox/'EI- Vital Signs/R coding of Threads/Ag Intensification/New code"
  #use updated "Ag.FieldData.VS.R in "Github - 09042015" folder
  data_dir2 <- "C:/Users/Administrator/Dropbox/'EI- Vital Signs/R coding of Threads/Ag Intensification/New code/Github - 09042015"
  #use updated "Ag.FieldData.VS.R in "Github - 09282015" folder
  data_dir3 <- "C:/Users/Administrator/Dropbox/'EI- Vital Signs/R coding of Threads/Ag Intensification/New code/Github - 09282015"
  
# data outputs
out_dir1 <- "C:/Data/Vital Signs/Ag Intensification/Outputs"

#########################

# 1. Run Ag.FieldData.R, Ag.PotentialYield.R, and Ag.CultivatedArea.R files: 

# AI10, AI12, AI13, AI16 (AI14, AI15, AI17)
# df1=field_data contains  (unique on hhid and field)
#       maize yield in kg/ha
#       n usage in kg/ha 
#       p usage in kg/ha
#       indicator for irrigation (1 = yes, 0 = no)
# AI7(AI8) - number of crops per year
# df2=num_crops contains  (unique on hhid and field)
#       number of times maize was grown
# AI35 - % cultivated allocated to crop (here: maize)
# df3= pct_maize is the % area devoted to maize (unique to household)
# source: LSMS (Vital signs surveys)
setwd(data_dir2)
run1<-parse("Ag.FieldData.VS.R")
eval(run1)
  # only keep relevant LANDSCAPE and YEAR: 
    #LANDSCAPE: L19
    #YEAR: 2014
    #the team visited L19 in 2014; however, some might get enterred in 2015; keep those 
    field_data <- subset(FieldData.TZA.VS, Landscape == "L19" & (Data.entry.year == "2014"| Data.entry.year == "2015"))
    num_crops <- subset(NumCrops.TZA.VS, Landscape == "L19" & (Data.entry.year == "2014"| Data.entry.year == "2015"), 
                    select = c(country_y3hhid_plotnum, y3_hhid, plotnum, numcrops))
    pct_maize <- subset(PctMaize.TZA.VS, Landscape == "L19" & (Data.entry.year == "2014"| Data.entry.year == "2015"), 
                    select = c(country_y3hhid_plotnum, y3_hhid, maizeareapercent))      

# AI11 - yp.sagcot is yield potential in kg/ha 
# source: IISA Total production capacity geotiff
data_dir3 <- "C:/Users/Administrator/Dropbox/'EI- Vital Signs/R coding of Threads/Ag Intensification/New code/Github - 09282015"
setwd(data_dir3)
run2 <- parse("Ag.PotentialYield.TZA.R") 
eval(run2)
AI11 <- yp.L19

# AI20 - cultivated_area in ha
  # source: "TZA.AreaCultivated.Family5.csv"output table 
  # generated from "Ag.CultivatedAreaFamily5.TZA.R" script 
out_dir1 <- "C:/Data/Vital Signs/Ag Intensification/Outputs"
setwd(out_dir1)
Family5 <- read.csv("TZA.AreaCultivated.Family5.csv")
  cult.area <- Family5[3, "Cult.area"]
  area <- Family5[3, "Area"]

# 2. Calculate the indicators: 

# AI39 - output intensity is maize yield times # of crops per year
#       merge df1=field_data and df2=num_crops
#       output_intensity <- df1$yield * df2$num_crops
field_data <- merge(field_data, num_crops, by ="country_y3hhid_plotnum")
output_intensity <- field_data$cropyieldkgperha * field_data$numcrops

AI39 <- mean(output_intensity, na.rm=TRUE)
AI39.med <- median(output_intensity, na.rm=TRUE)
AI39.sd <- sd(output_intensity, na.rm=TRUE)

# AI17(AI18) # of crops per year
AI17 <- mean(field_data$numcrops, na.rm=TRUE)

# AI42 - extent for maize is pct_maize times cultivated area for maize
#       extent_maize <- pct_maize (AI35) * cultivated_area
AI35 <- mean(pct_maize$maizeareapercent, na.rm=TRUE)  
AI42 <- AI35 * cult.area 

# AI47 - integrated output intensity is output intensity times extent for maize
#       integrated_output <- mean(output_intensity) (AI39) * extent_maize (AI42)
AI47 <- AI39 * AI42

# AI40 - attainable yield is potential yield times 0.8
#       attainable <- yp.sagcot (AI11) *0.8
AI40 <- 0.8 * AI11

# AI48 - yield gap is difference between attainable yield and avg realized yield
#       yield_gap <- attainable (AI40) - realized yields (AI38)
AI38 <- mean(field_data$cropyieldkgperha, na.rm=TRUE)
AI38.med <- median(field_data$cropyieldkgperha, na.rm=TRUE)
AI38.sd <- sd(field_data$cropyieldkgperha, na.rm=TRUE)
AI48 <- AI40 - AI38

# AI34 
# Input needed for optimal yield is inputs / optimal input amount
#       df1$n <- df1$n / 100
#       df1$p <- df1$p / 30
field_data$n <- field_data$TotNaddedkghaInorg / 100
field_data$p <- field_data$TotPaddedkghaInorg / 30

# Intensity per factor is factor times # of cropping seasons
# merge df1 and df2 by hhid, field id # already done above
#       df3 <- merge(df1, df2) # already done above
#       df3$n <- df3$n * df3$num_crops
#       df3$p <- df3$p * df3$num_crops
field_data$n_int <- field_data$n * field_data$numcrops
field_data$p_int <- field_data$p * field_data$numcrops
AI34_N <- mean(field_data$n_int, na.rm=TRUE)
AI34_P <- mean(field_data$p_int, na.rm=TRUE)

# AI41 - Combined input intensity per crop or land use
#       combined impact intensity is the weighted sum of average intensity factors
#       AI41 <- intensity_mean$n * 0.2 +  intensity_mean$p * 0.1 + intensity_mean$irr * 0.1 + 0.6 
AI41 <- mean(field_data$n_int, na.rm=TRUE)* .2 + mean(field_data$p_int, na.rm=TRUE)*0.1 + 0.6

# AI49 - Agriculture input intensity (impact/ha/yr/crop or land use)
AI49 <- (AI41 * AI42) / cult.area

# AI50 - Landscape imput impact intensity
AI50 <- (AI41 * AI42) / area

# set output directory: 
setwd(out_dir1)

# write output: 
country <- "TZA"
scale <- "Landscape"
year <- "2014"
landscape <- "L19"
output <- data.frame(country, scale, year, landscape, AI41, AI47, AI48, AI49, AI50)
output$AI41 <- round(output$AI41, 3)
output$AI47 <- round(output$AI47, 0)
output$AI48 <- round(output$AI48, 0)
output$AI49 <- round(output$AI49, 3)
output$AI50 <- round(output$AI50, 3)

write.csv(output, file = "Ag.Intensification.TZA.VS.L19.2014.csv")