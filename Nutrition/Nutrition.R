#############################
# Workflow - Nutrition
# functions
# 
#############################

#################
# classes
#################

# creates a class called myDate which casts a string to date in the format specified
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d") )

#################
# functions
#################

# anthro takes a dataframe containing the WHO zscores and (optionally) 
#     the sampling weights, and returns the prevalence rate
#
# parameters
#   df        = name of the dataframe
#   indicator = name of the column of interest (zwei for underweight, 
#                                               zlen for stunting,
#                                               zwfl for wasting,
#                                               zbmi for bmi)
#   z         = zscore threshold (set to 2 for indicator, set to 3 for severe)
#   weights   = set to TRUE if sampling weights are included in the WHO output
anthro <- function(df, indicator, z, lt = TRUE, f, weights=FALSE) {
  
  if (!(is.data.frame(df) & nrow(df) == 0)) {
    # remove NA responses for the indicator measure
    df <- df[!is.na(df[indicator]), ]
    
    # remove infeasible
    df <- df[!is.na(df[f]), ]
  
    if (lt) {
      # retain indicator values where indicator < z
      z.df <- df[df[indicator] < z, ]    
    } else {
      # retain indicator values where indicator >= z
      z.df <- df[df[indicator] >= z, ]    
    }
  
    if (weights) {
      # return sampling weighted proportion
      return (sum(z.df$sw) / sum(df$sw))
    }
    else {
      # return sample proportion
      return (dim(z.df)[1] / dim(df)[1])
    }
  } else {
    return (NA)
  }
}

# out_nutrition takes a dataframe containing the WHO zscores and (optionally) 
#     the sampling weights, and returns the prevalence rate
#
# parameters
#   outpath   = path of the output file, both for input and export
#   outfile   = name of the z score file for input
#   wts       = default to false, indicator if sampling weights are included

out_nutrition <- function(outpath, outfile, wts=FALSE) {
  
  # read WHO z scores
  z_file <- sprintf("%s/%s_z_st.csv", outpath, outfile)
  z_scores <- read.csv(z_file, sep = ",", header = T) 
  
  # retain children under 5 (<= 60 age months)
  z_scores <- z_scores[z_scores$age <= 60,]
  
  # N4 underweight
  underweight <- anthro(z_scores, indicator = "zwei", z = -2,  
                        lt = TRUE, f = "fwei", weights = wts)
  underweight_severe <- anthro(z_scores, indicator = "zwei", z = -3,
                               lt = TRUE, f = "fwei", weights = wts)
  
  # N5 stunting
  stunting <- anthro(z_scores, indicator = "zlen", z = -2, 
                     lt = TRUE, f = "flen", weights = wts)
  stunting_severe <- anthro(z_scores, indicator = "zlen", z = -3, 
                     lt = TRUE, f = "flen", weights = wts)
  # N6 wasting
  wasting <- anthro(z_scores, indicator = "zwfl", z = -2,
                    lt = TRUE, f = "fwfl", weights = wts)
  wasting_severe <- anthro(z_scores, indicator = "zwfl", z = -3,
                    lt = TRUE, f = "fwfl", weights = wts)
  
  # N3 overweight
  overweight <- anthro(z_scores, indicator = "cbmi", z = 25,
                       lt = FALSE, f = "fbmi", weights = wts)
  obese <- anthro(z_scores, indicator = "cbmi", z = 30,
                       lt = FALSE, f = "fbmi", weights = wts)
  
  # N2 CIAF - not specified
  
  # define output format
  output <- as.data.frame(list(underweight = underweight,
                              underweight.severe = underweight_severe,
                              stunting = stunting,
                              stunting.severe = stunting_severe,
                              wasting = wasting,
                              wasting.severe = wasting_severe,
                              overweight = overweight,
                              obese = obese))
  return(output)

}

# avg_year takes a dataframe and date column within the dataframe and returns the
#     average year over all records
#
# parameters
#   df        = dataframe
#   colname   = column name which contains dates

avg_year = function(df, colname) {
  
  years <- as.integer(format(df[,colname], "%Y"))
  
  return (round(mean(years)))
}

