#############################
# Workflow - Poverty
# functions
# 
#############################

#################
# classes
#################


#################
# functions
#################

# fgt takes a dataframe containing the household expenditure data, 
#     the sampling weights, and returns a given FGT metric
#
# parameters
#   df        = name of the dataframe
#   inc       = name of the income column in df
#   pline     = numeric of the poverty line
#   x         = FGT hyperparameter
#   hhsize    = name of the hhsize column in df
#   ef        = name of the expansion factor column in df
#   weights   = set to TRUE if sampling weights are included in the WHO output

fgt <- function(df, inc, pline, x = 0, hhsize, ef, weights = FALSE) {
  
  # household below the poverty line
  df2 <- df[df[inc] < pline, ]
  
  if (dim(df2)[1] > 0) {
  
    # FGT 
    v <- (((pline - df2[inc]) / pline) ^ x ) 
    
    # per capita
    v <- v * df2[hhsize]
    n <- sum(df[hhsize])
    
    # apply sampling weights
    if (weights) {
      v <- v * df2[ef]
      n <- sum(df[hhsize] * df[ef])
    } 
    return (sum(v) / n)
  } else {
    return (0)
  }
  
}


# apply_ef takes a dataframe containing a given column vector operation, 
#     and applies a per capita weighting, an expansion factor weigting, or both
#
# parameters
#   df        = name of the dataframe
#   the_col   = name of the column in df
#   house     = set to TRUE if expanding by household to per capita
#   hhsize    = name of the hhsize column in df
#   ef        = name of the expansion factor column in df
#   weights   = set to TRUE if sampling weights are included in the WHO output
#   func      = name of the function to apply, defaults to mean

apply_ef <- function(df, col, house = TRUE, hhsize, ef, weights = FALSE, func = "mean") {
  
  vec <- df[col]
  n <- rep(1, dim(vec)[1])
  
  if (house) {
    n <- n * df[hhsize]
    
  }
  
  if (weights) {
    n <- n * df[ef]
  }
  
  if (func == "mean") {
    return (sum(vec * n) / sum(n))
  } else {
    return (sum(vec * n) / sum(n))    
  }
  
}


# deflate deflates a value (nominal_x) to nominal_y
#
# parameters
#   base_x    = the base value in year x
#   base_y    = the base value in year y
#   nominal_x = the current value to be deflated in year x

deflate <- function(base_x, base_y, nominal_x) {
  
  nominal_y <- (nominal_x / (base_x / base_y))
  return (nominal_y)
}
