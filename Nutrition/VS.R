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

convert.magic <- function(obj,type){
    FUN1 <- switch(type, character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor,
                  myDate = as.myDate)
    obj <- lapply(obj, FUN1)
    as.data.frame(out)
}



# readVS takes a file that specifies a VS survey module and returns 
#     a dataframe of that module
#
# parameters
#   file      = csv file of the output
#   survey    = type of survey "HH" is household, "AGRIC" is agriculture
#   section   = identifies the section of the survey

readVS <- function(file, survey = "HH", section) {
  
  if (survey == "HH") {

    sect <- read.csv(file, stringsAsFactors = F)    
    
    ints <- NULL
    nums <- NULL
    mdates <- NULL

    if (sum(grepl("^L[0-9]{2}$", sect[, "Landscape.."])) != length(sect[, "Landscape.."]))
      sect[!grepl("^L[0-9]{2}$", sect[, "Landscape.."]), "Landscape.."] <- paste0("L", formatC(as.integer(sect[!grepl("^L[0-9]{2}$", sect[, "Landscape.."]), "Landscape.."]), width = 2, format = "d", flag = "0"))
    
    if (section == "A") {
      
      ints <-  c("Region", "District", "clerk_code", 
                 "data_clerk_code_2", "supervisor_code")
      mdates <- c("Data.entry.date", "data_entry_date_2",
                  "Questionnaire.inspection.date")
      
    } else if (section == "B") {
      
      # Household, individual ID, age
      ints <- c("hh_b02", "hh_b05", "hh_b04", "hh_b07", "hh_b08")
      mdates <- c("Data.entry.date", "hh_b03")

    } else if (section == "K1") {
      
      # Household, food consumption
      nums <- c("k_02_2", "k_03_2", "k_04", "k_05_2", "k_05a", "k_06_2")
      mdates <- c("Data.entry.date")
      
    } else if (section == "K2") {
      
      # Household, food consumption
      mdates <- c("Data.entry.date")
      
    } else if (section == "L") {
      
      # Household, nonfood consumption 
      mdates <- c("Data.entry.date")
      
    } else if (section == "U") {
      
      col_class <- c(rep("character", 2), 
                     "myDate",
                     "character",
                     "integer",
                     rep("numeric", 5),
                     "integer",
                     "character")
    }
  }
  
  if (length(ints)) {
    for (i in ints) {
      sect[,i] <- as.integer(sect[,i])
    }
  }
  if (length(nums)) {
    for (i in nums) {
      sect[,i] <- as.numeric(sect[,i])
    }
  }
  if (length(mdates)) {
    for (i in mdates) {
      if (grepl("-", as.character(sect[,i])))
        sect[,i] <- as.Date(as.character(sect[,i]), format="%Y-%m-%d")
      else
        sect[,i] <- as.Date(as.character(sect[,i]), format="%m/%d/%Y")
    }
  }
  
  return (sect)
  
}




# adulteq takes a dataframe of observations and a dataframe of adult
#     equivalent scales and returns a vector of corresponding adult eq
#
# parameters
#   df      = csv file of the output
#   survey    = type of survey "HH" is household, "AGRIC" is agriculture
#   section   = identifies the section of the survey

adulteq <- function(df, adulteq, country) {
  
  for (i in 1:dim(adulteq)[1]) {
    
    df[df$Country == country &
                df$age >= adulteq$Age.lower[i] & 
                df$age <= adulteq$Age.upper[i] &
                df$sex == adulteq$sex[i], "adult_eq"] <- adulteq$adult_eq[i]
  }
  return (df$adult_eq)
}

# food consumption
#     evaluates the valued food consumption from the household survey
#     and returns a dataframe with total valued food consumption by household
#
# parameters
#   df      = section k1 of the VS household survey, containing columns:
#                   k_04 - valued purchases
#                   k_05a - estimated value of home consumption
#                   Household.ID - identifier of the household

foodcons <- function(df) {
  
  df$k_04[is.na(df$k_04) ] <- 0
  df$k_05a[is.na(df$k_05a) ] <- 0
  
  food <- ddply(df, 
                .(Household.ID), 
                summarise, 
                food_cons = sum(k_04 + k_05a, na.rm = TRUE))
  
  # annualize
  food$food_cons <- food$food_cons / 7 * 365.24 
  
  return (food)
}

# nonfood consumption
#     evaluates the valued nonfood consumption from the household survey
#     and returns a dataframe with total valued nonfood consumption by household
#
# parameters
#   df      = section l or m of the VS household survey, containing columns:
#                   Country - country of the household response
#                   data entry date - estimated value of home consumption
#                   Household.ID - identifier of the household
#
#  section  = identifies the section of the survey, assume M default

nonfoodcons <- function (df, sec_m = TRUE) {
  
  # keep the first 4 columns, while dropping every other subsequent column
  df <- cbind(df[1:4], df[c(F, T)][c(-1, -2)])

  # melt to long shape
  df <- melt(df, 
             id.vars = c("Country", "Landscape..", "Household.ID", "Data.entry.date"),
             variable.name = "nonfood.code",
             value.name = "amount.spent")

  # sum valued nonfood consumption by household
  nonfood <- ddply(df, 
                      .(Household.ID), 
                      summarise, 
                      nonfood_cons = sum(amount.spent, na.rm = TRUE))
  
  # annualize nonfood consumption
  if (sec_m) {
    # monthly
    nonfood$nonfood_cons <- nonfood$nonfood_cons / 30 * 365.24     
  } else {
    # daily
    nonfood$nonfood_cons <- nonfood$nonfood_cons * 365.24     
  }
  
  return(nonfood)

}

