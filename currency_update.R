library(readr)

currency_data <- read_csv('posts/exchange_rates.csv')
currency_data <- currency_data[1:(nrow(currency_data)-4),]
inflation <- read_csv('summary-stats/GDPDEF.csv')

currency_data$`End Date` <- as.Date(currency_data$`End Date`, '%m/%d/%Y')
inflation$DATE <- as.Date(inflation$DATE)

usd_2009 <- c()
for(i in 1:nrow(currency_data)){
  diffs_all <- abs(currency_data$`End Date`[i] - inflation$DATE)
  diffs_inds_min <- order(diffs_all)[1:2]
  diffs_vals_min <- diffs_all[diffs_inds_min]
  weights <- as.numeric((sum(diffs_vals_min) - diffs_vals_min)/as.numeric(sum(diffs_vals_min)))
  
  val <- inflation$GDPDEF[diffs_inds_min[1]]*(weights[1]) + inflation$GDPDEF[diffs_inds_min[2]]*weights[2]
  val <- val/100

  usd_2009 <- c(usd_2009, val)
}

currency_data$USD.2009 <- usd_2009

write.csv(currency_data, 'summary-stats/exchange_rates_2009usd.csv')
