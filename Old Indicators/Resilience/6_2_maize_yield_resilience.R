source('0_settings.R')

library(dplyr)
library(lme4)
library(ggplot2)
library(ggmcmc)
library(rstan)
library(foreach)
library(doParallel)
library(stringr)

load(file.path(intermediate_folder, paste0(ISO3, '_merged_yielddata.RData')))
hh_loc <- read.csv(file.path(intermediate_folder,
                             paste0(ISO3, '_LSMS_hhs_anonomized.csv')))
hh_loc$HHID <- str_pad(hh_loc$HHID, 14, pad=0)

###############################################################################
### Implement model in R
yld <- dplyr::select(yld, EA, hh_ID, hh_size, yield_kg_ha_maize,
              Educ.Level, Soil.Quality, Seed.Purchased, 
              Irrigation, Org.Fert, InOrg.Fert, GOVT, NGO, COOP, PRIVATE, 
              OTHER,  SPI12)

# Convert SPI to regular units
yld$SPI12 <- yld$SPI12 / 1000

# There aren't enough university students to meaningfully estimate this 
# coefficient
yld$Educ.Level[yld$Educ.Level == "University"] <- "Secondary"

# Make coding of NGO, COOP, PRIVATE, OTHER - need to be factors, not numeric.
yld$Educ.Level<- factor(yld$Educ.Level)
yld$Soil.Quality <- factor(yld$Soil.Quality)
yld$Irrigation <- factor(yld$Irrigation)
yld$Org.Fert <- factor(yld$Org.Fert)
yld$InOrg.Fert <- factor(yld$InOrg.Fert)
yld$Seed.Purchased <- factor(yld$Seed.Purchased)
yld$GOVT <- factor(yld$GOVT, levels=c(0, 1), labels=c("NO", "YES"))
yld$NGO <- factor(yld$NGO, levels=c(0, 1), labels=c("NO", "YES"))
yld$COOP <- factor(yld$COOP, levels=c(0, 1), labels=c("NO", "YES"))
yld$PRIVATE <- factor(yld$PRIVATE, levels=c(0, 1), labels=c("NO", "YES"))
yld$OTHER <- factor(yld$OTHER, levels=c(0, 1), labels=c("NO", "YES"))

# Add a variable recoding any other extension services
yld$OtherExt <- with(yld, (NGO=="YES") | (COOP=="YES") | 
                          (PRIVATE=="YES") | (OTHER=="YES"))
table(yld$GOVT)
table(yld$OtherExt)

summary(lm(yield_kg_ha_maize ~ Soil.Quality + Irrigation + Org.Fert + InOrg.Fert + SPI12, data = yld))

maize_data <- filter(yld, !is.na(yield_kg_ha_maize))
maize_model <- lm(yield_kg_ha_maize ~ Educ.Level + Soil.Quality + Irrigation + 
                  Org.Fert + InOrg.Fert + Seed.Purchased + GOVT + NGO + COOP + 
                  PRIVATE + OTHER + SPI12, data = yld)
summary(maize_model)

maize_model_results <- data.frame(summary(maize_model)$coefficients)
maize_model_results <- cbind(row.names(maize_model_results), maize_model_results)
names(maize_model_results) <- c('variable', 'estimate', 'std_error', 't', 'p')

write.csv(maize_model_results,
          file=file.path(indicator_output_folder, paste0(ISO3, "_maize_model.csv")),
          row.names=FALSE)

ggplot(maize_model_results) +
    geom_point(aes(variable, estimate)) +
    geom_errorbar(aes(variable, ymin=estimate-2*std_error,
                      ymax=estimate+2*std_error))

yld_data <- dplyr::select(yld, hh_ID, yield_kg_ha_maize, Educ.Level, Soil.Quality, 
                          Irrigation, Org.Fert, InOrg.Fert, Seed.Purchased, 
                          GOVT, NGO, COOP, PRIVATE, OTHER, SPI12)
yld_data$in_model <- FALSE
yld_data$in_model[complete.cases(yld_data)] <- TRUE

yld_data <- left_join(yld_data, select(hh_loc, hh_ID=HHID, x, y))
write.csv(yld_data,
          file=file.path(indicator_output_folder, paste0(ISO3, "_maize_data.csv")),
          row.names=FALSE)

preds <- foreach(spi=c(-1, 0, 1), .combine=rbind) %do% {
    new_data <- yld_data[yld_data$in_model == 1, ]
    new_data$SPI12 <- spi
    data.frame(hh_ID=new_data$hh_ID,
               scenario=paste('SPI =', spi),
               yield=predict(maize_model, newdata=new_data))
}

spi_obs_ext_obs <- yld_data[yld_data$in_model == 1, ]
spi_obs_ext_obs_preds <- data.frame(hh_ID=spi_obs_ext_obs$hh_ID,
           scenario='SPI = observed, extension = observed',
           yield=predict(maize_model, newdata=spi_obs_ext_obs))

spi_obs_ext_full <- yld_data[yld_data$in_model == 1, ]
has_any_ext <- with(spi_obs_ext_full,
                    GOVT == "YES" |
                    NGO == "YES" |
                    COOP == "YES" | 
                    PRIVATE == "YES" |
                    OTHER == "YES")
spi_obs_ext_full$GOVT[!has_any_ext] <- "YES"
spi_obs_ext_full_preds <- data.frame(hh_ID=spi_obs_ext_full$hh_ID,
           scenario='SPI = observed, extension = full penetration',
           yield=predict(maize_model, newdata=spi_obs_ext_full))

spi_zero_ext_obs <- yld_data[yld_data$in_model == 1, ]
spi_zero_ext_obs$SPI12 <- 0
spi_zero_ext_obs_preds <- data.frame(hh_ID=spi_zero_ext_obs$hh_ID,
           scenario='SPI = 0, extension = observed',
           yield=predict(maize_model, newdata=spi_zero_ext_obs))

spi_neg1_ext_obs <- yld_data[yld_data$in_model == 1, ]
spi_neg1_ext_obs$SPI12 <- -1
spi_neg1_ext_obs_preds <- data.frame(hh_ID=spi_neg1_ext_obs$hh_ID,
           scenario='SPI = -1, extension = observed',
           yield=predict(maize_model, newdata=spi_neg1_ext_obs))

spi_neg1_ext_full <- yld_data[yld_data$in_model == 1, ]
spi_neg1_ext_full$SPI12 <- -1
spi_neg1_ext_full$GOVT[!has_any_ext] <- "YES"
spi_neg1_ext_full_preds <- data.frame(hh_ID=spi_neg1_ext_full$hh_ID,
           scenario='SPI = -1, extension = full penetration',
           yield=predict(maize_model, newdata=spi_neg1_ext_full))

spi_pos1_ext_obs <- yld_data[yld_data$in_model == 1, ]
spi_pos1_ext_obs$SPI12 <- 1
spi_pos1_ext_obs_preds <- data.frame(hh_ID=spi_pos1_ext_obs$hh_ID,
           scenario='SPI = 1, extension = observed',
           yield=predict(maize_model, newdata=spi_pos1_ext_obs))

spi_pos1_ext_full <- yld_data[yld_data$in_model == 1, ]
spi_pos1_ext_full$SPI12 <- 1
spi_pos1_ext_full$GOVT[!has_any_ext] <- "YES"
spi_pos1_ext_full_preds <- data.frame(hh_ID=spi_pos1_ext_full$hh_ID,
           scenario='SPI = 1, extension = full penetration',
           yield=predict(maize_model, newdata=spi_pos1_ext_full))

preds <- rbind(spi_obs_ext_obs_preds, spi_obs_ext_full_preds)
preds <- rbind(preds, spi_pos1_ext_obs_preds)
preds <- rbind(preds, spi_pos1_ext_full_preds)
preds <- rbind(preds, spi_neg1_ext_obs_preds)
preds <- rbind(preds, spi_neg1_ext_full_preds)
preds <- rbind(preds, spi_zero_ext_obs_preds)

preds$hh_size <- yld$hh_size[match(preds$hh_ID, yld$hh_ID)]
preds$yield_per_person <- preds$yield/preds$hh_size

preds$food_secure <- preds$yield_per_person > 100

basic_stats <- group_by(preds, scenario) %>%
    summarise(n_food_secure=sum(food_secure, na.rm=TRUE),
              n_food_insecure=sum(!food_secure, na.rm=TRUE))
write.csv(basic_stats,
          file=file.path(indicator_output_folder, paste0(ISO3, "_maize_scenario_summary.csv")),
          row.names=FALSE)

preds <- left_join(preds, select(hh_loc, hh_ID=HHID, x, y))
write.csv(preds,
          file=file.path(indicator_output_folder, paste0(ISO3, "_maize_predictions.csv")),
          row.names=FALSE)

ggplot(preds) + geom_bar(aes(yield_per_person, colour=scenario))

# Make predictions for plotting

stop()

###############################################################################
### Implement maize model in stan:

# Cluster is only used for Stan code
cl <- makeCluster(4, outfile="stan.log")
registerDoParallel(cl)

# What is the maximum potential sample size:
table(!is.na(yld$yield_kg_ha_maize))

names(yld)[names(yld) == "Educ.Level"] <- "EducLevel"
names(yld)[names(yld) == "Soil.Quality"] <- "SoilQuality"
names(yld)[names(yld) == "InOrg.Fert"] <- "InOrgFert"
names(yld)[names(yld) == "Org.Fert"] <- "OrgFert"
names(yld)[names(yld) == "Seed.Purchased"] <- "SeedPurchased"

# X <- model.matrix(yield_kg_ha_maize ~ EducLevel + SoilQuality + Irrigation + 
#                   OrgFert + InOrgFert + SeedPurchased + GOVT + NGO + COOP + 
#                   PRIVATE + OTHER + SPI12 + SPI12*SoilQuality + SPI12*GOVT, 
#                   data=yld)
X <- model.matrix(yield_kg_ha_maize ~ EducLevel + SoilQuality + Irrigation + 
                  OrgFert + InOrgFert + SeedPurchased + GOVT + NGO + COOP + 
                  PRIVATE + OTHER + SPI12, data=yld)
# Find the data rows included in X (as X is constructed omitting missing data 
# in X or y)
data_rows <- match(dimnames(X)[[1]], row.names(yld))
y <- yld$yield_kg_ha_maize[data_rows]
EA <- as.integer(factor(yld$EA[data_rows]))

model_data <- list(y=y,
                   X=X, 
                   N=nrow(X),
                   K=ncol(X),
                   EA=EA,
                   N_EA=length(unique(EA)))

stan_fit <- stan("maize_model.stan", data=model_data, chains=0)
rng_seed <- rnorm(1)
sflist <- foreach(cid=1:4, .packages=c('rstan'), .combine=c) %dopar% {
    stan(fit=stan_fit, data=model_data, chains=1, iter=500, seed=rng_seed, 
         chain_ID=cid)
}
fit <- sflist2stanfit(sflist)
s <- ggs(fit, family="beta")
ggs_caterpillar(s)
