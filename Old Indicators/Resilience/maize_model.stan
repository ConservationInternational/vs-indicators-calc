// Structure of X:
//  beta_1: Intercept
//  beta_2: EducLevelPRIMARY
//  beta_3: EducLevelSECONDARY
//  beta_4: SoilQualityGOOD
//  beta_5: SoilQualityAVERAGE
//  beta_6: IrrigationYES
//  beta_7: OrgFertYES
//  beta_8: InOrgFertYES
//  beta_9: SeedPurchasedNO
//  beta_10: GOVT
//  beta_11: NGO
//  beta_12: COOPYES
//  beta_13: PRIVATEYES
//  beta_14: OTHERYES
//  beta_15: SPI
//  beta_16: SPI*SoilQuality
//  beta_17: SPI*GOVT
data {
    int<lower=1>    N;      // number of households
    int<lower=1>    K;      // number of predictors
    vector[N]       y;      // outcome
    row_vector[K]   X[N];   // predictors
    int<lower=1>    EA[N];  // enumeration area IDs
    int<lower=1>    N_EA;   // number of enumeration areas
}

parameters {
    vector[K] beta;
    real<lower=0> sigma_y;
    real<lower=0> sigma_int_ea;
    real int_ea[N_EA]; // EA level random intercept
}

model {
    beta ~ normal(0, 1000);
    sigma_y ~ cauchy(0, 1000);
    sigma_int_ea ~ cauchy(0, 1000);
    int_ea ~ normal(0, sigma_int_ea); // Matt trick
    for (n in 1:N)
        y ~ normal(int_ea[EA[n]] + X[n] * beta, sigma_y);

    // Model missing covariate data
}
