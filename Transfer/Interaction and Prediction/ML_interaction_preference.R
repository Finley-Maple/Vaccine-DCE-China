# use apollo package to analyze
# ####################################################### #
#### 1. Definition of core settings
# ####################################################### #

### set the absolute path
#setwd("C:/Users/Finley/Desktop/research/healthcare/P1 COVID19/task 1 discrete choice/China/Quantitative/Transfer/Interaction and Prediction") 
setwd("/home/caozhong/Interaction_and_prediction")
### Clear memory
rm(list = ls())

### Install or Load library
if (!require('apollo')) install.packages('apollo')
if (!require('tidyverse')) install.packages('tidyverse')

library(tidyverse)
library(apollo)

### detect how many cores are accessible: recommended number of threads is equal 
### to the number of available processor cores in the machine minus one
parallel::detectCores()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control <- list(
  modelName = "Mixed logit model including interactions",
  modelDescr = "Mixed logit model to determine Adults' WTP for a vaccine product",
  indivID = "ID",
  mixing = TRUE, # mixed logit model for random coefficients, FALSE for MNL model
  nCores = 72 # use the number of cores
)
# ####################################################### #
#### 2. Data loading                                   ####
# ####################################################### #
database <- read.csv(file = "database.csv")

# # small sample for testing 
# database <- database[1:1440, ]

socio_dat <- read.csv(file = "socio_dat.csv")
database <- database %>%
  select(ID, price.1, risk2.1, risk3.1, duration2.1, duration3.1, efficacy2.1, efficacy3.1, admin.1, doses2.1, doses3.1, origin.1, price.2, risk2.2, risk3.2, duration2.2, duration3.2, efficacy2.2, efficacy3.2, admin.2, doses2.2, doses3.2, origin.2, choice) %>%
  left_join(socio_dat, by = "ID")

database <- as.data.frame(database)

# ####################################################### #
#### 3. Parameter definition                           ####
# ####################################################### #

### Vector of parameters, including any that are kept fixed
### during estimation

apollo_beta <- c(
  asc = 2.90,
  asc_loc = 0,
  b_price_mu  = -0.00165,
  b_risk2_mu  = -0.778,
  b_risk3_mu  = -0.717,
  b_duration2_mu  = 0.454,
  b_duration3_mu = 1.641,
  b_efficacy2_mu = 0.677,
  b_efficacy3_mu = 1.470,
  b_oral_mu = -0.106,
  b_dose2_mu = -0.196,
  b_dose3_mu = -0.226,
  b_imported_mu = -0.788,
  # b_gender_2l = 0,
  # b_age_high = 0,
  # b_age_mid = 0,
  # b_education_high = 0,
  # b_education_mid = 0,
  # b_marriage_2l = 0,
  # b_residence_2l = 0,
  # b_occupation_high = 0,
  # b_occupation_mid = 0,
  # b_insurance_2l = 0,
  # b_income_high = 0,
  # b_income_mid = 0,
  # b_chronic_2l = 0,
  # b_hasvaccined_2l = 0,
  # b_depression_2l = 0,
  b_Hebei1 = 0,
  b_Shanxi2 = 0,
  b_Liaoning3 = 0,
  b_Jilin4 = 0,
  b_Heilongjiang5 = 0,
  b_Jiangsu6 = 0,
  b_Zhejiang7 = 0,
  b_Anhui8 = 0,
  b_Fujian9 = 0,
  b_Jiangxi10 = 0,
  b_Shandong11 = 0,
  b_Henan12 = 0,
  b_Hubei13 = 0,
  b_Hunan14 = 0,
  b_Guangdong15 = 0,
  b_Hainan16 = 0,
  b_Sichuan17 = 0,
  b_Guizhou18 = 0,
  b_Yunnan19 = 0,
  b_Shaanxi20 = 0,
  b_Gansu21 = 0,
  b_Qinghai22 = 0,
  b_Neimongol23 = 0,
  b_Guangxi24 = 0,
  b_Tibet25 = 0,
  b_Ningxia26 = 0,
  b_Xinjiang27 = 0,
  b_Beijing28 = 0,
  b_Shanghai29 = 0,
  b_Tianjin30 = 0,
  b_price_gender_2l = 0,
  b_risk2_gender_2l = 0,
  b_risk3_gender_2l = 0,
  b_duration2_gender_2l = 0,
  b_duration3_gender_2l = 0,
  b_efficacy2_gender_2l = 0,
  b_efficacy3_gender_2l = 0,
  b_oral_gender_2l = 0,
  b_dose3_gender_2l = 0,
  b_imported_gender_2l = 0,
  b_price_age_high = 0,
  b_risk2_age_high = 0,
  b_risk3_age_high = 0,
  b_duration2_age_high = 0,
  b_duration3_age_high = 0,
  b_efficacy2_age_high = 0,
  b_efficacy3_age_high = 0,
  b_oral_age_high = 0,
  b_dose3_age_high = 0,
  b_imported_age_high = 0,
  b_price_age_mid = 0,
  b_risk2_age_mid = 0,
  b_risk3_age_mid = 0,
  b_duration2_age_mid = 0,
  b_duration3_age_mid = 0,
  b_efficacy2_age_mid = 0,
  b_efficacy3_age_mid = 0,
  b_oral_age_mid = 0,
  b_dose3_age_mid = 0,
  b_imported_age_mid = 0,
  b_price_education_high = 0,
  b_risk2_education_high = 0,
  b_risk3_education_high = 0,
  b_duration2_education_high = 0,
  b_duration3_education_high = 0,
  b_efficacy2_education_high = 0,
  b_efficacy3_education_high = 0,
  b_oral_education_high = 0,
  b_dose3_education_high = 0,
  b_imported_education_high = 0,
  b_price_education_mid = 0,
  b_risk2_education_mid = 0,
  b_risk3_education_mid = 0,
  b_duration2_education_mid = 0,
  b_duration3_education_mid = 0,
  b_efficacy2_education_mid = 0,
  b_efficacy3_education_mid = 0,
  b_oral_education_mid = 0,
  b_dose3_education_mid = 0,
  b_imported_education_mid = 0,
  b_price_marriage_2l = 0,
  b_risk2_marriage_2l = 0,
  b_risk3_marriage_2l = 0,
  b_duration2_marriage_2l = 0,
  b_duration3_marriage_2l = 0,
  b_efficacy2_marriage_2l = 0,
  b_efficacy3_marriage_2l = 0,
  b_oral_marriage_2l = 0,
  b_dose3_marriage_2l = 0,
  b_imported_marriage_2l = 0,
  b_price_residence_2l = 0,
  b_risk2_residence_2l = 0,
  b_risk3_residence_2l = 0,
  b_duration2_residence_2l = 0,
  b_duration3_residence_2l = 0,
  b_efficacy2_residence_2l = 0,
  b_efficacy3_residence_2l = 0,
  b_oral_residence_2l = 0,
  b_dose3_residence_2l = 0,
  b_imported_residence_2l = 0,
  b_price_occupation_high = 0,
  b_risk2_occupation_high = 0,
  b_risk3_occupation_high = 0,
  b_duration2_occupation_high = 0,
  b_duration3_occupation_high = 0,
  b_efficacy2_occupation_high = 0,
  b_efficacy3_occupation_high = 0,
  b_oral_occupation_high = 0,
  b_dose3_occupation_high = 0,
  b_imported_occupation_high = 0,
  b_price_occupation_mid = 0,
  b_risk2_occupation_mid = 0,
  b_risk3_occupation_mid = 0,
  b_duration2_occupation_mid = 0,
  b_duration3_occupation_mid = 0,
  b_efficacy2_occupation_mid = 0,
  b_efficacy3_occupation_mid = 0,
  b_oral_occupation_mid = 0,
  b_dose3_occupation_mid = 0,
  b_imported_occupation_mid = 0,
  b_price_insurance_2l = 0,
  b_risk2_insurance_2l = 0,
  b_risk3_insurance_2l = 0,
  b_duration2_insurance_2l = 0,
  b_duration3_insurance_2l = 0,
  b_efficacy2_insurance_2l = 0,
  b_efficacy3_insurance_2l = 0,
  b_oral_insurance_2l = 0,
  b_dose3_insurance_2l = 0,
  b_imported_insurance_2l = 0,
  b_price_income_high = 0,
  b_risk2_income_high = 0,
  b_risk3_income_high = 0,
  b_duration2_income_high = 0,
  b_duration3_income_high = 0,
  b_efficacy2_income_high = 0,
  b_efficacy3_income_high = 0,
  b_oral_income_high = 0,
  b_dose3_income_high = 0,
  b_imported_income_high = 0,
  b_price_income_mid = 0,
  b_risk2_income_mid = 0,
  b_risk3_income_mid = 0,
  b_duration2_income_mid = 0,
  b_duration3_income_mid = 0,
  b_efficacy2_income_mid = 0,
  b_efficacy3_income_mid = 0,
  b_oral_income_mid = 0,
  b_dose3_income_mid = 0,
  b_imported_income_mid = 0,
  b_price_chronic_2l = 0,
  b_risk2_chronic_2l = 0,
  b_risk3_chronic_2l = 0,
  b_duration2_chronic_2l = 0,
  b_duration3_chronic_2l = 0,
  b_efficacy2_chronic_2l = 0,
  b_efficacy3_chronic_2l = 0,
  b_oral_chronic_2l = 0,
  b_dose3_chronic_2l = 0,
  b_imported_chronic_2l = 0,
  b_price_hasvaccined_2l = 0,
  b_risk2_hasvaccined_2l = 0,
  b_risk3_hasvaccined_2l = 0,
  b_duration2_hasvaccined_2l = 0,
  b_duration3_hasvaccined_2l = 0,
  b_efficacy2_hasvaccined_2l = 0,
  b_efficacy3_hasvaccined_2l = 0,
  b_oral_hasvaccined_2l = 0,
  b_dose3_hasvaccined_2l = 0,
  b_imported_hasvaccined_2l = 0,
  b_price_sigma  = 0.0038,
  b_risk2_sigma  = 0.794,
  b_risk3_sigma  = 0.719,
  b_duration2_sigma  = -0.395,
  b_duration3_sigma = 1.418,
  b_efficacy2_sigma = -0.426,
  b_efficacy3_sigma = -1.195,
  b_oral_sigma = -0.741,
  b_dose2_sigma = -0.022,
  b_dose3_sigma = 0.626,
  b_imported_sigma = 1.440
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed <- c()

# ################################################################# #
#### 4. Define Random Components                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws <- list(
  interDrawsType = "mlhs",
  interNDraws = 500,
  interUnifDraws = c(),
  interNormDraws = paste0("draws_", c(
    "price", "risk2",
    "risk3", "duration2",
    "duration3", "efficacy2",
    "efficacy3", "oral",
    "dose2", "dose3",
    "imported"
  )),
  intraDrawsType = "",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  
  randcoeff[["b_price"]] <- b_price_mu + b_price_sigma * draws_price
  randcoeff[["b_risk2"]] <- b_risk2_mu + b_risk2_sigma * draws_risk2
  randcoeff[["b_risk3"]] <- b_risk3_mu + b_risk3_sigma * draws_risk3
  randcoeff[["b_duration2"]] <- b_duration2_mu + b_duration2_sigma * draws_duration2
  randcoeff[["b_duration3"]] <- b_duration3_mu + b_duration3_sigma * draws_duration3
  randcoeff[["b_efficacy2"]] <- b_efficacy2_mu + b_efficacy2_sigma * draws_efficacy2
  randcoeff[["b_efficacy3"]] <- b_efficacy3_mu + b_efficacy3_sigma * draws_efficacy3
  randcoeff[["b_oral"]] <- b_oral_mu + b_oral_sigma * draws_oral
  randcoeff[["b_dose2"]] <- b_dose2_mu + b_dose2_sigma * draws_dose2
  randcoeff[["b_dose3"]] <- b_dose3_mu + b_dose3_sigma * draws_dose3
  randcoeff[["b_imported"]] <- b_imported_mu + b_imported_sigma * draws_imported
  
  return(randcoeff)
}

# ####################################################### #
#### 5. Input validation                               ####
# ####################################################### #

apollo_inputs <- apollo_validateInputs()

# ####################################################### #
#### 6. Likelihood definition                          ####
# ####################################################### #

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P <- list()
  
  ### Effect codings constraint
  b_Chongqing31 = -b_Hebei1-b_Shanxi2-b_Liaoning3-b_Jilin4-b_Heilongjiang5-b_Jiangsu6-b_Zhejiang7-b_Anhui8-b_Fujian9-b_Jiangxi10-b_Shandong11-b_Henan12-b_Hubei13-b_Hunan14-b_Guangdong15-b_Hainan16-b_Sichuan17-b_Guizhou18-b_Yunnan19-b_Shaanxi20-b_Gansu21-b_Qinghai22-b_Neimongol23-b_Guangxi24-b_Tibet25-b_Ningxia26-b_Xinjiang27-b_Beijing28-b_Shanghai29-b_Tianjin30
  
  V <- list()
  
  ### List of utilities: these must use the same names as
  ### in mnl_settings, order is irrelevant.
  V[["alt1"]] <- asc + asc_loc + b_price * price.1 + b_risk2 * risk2.1 + b_risk3 * risk3.1 +
    b_duration2 * duration2.1 + b_duration3 * duration3.1 + b_efficacy2 * efficacy2.1 +
    b_efficacy3 * efficacy3.1 + b_oral * admin.1 + b_dose2 * doses2.1 +
    b_dose3*doses3.1 + b_imported*origin.1 + # below are respondents characteristics
    # b_gender_2l*gender_2l + b_age_high*age_high + b_age_mid*age_mid + 
    # b_education_high*education_high + b_education_mid*education_mid + 
    # b_marriage_2l*marriage_2l + b_residence_2l*residence_2l + 
    # b_occupation_high*occupation_high + b_occupation_mid*occupation_mid + 
    # b_insurance_2l*insurance_2l + b_income_high*income_high + 
    # b_income_mid*income_mid + b_chronic_2l*chronic_2l + 
    # b_hasvaccined_2l*hasvaccined_2l + b_depression_2l*depression_2l + # geographic
    b_Hebei1*(province_31l == 1) + b_Shanxi2*(province_31l == 2) +
    b_Liaoning3 *(province_31l == 3) + b_Jilin4*(province_31l == 4) +
    b_Heilongjiang5*(province_31l == 5) + b_Jiangsu6*(province_31l == 6) +
    b_Zhejiang7*(province_31l == 7) + b_Anhui8*(province_31l == 8) +
    b_Fujian9*(province_31l == 9) + b_Jiangxi10*(province_31l == 10) +
    b_Shandong11*(province_31l == 11) + b_Henan12*(province_31l == 12) +
    b_Hubei13*(province_31l == 13) + b_Hunan14*(province_31l == 14) +
    b_Guangdong15*(province_31l == 15) + b_Hainan16*(province_31l == 16) +
    b_Sichuan17*(province_31l == 17) + b_Guizhou18*(province_31l == 18) +
    b_Yunnan19*(province_31l == 19) + b_Shaanxi20*(province_31l == 20) +
    b_Gansu21*(province_31l == 21) + b_Qinghai22*(province_31l == 22) +
    b_Neimongol23*(province_31l == 23) + b_Guangxi24*(province_31l == 24) +
    b_Tibet25*(province_31l == 25) + b_Ningxia26*(province_31l == 26) +
    b_Xinjiang27*(province_31l == 27) + b_Beijing28*(province_31l == 28) +
    b_Shanghai29*(province_31l == 29) + b_Tianjin30*(province_31l == 30) +
    b_Chongqing31*(province_31l == 31) + # below are interaction terms
    b_price_gender_2l * price.1 * gender_2l +
    b_risk2_gender_2l * risk2.1 * gender_2l +
    b_risk3_gender_2l * risk3.1 * gender_2l +
    b_duration2_gender_2l * duration2.1 * gender_2l +
    b_duration3_gender_2l * duration3.1 * gender_2l +
    b_efficacy2_gender_2l * efficacy2.1 * gender_2l +
    b_efficacy3_gender_2l * efficacy3.1 * gender_2l +
    b_oral_gender_2l * admin.1 * gender_2l +
    b_dose3_gender_2l * doses3.1 * gender_2l +
    b_imported_gender_2l * origin.1 * gender_2l +
    b_price_age_high * price.1 * age_high +
    b_risk2_age_high * risk2.1 * age_high +
    b_risk3_age_high * risk3.1 * age_high +
    b_duration2_age_high * duration2.1 * age_high +
    b_duration3_age_high * duration3.1 * age_high +
    b_efficacy2_age_high * efficacy2.1 * age_high +
    b_efficacy3_age_high * efficacy3.1 * age_high +
    b_oral_age_high * admin.1 * age_high +
    b_dose3_age_high * doses3.1 * age_high +
    b_imported_age_high * origin.1 * age_high +
    b_price_age_mid * price.1 * age_mid +
    b_risk2_age_mid * risk2.1 * age_mid +
    b_risk3_age_mid * risk3.1 * age_mid +
    b_duration2_age_mid * duration2.1 * age_mid +
    b_duration3_age_mid * duration3.1 * age_mid +
    b_efficacy2_age_mid * efficacy2.1 * age_mid +
    b_efficacy3_age_mid * efficacy3.1 * age_mid +
    b_oral_age_mid * admin.1 * age_mid +
    b_dose3_age_mid * doses3.1 * age_mid +
    b_imported_age_mid * origin.1 * age_mid +
    b_price_education_high * price.1 * education_high +
    b_risk2_education_high * risk2.1 * education_high +
    b_risk3_education_high * risk3.1 * education_high +
    b_duration2_education_high * duration2.1 * education_high +
    b_duration3_education_high * duration3.1 * education_high +
    b_efficacy2_education_high * efficacy2.1 * education_high +
    b_efficacy3_education_high * efficacy3.1 * education_high +
    b_oral_education_high * admin.1 * education_high +
    b_dose3_education_high * doses3.1 * education_high +
    b_imported_education_high * origin.1 * education_high +
    b_price_education_mid * price.1 * education_mid +
    b_risk2_education_mid * risk2.1 * education_mid +
    b_risk3_education_mid * risk3.1 * education_mid +
    b_duration2_education_mid * duration2.1 * education_mid +
    b_duration3_education_mid * duration3.1 * education_mid +
    b_efficacy2_education_mid * efficacy2.1 * education_mid +
    b_efficacy3_education_mid * efficacy3.1 * education_mid +
    b_oral_education_mid * admin.1 * education_mid +
    b_dose3_education_mid * doses3.1 * education_mid +
    b_imported_education_mid * origin.1 * education_mid +
    b_price_marriage_2l * price.1 * marriage_2l +
    b_risk2_marriage_2l * risk2.1 * marriage_2l +
    b_risk3_marriage_2l * risk3.1 * marriage_2l +
    b_duration2_marriage_2l * duration2.1 * marriage_2l +
    b_duration3_marriage_2l * duration3.1 * marriage_2l +
    b_efficacy2_marriage_2l * efficacy2.1 * marriage_2l +
    b_efficacy3_marriage_2l * efficacy3.1 * marriage_2l +
    b_oral_marriage_2l * admin.1 * marriage_2l +
    b_dose3_marriage_2l * doses3.1 * marriage_2l +
    b_imported_marriage_2l * origin.1 * marriage_2l +
    b_price_residence_2l * price.1 * residence_2l +
    b_risk2_residence_2l * risk2.1 * residence_2l +
    b_risk3_residence_2l * risk3.1 * residence_2l +
    b_duration2_residence_2l * duration2.1 * residence_2l +
    b_duration3_residence_2l * duration3.1 * residence_2l +
    b_efficacy2_residence_2l * efficacy2.1 * residence_2l +
    b_efficacy3_residence_2l * efficacy3.1 * residence_2l +
    b_oral_residence_2l * admin.1 * residence_2l +
    b_dose3_residence_2l * doses3.1 * residence_2l +
    b_imported_residence_2l * origin.1 * residence_2l +
    b_price_occupation_high * price.1 * occupation_high +
    b_risk2_occupation_high * risk2.1 * occupation_high +
    b_risk3_occupation_high * risk3.1 * occupation_high +
    b_duration2_occupation_high * duration2.1 * occupation_high +
    b_duration3_occupation_high * duration3.1 * occupation_high +
    b_efficacy2_occupation_high * efficacy2.1 * occupation_high +
    b_efficacy3_occupation_high * efficacy3.1 * occupation_high +
    b_oral_occupation_high * admin.1 * occupation_high +
    b_dose3_occupation_high * doses3.1 * occupation_high +
    b_imported_occupation_high * origin.1 * occupation_high +
    b_price_occupation_mid * price.1 * occupation_mid +
    b_risk2_occupation_mid * risk2.1 * occupation_mid +
    b_risk3_occupation_mid * risk3.1 * occupation_mid +
    b_duration2_occupation_mid * duration2.1 * occupation_mid +
    b_duration3_occupation_mid * duration3.1 * occupation_mid +
    b_efficacy2_occupation_mid * efficacy2.1 * occupation_mid +
    b_efficacy3_occupation_mid * efficacy3.1 * occupation_mid +
    b_oral_occupation_mid * admin.1 * occupation_mid +
    b_dose3_occupation_mid * doses3.1 * occupation_mid +
    b_imported_occupation_mid * origin.1 * occupation_mid +
    b_price_insurance_2l * price.1 * insurance_2l +
    b_risk2_insurance_2l * risk2.1 * insurance_2l +
    b_risk3_insurance_2l * risk3.1 * insurance_2l +
    b_duration2_insurance_2l * duration2.1 * insurance_2l +
    b_duration3_insurance_2l * duration3.1 * insurance_2l +
    b_efficacy2_insurance_2l * efficacy2.1 * insurance_2l +
    b_efficacy3_insurance_2l * efficacy3.1 * insurance_2l +
    b_oral_insurance_2l * admin.1 * insurance_2l +
    b_dose3_insurance_2l * doses3.1 * insurance_2l +
    b_imported_insurance_2l * origin.1 * insurance_2l +
    b_price_income_high * price.1 * income_high +
    b_risk2_income_high * risk2.1 * income_high +
    b_risk3_income_high * risk3.1 * income_high +
    b_duration2_income_high * duration2.1 * income_high +
    b_duration3_income_high * duration3.1 * income_high +
    b_efficacy2_income_high * efficacy2.1 * income_high +
    b_efficacy3_income_high * efficacy3.1 * income_high +
    b_oral_income_high * admin.1 * income_high +
    b_dose3_income_high * doses3.1 * income_high +
    b_imported_income_high * origin.1 * income_high +
    b_price_income_mid * price.1 * income_mid +
    b_risk2_income_mid * risk2.1 * income_mid +
    b_risk3_income_mid * risk3.1 * income_mid +
    b_duration2_income_mid * duration2.1 * income_mid +
    b_duration3_income_mid * duration3.1 * income_mid +
    b_efficacy2_income_mid * efficacy2.1 * income_mid +
    b_efficacy3_income_mid * efficacy3.1 * income_mid +
    b_oral_income_mid * admin.1 * income_mid +
    b_dose3_income_mid * doses3.1 * income_mid +
    b_imported_income_mid * origin.1 * income_mid +
    b_price_chronic_2l * price.1 * chronic_2l +
    b_risk2_chronic_2l * risk2.1 * chronic_2l +
    b_risk3_chronic_2l * risk3.1 * chronic_2l +
    b_duration2_chronic_2l * duration2.1 * chronic_2l +
    b_duration3_chronic_2l * duration3.1 * chronic_2l +
    b_efficacy2_chronic_2l * efficacy2.1 * chronic_2l +
    b_efficacy3_chronic_2l * efficacy3.1 * chronic_2l +
    b_oral_chronic_2l * admin.1 * chronic_2l +
    b_dose3_chronic_2l * doses3.1 * chronic_2l +
    b_imported_chronic_2l * origin.1 * chronic_2l +
    b_price_hasvaccined_2l * price.1 * hasvaccined_2l +
    b_risk2_hasvaccined_2l * risk2.1 * hasvaccined_2l +
    b_risk3_hasvaccined_2l * risk3.1 * hasvaccined_2l +
    b_duration2_hasvaccined_2l * duration2.1 * hasvaccined_2l +
    b_duration3_hasvaccined_2l * duration3.1 * hasvaccined_2l +
    b_efficacy2_hasvaccined_2l * efficacy2.1 * hasvaccined_2l +
    b_efficacy3_hasvaccined_2l * efficacy3.1 * hasvaccined_2l +
    b_oral_hasvaccined_2l * admin.1 * hasvaccined_2l +
    b_dose3_hasvaccined_2l * doses3.1 * hasvaccined_2l +
    b_imported_hasvaccined_2l * origin.1 * hasvaccined_2l
  V[["alt2"]] <- asc + b_price * price.2 + b_risk2 * risk2.2 + b_risk3 * risk3.2 +
    b_duration2 * duration2.2 + b_duration3 * duration3.2 + b_efficacy2 * efficacy2.2 +
    b_efficacy3 * efficacy3.2 + b_oral * admin.2 + b_dose2 * doses2.2 +
    b_dose3*doses3.2 + b_imported*origin.2 + # below are respondents characteristics
    # b_gender_2l*gender_2l + b_age_high*age_high + b_age_mid*age_mid + 
    # b_education_high*education_high + b_education_mid*education_mid + 
    # b_marriage_2l*marriage_2l + b_residence_2l*residence_2l + 
    # b_occupation_high*occupation_high + b_occupation_mid*occupation_mid + 
    # b_insurance_2l*insurance_2l + b_income_high*income_high + 
    # b_income_mid*income_mid + b_chronic_2l*chronic_2l + 
    # b_hasvaccined_2l*hasvaccined_2l + b_depression_2l*depression_2l +# geographic terms
    b_Hebei1*(province_31l == 1) + b_Shanxi2*(province_31l == 2) +
    b_Liaoning3 *(province_31l == 3) + b_Jilin4*(province_31l == 4) +
    b_Heilongjiang5*(province_31l == 5) + b_Jiangsu6*(province_31l == 6) +
    b_Zhejiang7*(province_31l == 7) + b_Anhui8*(province_31l == 8) +
    b_Fujian9*(province_31l == 9) + b_Jiangxi10*(province_31l == 10) +
    b_Shandong11*(province_31l == 11) + b_Henan12*(province_31l == 12) +
    b_Hubei13*(province_31l == 13) + b_Hunan14*(province_31l == 14) +
    b_Guangdong15*(province_31l == 15) + b_Hainan16*(province_31l == 16) +
    b_Sichuan17*(province_31l == 17) + b_Guizhou18*(province_31l == 18) +
    b_Yunnan19*(province_31l == 19) + b_Shaanxi20*(province_31l == 20) +
    b_Gansu21*(province_31l == 21) + b_Qinghai22*(province_31l == 22) +
    b_Neimongol23*(province_31l == 23) + b_Guangxi24*(province_31l == 24) +
    b_Tibet25*(province_31l == 25) + b_Ningxia26*(province_31l == 26) +
    b_Xinjiang27*(province_31l == 27) + b_Beijing28*(province_31l == 28) +
    b_Shanghai29*(province_31l == 29) + b_Tianjin30*(province_31l == 30) +
    b_Chongqing31*(province_31l == 31) + # below are interaction terms
    b_price_gender_2l * price.2 * gender_2l +
    b_risk2_gender_2l * risk2.2 * gender_2l +
    b_risk3_gender_2l * risk3.2 * gender_2l +
    b_duration2_gender_2l * duration2.2 * gender_2l +
    b_duration3_gender_2l * duration3.2 * gender_2l +
    b_efficacy2_gender_2l * efficacy2.2 * gender_2l +
    b_efficacy3_gender_2l * efficacy3.2 * gender_2l +
    b_oral_gender_2l * admin.2 * gender_2l +
    b_dose3_gender_2l * doses3.2 * gender_2l +
    b_imported_gender_2l * origin.2 * gender_2l +
    b_price_age_high * price.2 * age_high +
    b_risk2_age_high * risk2.2 * age_high +
    b_risk3_age_high * risk3.2 * age_high +
    b_duration2_age_high * duration2.2 * age_high +
    b_duration3_age_high * duration3.2 * age_high +
    b_efficacy2_age_high * efficacy2.2 * age_high +
    b_efficacy3_age_high * efficacy3.2 * age_high +
    b_oral_age_high * admin.2 * age_high +
    b_dose3_age_high * doses3.2 * age_high +
    b_imported_age_high * origin.2 * age_high +
    b_price_age_mid * price.2 * age_mid +
    b_risk2_age_mid * risk2.2 * age_mid +
    b_risk3_age_mid * risk3.2 * age_mid +
    b_duration2_age_mid * duration2.2 * age_mid +
    b_duration3_age_mid * duration3.2 * age_mid +
    b_efficacy2_age_mid * efficacy2.2 * age_mid +
    b_efficacy3_age_mid * efficacy3.2 * age_mid +
    b_oral_age_mid * admin.2 * age_mid +
    b_dose3_age_mid * doses3.2 * age_mid +
    b_imported_age_mid * origin.2 * age_mid +
    b_price_education_high * price.2 * education_high +
    b_risk2_education_high * risk2.2 * education_high +
    b_risk3_education_high * risk3.2 * education_high +
    b_duration2_education_high * duration2.2 * education_high +
    b_duration3_education_high * duration3.2 * education_high +
    b_efficacy2_education_high * efficacy2.2 * education_high +
    b_efficacy3_education_high * efficacy3.2 * education_high +
    b_oral_education_high * admin.2 * education_high +
    b_dose3_education_high * doses3.2 * education_high +
    b_imported_education_high * origin.2 * education_high +
    b_price_education_mid * price.2 * education_mid +
    b_risk2_education_mid * risk2.2 * education_mid +
    b_risk3_education_mid * risk3.2 * education_mid +
    b_duration2_education_mid * duration2.2 * education_mid +
    b_duration3_education_mid * duration3.2 * education_mid +
    b_efficacy2_education_mid * efficacy2.2 * education_mid +
    b_efficacy3_education_mid * efficacy3.2 * education_mid +
    b_oral_education_mid * admin.2 * education_mid +
    b_dose3_education_mid * doses3.2 * education_mid +
    b_imported_education_mid * origin.2 * education_mid +
    b_price_marriage_2l * price.2 * marriage_2l +
    b_risk2_marriage_2l * risk2.2 * marriage_2l +
    b_risk3_marriage_2l * risk3.2 * marriage_2l +
    b_duration2_marriage_2l * duration2.2 * marriage_2l +
    b_duration3_marriage_2l * duration3.2 * marriage_2l +
    b_efficacy2_marriage_2l * efficacy2.2 * marriage_2l +
    b_efficacy3_marriage_2l * efficacy3.2 * marriage_2l +
    b_oral_marriage_2l * admin.2 * marriage_2l +
    b_dose3_marriage_2l * doses3.2 * marriage_2l +
    b_imported_marriage_2l * origin.2 * marriage_2l +
    b_price_residence_2l * price.2 * residence_2l +
    b_risk2_residence_2l * risk2.2 * residence_2l +
    b_risk3_residence_2l * risk3.2 * residence_2l +
    b_duration2_residence_2l * duration2.2 * residence_2l +
    b_duration3_residence_2l * duration3.2 * residence_2l +
    b_efficacy2_residence_2l * efficacy2.2 * residence_2l +
    b_efficacy3_residence_2l * efficacy3.2 * residence_2l +
    b_oral_residence_2l * admin.2 * residence_2l +
    b_dose3_residence_2l * doses3.2 * residence_2l +
    b_imported_residence_2l * origin.2 * residence_2l +
    b_price_occupation_high * price.2 * occupation_high +
    b_risk2_occupation_high * risk2.2 * occupation_high +
    b_risk3_occupation_high * risk3.2 * occupation_high +
    b_duration2_occupation_high * duration2.2 * occupation_high +
    b_duration3_occupation_high * duration3.2 * occupation_high +
    b_efficacy2_occupation_high * efficacy2.2 * occupation_high +
    b_efficacy3_occupation_high * efficacy3.2 * occupation_high +
    b_oral_occupation_high * admin.2 * occupation_high +
    b_dose3_occupation_high * doses3.2 * occupation_high +
    b_imported_occupation_high * origin.2 * occupation_high +
    b_price_occupation_mid * price.2 * occupation_mid +
    b_risk2_occupation_mid * risk2.2 * occupation_mid +
    b_risk3_occupation_mid * risk3.2 * occupation_mid +
    b_duration2_occupation_mid * duration2.2 * occupation_mid +
    b_duration3_occupation_mid * duration3.2 * occupation_mid +
    b_efficacy2_occupation_mid * efficacy2.2 * occupation_mid +
    b_efficacy3_occupation_mid * efficacy3.2 * occupation_mid +
    b_oral_occupation_mid * admin.2 * occupation_mid +
    b_dose3_occupation_mid * doses3.2 * occupation_mid +
    b_imported_occupation_mid * origin.2 * occupation_mid +
    b_price_insurance_2l * price.2 * insurance_2l +
    b_risk2_insurance_2l * risk2.2 * insurance_2l +
    b_risk3_insurance_2l * risk3.2 * insurance_2l +
    b_duration2_insurance_2l * duration2.2 * insurance_2l +
    b_duration3_insurance_2l * duration3.2 * insurance_2l +
    b_efficacy2_insurance_2l * efficacy2.2 * insurance_2l +
    b_efficacy3_insurance_2l * efficacy3.2 * insurance_2l +
    b_oral_insurance_2l * admin.2 * insurance_2l +
    b_dose3_insurance_2l * doses3.2 * insurance_2l +
    b_imported_insurance_2l * origin.2 * insurance_2l +
    b_price_income_high * price.2 * income_high +
    b_risk2_income_high * risk2.2 * income_high +
    b_risk3_income_high * risk3.2 * income_high +
    b_duration2_income_high * duration2.2 * income_high +
    b_duration3_income_high * duration3.2 * income_high +
    b_efficacy2_income_high * efficacy2.2 * income_high +
    b_efficacy3_income_high * efficacy3.2 * income_high +
    b_oral_income_high * admin.2 * income_high +
    b_dose3_income_high * doses3.2 * income_high +
    b_imported_income_high * origin.2 * income_high +
    b_price_income_mid * price.2 * income_mid +
    b_risk2_income_mid * risk2.2 * income_mid +
    b_risk3_income_mid * risk3.2 * income_mid +
    b_duration2_income_mid * duration2.2 * income_mid +
    b_duration3_income_mid * duration3.2 * income_mid +
    b_efficacy2_income_mid * efficacy2.2 * income_mid +
    b_efficacy3_income_mid * efficacy3.2 * income_mid +
    b_oral_income_mid * admin.2 * income_mid +
    b_dose3_income_mid * doses3.2 * income_mid +
    b_imported_income_mid * origin.2 * income_mid +
    b_price_chronic_2l * price.2 * chronic_2l +
    b_risk2_chronic_2l * risk2.2 * chronic_2l +
    b_risk3_chronic_2l * risk3.2 * chronic_2l +
    b_duration2_chronic_2l * duration2.2 * chronic_2l +
    b_duration3_chronic_2l * duration3.2 * chronic_2l +
    b_efficacy2_chronic_2l * efficacy2.2 * chronic_2l +
    b_efficacy3_chronic_2l * efficacy3.2 * chronic_2l +
    b_oral_chronic_2l * admin.2 * chronic_2l +
    b_dose3_chronic_2l * doses3.2 * chronic_2l +
    b_imported_chronic_2l * origin.2 * chronic_2l +
    b_price_hasvaccined_2l * price.2 * hasvaccined_2l +
    b_risk2_hasvaccined_2l * risk2.2 * hasvaccined_2l +
    b_risk3_hasvaccined_2l * risk3.2 * hasvaccined_2l +
    b_duration2_hasvaccined_2l * duration2.2 * hasvaccined_2l +
    b_duration3_hasvaccined_2l * duration3.2 * hasvaccined_2l +
    b_efficacy2_hasvaccined_2l * efficacy2.2 * hasvaccined_2l +
    b_efficacy3_hasvaccined_2l * efficacy3.2 * hasvaccined_2l +
    b_oral_hasvaccined_2l * admin.2 * hasvaccined_2l +
    b_dose3_hasvaccined_2l * doses3.2 * hasvaccined_2l +
    b_imported_hasvaccined_2l * origin.2 * hasvaccined_2l
  V[["alt3"]] <- 0
  
  ### Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  ### Using sampling weights in estimation and prediction
  # P = apollo_weighting(P, apollo_inputs, functionality)
  
  return(P)
}

fit <- apollo_estimate(
  apollo_beta, apollo_fixed,
  apollo_probabilities, apollo_inputs
)

apollo_saveOutput(fit)
save(fit, "ML_interaction_preference_loc.RData")
fit <- apollo_loadModel("Mixed logit model including interactions")


