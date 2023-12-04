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
  # b_Hebei1 = 0,
  # b_Shanxi2 = 0,
  # b_Liaoning3 = 0,
  # b_Jilin4 = 0,
  # b_Heilongjiang5 = 0,
  # b_Jiangsu6 = 0,
  # b_Zhejiang7 = 0,
  # b_Anhui8 = 0,
  # b_Fujian9 = 0,
  # b_Jiangxi10 = 0,
  # b_Shandong11 = 0,
  # b_Henan12 = 0,
  # b_Hubei13 = 0,
  # b_Hunan14 = 0,
  # b_Guangdong15 = 0,
  # b_Hainan16 = 0,
  # b_Sichuan17 = 0,
  # b_Guizhou18 = 0,
  # b_Yunnan19 = 0,
  # b_Shaanxi20 = 0,
  # b_Gansu21 = 0,
  # b_Qinghai22 = 0,
  # b_Neimongol23 = 0,
  # b_Guangxi24 = 0,
  # b_Tibet25 = 0,
  # b_Ningxia26 = 0,
  # b_Xinjiang27 = 0,
  # b_Beijing28 = 0,
  # b_Shanghai29 = 0,
  # b_Tianjin30 = 0,
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

# ####################################################### #
#### 7. Model prediction for specific vaccine                ####
# ####################################################### #

fit <- apollo_loadModel("Mixed logit model including interactions")

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P <- list()
  
  ### Effect codings constraint
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
  V[["alt2"]] <- 0
  
  ### Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
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



# ### Use the estimated model to make predictions
# apollo_inputs = apollo_validateInputs()
# 
# ### Uptake rates for the DCE question we provided
# 
# predictions_base = apollo_prediction(fit, apollo_probabilities, apollo_inputs, prediction_settings=list(runs=30))
# predictions_base = predictions_base[["at_estimates"]] %>% 
#   mutate(uptake_rate = alt1 + alt2)
# mean(predictions_base$uptake_rate)

# Predict the uptake rate for a specific vaccine

Predict_uptake_rate = function(database, vaccine = list(price = c("0", "200", "400", "600"), risk = c("0", "1/100,000", "1/1,000,000"), duration = c("6 months", "12 months", "Life long"), efficacy = c("50%", "70%", "90%"), admin = c("Injection", "Oral"), doses = c("1 dose", "2 doses", "3 doses"), origin = c("Domestic", "Imported")), fit, apollo_probabilities, apollo_inputs, Resampling_times = 3){
  database <- database %>%
    mutate(
      price.1 = as.numeric(vaccine$price),
      risk2.1 = ifelse(vaccine$risk == "1/1,000,000", 1, 0),
      risk3.1 = ifelse(vaccine$risk == "1/100,000", 1, 0),
      duration2.1 = ifelse(vaccine$duration == "12 months", 1, 0),
      duration3.1 = ifelse(vaccine$duration == "Life long", 1, 0),
      efficacy2.1 = ifelse(vaccine$efficacy == "70%", 1, 0),
      efficacy3.1 = ifelse(vaccine$efficacy == "90%", 1, 0),
      admin.1 = ifelse(vaccine$admin == "Oral", 1, 0),
      doses2.1 = ifelse(vaccine$doses == "2 doses", 1, 0),
      doses3.1 = ifelse(vaccine$doses == "3 doses", 1, 0),
      origin.1 = ifelse(vaccine$origin == "Imported", 1, 0),
      price.2 = as.numeric(vaccine$price),
      risk3.2 = ifelse(vaccine$risk == "1/1,000,000", 1, 0),
      risk2.2 = ifelse(vaccine$risk == "1/100,000", 1, 0),
      duration2.2 = ifelse(vaccine$duration == "12 months", 1, 0),
      duration3.2 = ifelse(vaccine$duration == "Life long", 1, 0),
      efficacy2.2 = ifelse(vaccine$efficacy == "70%", 1, 0),
      efficacy3.2 = ifelse(vaccine$efficacy == "90%", 1, 0),
      admin.2 = ifelse(vaccine$admin == "Oral", 1, 0),
      doses2.2 = ifelse(vaccine$doses == "2 doses", 1, 0),
      doses3.2 = ifelse(vaccine$doses == "3 doses", 1, 0),
      origin.2 = ifelse(vaccine$origin == "Imported", 1, 0),
    )
  
  apollo_inputs = apollo_validateInputs(database = database)
  predictions_new = apollo_prediction(fit, apollo_probabilities, apollo_inputs, prediction_settings=list(runs=Resampling_times))
  predictions_new = predictions_new[["at_estimates"]] %>%
    mutate(
      uptake_rate = alt1,
      opt_out = alt2
    )
  return(predictions_new)
}

sink(file = "Prediction for various attributes.txt", type = "output")

# 0. baseline vaccine
predictions_0 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)


# 1. price up vaccine
predictions_price_1 = Predict_uptake_rate(database, vaccine = list(price = "200", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_price_2 = Predict_uptake_rate(database, vaccine = list(price = "400", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_price_3 = Predict_uptake_rate(database, vaccine = list(price = "600", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 doses", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 2. high risk vaccine
predictions_risk_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 doses", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_risk_2 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "1/100,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 doses", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 3. long protection duration vaccine
predictions_duration_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "12 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_duration_2 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "Life long", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 4. high efficacy vaccine
predictions_efficacy_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "70%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_efficacy_2 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "90%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 5. oral vaccine
predictions_oral_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "50%", admin = "Oral", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 6. many doses vaccine
predictions_dose_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

predictions_dose_2 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "3 doses", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)

# 7. imported vaccine
predictions_imported_1 = Predict_uptake_rate(database, vaccine = list(price = "0", risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Imported"), fit, apollo_probabilities, apollo_inputs)

# Preparation for figure 2 ----

vaccine_price = seq(0, 1000, 100)
list_predictions_price = list()
for (price in vaccine_price) {
  predictions_price = Predict_uptake_rate(database, vaccine = list(price = as.character(price), risk = "0", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "1 dose", origin = "Domestic"), fit, apollo_probabilities, apollo_inputs)
  list_predictions_price = append(list_predictions_price, list(predictions_price))
}

sink(file = "Prediction for various attributes.txt", type = "output")
mean(predictions_0$uptake_rate)
mean(predictions_price_1$uptake_rate)
mean(predictions_price_2$uptake_rate)
mean(predictions_price_3$uptake_rate)
mean(predictions_risk_1$uptake_rate)
mean(predictions_risk_2$uptake_rate)
mean(predictions_duration_1$uptake_rate)
mean(predictions_duration_2$uptake_rate)
mean(predictions_efficacy_1$uptake_rate)
mean(predictions_efficacy_2$uptake_rate)
mean(predictions_oral_1$uptake_rate)
mean(predictions_dose_1$uptake_rate)
mean(predictions_dose_2$uptake_rate)
mean(predictions_imported_1$uptake_rate)
sink()



# 1. Sinovac: Now set up a safe and effective scenario with different price and see the expected uptake rates
vaccine_Kexing_0 = list(price = "0", risk = "1/100,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Kexing_0 <- Predict_uptake_rate(database, vaccine = vaccine_Kexing_0, fit, apollo_probabilities, apollo_inputs)

vaccine_Kexing_100 = list(price = "100", risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Kexing_100 <- Predict_uptake_rate(database, vaccine = vaccine_Kexing_100, fit, apollo_probabilities, apollo_inputs)

vaccine_Kexing_200 = list(price = "200", risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Kexing_200 <- Predict_uptake_rate(database, vaccine = vaccine_Kexing_200, fit, apollo_probabilities, apollo_inputs)

vaccine_Kexing_400 = list(price = "400", risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Kexing_400 <- Predict_uptake_rate(database, vaccine = vaccine_Kexing_400, fit, apollo_probabilities, apollo_inputs)

vaccine_Kexing_600 = list(price = "600", risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Kexing_600 <- Predict_uptake_rate(database, vaccine = vaccine_Kexing_600, fit, apollo_probabilities, apollo_inputs)

# 2. Pfizer
vaccine_Pfizer = list(price = "0", risk = "1/1,000,000", duration = "6 months", efficacy = "90%", admin = "Injection", doses = "2 doses", origin = "Imported")
predictions_Pfizer <- Predict_uptake_rate(database, vaccine = vaccine_Pfizer, fit, apollo_probabilities, apollo_inputs)
mean(predictions_Pfizer$uptake_rate)

# 4. Wuhan Shengwu
vaccine_Shengwu = list(price = "0", risk = "1/1,000,000", duration = "6 months", efficacy = "70%", admin = "Injection", doses = "2 doses", origin = "Domestic")
predictions_Shengwu <- Predict_uptake_rate(database, vaccine = vaccine_Shengwu, fit, apollo_probabilities, apollo_inputs)
mean(predictions_Shengwu$uptake_rate)

sink("Prediction for various vaccine.txt", type = "output")
mean(predictions_Kexing_0$uptake_rate)
mean(predictions_Kexing_100$uptake_rate)
mean(predictions_Kexing_200$uptake_rate)
mean(predictions_Kexing_400$uptake_rate)
mean(predictions_Kexing_600$uptake_rate)
sink()

save(list_predictions_price, file = paste0("Prediction_price.RData"))
save(predictions_Kexing_0, predictions_Kexing_100, predictions_Kexing_200, predictions_Kexing_400, predictions_Kexing_600, file = "Prediction_Kexing_price.RData")
save(predictions_Kexing_0, predictions_Pfizer, predictions_Pfizer, file = "Prediction_map.RData")