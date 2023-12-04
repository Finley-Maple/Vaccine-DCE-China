# use apollo package to analyze
# ####################################################### #
#### 1. Definition of core settings                        
# ####################################################### #
### clear
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
  modelName = "Mixed logit model",
  modelDescr = "Mixed logit model to determine Adults' preferences for a vaccine product",
  indivID = "ID",  
  mixing = TRUE, # mixed logit model for random coefficients, FALSE for MNL model
  nCores = 72
  # weights = "weight" # apply sample weights
)
# ####################################################### #
#### 2. Data loading                                   ####
# ####################################################### #
database <- read.csv(file = "Data/database.csv")

# database <- database[1:1440, ] # use a 1% of sample

database <- subset(database, select = c("ID","price.1","risk2.1","risk3.1","duration2.1",
                                        "duration3.1","efficacy2.1","efficacy3.1",
                                        "admin.1","doses2.1","doses3.1","origin.1",
                                        "price.2","risk2.2","risk3.2",
                                        "duration2.2","duration3.2","efficacy2.2",
                                        "efficacy3.2","admin.2","doses2.2",
                                        "doses3.2","origin.2","choice","weight"))

database <- as.data.frame(database)

# ####################################################### #
#### 3. Parameter definition                           ####
# ####################################################### #

### Vector of parameters, including any that are kept fixed
### during estimation

apollo_beta <- c(
  asc = 0,
  asc_loc = 0,
  b_price_mu = 0,
  b_risk2_mu = 0,
  b_risk3_mu = 0,
  b_duration2_mu = 0,
  b_duration3_mu = 0,
  b_efficacy2_mu = 0,
  b_efficacy3_mu = 0,
  b_oral_mu = 0,
  # b_dose2_mu = 0,
  # b_dose3_mu = 0,
  b_imported_mu = 0,
  b_price_sigma = 0,
  b_risk2_sigma = 0,
  b_risk3_sigma = 0,
  b_duration2_sigma = 0,
  b_duration3_sigma = 0,
  b_efficacy2_sigma = 0,
  b_efficacy3_sigma = 0,
  b_oral_sigma = 0,
  # b_dose2_sigma = 0,
  # b_dose3_sigma = 0,
  b_imported_sigma = 0
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
    "price", "risk2", "risk3",
    "duration2", "duration3",
    "efficacy2", "efficacy3", "oral",
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
  # randcoeff[["b_dose2"]] <- b_dose2_mu + b_dose2_sigma * draws_dose2
  # randcoeff[["b_dose3"]] <- b_dose3_mu + b_dose3_sigma * draws_dose3
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
  
  V <- list()
  
  ### List of utilities: these must use the same names as
  ### in mnl_settings, order is irrelevant.
  V[["alt1"]] <- asc + asc_loc + b_price * price.1 + b_risk2 * risk2.1 + 
    b_risk3 * risk3.1 + b_duration2 * duration2.1 +
    b_duration3 * duration3.1 + b_efficacy2 * efficacy2.1 +
    b_efficacy3 * efficacy3.1 + b_oral * admin.1 + b_imported * origin.1
  V[["alt2"]] <- asc + b_price * price.2 + b_risk2 * risk2.2 +
    b_risk3 * risk3.2 + b_duration2 * duration2.2 + 
    b_duration3 * duration3.2 + b_efficacy2 * efficacy2.2 +
    b_efficacy3 * efficacy3.2 + b_oral * admin.2 + b_imported * origin.2
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

# ####################################################### #
#### 7. Model estimation                   ####
# ####################################################### #
fit <- apollo_estimate(apollo_beta, apollo_fixed, 
                       apollo_probabilities, apollo_inputs)

save(fit, file = paste0(getwd(), "/ML_Exclude_doses.RData"))

# apollo_modelOutput(fit, list(printPVal = TRUE)) # LL(final) = 