### set the absolute path
#setwd("C:/Users/Finley/Desktop/research/healthcare/P1 COVID19/task 1 discrete choice/China/Quantitative/Transfer/Interaction and Prediction") 
setwd("/home/caozhong/Interaction_and_prediction")
### Clear memory
rm(list = ls())

### Install or Load library
if (!require('apollo')) install.packages('apollo')
if (!require('tidyverse')) install.packages('tidyverse')

price = c(0, 100, 200, 400, 600)
province_dat2 = data.frame(province_31l = c(), price = c(), Expected.uptake.rate = c())

for (i in 1:length(price)) {
  vaccine_Kexing = list(price = as.character(price[i]), risk = "1/1,000,000", duration = "6 months", efficacy = "50%", admin = "Injection", doses = "2 doses", origin = "Domestic")
  
  ### Initialise code
  apollo_initialise()
  
  ### Set core controls
  apollo_control <- list(
    modelName = "Mixed logit model",
    modelDescr = "Mixed logit model to determine Adults' preferences for a vaccine product",
    indivID = "ID",
    mixing = TRUE, # mixed logit model for random coefficients, FALSE for MNL model
    nCores = 36
    #,weights = "weight"
  )
  
  # ####################################################### #
  #### 2. Parameter definition                           ####
  # ####################################################### #
  
  ### Vector of parameters, including any that are kept fixed
  ### during estimation
  
  apollo_beta <- c(
    asc = 0,
    asc_loc = 0, # ASC: location parameter, accounting for the left to right bias
    b_price_mu = 0,
    b_risk2_mu = 0,
    b_risk3_mu = 0,
    b_duration2_mu = 0,
    b_duration3_mu = 0,
    b_efficacy2_mu = 0,
    b_efficacy3_mu = 0,
    b_oral_mu = 0,
    b_dose2_mu = 0,
    b_dose3_mu = 0,
    b_imported_mu = 0,
    b_price_sigma = 0,
    b_risk2_sigma = 0,
    b_risk3_sigma = 0,
    b_duration2_sigma = 0,
    b_duration3_sigma = 0,
    b_efficacy2_sigma = 0,
    b_efficacy3_sigma = 0,
    b_oral_sigma = 0,
    b_dose2_sigma = 0,
    b_dose3_sigma = 0,
    b_imported_sigma = 0
  )
  
  ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
  apollo_fixed <- c()
  
  # ################################################################# #
  #### 3. Define Random Components                                    ####
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
  #### 4. Probabilities                               ####
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
    V[["alt1"]] <- asc + b_price * price.1 + b_risk2 * risk2.1 + b_risk3 * risk3.1 +
      b_duration2 * duration2.1 + b_duration3 * duration3.1 + b_efficacy2 * efficacy2.1 +
      b_efficacy3 * efficacy3.1 + b_oral * admin.1 + b_dose2 * doses2.1 +
      b_dose3 * doses3.1 + b_imported * origin.1
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
  
  Predict_uptake_rate = function(database, vaccine = list(price = c("0", "200", "400", "600"), risk = c("0", "1/100,000", "1/1,000,000"), duration = c("6 months", "12 months", "Life long"), efficacy = c("50%", "70%", "90%"), admin = c("Injection", "Oral"), doses = c("1 dose", "2 doses", "3 doses"), origin = c("Domestic", "Imported")), fit, apollo_probabilities, apollo_inputs, Resampling_times = 3){
    database <- database %>%
      mutate(
        price.1 = as.numeric(vaccine$price),
        risk2.1 = ifelse(vaccine$risk == "1/100,000", 1, 0), # moderate level risk
        risk3.1 = ifelse(vaccine$risk == "1/1,000,000", 1, 0), # high level risk
        duration2.1 = ifelse(vaccine$duration == "12 months", 1, 0),
        duration3.1 = ifelse(vaccine$duration == "Life long", 1, 0),
        efficacy2.1 = ifelse(vaccine$efficacy == "70%", 1, 0),
        efficacy3.1 = ifelse(vaccine$efficacy == "90%", 1, 0),
        admin.1 = ifelse(vaccine$admin == "Oral", 1, 0),
        doses2.1 = ifelse(vaccine$doses == "2 doses", 1, 0),
        doses3.1 = ifelse(vaccine$doses == "3 doses", 1, 0),
        origin.1 = ifelse(vaccine$origin == "Imported", 1, 0)
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
  
  # ####################################################### #
  #### 5. Prediction                               ####
  # ####################################################### #
  
  temp_dat = data.frame(province_31l = 1:31, price = price[i])
  
  for (i in 1:31) {
    load(paste0(getwd(), "/Model/ML_province", i, ".RData"))
    
    database <- read.csv(file = "database.csv")
    socio_dat <- read.csv(file = "socio_dat.csv")
    database <- database %>%
      select(ID, price.1, risk2.1, risk3.1, duration2.1, duration3.1, efficacy2.1, efficacy3.1, admin.1, doses2.1, doses3.1, origin.1, price.2, risk2.2, risk3.2, duration2.2, duration3.2, efficacy2.2, efficacy3.2, admin.2, doses2.2, doses3.2, origin.2, choice) %>%
      left_join(socio_dat, by = "ID") %>%
      filter(province_31l == i)
    
    apollo_inputs <- apollo_validateInputs()
    
    predictions = Predict_uptake_rate(database, vaccine = vaccine_Kexing, fit, apollo_probabilities, apollo_inputs)
    temp_dat[i, "Expected.uptake.rate"] = mean(predictions$uptake_rate)
  }
  province_dat2 = rbind(province_dat2, temp_dat)
}

save(province_dat2, file = "province_dat2.RData")