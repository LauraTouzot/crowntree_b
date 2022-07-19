################################################################################
#                            LINEAR RELATIONSHIPS                              #
################################################################################

## Linear model - No competition - All data

mod_linear_alldata_nocomp <- function(data, linear_alldata_nocomp) {
  
  ## running the models
  tryCatch({  
    
    m1_l <- lme(y ~ x,
                data = data,
                random = ~ 1|location,
                weights = varPower(form = ~fitted(.)),
                method = "ML",  control = lmeControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
    
    if (length(unique(data$protocol)) > 1) {
      
      m2_l <- lme(y ~ x + protocol,
                  data = data,
                  random = ~ 1|location,
                  weights = varPower(form = ~fitted(.)),
                  method = "ML",  control = lmeControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
      
      linear_alldata_nocomp[1, "slope"] <- fixed.effects(m2_l)[2]
      linear_alldata_nocomp[1, "AIC"] <- AIC(m2_l)
      
      linear_alldata_nocomp[1,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m2_l)[1]
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        linear_alldata_nocomp[1,k] <- fixed.effects(m2_l)[1] + fixed.effects(m2_l)[k]
      }
      
      
    } else {
      
      linear_alldata_nocomp[1, "slope"] <- fixed.effects(m1_l)[2]
      linear_alldata_nocomp[1, "AIC"] <- AIC(m1_l)
      linear_alldata_nocomp[1,paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_l)[1]
      
    }
    
    ## completing storage file with weighted parameters
    linear_alldata_nocomp[2,"slope"] <- linear_alldata_nocomp[1,"slope"]
    linear_alldata_nocomp[2,"AIC"] <- linear_alldata_nocomp[1,"AIC"]
    
    ## extracting number of observations per protocol to attribute weights
    n_tot <- dim(data)[1]
    nobs <- data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
    
    for (g in levels(data$protocol)) {
      linear_alldata_nocomp[2,paste0("protocol",g)] <- linear_alldata_nocomp[1,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
    }
    
    upper_range <- dim(linear_alldata_nocomp)[2]-3
    linear_alldata_nocomp[1, "intercept"] <- apply(linear_alldata_nocomp[1,c(2:upper_range)], 1, mean, na.rm = TRUE)
    linear_alldata_nocomp[2, "intercept"] <- apply(linear_alldata_nocomp[1,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
  }, 
  
  error = function(e) {
    print("error")
  }) 
  

  return(linear_alldata_nocomp)
  
}




## Linear model - No competition - Resampling

mod_linear_resampling_nocomp <- function(ranged_data, nb_datasets_all, sample_size, linear_resampling_nocomp, linear_resampling_nocomp_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {

    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      m1_l_rs <- gls(y ~ x,
                   data = new_data,
                   weights = varPower(form = ~fitted(.)),
                   method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_l_rs <- gls(y ~ x + protocol,
                     data = new_data,
                     weights = varPower(form = ~fitted(.)),
                     method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
        
        linear_resampling_nocomp[f,"slope"] <- coefficients(m2_l_rs)[2]
        linear_resampling_nocomp[f,"AIC"] <- AIC(m2_l_rs)
        
        linear_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_l_rs)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          linear_resampling_nocomp[f,k] <- coefficients(m2_l_rs)[1] + coefficients(m2_l_rs)[k]
        }
        
        
      } else {
        
        linear_resampling_nocomp[f,"slope"] <- coefficients(m1_l_rs)[2]
        linear_resampling_nocomp[f,"AIC"] <- AIC(m1_l_rs)
        linear_resampling_nocomp[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_l_rs)[1]
        
      }
      
      
      ## completing storage file with weighted parameters
      linear_resampling_nocomp_w[f,"AIC"] <- linear_resampling_nocomp[f,"AIC"]
      linear_resampling_nocomp_w[f,"slope"] <- linear_resampling_nocomp[f,"slope"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        linear_resampling_nocomp_w[f,paste0("protocol",g)] <- linear_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(linear_resampling_nocomp)[2]-5
      linear_resampling_nocomp[f, "intercept"] <- apply(linear_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      linear_resampling_nocomp_w[f, "intercept"] <- apply(linear_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = linear_resampling_nocomp[f,"intercept"] + linear_resampling_nocomp[f,"slope"]*x,
                                          y_pred_b = linear_resampling_nocomp_w[f,"intercept"] + linear_resampling_nocomp_w[f,"slope"]*x)
      
      
      ## computing RMSE
      linear_resampling_nocomp[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      linear_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      linear_resampling_nocomp[f,"weighted"] <- "no" 
      linear_resampling_nocomp_w[f,"weighted"] <- "yes"
      
    }
      ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
    
    linear_resampling_nocomp <- bind_rows(linear_resampling_nocomp, linear_resampling_nocomp_w)
    return(linear_resampling_nocomp)
  
}


  



################################################################################
#                            POWER RELATIONSHIPS                               #
################################################################################


## Power model - No competition - All data

mod_power_alldata_nocomp <- function(data,  power_alldata_nocomp) {
  
  mod_power <- y ~ a1 * (x ^ a2)
    
  ## running the models
  tryCatch({
    
    # fitting power relationships
    init <- fixef(lmer(log(y) ~ log(x) + (1|location), data)) # initializing values for power models
    
    m1_p <- nlme(mod_power,
               data = data,
               fixed = list(a1 ~ 1, a2 ~ 1),
               random = a1 ~ 1|location,
               start = c(a1 = exp(init[1]), a2 = init[2]),
               weights = varPower(form = ~fitted(.)),
               method = "ML",  control = nlmeControl(maxIter = 1000, tolerance = 1e-2, pnlsTol = 1e-1))
    
    if (length(unique(data$protocol)) > 1) {
      
      m2_p <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ protocol, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = c(rep(exp(init[1]), length(unique(data$protocol)))), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1000, tolerance = 1e-2, pnlsTol = 1e-1))
      
      power_alldata_nocomp[1,"a2"] <- fixed.effects(m2_p)["a2"]
      power_alldata_nocomp[1,"AIC"] <- AIC(m2_p)
      
      power_alldata_nocomp[1, paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m2_p)[1]
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        power_alldata_nocomp[1,k] <- fixed.effects(m2_p)[1] + fixed.effects(m2_p)[paste0("a1.", k)]
      }

      
    } else {
      
      power_alldata_nocomp[1,"a2"] <- fixed.effects(m1_p)["a2"]
      power_alldata_nocomp[1,"AIC"] <- AIC(m1_p)
      power_alldata_nocomp[1, paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_p)[1]
      
    }
    
    ## completing storage file with weighted parameters
    power_alldata_nocomp[2,"a2"] <- power_alldata_nocomp[1,"a2"]
    power_alldata_nocomp[2,"AIC"] <- power_alldata_nocomp[1,"AIC"]
    
    ## extracting number of observations per protocol to attribute weights
    n_tot <- dim(data)[1]
    nobs <- data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
    
    for (g in levels(data$protocol)) {
      power_alldata_nocomp[2,paste0("protocol",g)] <- power_alldata_nocomp[1,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
    }
    
    
    upper_range <- dim(power_alldata_nocomp)[2]-3
    power_alldata_nocomp[1, "a1"] <- apply(power_alldata_nocomp[1,c(2:upper_range)], 1, mean, na.rm = TRUE)
    power_alldata_nocomp[2, "a1"] <- apply(power_alldata_nocomp[1,c(2:upper_range)], 1, sum, na.rm = TRUE)
    
    
  },
  
  error = function(e) {
    print("error")
  }) 
  
  return(power_alldata_nocomp)
    
}




mod_power_alldata_nocomp_log <- function(data,  power_alldata_nocomp_log) {
  
  ## running the models
  tryCatch({
    
    # fitting power relationships: log-log version
    m1_p_log <- lmer(log(y) ~ log(x) + (1|location), data) # initializing values for power models
    
    if (length(unique(data$protocol)) > 1) {
      
      m2_p_log <- lmer(log(y) ~ log(x) + protocol + (1|location), data)

      power_alldata_nocomp_log[1,"a2"] <- fixed.effects(m2_p_log)["log(x)"]
      power_alldata_nocomp_log[1,"AIC"] <- AIC(m2_p_log)
      power_alldata_nocomp_log[1,"sigma"] <- sigma(m2_p_log)
      
      power_alldata_nocomp_log[1, paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m2_p_log)[1]
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        power_alldata_nocomp_log[1,k] <- fixed.effects(m2_p_log)[1] + fixed.effects(m2_p_log)[k]
      }
      
      
    } else {
      
      power_alldata_nocomp_log[1,"a2"] <- fixed.effects(m1_p_log)["log(x)"]
      power_alldata_nocomp_log[1,"AIC"] <- AIC(m1_p_log)
      power_alldata_nocomp_log[1, paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_p_log)[1]
      power_alldata_nocomp_log[1,"sigma"] <- sigma(m1_p_log)
      
    }
    
    ## completing storage file with weighted parameters
    power_alldata_nocomp_log[2,"a2"] <- power_alldata_nocomp_log[1,"a2"]
    power_alldata_nocomp_log[2,"AIC"] <- power_alldata_nocomp_log[1,"AIC"]
    power_alldata_nocomp_log[2,"sigma"] <- power_alldata_nocomp_log[1,"sigma"]
    
    ## extracting number of observations per protocol to attribute weights
    n_tot <- dim(data)[1]
    nobs <- data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
    
    for (g in levels(data$protocol)) {
      power_alldata_nocomp_log[2,paste0("protocol",g)] <- power_alldata_nocomp_log[1,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
    }
    
    upper_range <- dim(power_alldata_nocomp_log)[2]-4
    power_alldata_nocomp_log[1, "a1"] <- exp(apply(power_alldata_nocomp_log[1,c(2:upper_range)], 1, mean, na.rm = TRUE))
    power_alldata_nocomp_log[2, "a1"] <- exp(apply(power_alldata_nocomp_log[1,c(2:upper_range)], 1, sum, na.rm = TRUE))
  
    
  },
  
  error = function(e) {
    print("error")
  }) 
  
  return(power_alldata_nocomp_log)
  
}




## Power model - No competition - Resampling

mod_power_resampling_nocomp <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_nocomp, power_resampling_nocomp_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## defining the model
    mod_power <- y ~ a1 * (x ^ a2)
    
    ## running the models
    tryCatch({
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      # fitting power relationships
      init_rs <- coefficients(lm(log(y) ~ log(x), new_data)) # initializing values for power models
      
      m1_p_rs <- gnls(mod_power,
                   data = new_data,
                   params = list(a1 ~ 1, a2 ~ 1),
                   start = c(a1 = exp(init_rs[1]), a2 = init_rs[2]),
                   weights = varPower(form = ~fitted(.)),
                   control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs <- gnls(mod_power,
                     data = new_data,
                     params = list(a1 ~ protocol, a2 ~ 1),
                     start = c(a1 = c(rep(exp(init_rs[1]), length(unique(new_data$protocol)))), a2 = init_rs[2]),
                     weights = varPower(form = ~fitted(.)),
                     control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
        
        power_resampling_nocomp[f,"a2"] <- coefficients(m2_p_rs)["a2"]
        power_resampling_nocomp[f,"AIC"] <- AIC(m2_p_rs)
        
        power_resampling_nocomp[f, paste0("protocol", levels(new_data$protocol)[1])] <- coefficients(m2_p_rs)[1]
        
        for (k in paste0("protocol", levels(new_data$protocol)[-1])) {
          power_resampling_nocomp[f,k] <- coefficients(m2_p_rs)[1] + coefficients(m2_p_rs)[paste0("a1.", k)]
        }
        
        
        
      } else {
        
        power_resampling_nocomp[f,"a2"] <- coefficients(m1_p_rs)["a2"]
        power_resampling_nocomp[f,"AIC"] <- AIC(m1_p_rs)
        power_resampling_nocomp[f, paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_nocomp_w[f,"AIC"] <- power_resampling_nocomp[f,"AIC"]
      power_resampling_nocomp_w[f,"a2"] <- power_resampling_nocomp[f,"a2"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_nocomp_w[f,paste0("protocol",g)] <- power_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(power_resampling_nocomp)[2]-5
      power_resampling_nocomp[f, "a1"] <- apply(power_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_nocomp_w[f, "a1"] <- apply(power_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = power_resampling_nocomp[f,"a1"] * (x^power_resampling_nocomp[f,"a2"]),
                                          y_pred_b = power_resampling_nocomp_w[f,"a1"] * (x^power_resampling_nocomp_w[f,"a2"]))
      
      
      ## computing RMSE
      power_resampling_nocomp[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_nocomp[f,"weighted"] <- "no"
      power_resampling_nocomp_w[f,"weighted"] <- "yes"
      
    },
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  power_resampling_nocomp <- bind_rows(power_resampling_nocomp, power_resampling_nocomp_w)
  return(power_resampling_nocomp)
  
}




## Power model - No competition - Resampling : log-log version

mod_power_resampling_nocomp_log <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_nocomp_log, power_resampling_nocomp_w_log, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {

    ## running the models
    tryCatch({
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      # fitting power relationships
      m1_p_rs_log <- lm(log(y) ~ log(x), new_data)
      
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs_log <- lm(log(y) ~ log(x) + protocol, new_data)
        
        power_resampling_nocomp_log[f,"a2"] <- coefficients(m2_p_rs_log)["log(x)"]
        power_resampling_nocomp_log[f,"AIC"] <- AIC(m2_p_rs_log)
        power_resampling_nocomp_log[f,"sigma"] <- sigma(m2_p_rs_log)
        
        power_resampling_nocomp[f, paste0("protocol", levels(new_data$protocol)[1])] <- coefficients(m2_p_rs_log)[1]
        
        for (k in paste0("protocol", levels(new_data$protocol)[-1])) {
          power_resampling_nocomp_log[f,k] <- coefficients(m2_p_rs)[1] + coefficients(m2_p_rs)[paste0("a1.", k)]
        }

        
      } else {
        
        power_resampling_nocomp_log[f,"a2"] <- fixed.effects(m1_p_rs_log)["log(x)"]
        power_resampling_nocomp_log[f,"AIC"] <- AIC(m1_p_rs_log)
        power_resampling_nocomp_log[f,"sigma"] <- sigma(m1_p_rs_log)
        power_resampling_nocomp_log[f, paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_log)[1] 

        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_nocomp_w_log[f,"AIC"] <- power_resampling_nocomp_log[f,"AIC"]
      power_resampling_nocomp_w_log[f,"a2"] <- power_resampling_nocomp_log[f,"a2"]
      power_resampling_nocomp_w_log[f,"sigma"] <- power_resampling_nocomp_log[f,"sigma"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_nocomp_w_log[f,paste0("protocol",g)] <- power_resampling_nocomp_log[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      upper_range <- dim(power_resampling_nocomp_log)[2]-6
      power_alldata_resampling_log[f, "a1"] <- exp(apply(power_resampling_nocomp_log[f,c(2:upper_range)], 1, mean, na.rm = TRUE))
      power_alldata_resampling_log[f, "a1"] <- exp(apply(power_resampling_nocomp_log[f,c(2:upper_range)], 1, sum, na.rm = TRUE))
      
    
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = power_resampling_nocomp_log[f,"a1"] * (x^power_resampling_nocomp_log[f,"a2"]) * ((1/2)*(exp(power_resampling_nocomp_log[f,"sigma"]^2))),
                                          y_pred_b = power_resampling_nocomp_w_log[f,"a1"] * (x^power_resampling_nocomp_w_log[f,"a2"]) * ((1/2)*(exp(power_resampling_nocomp_w_log[f,"sigma"]^2))))
      
      
      ## computing RMSE
      power_resampling_nocomp_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_nocomp_w_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_nocomp_log[f,"weighted"] <- "no"
      power_resampling_nocomp_w_log[f,"weighted"] <- "yes"
      
    },
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  power_resampling_nocomp_log <- bind_rows(power_resampling_nocomp_log, power_resampling_nocomp_w_log)
  return(power_resampling_nocomp_log)
  
}


      


################################################################################
#                       ASYMPTOTIC RELATIONSHIPS                               #
################################################################################    

## Asymptotic model - No competition - All data

mod_asympt_alldata_nocomp <- function(data, asymptot_alldata_nocomp) {
  
  mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
  
  ## running the models
  tryCatch({
  
  vars <- data.frame(var = c("b1", "b2", "b3"), start = c(quantile(data$y, probs = 0.97)*0.8, 0.07, 0.9))
  
  m0_a <- nls(mod_asympt,
              data = data,
              start = setNames(as.list(vars$start), vars$var),
              lower = c(0.0001, 0.0001, 0.0001), algorithm = "port",
              nls.control(maxiter = 1000, tol = 1e-2))
  
  m1_a <- nlme(mod_asympt,
               data = data,
               fixed = list(b1 ~ 1, b2 ~ 1, b3 ~ 1),
               random = b1 ~ 1|location,
               start = c(b1 = coefficients(m0_a)["b1"], b2 = coefficients(m0_a)["b2"], b3 = coefficients(m0_a)["b3"]),
               method = "ML", 
               weights = varPower(form = ~fitted(.)),
               control = nlmeControl(maxIter = 1000, tolerance = 1e-2, pnlsTol = 1e-1))
  
  if (length(unique(data$protocol)) > 1) {
    
    m2_a <- nlme(mod_asympt,
                 data = data,
                 fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
                 random = b1 ~ 1|location,
                 start = c(b1 = c(rep(fixef(m1_a)["b1"], length(unique(data$protocol))), b2 = fixef(m1_a)["b2"], b3 = fixef(m1_a)["b3"])),
                 method = "ML", 
                 weights = varPower(form = ~fitted(.)),
                 control = nlmeControl(maxIter = 1000, tolerance = 1e-2, pnlsTol = 1e-1))
    
    asymptot_alldata_nocomp[1,"b2"] <- fixed.effects(m2_a)["b2"]
    asymptot_alldata_nocomp[1,"b3"] <- fixed.effects(m2_a)["b3"]
    asymptot_alldata_nocomp[1,"AIC"] <- AIC(m2_a)
    
    asymptot_alldata_nocomp[1,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m2_a)[1]
    
    for (k in paste0("protocol", levels(data$protocol)[-1])) {
      asymptot_alldata_nocomp[1,k] <- fixed.effects(m2_a)[1] + fixed.effects(m2_a)[paste0("b1.", k)]
    }
    
    
    
  } else {
    
    asymptot_alldata_nocomp[1,"b2"] <- fixed.effects(m1_a)["b2"]
    asymptot_alldata_nocomp[1,"b3"] <- fixed.effects(m1_a)["b3"]
    asymptot_alldata_nocomp[1,"AIC"] <- AIC(m1_a)
    asymptot_alldata_nocomp[1,paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_a)[1]
    
  }
  
  ## completing storage file with weighted parameters
  asymptot_alldata_nocomp[2,"b2"] <- asymptot_alldata_nocomp[1,"b2"]
  asymptot_alldata_nocomp[2,"b3"] <- asymptot_alldata_nocomp[1,"b3"]
  asymptot_alldata_nocomp[2,"AIC"] <- asymptot_alldata_nocomp[1,"AIC"]
  
  ## extracting number of observations per protocol to attribute weights
  n_tot <- dim(data)[1]
  nobs <- data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
  
  for (g in levels(data$protocol)) {
    asymptot_alldata_nocomp[2,paste0("protocol",g)] <- asymptot_alldata_nocomp[1,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
  }
  
  upper_range <- dim(asymptot_alldata_nocomp)[2]-4
  asymptot_alldata_nocomp[1, "b1"] <- apply(asymptot_alldata_nocomp[1,c(2:upper_range)], 1, mean, na.rm = TRUE)
  asymptot_alldata_nocomp[2, "b1"] <- apply(asymptot_alldata_nocomp[1,c(2:upper_range)], 1, sum, na.rm = TRUE)
  
  
  },

  error = function(e) {
    print("error")
  })
  
  return(asymptot_alldata_nocomp)
  
}





## Asymptotic model - No competition - Resampling

mod_asympt_resampling_nocomp <- function(ranged_data, nb_datasets_all, sample_size,  asymptot_resampling_nocomp, asymptot_resampling_nocomp_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
  
  ## defining the model
  mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
  
  ## running the models
  tryCatch({
    
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    vars_rs <- data.frame(var = c("b1", "b2", "b3"), start = c(quantile(new_data$y, probs = 0.97)*0.8, 0.07, 0.9))
    
    m0_a_rs <- nls(mod_asympt,
                   data = new_data,
                   start = setNames(as.list(vars_rs$start), vars_rs$var),
                   lower = c(0.0001, 0.0001, 0.0001), algorithm = "port",
                   nls.control(maxiter = 1000, tol = 1e-2))
    
    m1_a_rs <- gnls(mod_asympt,
                    data = new_data,
                    params = list(b1 ~ 1, b2 ~ 1, b3 ~ 1),
                    start = c(b1 = coefficients(m0_a_rs)["b1"], b2 = coefficients(m0_a_rs)["b2"], b3 = coefficients(m0_a_rs)["b3"]),
                    weights = varPower(form = ~fitted(.)),
                    control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
    
    if (length(unique(new_data$protocol)) > 1) {
      
      m2_a_rs <- gnls(mod_asympt,
                      data = new_data,
                      params = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
                      start = c(b1 = c(rep(coefficients(m1_a_rs)["b1"], length(unique(new_data$protocol)))), 
                                b2 = coefficients(m1_a_rs)["b2"], b3 = coefficients(m1_a_rs)["b3"]),
                      weights = varPower(form = ~fitted(.)),
                      control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
      
      asymptot_resampling_nocomp[f,"b2"] <- coefficients(m2_a_rs)["b2"]
      asymptot_resampling_nocomp[f,"b3"] <- coefficients(m2_a_rs)["b3"]
      asymptot_resampling_nocomp[f,"AIC"] <- AIC(m2_a_rs)
      
      asymptot_resampling_nocomp[f, paste0("protocol", levels(new_data$protocol)[1])] <- coefficients(m2_a_rs)[1]
      
      for (k in paste0("protocol", levels(new_data$protocol)[-1])) {
        asymptot_resampling_nocomp[f,k] <- coefficients(m2_a_rs)[1] + coefficients(m2_a_rs)[paste0("b1.", k)]
      }
      
      
    } else {
      
      asymptot_resampling_nocomp[f,"b2"] <- coefficients(m1_a_rs)["b2"]
      asymptot_resampling_nocomp[f,"b3"] <- coefficients(m1_a_rs)["b3"]
      asymptot_resampling_nocomp[f,"AIC"] <- AIC(m1_a_rs)
      asymptot_resampling_nocomp[f, paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_a_rs)[1]
      
    }
    
    
    ## completing storage file with weighted parameters
    asymptot_resampling_nocomp_w[f,"AIC"] <- asymptot_resampling_nocomp[f,"AIC"]
    asymptot_resampling_nocomp_w[f,"b2"] <- asymptot_resampling_nocomp[f,"b2"]
    asymptot_resampling_nocomp_w[f,"b3"] <- asymptot_resampling_nocomp[f,"b3"]
    
    ## extracting number of observations per protocol to attribute weights
    n_tot <- dim(new_data)[1]
    nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
    
    for (g in levels(new_data$protocol)) {
      asymptot_resampling_nocomp_w[f,paste0("protocol",g)] <- asymptot_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
    }
    
    
    upper_range <- dim(asymptot_resampling_nocomp)[2]-5
    asymptot_resampling_nocomp[f, "b1"] <- apply(asymptot_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
    asymptot_resampling_nocomp_w[f, "b1"] <- apply(asymptot_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
    
    
    
    
    ## predicting y on subsampled data
    test_data_b <- test_data %>% mutate(y_pred_a = 1.3 + asymptot_resampling_nocomp[f,"b1"] * (1-exp(-asymptot_resampling_nocomp[f,"b2"] * x)) ^ asymptot_resampling_nocomp[f,"b3"],
                                        y_pred_b = 1.3 + asymptot_resampling_nocomp_w[f,"b1"] * (1-exp(-asymptot_resampling_nocomp_w[f,"b2"] * x)) ^ asymptot_resampling_nocomp_w[f,"b3"])
    

    ## computing RMSE
    asymptot_resampling_nocomp[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
    asymptot_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
    
    
    ## combining both files into one for a single return
    asymptot_resampling_nocomp[f,"weighted"] <- "no"
    asymptot_resampling_nocomp_w[f,"weighted"] <- "yes"
    
  },
  
  error = function(e) {
    print("error")
  })
  
  }
  
  asymptot_resampling_nocomp <- bind_rows(asymptot_resampling_nocomp, asymptot_resampling_nocomp_w)
  return(asymptot_resampling_nocomp)
  
}
  
  

  
