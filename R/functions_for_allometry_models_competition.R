################################################################################
#                            LINEAR RELATIONSHIPS                              #
################################################################################

## Linear model - Competition : ba_plot - Resampling

mod_linear_resampling_c1 <- function(ranged_data, nb_datasets_all, sample_size, linear_resampling_c1, linear_resampling_c1_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
 
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## running the models
    tryCatch({  
      
      m1_l_rs_c1 <- gls(y ~ x + ba_plot,
                     data = new_data,
                     weights = varPower(form = ~fitted(.)),
                     method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))

      if(length(unique(new_data$protocol)) > 1){
        
        m2_l_rs_c1 <- gls(y ~ x + protocol + ba_plot,
                       data = new_data,
                       weights = varPower(form = ~fitted(.)),
                       method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
        
        linear_resampling_c1[f,"slope"] <- coefficients(m2_l_rs_c1)["x"]
        linear_resampling_c1[f,"comp"] <- coefficients(m2_l_rs_c1)["ba_plot"]
        linear_resampling_c1[f,"AIC"] <- AIC(m2_l_rs_c1)
        
        linear_resampling_c1[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_l_rs_c1)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          linear_resampling_c1[f,k] <- coefficients(m2_l_rs_c1)[1] + coefficients(m2_l_rs_c1)[k]
        }
        
        
      } else {
        
        linear_resampling_c1[f,"slope"] <- coefficients(m1_l_rs_c1)["x"]
        linear_resampling_c1[f,"comp"] <- coefficients(m1_l_rs_c1)["ba_plot"]
        linear_resampling_c1[f,"AIC"] <- AIC(m1_l_rs_c1)
        linear_resampling_c1[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_l_rs_c1)[1]
        
      }
      
      ## completing storage file with weighted parameters
      linear_resampling_c1_w[f,"AIC"] <- linear_resampling_c1[f,"AIC"]
      linear_resampling_c1_w[f,"comp"] <- linear_resampling_c1[f,"comp"]
      linear_resampling_c1_w[f,"slope"] <- linear_resampling_c1[f,"slope"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        linear_resampling_c1_w[f,paste0("protocol",g)] <- linear_resampling_c1[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(linear_resampling_c1)[2]-5
      linear_resampling_c1[f, "intercept"] <- apply(linear_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      linear_resampling_c1_w[f, "intercept"] <- apply(linear_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = linear_resampling_c1[f,"intercept"] + linear_resampling_c1[f,"comp"]*ba_plot + linear_resampling_c1[f,"slope"]*x,
                                          y_pred_b = linear_resampling_c1_w[f,"intercept"] + linear_resampling_c1_w[f,"comp"]*ba_plot + linear_resampling_c1_w[f,"slope"]*x)
      
      
      ## computing RMSE
      linear_resampling_c1[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      linear_resampling_c1_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      linear_resampling_c1 <- linear_resampling_c1 %>% mutate(weighted = "no")
      linear_resampling_c1_w <- linear_resampling_c1_w %>% mutate(weighted = "yes")
      
      linear_resampling_c1 <- bind_rows(linear_resampling_c1, linear_resampling_c1_w)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(linear_resampling_c1)
  
}



## Linear model - Competition : ba_larger - Resampling


mod_linear_resampling_c2 <- function(ranged_data, nb_datasets_all, sample_size, linear_resampling_c2, linear_resampling_c2_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {

    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## running the models
    tryCatch({  
      
      m1_l_rs_c2 <- gls(y ~ x + ba_larger,
                        data = new_data,
                        weights = varPower(form = ~fitted(.)),
                        method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_l_rs_c2 <- gls(y ~ x + protocol + ba_larger,
                          data = new_data,
                          weights = varPower(form = ~fitted(.)),
                          method = "ML", control = glsControl(maxIter = 1000, tolerance = 1e-2, msTol = 1e-1))
        
        linear_resampling_c2[f,"slope"] <- coefficients(m2_l_rs_c2)["x"]
        linear_resampling_c2[f,"comp"] <- coefficients(m2_l_rs_c2)["ba_larger"]
        linear_resampling_c2[f,"AIC"] <- AIC(m2_l_rs_c2)
        
        linear_resampling_c2[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_l_rs_c2)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          linear_resampling_c2[f,k] <- coefficients(m2_l_rs_c2)[1] + coefficients(m2_l_rs_c2)[k]
        }
        
        
      } else {
        
        linear_resampling_c2[f,"slope"] <- coefficients(m1_l_rs_c2)["x"]
        linear_resampling_c2[f,"comp"] <- coefficients(m1_l_rs_c2)["ba_larger"]
        linear_resampling_c2[f,"AIC"] <- AIC(m1_l_rs)
        linear_resampling_c2[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_l_rs_c2)[1]
        
      }
      
      ## completing storage file with weighted parameters
      linear_resampling_c2_w[f,"AIC"] <- linear_resampling_c2[f,"AIC"]
      linear_resampling_c2_w[f,"comp"] <- linear_resampling_c2[f,"comp"]
      linear_resampling_c2_w[f,"slope"] <- linear_resampling_c2[f,"slope"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        linear_resampling_c2_w[f,paste0("protocol",g)] <- linear_resampling_c2[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(linear_resampling_c2)[2]-5
      linear_resampling_c2[f, "intercept"] <- apply(linear_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      linear_resampling_c2_w[f, "intercept"] <- apply(linear_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = linear_resampling_c2[f,"intercept"] + linear_resampling_c2[f,"comp"]*ba_plot + linear_resampling_c2[f,"slope"]*x,
                                          y_pred_b = linear_resampling_c2_w[f,"intercept"] + linear_resampling_c2_w[f,"comp"]*ba_plot + linear_resampling_c2_w[f,"slope"]*x)
      
      
      ## computing RMSE
      linear_resampling_c2[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      linear_resampling_c2_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      linear_resampling_c2 <- linear_resampling_c2 %>% mutate(weighted = "no")
      linear_resampling_c2_w <- linear_resampling_c2_w %>% mutate(weighted = "yes")
      
      linear_resampling_c2 <- bind_rows(linear_resampling_c2, linear_resampling_c2_w)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(linear_resampling_c2)
  
}










################################################################################
#                            POWER RELATIONSHIPS                               #
################################################################################


## Power model - Competition : ba_plot - Resampling

mod_power_resampling_c1 <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_c1, power_resampling_c1_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## defining the model
    mod_power <- y ~ (a1 + ba_plot) * (x ^ a2)
    
    ## running the models
    tryCatch({  
      
      init_rs_c1 <- coefficients(lm(log(y) ~ log(x) + ba_plot, new_data)) # initializing values for power models
      
      m1_p_rs_c1 <- gnls(mod_power,
                         data = new_data,
                         params = list(a1 ~ 1, a2 ~ 1, ba_plot ~ 1),
                         start = c(a1 = exp(init_rs_c1[1]), comp = exp(init_rs_c1[1]), a2 = init_rs_c1[2]),
                         weights = varPower(form = ~fitted(.)),
                         control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs_c1 <- gnls(mod_power,
                           data = new_data,
                           params = list(a1 ~ protocol, a2 ~ 1, ba_plot ~ 1),
                           start = c(a1 = c(rep(exp(init_rs_c1[1]), length(unique(new_data$protocol)))), comp = exp(init_rs_c1[1]), a2 = init_rs_c1[2]),
                           weights = varPower(form = ~fitted(.)),
                           control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
        
        
        power_resampling_c1[f,"a2"] <- coefficients(m2_p_rs_c1)["a2"]
        power_resampling_c1[f,"comp"] <- coefficients(m2_p_rs_c1)["ba_plot"]
        power_resampling_c1[f,"AIC"] <- AIC(m2_p_rs_c1)
        
        power_resampling_c1[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c1)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          power_resampling_c1[f,k] <- coefficients(m2_p_rs_c1)[1] + coefficients(m2_p_rs_c1)[k]
        }
        
        
      } else {
        
        power_resampling_c1[f,"a2"] <- coefficients(m1_p_rs_c1)["a2"]
        power_resampling_c1[f,"comp"] <- coefficients(m1_p_rs_c1)["ba_plot"]
        power_resampling_c1[f,"AIC"] <- AIC(m1_p_rs_c1)
        power_resampling_c1[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c1)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_c1_w[f,"AIC"] <- power_resampling_c1[f,"AIC"]
      power_resampling_c1_w[f,"comp"] <- power_resampling_c1[f,"comp"]
      power_resampling_c1_w[f,"a2"] <- power_resampling_c1[f,"a2"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_c1_w[f,paste0("protocol",g)] <- power_resampling_c1[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(power_resampling_c1)[2]-5
      power_resampling_c1[f, "a1"] <- apply(power_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c1_w[f, "a1"] <- apply(power_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = power_resampling_c1[f,"a1"] + (power_resampling_c1[f,"comp"]*ba_plot) * (x^power_resampling_c1[f,"a2"]),
                                          y_pred_b = power_resampling_c1_w[f,"a1"] + (power_resampling_c1_w[f,"comp"]*ba_plot) * (x^power_resampling_c1_w[f,"a2"]))
      
      
      ## computing RMSE
      power_resampling_c1[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_c1_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_c1 <- power_resampling_c1 %>% mutate(weighted = "no")
      power_resampling_c1_w <- power_resampling_c1_w %>% mutate(weighted = "yes")
      
      power_resampling_c1 <- bind_rows(power_resampling_c1, power_resampling_c1_w)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(power_resampling_c1)
  
}




## Power model - Competition : ba_plot - Resampling (log - log version)

mod_power_resampling_c1_log <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_c1_log, power_resampling_c1_w_log, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## running the models
    tryCatch({  

      m1_p_rs_c1_log <- lm(log(y) ~ ba_plot + log(x), new_data)
 
            if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs_c1_log <- lm(log(y) ~ protocol + ba_plot + log(x), new_data)
        
        power_resampling_c1_log[f,"a2"] <- coefficients(m2_p_rs_c1_log)["log(x)"]
        power_resampling_c1_log[f,"comp"] <- coefficients(m2_p_rs_c1_log)["ba_plot"]
        power_resampling_c1_log[f,"sigma"] <- sigma(m2_p_rs_c1_log)
        power_resampling_c1_log[f,"AIC"] <- AIC(m2_p_rs_c1_log)
        
        power_resampling_c1_log[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c1_log)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          power_resampling_c1_log[f,k] <- coefficients(m2_p_rs_c1_log)[1] + coefficients(m2_p_rs_c1_log)[k]
        }
        
        
      } else {
        
        power_resampling_c1_log[f,"a2"] <- coefficients(m1_p_rs_c1_log)["a2"]
        power_resampling_c1_log[f,"comp"] <- coefficients(m1_p_rs_c1_log)["ba_plot"]
        power_resampling_c1_log[f,"AIC"] <- AIC(m1_p_rs_c1_log)
        power_resampling_c1_log[f,"sigla"] <- sigma(m1_p_rs_c1_log)
        power_resampling_c1_log[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c1_log)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_c1_w_log[f,"AIC"] <- power_resampling_c1_log[f,"AIC"]
      power_resampling_c1_w_log[f,"comp"] <- power_resampling_c1_log[f,"comp"]
      power_resampling_c1_w_log[f,"a2"] <- power_resampling_c1_log[f,"a2"]
      power_resampling_c1_w_log[f,"sigma"] <- power_resampling_c1_log[f,"sigma"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_c1_w[f,paste0("protocol",g)] <- power_resampling_c1[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(power_resampling_c1_log)[2]-5
      power_resampling_c1_log[f, "a1"] <- apply(power_resampling_c1_log[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c1_w_log[f, "a1"] <- apply(power_resampling_c1_w_log[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(power_resampling_c1_log[f,"a1"] + (power_resampling_c1_log[f,"comp"]*ba_plot)) * (x^power_resampling_c1[f,"a2"]) *  ((1/2)*(exp(power_resampling_c1_log[f,"sigma"]^2))),
                                          y_pred_b = exp(power_resampling_c1_w_log[f,"a1"] + (power_resampling_c1_w_log[f,"comp"]*ba_plot)) * (x^power_resampling_c1_w_log[f,"a2"]) * ((1/2)*(exp(power_resampling_c1_log[f,"sigma"]^2))))
      
      
      ## computing RMSE
      power_resampling_c1_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_c1_w_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_c1_log <- power_resampling_c1_log %>% mutate(weighted = "no")
      power_resampling_c1_w_log <- power_resampling_c1_w_log %>% mutate(weighted = "yes")
      
      power_resampling_c1_log <- bind_rows(power_resampling_c1_log, power_resampling_c1_w_log)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(power_resampling_c1_log)
  
}







## Power model - Competition : ba_larger - Resampling


mod_power_resampling_c2 <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_c2, power_resampling_c2_w, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## defining the model
    mod_power <- y ~ (a1 + ba_larger) * (x ^ a2)
    
    ## running the models
    tryCatch({  
      
      init_rs_c2 <- coefficients(lm(log(y) ~ log(x), new_data)) # initializing values for power models
      
      m1_p_rs_c2 <- gnls(mod_power,
                         data = new_data,
                         params = list(a1 ~ 1, a2 ~ 1, ba_larger ~ 1),
                         start = c(a1 = exp(init_rs_c2[1]), comp = exp(init_rs_c2[1]), a2 = init_rs_c2[2]),
                         weights = varPower(form = ~fitted(.)),
                         control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs_c2 <- gnls(mod_power,
                           data = new_data,
                           params = list(a1 ~ protocol, a2 ~ 1, ba_larger ~ 1),
                           start = c(a1 = c(rep(exp(init_rs_c2[1]), length(unique(new_data$protocol)))), comp = exp(init_rs_c2[1]), a2 = init_rs_c2[2]),
                           weights = varPower(form = ~fitted(.)),
                           control = gnlsControl(maxIter = 1000, tolerance = 1e-2, nlsTol = 1e-1))
        
        
        power_resampling_c2[f,"a2"] <- coefficients(m2_p_rs_c2)["a2"]
        power_resampling_c2[f,"comp"] <- coefficients(m2_p_rs_c2)["ba_larger"]
        power_resampling_c2[f,"AIC"] <- AIC(m2_p_rs_c2)
        
        power_resampling_c2[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c2)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          power_resampling_c2[f,k] <- coefficients(m2_p_rs_c2)[1] + coefficients(m2_p_rs_c2)[k]
        }
        
        
      } else {
        
        power_resampling_c2[f,"a2"] <- coefficients(m1_p_rs_c2)["a2"]
        power_resampling_c2[f,"comp"] <- coefficients(m1_p_rs_c2)["ba_larger"]
        power_resampling_c2[f,"AIC"] <- AIC(m1_p_rs_c2)
        power_resampling_c2[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c2)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_c2_w[f,"AIC"] <- power_resampling_c2[f,"AIC"]
      power_resampling_c2_w[f,"comp"] <- power_resampling_c2[f,"comp"]
      power_resampling_c2_w[f,"a2"] <- power_resampling_c2[f,"a2"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_c2_w[f,paste0("protocol",g)] <- power_resampling_c2[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(power_resampling_c2)[2]-5
      power_resampling_c2[f, "a1"] <- apply(power_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c2_w[f, "a1"] <- apply(power_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = power_resampling_c2[f,"a1"] + (power_resampling_c2[f,"comp"]*ba_plot) * (x^power_resampling_c2[f,"a2"]),
                                          y_pred_b = power_resampling_c2_w[f,"a1"] + (power_resampling_c2_w[f,"comp"]*ba_plot) * (x^power_resampling_c2_w[f,"a2"]))
      
      
      ## computing RMSE
      power_resampling_c2[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_c2_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_c2 <- power_resampling_c2 %>% mutate(weighted = "no")
      power_resampling_c2_w <- power_resampling_c2_w %>% mutate(weighted = "yes")
      
      power_resampling_c2 <- bind_rows(power_resampling_c2, power_resampling_c2_w)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(power_resampling_c2)
  
}




## Power model - Competition : ba_larger - Resampling (log - log version)

mod_power_resampling_c2_log <- function(ranged_data, nb_datasets_all, sample_size, power_resampling_c2_log, power_resampling_c2_w_log, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## computing new dataset based on defined sampling protocol
    new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
    
    ## computing test dataset 
    test_data <- testing_data(ranged_data, new_data, sample_size)
    
    ## running the models
    tryCatch({  
      
      m1_p_rs_c2_log <- lm(log(y) ~ ba_larger + log(x), new_data)
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs_c2_log <- lm(log(y) ~ protocol + ba_plot + log(x), new_data)
        
        power_resampling_c2_log[f,"a2"] <- coefficients(m2_p_rs_c2_log)["log(x)"]
        power_resampling_c2_log[f,"comp"] <- coefficients(m2_p_rs_c2_log)["ba_larger"]
        power_resampling_c2_log[f,"sigma"] <- sigma(m2_p_rs_c2_log)
        power_resampling_c2_log[f,"AIC"] <- AIC(m2_p_rs_c2_log)
        
        power_resampling_c2_log[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c2_log)[1]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          power_resampling_c2_log[f,k] <- coefficients(m2_p_rs_c2_log)[1] + coefficients(m2_p_rs_c2_log)[k]
        }
        
        
      } else {
        
        power_resampling_c2_log[f,"a2"] <- coefficients(m1_p_rs_c2_log)["a2"]
        power_resampling_c2_log[f,"comp"] <- coefficients(m1_p_rs_c2_log)["ba_larger"]
        power_resampling_c2_log[f,"AIC"] <- AIC(m1_p_rs_c2_log)
        power_resampling_c2_log[f,"sigma"] <- sigma(m1_p_rs_c2_log)
        power_resampling_c2_log[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c2_log)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_c2_w_log[f,"AIC"] <- power_resampling_c2_log[f,"AIC"]
      power_resampling_c2_w_log[f,"comp"] <- power_resampling_c2_log[f,"comp"]
      power_resampling_c2_w_log[f,"a2"] <- power_resampling_c2_log[f,"a2"]
      power_resampling_c2_w_log[f,"sigma"] <- power_resampling_c2_log[f,"sigma"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% group_by(protocol) %>% summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        power_resampling_c2_w[f,paste0("protocol",g)] <- power_resampling_c2[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(power_resampling_c2_log)[2]-5
      power_resampling_c2_log[f, "a1"] <- apply(power_resampling_c2_log[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c2_w_log[f, "a1"] <- apply(power_resampling_c2_w_log[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(power_resampling_c2_log[f,"a1"] + (power_resampling_c2_log[f,"comp"]*ba_plot)) * (x^power_resampling_c2[f,"a2"]) *  ((1/2)*(exp(power_resampling_c2_log[f,"sigma"]^2))),
                                          y_pred_b = exp(power_resampling_c2_w_log[f,"a1"] + (power_resampling_c2_w_log[f,"comp"]*ba_plot)) * (x^power_resampling_c2_w_log[f,"a2"]) * ((1/2)*(exp(power_resampling_c2_log[f,"sigma"]^2))))
      
      
      ## computing RMSE
      power_resampling_c2_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      power_resampling_c2_w_log[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_c2_log <- power_resampling_c2_log %>% mutate(weighted = "no")
      power_resampling_c2_w_log <- power_resampling_c2_w_log %>% mutate(weighted = "yes")
      
      power_resampling_c2_log <- bind_rows(power_resampling_c2_log, power_resampling_c2_w_log)
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  return(power_resampling_c2_log)
  
}





