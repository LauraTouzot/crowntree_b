##### HEIGHT MODEL #####

mod_height <- function(ranged_data, nb_datasets_all, sample_size,  asymptot_resampling_nocomp, asymptot_resampling_nocomp_w, n_repetition) {
  
  nrep = n_repetition

  set.seed(123)
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
                     nls.control(maxiter = 1000, tol = 1e-1))
      
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
      nobs <- new_data %>% dplyr::group_by(protocol) %>% dplyr::summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        asymptot_resampling_nocomp_w[f,paste0("protocol",g)] <- asymptot_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      
      upper_range <- dim(asymptot_resampling_nocomp)[2]-6
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


##### DIAMETER MODEL #####

mod_diameter <- function(ranged_data, nb_datasets_all, sample_size, 
                         power_resampling_nocomp, power_resampling_nocomp_w, 
                         power_resampling_c1, power_resampling_c1_w, 
                         power_resampling_c2, power_resampling_c2_w, 
                         n_repetition) {
  
  nrep = n_repetition
  
  set.seed(123)
  for (f in 1:nrep) {
    
    ## defining the model
    mod_power <- y ~ a1 * (x ^ a2)
    mod_power_c1 <- y ~ (a1 + a3 * ba_plot) * (x ^ a2)
    mod_power_c2 <- y ~ (a1 + a3 * ba_larger) * (x ^ a2)
    
    ## running the models
    tryCatch({
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      ## fitting power relationships
      
      # no competition
      init_rs <- coefficients(lm(log(y) ~ log(x), new_data)) # initializing values for power models
      
      m1_p_rs <- gnls(mod_power,
                      data = new_data,
                      params = list(a1 ~ 1, a2 ~ 1),
                      start = c(a1 = exp(init_rs[1]), a2 = init_rs[2]),
                      weights = varPower(form = ~fitted(.)),
                      control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
      
    
      # total ba plot
      init_rs_c1 <- coefficients(lm(log(y) ~ log(x) , new_data))  # initializing values for power models
      
      m1_p_rs_c1 <- gnls(mod_power_c1,
                         data = new_data,
                         params = list(a1 ~ 1, a2 ~ 1, a3 ~ 1),
                         start = c(a1 = exp(init_rs_c1[1]), a3 = -0.01, a2 = init_rs_c1[2]),
                         weights = varPower(form = ~fitted(.)),
                         control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
      
      # ba larger trees
      init_rs_c2 <- coefficients(lm(log(y) ~ log(x) , new_data))  # initializing values for power models
      
      m1_p_rs_c2 <- gnls(mod_power_c2,
                         data = new_data,
                         params = list(a1 ~ 1, a2 ~ 1, a3 ~ 1),
                         start = c(a1 = exp(init_rs_c2[1]), a3 = -0.01, a2 = init_rs_c2[2]),
                         weights = varPower(form = ~fitted(.)),
                         control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
      
      
      if (length(unique(new_data$protocol)) > 1) {
        
        m2_p_rs <- gnls(mod_power,
                        data = new_data,
                        params = list(a1 ~ protocol, a2 ~ 1),
                        start = c(a1 = c(rep(exp(init_rs[1]), length(unique(new_data$protocol)))), a2 = init_rs[2]),
                        weights = varPower(form = ~fitted(.)),
                        control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
        
        m2_p_rs_c1 <- gnls(mod_power_c1,
                           data = new_data,
                           params = list(a1 ~ protocol, a2 ~ 1, a3 ~ 1),
                           start = c(a1 = c(rep(exp(init_rs_c1[1]), length(unique(new_data$protocol)))), a3 = -0.01, a2 = init_rs_c1[2]),
                           weights = varPower(form = ~fitted(.)),
                           control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
        
        m2_p_rs_c2 <- gnls(mod_power_c2,
                           data = new_data,
                           params = list(a1 ~ protocol, a2 ~ 1, a3 ~ 1),
                           start = c(a1 = c(rep(exp(init_rs_c2[1]), length(unique(new_data$protocol)))), a3 = -0.01, a2 = init_rs_c2[2]),
                           weights = varPower(form = ~fitted(.)),
                           control = gnlsControl(maxIter = 1000, tolerance = 0.1, nlsTol = 0.1))
        
        power_resampling_nocomp[f,"a2"] <- coefficients(m2_p_rs)["a2"]
        power_resampling_nocomp[f,"comp"] <- NA
        power_resampling_nocomp[f,"AIC"] <- AIC(m2_p_rs)
        
        for (k in paste0("protocol", levels(new_data$protocol)[-1])) {
          power_resampling_nocomp[f,k] <- coefficients(m2_p_rs)[1] + coefficients(m2_p_rs)[paste0("a1.", k)]
        }
        
        power_resampling_nocomp[f, paste0("protocol", levels(new_data$protocol)[1])] <- coefficients(m2_p_rs)[1]
        
        
        power_resampling_c1[f,"a2"] <- coefficients(m2_p_rs_c1)["a2"]
        power_resampling_c1[f,"comp"] <- coefficients(m2_p_rs_c1)["a3"]
        power_resampling_c1[f,"AIC"] <- AIC(m2_p_rs_c1)
        
        power_resampling_c2[f,"a2"] <- coefficients(m2_p_rs_c2)["a2"]
        power_resampling_c2[f,"comp"] <- coefficients(m2_p_rs_c2)["a3"]
        power_resampling_c2[f,"AIC"] <- AIC(m2_p_rs_c2)
        
        power_resampling_c1[f, paste0("a1.protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c1)[1]
        power_resampling_c2[f, paste0("a1.protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_p_rs_c2)[1]
        
        for (k in paste0("a1.protocol", levels(factor(new_data$protocol))[-1])) {
          power_resampling_c1[f,k] <- coefficients(m2_p_rs_c1)[1] + coefficients(m2_p_rs_c1)[k]
          power_resampling_c2[f,k] <- coefficients(m2_p_rs_c2)[1] + coefficients(m2_p_rs_c2)[k]
        }
        
        
      } else {
        
        power_resampling_nocomp[f,"a2"] <- coefficients(m1_p_rs)["a2"]
        power_resampling_nocomp[f,"AIC"] <- AIC(m1_p_rs)
        power_resampling_nocomp[f,"comp"] <- NA
        power_resampling_nocomp[f, paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs)[1]
        
        power_resampling_c1[f,"a2"] <- coefficients(m1_p_rs_c1)["a2"]
        power_resampling_c1[f,"comp"] <- coefficients(m1_p_rs_c1)["a3"]
        power_resampling_c1[f,"AIC"] <- AIC(m1_p_rs_c1)
        power_resampling_c1[f,paste0("a1.protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c1)[1]
        
        power_resampling_c2[f,"a2"] <- coefficients(m1_p_rs_c2)["a2"]
        power_resampling_c2[f,"comp"] <- coefficients(m1_p_rs_c2)["a3"]
        power_resampling_c2[f,"AIC"] <- AIC(m1_p_rs_c2)
        power_resampling_c2[f,paste0("a1.protocol", unique(new_data$protocol))] <- coefficients(m1_p_rs_c2)[1]
        
      }
      
      ## completing storage file with weighted parameters
      power_resampling_nocomp_w[f,"AIC"] <- power_resampling_nocomp[f,"AIC"]
      power_resampling_nocomp_w[f,"comp"] <- power_resampling_nocomp[f,"comp"]
      power_resampling_nocomp_w[f,"a2"] <- power_resampling_nocomp[f,"a2"]
      
      power_resampling_c1_w[f,"AIC"] <- power_resampling_c1[f,"AIC"]
      power_resampling_c1_w[f,"comp"] <- power_resampling_c1[f,"comp"]
      power_resampling_c1_w[f,"a2"] <- power_resampling_c1[f,"a2"]
      
      power_resampling_c2_w[f,"AIC"] <- power_resampling_c2[f,"AIC"]
      power_resampling_c2_w[f,"comp"] <- power_resampling_c2[f,"comp"]
      power_resampling_c2_w[f,"a2"] <- power_resampling_c2[f,"a2"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% 
                           dplyr::summarise(n = n()/n_tot) %>% 
                           dplyr::ungroup()

      
      for (g in levels(new_data$protocol)) {
        power_resampling_nocomp_w[f,paste0("protocol",g)] <- power_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% dplyr::filter(protocol== g)) [1,"n"] 
        power_resampling_c1_w[f,paste0("a1.protocol",g)] <- power_resampling_c1[f,paste0("a1.protocol",g)] * (nobs %>% dplyr::filter(protocol == g)) [1,"n"] 
        power_resampling_c2_w[f,paste0("a1.protocol",g)] <- power_resampling_c2[f,paste0("a1.protocol",g)] * (nobs %>% dplyr::filter(protocol == g)) [1,"n"] 
        
      }
      
      upper_range <- dim(power_resampling_c2)[2]-7
      power_resampling_nocomp[f, "a1"] <- apply(power_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_nocomp_w[f, "a1"] <- apply(power_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      power_resampling_c1[f, "a1"] <- apply(power_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c1_w[f, "a1"] <- apply(power_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      power_resampling_c2[f, "a1"] <- apply(power_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      power_resampling_c2_w[f, "a1"] <- apply(power_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)

      
      ## predicting y on subsampled data
      test_data_nocomp <- test_data %>% mutate(y_pred_a = power_resampling_nocomp[f,"a1"] * (x^power_resampling_nocomp[f,"a2"]),
                                               y_pred_b = power_resampling_nocomp_w[f,"a1"] * (x^power_resampling_nocomp_w[f,"a2"]))
      
      test_data_c1 <- test_data %>% dplyr::mutate(y_pred_a = (power_resampling_c1[f,"a1"] + (power_resampling_c1[f,"comp"]*ba_plot)) * (x^power_resampling_c1[f,"a2"]),
                                                  y_pred_b = (power_resampling_c1_w[f,"a1"] + (power_resampling_c1_w[f,"comp"]*ba_plot)) * (x^power_resampling_c1_w[f,"a2"]))
      
      
      test_data_c2 <- test_data %>% dplyr::mutate(y_pred_a = (power_resampling_c2[f,"a1"] + (power_resampling_c2[f,"comp"]*ba_larger)) * (x^power_resampling_c2[f,"a2"]),
                                                  y_pred_b = (power_resampling_c2_w[f,"a1"] + (power_resampling_c2_w[f,"comp"]*ba_larger)) * (x^power_resampling_c2_w[f,"a2"]))
      
      ## computing RMSE
      power_resampling_nocomp[f,"RMSE"] <- rmse(test_data_nocomp$y, test_data_nocomp$y_pred_a)
      power_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_nocomp$y, test_data_nocomp$y_pred_b)
      
      power_resampling_c1[f,"RMSE"] <- rmse(test_data_c1$y, test_data_c1$y_pred_a)
      power_resampling_c1_w[f,"RMSE"] <- rmse(test_data_c1$y, test_data_c1$y_pred_b)
      
      power_resampling_c2[f,"RMSE"] <- rmse(test_data_c2$y, test_data_c2$y_pred_a)
      power_resampling_c2_w[f,"RMSE"] <- rmse(test_data_c2$y, test_data_c2$y_pred_b)
      
      
      ## combining both files into one for a single return
      power_resampling_nocomp[f,"weighted"] <- "no"
      power_resampling_nocomp[f,"condition"] <- "nocomp"
      power_resampling_nocomp_w[f,"weighted"] <- "yes"
      power_resampling_nocomp_w[f,"condition"] <- "nocomp"
      
      power_resampling_c1[f,"weighted"] <- "no"
      power_resampling_c1[f,"condition"] <- "c1"
      power_resampling_c1_w[f,"weighted"] <- "yes"
      power_resampling_c1_w[f,"condition"] <- "c1"
      
      power_resampling_c2[f,"weighted"] <- "no"
      power_resampling_c2[f,"condition"] <- "c2"
      power_resampling_c2_w[f,"weighted"] <- "yes"
      power_resampling_c2_w[f,"condition"] <- "c2"
      
    },
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  power_diameter <- bind_rows(power_resampling_nocomp, power_resampling_nocomp_w, 
                              power_resampling_c1, power_resampling_c1_w,
                              power_resampling_c2, power_resampling_c2_w)
  return(power_diameter)
  
}



##### RATIO MODEL #####

mod_ratio <- function(ranged_data, nb_datasets_all, sample_size, 
                                    beta_resampling_nocomp, beta_resampling_nocomp_w, 
                                    beta_resampling_c1, beta_resampling_c1_w,
                                    beta_resampling_c2, beta_resampling_c2_w,
                                    n_repetition) {
  
  nrep = n_repetition
  
  set.seed(123)
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      upper_range <- dim(beta_resampling_c1)[2]-6
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_rs <- betareg(y ~ protocol,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_rs)["(Intercept)"]
        beta_resampling_nocomp[f,"comp"] <- NA
        beta_resampling_nocomp[f,"AIC"] <- AIC(m2_b_rs)
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_nocomp[f,k] <- coefficients(m2_b_rs)["(Intercept)"] + coefficients(m2_b_rs)[k]
        }
        
        beta_resampling_nocomp[f,"a1"] <- apply(beta_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)  
        
        
        m2_b_c1 <- betareg(y ~ protocol + ba_plot,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c1[f,"comp"] <- coefficients(m2_b_c1)["ba_plot"]
        beta_resampling_c1[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c1)["(Intercept)"]
        beta_resampling_c1[f,"AIC"] <- AIC(m2_b_c1)
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c1[f,k] <- coefficients(m2_b_c1)["(Intercept)"] + coefficients(m2_b_c1)[k]
        }
        
        beta_resampling_c1[f,"a1"] <- apply(beta_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
        
        
        m2_b_c2 <- betareg(y ~ protocol + ba_larger,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c2[f,"comp"] <- coefficients(m2_b_c2)["ba_larger"]
        beta_resampling_c2[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c2)["(Intercept)"]
        beta_resampling_c2[f,"AIC"] <- AIC(m2_b_c2)
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c2[f,k] <- coefficients(m2_b_c2)["(Intercept)"] + coefficients(m2_b_c2)[k]
        }
        
        beta_resampling_c2[f,"a1"] <- apply(beta_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE) 
        
        
      } else {
        
        
        m1_b_rs <- betareg(y ~ 1,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m1_b_rs)["(Intercept)"]
        beta_resampling_nocomp[f,"comp"] <- NA
        beta_resampling_nocomp[f,"a1"] <- coefficients(m1_b_rs)["(Intercept)"]
        beta_resampling_nocomp[f,"AIC"] <- AIC(m1_b_rs)
        
        
        m1_b_c1 <- betareg(y ~ ba_plot,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c1[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c1)["(Intercept)"]
        beta_resampling_c1[f,"a1"] <- coefficients(m1_b_c1)["(Intercept)"]
        beta_resampling_c1[f,"AIC"] <- AIC(m1_b_c1)
        beta_resampling_c1[f,"comp"] <- coefficients(m1_b_c1)["ba_plot"]
        
        
        m1_b_c2 <- betareg(y ~ ba_larger,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c2[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c2)["(Intercept)"]
        beta_resampling_c2[f,"a1"] <- coefficients(m1_b_c2)["(Intercept)"]
        beta_resampling_c2[f,"AIC"] <- AIC(m1_b_c2)
        beta_resampling_c2[f,"comp"] <- coefficients(m1_b_c2)["ba_larger"]
        
      }
      
      
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% 
                           dplyr::summarise(n = n()/n_tot) %>% 
                           ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_nocomp_w[f,paste0("protocol",g)] <- beta_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% dplyr::filter(protocol== g)) [1,"n"]
        beta_resampling_c1_w[f,paste0("protocol",g)] <- beta_resampling_c1[f,paste0("protocol",g)] * (nobs %>% dplyr::filter(protocol== g)) [1,"n"] 
        beta_resampling_c2_w[f,paste0("protocol",g)] <- beta_resampling_c2[f,paste0("protocol",g)] * (nobs %>% dplyr::filter(protocol== g)) [1,"n"] 
        
      }
      
      
      beta_resampling_nocomp_w[f, "a1"] <- apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      beta_resampling_nocomp_w[f, "comp"] <- beta_resampling_nocomp[f, "comp"] 
      beta_resampling_nocomp_w[f, "AIC"] <- beta_resampling_nocomp[f, "AIC"] 
      
      beta_resampling_c1_w[f, "a1"] <- apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      beta_resampling_c1_w[f, "comp"] <- beta_resampling_c1[f, "comp"] 
      beta_resampling_c1_w[f, "AIC"] <- beta_resampling_c1[f, "AIC"] 
      
      beta_resampling_c2_w[f, "a1"] <- apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      beta_resampling_c2_w[f, "comp"] <- beta_resampling_c2[f, "comp"] 
      beta_resampling_c2_w[f, "AIC"] <- beta_resampling_c2[f, "AIC"] 
      
      
      ## predicting y on subsampled data
      test_data_nocomp <- test_data %>% mutate(y_pred_a = exp(beta_resampling_nocomp[f,"a1"])/(1+exp(beta_resampling_nocomp[f,"a1"])),
                                               y_pred_b = exp(beta_resampling_nocomp_w[f,"a1"])/(1+exp(beta_resampling_nocomp_w[f,"a1"])))
      
      test_data_c1 <- test_data %>% mutate(y_pred_a = exp(beta_resampling_c1[f,"a1"] + beta_resampling_c1[f,"comp"]*ba_plot)/(1+exp(beta_resampling_c1[f,"a1"] + beta_resampling_c1[f,"comp"]*ba_plot)),
                                           y_pred_b = exp(beta_resampling_c1_w[f,"a1"] + beta_resampling_c1_w[f,"comp"]*ba_plot)/(1+exp(beta_resampling_c1_w[f,"a1"] + beta_resampling_c1_w[f,"comp"]*ba_plot)))
      
      test_data_c2 <- test_data %>% mutate(y_pred_a = exp(beta_resampling_c2[f,"a1"] + beta_resampling_c2[f,"comp"]*ba_larger)/(1+exp(beta_resampling_c2[f,"a1"] + beta_resampling_c2[f,"comp"]*ba_larger)),
                                           y_pred_b = exp(beta_resampling_c2_w[f,"a1"] + beta_resampling_c2_w[f,"comp"]*ba_larger)/(1+exp(beta_resampling_c2_w[f,"a1"] + beta_resampling_c2_w[f,"comp"]*ba_larger)))
      
      
      ## computing RMSE
      beta_resampling_nocomp[f,"RMSE"] <- rmse(test_data_nocomp$y, test_data_nocomp$y_pred_a)
      beta_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_nocomp$y, test_data_nocomp$y_pred_b)
      
      beta_resampling_c1[f,"RMSE"] <- rmse(test_data_c1$y, test_data_c1$y_pred_a)
      beta_resampling_c1_w[f,"RMSE"] <- rmse(test_data_c1$y, test_data_c1$y_pred_b)
      
      beta_resampling_c2[f,"RMSE"] <- rmse(test_data_c2$y, test_data_c2$y_pred_a)
      beta_resampling_c2_w[f,"RMSE"] <- rmse(test_data_c2$y, test_data_c2$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_nocomp[f,"weighted"] <- "no"
      beta_resampling_nocomp[f,"condition"] <- "nocomp" 
      beta_resampling_nocomp_w[f,"weighted"] <- "yes"
      beta_resampling_nocomp_w[f,"condition"] <- "nocomp" 
      
      beta_resampling_c1[f,"weighted"] <- "no"
      beta_resampling_c1[f,"condition"] <- "c1" 
      beta_resampling_c1_w[f,"weighted"] <- "yes"
      beta_resampling_c1_w[f,"condition"] <- "c1" 
      
      beta_resampling_c2[f,"weighted"] <- "no"
      beta_resampling_c2[f,"condition"] <- "c2" 
      beta_resampling_c2_w[f,"weighted"] <- "yes"
      beta_resampling_c2_w[f,"condition"] <- "c2" 
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_mean <- bind_rows(beta_resampling_nocomp, beta_resampling_nocomp_w, 
                                    beta_resampling_c1, beta_resampling_c1_w,
                                    beta_resampling_c2, beta_resampling_c2_w)
  
  return(beta_resampling_mean)
  
}





