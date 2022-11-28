## Beta linear model - No competition - Resampling
# using logit link for the conditional mean

mod_beta_resampling_nocomp <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_nocomp, beta_resampling_nocomp_w, beta_dbh, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      m1_b_rs <- betareg(y ~ x,
                         data = new_data,
                         type = "ML", 
                         control = betareg.control(maxit = 1000, fstol = 1e-1))
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_rs <- betareg(y ~ x + protocol,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_nocomp[f,"a2"] <- coefficients(m2_b_rs)["x"]
        beta_resampling_nocomp[f,"AIC"] <- AIC(m2_b_rs)
        beta_resampling_nocomp[f,"zvalue"] <- as.data.frame(summary(m2_b_rs)$coefficients$mean)["x","z value"]
        beta_resampling_nocomp[f,"pr_z"] <- as.data.frame(summary(m2_b_rs)$coefficients$mean)["x","Pr(>|z|)"]
        
        beta_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_rs)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_nocomp[f,k] <- coefficients(m2_b_rs)["(Intercept)"] + coefficients(m2_b_rs)[k]
        }
        
        
      } else {
        
        beta_resampling_nocomp[f,"a2"] <- coefficients(m1_b_rs)["x"]
        beta_resampling_nocomp[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_rs)["(Intercept)"]
        beta_resampling_nocomp[f,"zvalue"] <- as.data.frame(summary(m1_b_rs)$coefficients$mean)["x","z value"]
        beta_resampling_nocomp[f,"pr_z"] <- as.data.frame(summary(m1_b_rs)$coefficients$mean)["x","Pr(>|z|)"]
        
      }
      
      
      ## completing storage file with weighted parameters
      beta_resampling_nocomp_w[f,"a2"] <- beta_resampling_nocomp[f,"a2"]
      beta_resampling_nocomp_w[f,"AIC"] <- beta_resampling_nocomp[f,"AIC"]
      beta_resampling_nocomp_w[f,"zvalue"] <- beta_resampling_nocomp[f,"zvalue"]
      beta_resampling_nocomp_w[f,"pr_z"] <- beta_resampling_nocomp[f,"pr_z"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% 
                           dplyr::summarise(n = n()/n_tot) %>% 
                           ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_nocomp_w[f,paste0("protocol",g)] <- beta_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      upper_range <- dim(beta_resampling_nocomp)[2]-7
      
      beta_resampling_nocomp[f, "a1"] <- apply(beta_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      beta_resampling_nocomp_w[f, "a1"] <- apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      # beta_dbh[f,"dbh_10_w"] <- exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*10)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*10))
      # beta_dbh[f,"dbh_15_w"] <- exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*15)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*15))
      # beta_dbh[f,"dbh_20_w"] <- exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*20)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*20))
      # beta_dbh[f,"dbh_30_w"] <- exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*30)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*30))
      # beta_dbh[f,"dbh_40_w"] <- exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*40)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*40))
    
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(apply(beta_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_nocomp[f,"a2"]*x)/(1+exp(apply(beta_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_nocomp[f,"a2"]*x)),
                                          y_pred_b = exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*x)/(1+exp(apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_nocomp_w[f,"a2"]*x)))
      
      
      ## computing RMSE
      beta_resampling_nocomp[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_nocomp[f,"weighted"] <- "no" 
      beta_resampling_nocomp_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_nocomp <- bind_rows(beta_resampling_nocomp, beta_resampling_nocomp_w)
  # write.csv(beta_dbh, file =  paste0("output/output_beta_dbh_", beta_dbh[1,1], ".csv"))
  
  return(beta_resampling_nocomp)
  
}




mod_beta_resampling_nocomp_mean <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_nocomp, beta_resampling_nocomp_w, beta_dbh, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      upper_range <- dim(beta_resampling_nocomp)[2]-3
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_rs <- betareg(y ~ protocol,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_rs)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_nocomp[f,k] <- coefficients(m2_b_rs)["(Intercept)"] + coefficients(m2_b_rs)[k]
        }
        
        beta_resampling_nocomp[f,"a1"] <- apply(beta_resampling_nocomp[f,c(2:upper_range)], 1, mean, na.rm = TRUE)  
        
      } else {
        
      
      m1_b_rs <- betareg(y ~ 1,
                         data = new_data,
                         type = "ML", 
                         control = betareg.control(maxit = 1000, fstol = 1e-1))
      
      beta_resampling_nocomp[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m1_b_rs)["(Intercept)"]
      beta_resampling_nocomp[f,"a1"] <- coefficients(m1_b_rs)["(Intercept)"]

      }
      
    
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% 
                           dplyr::summarise(n = n()/n_tot) %>% 
                           ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_nocomp_w[f,paste0("protocol",g)] <- beta_resampling_nocomp[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      

      beta_resampling_nocomp_w[f, "a1"] <- apply(beta_resampling_nocomp_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
 
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(beta_resampling_nocomp[f,"a1"])/(1+exp(beta_resampling_nocomp[f,"a1"])),
                                          y_pred_b = exp(beta_resampling_nocomp_w[f,"a1"])/(1+exp(beta_resampling_nocomp_w[f,"a1"])))
      
      
      ## computing RMSE
      beta_resampling_nocomp[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_nocomp_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_nocomp[f,"weighted"] <- "no" 
      beta_resampling_nocomp_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_nocomp_mean <- bind_rows(beta_resampling_nocomp, beta_resampling_nocomp_w)
  
  return(beta_resampling_nocomp_mean)
  
}






## Beta linear model - Competition: ba plot - Resampling
# using logit link for the conditional mean

mod_beta_resampling_c1 <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_c1, beta_resampling_c1_w, beta_dbh_c1, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      m1_b_c1 <- betareg(y ~ ba_plot + x,
                         data = new_data,
                         type = "ML", 
                         control = betareg.control(maxit = 1000, fstol = 1e-1))
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_c1 <- betareg(y ~ x + protocol + ba_plot,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c1[f,"a2"] <- coefficients(m2_b_c1)["x"]
        beta_resampling_c1[f,"comp"] <- coefficients(m2_b_c1)["ba_plot"]
        beta_resampling_c1[f,"AIC"] <- AIC(m2_b_c1)
        beta_resampling_c1[f,"zvalue"] <- as.data.frame(summary(m2_b_c1)$coefficients$mean)["x","z value"]
        beta_resampling_c1[f,"pr_z"] <- as.data.frame(summary(m2_b_c1)$coefficients$mean)["x","Pr(>|z|)"]
        
        beta_resampling_c1[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c1)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c1[f,k] <- coefficients(m2_b_c1)["(Intercept)"] + coefficients(m2_b_c1)[k]
        }
        
        
      } else {
        
        beta_resampling_c1[f,"a2"] <- coefficients(m1_b_c1)["x"]
        beta_resampling_c1[f,"comp"] <- coefficients(m1_b_c1)["ba_plot"]
        beta_resampling_c1[f,"AIC"] <- AIC(m1_b_c1)
        beta_resampling_c1[f,"zvalue"] <- as.data.frame(summary(m1_b_c1)$coefficients$mean)["x","z value"]
        beta_resampling_c1[f,"pr_z"] <- as.data.frame(summary(m1_b_c1)$coefficients$mean)["x","Pr(>|z|)"]
        beta_resampling_c1[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c1)["(Intercept)"]
        
      }
      
      
      ## completing storage file with weighted parameters
      beta_resampling_c1_w[f,"a2"] <- beta_resampling_c1[f,"a2"]
      beta_resampling_c1_w[f,"comp"] <- beta_resampling_c1[f,"comp"]
      beta_resampling_c1_w[f,"AIC"] <- beta_resampling_c1[f,"AIC"]
      beta_resampling_c1_w[f,"zvalue"] <- beta_resampling_c1[f,"zvalue"]
      beta_resampling_c1_w[f,"pr_z"] <- beta_resampling_c1[f,"pr_z"]
      
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% dplyr::summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_c1_w[f,paste0("protocol",g)] <- beta_resampling_c1[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      upper_range <- dim(beta_resampling_c1)[2]-8
      
      beta_resampling_c1[f, "a1"] <- apply(beta_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      beta_resampling_c1_w[f, "a1"] <- apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      # beta_dbh_c1[f,"dbh_10_w"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*10)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*10))
      # beta_dbh_c1[f,"dbh_15_w_a"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*5 + beta_resampling_c1_w[f,"a2"]*15)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*5 + beta_resampling_c1_w[f,"a2"]*15))
      # beta_dbh_c1[f,"dbh_15_w_b"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*15)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*15))
      # beta_dbh_c1[f,"dbh_20_w"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*20)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*20))
      # beta_dbh_c1[f,"dbh_30_w"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*30)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*30))
      # beta_dbh_c1[f,"dbh_40_w"] <- exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*40)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*20 + beta_resampling_c1_w[f,"a2"]*40))
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(apply(beta_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_c1[f,"comp"]*ba_plot + beta_resampling_c1[f,"a2"]*x)/(1+exp(apply(beta_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_c1[f,"comp"]*ba_plot + beta_resampling_c1[f,"a2"]*x)),
                                          y_pred_b = exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*ba_plot + beta_resampling_c1_w[f,"a2"]*x)/(1+exp(apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c1_w[f,"comp"]*ba_plot + beta_resampling_c1_w[f,"a2"]*x)))
      
      
      ## computing RMSE
      beta_resampling_c1[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_c1_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_c1[f,"weighted"] <- "no" 
      beta_resampling_c1_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_c1 <- bind_rows(beta_resampling_c1, beta_resampling_c1_w)
  # write.csv(beta_dbh_c1, file =  paste0("output/output_beta_c1_dbh_", beta_dbh_c1[1,1], ".csv"))
  
  return(beta_resampling_c1)
  
}




## Beta linear model - Competition: ba larger - Resampling
# using logit link for the conditional mean

mod_beta_resampling_c2 <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_c2, beta_resampling_c2_w, beta_dbh_c2, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      m1_b_c2 <- betareg(y ~ x + ba_larger,
                         data = new_data,
                         type = "ML", 
                         control = betareg.control(maxit = 1000, fstol = 1e-1))
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_c2 <- betareg(y ~ x + protocol + ba_larger,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c2[f,"a2"] <- coefficients(m2_b_c2)["x"]
        beta_resampling_c2[f,"comp"] <- coefficients(m2_b_c2)["ba_larger"]
        beta_resampling_c2[f,"AIC"] <- AIC(m2_b_c2)
        beta_resampling_c2[f,"zvalue"] <- as.data.frame(summary(m2_b_c2)$coefficients$mean)["x","z value"]
        beta_resampling_c2[f,"pr_z"] <- as.data.frame(summary(m2_b_c2)$coefficients$mean)["x","Pr(>|z|)"]
        
        
        beta_resampling_c2[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c2)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c2[f,k] <- coefficients(m2_b_c2)["(Intercept)"] + coefficients(m2_b_c2)[k]
        }
        
        
      } else {
        
        beta_resampling_c2[f,"a2"] <- coefficients(m1_b_c2)["x"]
        beta_resampling_c2[f,"comp"] <- coefficients(m1_b_c2)["ba_larger"]
        beta_resampling_c2[f,"AIC"] <- AIC(m1_b_c2)
        beta_resampling_c2[f,"zvalue"] <- as.data.frame(summary(m1_b_c1)$coefficients$mean)["x","z value"]
        beta_resampling_c2[f,"pr_z"] <- as.data.frame(summary(m1_b_c1)$coefficients$mean)["x","Pr(>|z|)"]
        beta_resampling_c2[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c2)["(Intercept)"]
        
      }
      
      
      ## completing storage file with weighted parameters
      beta_resampling_c2_w[f,"a2"] <- beta_resampling_c2[f,"a2"]
      beta_resampling_c2_w[f,"comp"] <- beta_resampling_c2[f,"comp"]
      beta_resampling_c2_w[f,"AIC"] <- beta_resampling_c2[f,"AIC"]
      beta_resampling_c2_w[f,"zvalue"] <- beta_resampling_c2[f,"zvalue"]
      beta_resampling_c2_w[f,"pr_z"] <- beta_resampling_c2[f,"pr_z"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% dplyr::summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_c2_w[f,paste0("protocol",g)] <- beta_resampling_c2[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      upper_range <- dim(beta_resampling_c2)[2]-8
      
      beta_resampling_c2[f, "a1"] <- apply(beta_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE)
      beta_resampling_c2_w[f, "a1"] <- apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      
      # beta_dbh_c2[f,"dbh_10_w"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*10)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*10))
      # beta_dbh_c2[f,"dbh_15_w_a"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*5 + beta_resampling_c2_w[f,"a2"]*15)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*5 + beta_resampling_c2_w[f,"a2"]*15))
      # beta_dbh_c2[f,"dbh_15_w_b"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*15)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*15))
      # beta_dbh_c2[f,"dbh_20_w"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*20)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*20))
      # beta_dbh_c2[f,"dbh_30_w"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*30)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*30))
      # beta_dbh_c2[f,"dbh_40_w"] <- exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*40)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*20 + beta_resampling_c2_w[f,"a2"]*40))
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(apply(beta_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_c2[f,"comp"]*ba_larger + beta_resampling_c2[f,"a2"]*x)/(1+exp(apply(beta_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE) + beta_resampling_c2[f,"comp"]*ba_larger + beta_resampling_c2[f,"a2"]*x)),
                                          y_pred_b = exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*ba_larger + beta_resampling_c2_w[f,"a2"]*x)/(1+exp(apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE) + beta_resampling_c2_w[f,"comp"]*ba_larger + beta_resampling_c2_w[f,"a2"]*x)))
      
      
      ## computing RMSE
      beta_resampling_c2[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_c2_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_c2[f,"weighted"] <- "no" 
      beta_resampling_c2_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_c2 <- bind_rows(beta_resampling_c2, beta_resampling_c2_w)
  # write.csv(beta_dbh_c2, file =  paste0("output/output_beta_c2_dbh_", beta_dbh_c2[1,1], ".csv"))
  
  return(beta_resampling_c2)
  
}







mod_beta_resampling_mean_c1 <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_c1, beta_resampling_c1_w, beta_dbh_c1, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      upper_range <- dim(beta_resampling_c1)[2]-4
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_c1 <- betareg(y ~ protocol + ba_plot,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c1[f,"comp"] <- coefficients(m2_b_c1)["ba_plot"]
        beta_resampling_c1[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c1)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c1[f,k] <- coefficients(m2_b_c1)["(Intercept)"] + coefficients(m2_b_c1)[k]
        }
        
        beta_resampling_c1[f,"a1"] <- apply(beta_resampling_c1[f,c(2:upper_range)], 1, mean, na.rm = TRUE) 
        
        
      } else {
        
        
        m1_b_c1 <- betareg(y ~ ba_plot,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c1[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c1)["(Intercept)"]
        beta_resampling_c1[f,"a1"] <- coefficients(m1_b_c1)["(Intercept)"]
        
        
      }
      
      
      ## completing storage file with weighted parameters
      beta_resampling_c1_w[f,"comp"] <- beta_resampling_c1[f,"comp"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% dplyr::summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_c1_w[f,paste0("protocol",g)] <- beta_resampling_c1[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      beta_resampling_c1_w[f, "a1"] <- apply(beta_resampling_c1_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(beta_resampling_c1[f,"a1"] + beta_resampling_c1[f,"comp"]*ba_plot)/(1+exp(beta_resampling_c1[f,"a1"] + beta_resampling_c1[f,"comp"]*ba_plot)),
                                          y_pred_b = exp(beta_resampling_c1_w[f,"a1"] + beta_resampling_c1_w[f,"comp"]*ba_plot)/(1+exp(beta_resampling_c1_w[f,"a1"] + beta_resampling_c1_w[f,"comp"]*ba_plot)))
      
      
      ## computing RMSE
      beta_resampling_c1[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_c1_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_c1[f,"weighted"] <- "no" 
      beta_resampling_c1_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_c1_mean <- bind_rows(beta_resampling_c1, beta_resampling_c1_w)
  return(beta_resampling_c1_mean)
  
}



mod_beta_resampling_mean_c2 <- function(ranged_data, nb_datasets_all, sample_size, beta_resampling_c2, beta_resampling_c2_w, beta_dbh_c2, n_repetition) {
  
  nrep = n_repetition
  
  for (f in 1:nrep) {
    
    ## running the models
    tryCatch({  
      
      ## computing new dataset based on defined sampling protocol
      new_data <- sampling_protocol(ranged_data, nb_datasets_all, sample_size)
      
      ## computing test dataset 
      test_data <- testing_data(ranged_data, new_data, sample_size)
      
      upper_range <- dim(beta_resampling_c2)[2]-4
      
      if(length(unique(new_data$protocol)) > 1){
        
        m2_b_c2 <- betareg(y ~ protocol + ba_larger,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c2[f,"comp"] <- coefficients(m2_b_c2)["ba_larger"]
        beta_resampling_c2[f, paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_b_c2)["(Intercept)"]
        
        for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
          beta_resampling_c2[f,k] <- coefficients(m2_b_c2)["(Intercept)"] + coefficients(m2_b_c2)[k]
        }
        
        beta_resampling_c2[f,"a1"] <- apply(beta_resampling_c2[f,c(2:upper_range)], 1, mean, na.rm = TRUE) 
        
        
      } else {
        
        
        m1_b_c2 <- betareg(y ~ ba_larger,
                           data = new_data,
                           type = "ML", 
                           control = betareg.control(maxit = 1000, fstol = 1e-1))
        
        beta_resampling_c2[f,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_b_c2)["(Intercept)"]
        beta_resampling_c2[f,"a1"] <- coefficients(m1_b_c2)["(Intercept)"]
        
        
      }
      
      
      ## completing storage file with weighted parameters
      beta_resampling_c2_w[f,"comp"] <- beta_resampling_c2[f,"comp"]
      
      ## extracting number of observations per protocol to attribute weights
      n_tot <- dim(new_data)[1]
      nobs <- new_data %>% dplyr::group_by(protocol) %>% dplyr::summarise(n = n()/n_tot) %>% ungroup()
      
      for (g in levels(new_data$protocol)) {
        beta_resampling_c2_w[f,paste0("protocol",g)] <- beta_resampling_c2[f,paste0("protocol",g)] * (nobs %>% filter(protocol== g)) [1,"n"] 
      }
      
      beta_resampling_c2_w[f, "a1"] <- apply(beta_resampling_c2_w[f,c(2:upper_range)], 1, sum, na.rm = TRUE)
      
      ## predicting y on subsampled data
      test_data_b <- test_data %>% mutate(y_pred_a = exp(beta_resampling_c2[f,"a1"] + beta_resampling_c2[f,"comp"]*ba_larger)/(1+exp(beta_resampling_c2[f,"a1"] + beta_resampling_c2[f,"comp"]*ba_larger)),
                                          y_pred_b = exp(beta_resampling_c2_w[f,"a1"] + beta_resampling_c2_w[f,"comp"]*ba_larger)/(1+exp(beta_resampling_c2_w[f,"a1"] + beta_resampling_c2_w[f,"comp"]*ba_larger)))
      
      
      ## computing RMSE
      beta_resampling_c2[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_a)
      beta_resampling_c2_w[f,"RMSE"] <- rmse(test_data_b$y, test_data_b$y_pred_b)
      
      
      ## combining both files into one for a single return
      beta_resampling_c2[f,"weighted"] <- "no" 
      beta_resampling_c2_w[f,"weighted"] <- "yes"
      
    }
    ,
    
    error = function(e) {
      print("error")
    }) 
    
  }
  
  beta_resampling_c2_mean <- bind_rows(beta_resampling_c2, beta_resampling_c2_w)
  return(beta_resampling_c2_mean)
  
}


