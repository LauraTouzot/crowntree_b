get_height_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from height models
  dd <- list.files(path = "output/", pattern = "asympt_height_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  height_resampling_asympt <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

  dd <- list.files(path = "output/", pattern = "power_height_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  height_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  power_summary <- height_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  # no less than 100 obs for any species in the models using the power relationship
  
  
  asymptot_summary <- height_resampling_asympt %>% dplyr::filter(weighted == "no") %>%
                                                   dplyr::group_by(species) %>%
                                                   dplyr::summarise(nobs = n()) 
  
  # computing height species list with species for which at least 100 model results were available
  # for both tested allometric relationships
  
  height_sp_list <- asymptot_summary[asymptot_summary$nobs >= 100,]$species
  
  height_resampling_power <- height_resampling_power[height_resampling_power$species %in% height_sp_list,]
  height_resampling_asympt <- height_resampling_asympt[height_resampling_asympt$species %in% height_sp_list,]
  
  
  height_power <- height_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                              dplyr::group_by(species) %>%
                                              dplyr::filter(!is.na(a1)) %>%
                                              dplyr::slice_sample(n = 100) %>%
                                              dplyr::mutate(ID_b = X + 300) %>%
                                              dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  # total AIC height power: 154 434 902
  
  height_power_b <- height_resampling_power %>% dplyr::filter(weighted == "yes") %>%
                                                dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  height_power_b <- height_power_b[height_power_b$ID_tofilter %in% height_power$ID_species,]
  
  # total RMSE weighted: 62 195.86
  # total RMSE not weighted: 63 387.97
  
  height_asympt <- height_resampling_asympt %>% dplyr::filter(weighted == "yes") %>%
                                                dplyr::group_by(species) %>%
                                                dplyr::filter(!is.na(b1)) %>% 
                                                dplyr::slice_sample(n = 100)
  
  # height_asympt <- height_resampling_asympt %>% dplyr::filter(weighted == "no") %>%
  #                                               dplyr::group_by(species) %>%
  #                                               dplyr::filter(!is.na(b1)) %>% 
  #                                               dplyr::slice_sample(n = 100) %>%
  #                                               dplyr::mutate(ID_b = X + 300) %>%
  #                                               dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  # 
  # # total AIC height asymptot: 160 844 460
  # 
  # height_asympt_b <- height_resampling_asympt %>% dplyr::filter(weighted == "yes") %>%
  #                                                 dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  # height_asympt_b <- height_asympt_b[height_asympt_b$ID_tofilter %in% height_asympt$ID_species,]
  
  # total RMSE weighted: 62 675.89
  # total RMSE not weighted: 63 958.59
  
  height_parameters <- height_asympt %>% dplyr::select(species, b1, b2, b3) %>%
                                         dplyr::group_by(species) %>%
                                         dplyr::mutate(id = c(1:100)) %>%
                                         dplyr::ungroup()
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea")
  height_parameters <- height_parameters[!height_parameters$species %in% to_remove,]
  
  
  write.csv(file = "output/new_height_parameters.csv", height_parameters)
  return(height_parameters)
  
  # height_asympt_results_b <- height_asympt_b %>% dplyr::group_by(species) %>%
  #                                                dplyr::summarise(mean_b1 = mean(b1),
  #                                                                 sd_b1 = sd(b1),
  #                                                                 min_b1 = quantile(b1, probs = 0.025),
  #                                                                 max_b1 = quantile(b1, probs = 0.975),
  #                                                                 se_b1 = se(b1),
  #                                                                 mean_b2 = mean(b2),
  #                                                                 sd_b2 = sd(b2),
  #                                                                 min_b2 = quantile(b2, probs = 0.025),
  #                                                                 max_b2 = quantile(b2, probs = 0.975),
  #                                                                 se_b2 = se(b2),
  #                                                                 mean_b3 = mean(b3),
  #                                                                 sd_b3 = sd(b3),
  #                                                                 min_b3 = quantile(b3, probs = 0.025),
  #                                                                 max_b3 = quantile(b3, probs = 0.975),
  #                                                                 se_b3 = se(b3))
  # 
  # height_asympt_results_b <- height_asympt_results_b %>% dplyr::mutate(cv_b1 = abs(sd_b1)/abs(mean_b1),
  #                                                                      cv_b2 = abs(sd_b2)/abs(mean_b2),
  #                                                                      cv_b3 = abs(sd_b3)/abs(mean_b3))
  # 
  # par(mfrow = c(1,3))
  # 
  # plot(density(height_asympt_results_b$cv_b1), main = "b1", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(height_asympt_results_b$cv_b2), main = "b2", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(height_asympt_results_b$cv_b3), main = "b3", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  
  
  # height_asympt_results_c <- height_asympt_results_b %>% dplyr::filter(cv_b1 > 0,
  #                                                                      cv_b1 <= 0.5,
  #                                                                      cv_b2 > 0,
  #                                                                      cv_b2 <= 0.5,
  #                                                                      cv_b3 > 0,
  #                                                                      cv_b3 <= 0.5) # 128 sp left out of 156
                                              
  
  # write.csv(file = "output/height_asympt_parameters.csv", height_asympt_results_c)
  # 
  # write.csv(file = "output/height_asympt_parameters.csv", height_asympt_results_b)
  # return(height_asympt_results_b)
  
}


get_height_max <- function(height_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))

  ### 100 resampling to estimate maximal height +/- se and sd
  
  for (i in 1:dim(height_parameters)[1]) {

        n_repetition = 100

        height_max <- as.data.frame(matrix(nrow = n_repetition, ncol = 2))
        height_max[,1] <- rep(height_parameters[i,"species"], n_repetition)
        names(height_max) <- c("species", "h_max")

        mean_b1 = height_parameters$mean_b1[i]
        sd_b1 = height_parameters$sd_b1[i]


        for (j in 1:n_repetition) {
          height_max[j,"h_max"] = rtruncnorm(1, a = mean_b1 - 2*sd_b1, b = mean_b1 + 2*sd_b1, mean = mean_b1, sd = sd_b1)  }

        write.csv(height_max, file =  paste0("output/height_max_", height_parameters[i,"species"], ".csv"))

  }
  
  
  dd <- list.files(path = "output/", pattern = "height_max_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)

  height_max_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

  h_max_summary <- height_max_allsp %>% dplyr::group_by(species) %>%
                                        dplyr::summarise(mean_hmax = mean(h_max),
                                                         sd_hmax = sd(h_max),
                                                         se_hmax = se(h_max))
  
  write.csv(h_max_summary, file =  "output/height_max_summary.csv")
  return(height_max_summary)
  
}




get_height_dbh <- function(height_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate maximal height +/- se and sd
  
  for (i in 1:dim(height_parameters)[1]) {
    
    n_repetition = 100
    
    height_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 3))
    height_est[,1] <- rep(height_parameters[i,"species"], n_repetition)
    names(height_est) <- c("species", "dbh_15", "dbh_30")
    
    mean_b1 = height_parameters$mean_b1[i]
    sd_b1 = height_parameters$sd_b1[i]
    
    mean_b2 = height_parameters$mean_b2[i]
    sd_b2 = height_parameters$sd_b2[i]
    
    mean_b3 = height_parameters$mean_b3[i]
    sd_b3 = height_parameters$sd_b3[i]
    
    
    for (j in 1:n_repetition) {
      
      b1 = rtruncnorm(1, a = mean_b1 - 2*sd_b1, b = mean_b1 + 2*sd_b1, mean = mean_b1, sd = sd_b1)  
      b2 = rtruncnorm(1, a = mean_b2 - 2*sd_b2, b = mean_b2 + 2*sd_b2, mean = mean_b2, sd = sd_b2)  
      b3 = rtruncnorm(1, a = mean_b3 - 2*sd_b3, b = mean_b3 + 2*sd_b3, mean = mean_b3, sd = sd_b3)  
      
      height_est[j,"dbh_15"] = 1.3 + b1 * (1 - exp( - b2 * 15)) ^ b3
      height_est[j,"dbh_30"] = 1.3 + b1 * (1 - exp( - b2 * 30)) ^ b3
      
    }
    
    write.csv(height_est, file =  paste0("output/height_est_", height_parameters[i,"species"], ".csv"))
    
  }
  
  
  dd <- list.files(path = "output/", pattern = "height_est_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  height_dbh_est <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  height_dbh_est <- na.omit(height_dbh_est)
  
  h_dbh_summary <- height_dbh_est %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean_h_15 = mean(dbh_15),
                                                       sd_h_15 = sd(dbh_15),
                                                       se_h_15 = se(dbh_15),
                                                       mean_h_30 = mean(dbh_30),
                                                       sd_h_30 = sd(dbh_30),
                                                       se_h_30 = se(dbh_30))
  
  write.csv(h_dbh_summary, file =  "output/height_dbh_summary.csv")
  return(h_dbh_summary)
  
}


## Checking values obtained for the maximum height of tree species

asymptot_validation <- function(height_data) {
  
  data <- height_data
  parameters <- read.csv(file = "output/height_asympt_parameters.csv")
  h_comparisons <- as.data.frame(matrix(nrow = dim(parameters)[1], ncol = 6))
  colnames(h_comparisons) <- c("sp", "h_max_obs", "h_max_dbh", "h_99_obs", "h_99_est", "h_max_est")
  
  for (i in 1:dim(parameters)[1]) {
    
    h_comparisons[i,"sp"] <- parameters[i,"species"]
    
    df <- data[data$sp_name %in% parameters[i,"species"],]
    df <- arrange(df, df$x)
    xbis <- c(min(df$x):max(df$x))
    ymean <- 1.3 + parameters[i,"mean_b1"] * (1 - exp( - parameters[i,"mean_b2"] * xbis)) ^ parameters[i,"mean_b3"]
    ymin <- 1.3 + (parameters[i,"mean_b1"]-parameters[i,"sd_b1"]) * (1 - exp( - (parameters[i,"mean_b2"]-parameters[i,"sd_b2"]) * xbis)) ^ (parameters[i,"mean_b3"]-parameters[i,"sd_b3"])
    ymax <- 1.3 + (parameters[i,"mean_b1"]+parameters[i,"sd_b1"]) * (1 - exp( - (parameters[i,"mean_b2"]+parameters[i,"sd_b2"]) * xbis)) ^ (parameters[i,"mean_b3"]+parameters[i,"sd_b3"])
    df_bis <- as.data.frame(cbind(xbis, ymean, ymin, ymax))
    
    q_99 <- quantile(df$x, probs = 0.99)
    df <- df %>% dplyr::mutate(abs_q99 = abs(q_99-x))
    obs_q99 <- df %>% dplyr::filter(abs_q99 == min(abs_q99))
    
    h_comparisons[i,"h_99_obs"] <- mean(obs_q99$y)
    h_comparisons[i,"h_99_est"] <- 1.3 + parameters[i,"mean_b1"] * (1 - exp( - parameters[i,"mean_b2"] * q_99)) ^ parameters[i,"mean_b3"]
    h_comparisons[i,"h_max_obs"] <- max(df$y)
    h_comparisons[i,"h_max_dbh"] <- df[dim(df)[1],"y"]
    h_comparisons[i,"h_max_est"] <- 1.3 + parameters[i,"mean_b1"] * (1 - exp( - parameters[i,"mean_b2"] * df[dim(df)[1],"y"])) ^ parameters[i,"mean_b3"]
    
    h <- ggplot(df, aes(x, y)) + 
      labs(
        title = parameters[i,"species"],
        x = "diameter at breast height (cm)", 
        y = "tree height (m)") +
      geom_point(size = 0.1) +
      geom_ribbon(data = df_bis, aes(x = xbis, y = ymean, ymin = ymin, ymax = ymax), fill = "darkseagreen1", alpha = 0.5) +
      geom_line(data = df_bis, mapping = aes(xbis, ymean), colour = "darkseagreen4") +
      theme_classic()
    
    ggsave(path = "figures/asymptot/",
           filename = paste("check_",parameters[i,"species"],".pdf"),
           width = 12, height = 8, units = "cm")
  }
  dev.off()
  
  # correct all files based on the selection performed here
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea")
  
  h_comparisons <- h_comparisons[!h_comparisons$sp %in% to_remove,]
  write.csv(file = "output/asymptot_check.csv", h_comparisons)
  
  parameters <- parameters[!parameters$species %in% to_remove,]
  write.csv(file = "output/height_asympt_parameters.csv", parameters)
  
  h_max <- read.csv(file = "output/height_max_summary.csv")
  h_max <- h_max[!h_max$species %in% to_remove,]
  write.csv(file = "output/height_max_summary.csv", h_max)
  
  h_dbh <- read.csv(file =  "output/height_dbh_summary.csv")
  h_dbh <- h_dbh[!h_dbh$species %in% to_remove,]
  write.csv(file = "output/height_dbh_summary.csv", h_dbh)
  
  # dd <- read.csv(file = "output/asymptot_check.csv")
  # plot(dd$h_99_obs, dd$h_max_est, 
  #      xlim = c(0,60), ylim = c(0,60),
  #      type = "p", pch = 16, las = 1, 
  #      xlab = "maximal tree height observed (m) - (99% quantile)",
  #      ylab = "maximal tree height estimated (m)")
  # lines(c(1:60), c(1:60), col = "darkgreen", lwd = 3)
  
}

  
  
  
  
  
  