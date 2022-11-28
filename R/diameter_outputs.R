get_diameter_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from diameter models
  dd <- list.files(path = "output/", pattern = "linear_diameter_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  linear_summary <- diameter_resampling_linear %>% dplyr::filter(weighted == "no") %>%
                                                   dplyr::group_by(species) %>%
                                                   dplyr::summarise(nobs = n()) 
  
  
  
  dd <- list.files(path = "output/", pattern = "power_diameter_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  power_summary <- diameter_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                                 dplyr::group_by(species) %>%
                                                 dplyr::summarise(nobs = n()) 
  
  
  diameter_sp_list <- power_summary[power_summary$nobs >= 100,]$species
  
  diameter_resampling_power <- diameter_resampling_power[diameter_resampling_power$species %in% diameter_sp_list,]
  diameter_resampling_linear <- diameter_resampling_linear[diameter_resampling_linear$species %in% diameter_sp_list,]
  
  # diameter_power <- diameter_resampling_power %>% dplyr::filter(weighted == "yes") %>%
  #                                                 dplyr::group_by(species) %>%
  #                                                 dplyr::filter(!is.na(a1)) %>%
  #                                                 dplyr::slice_sample(n = 100) %>%
  #                                                 dplyr::mutate(ID_b = X + 300) %>%
  #                                                 dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  diameter_power <- diameter_resampling_power %>% dplyr::filter(weighted == "yes") %>%
                                                  dplyr::group_by(species) %>%
                                                  dplyr::filter(!is.na(a1)) %>%
                                                  dplyr::slice_sample(n = 100)
  
  # # total AIC diameter power: 7 868 280
  # 
  # diameter_power_b <- diameter_resampling_power %>% dplyr::filter(weighted == "yes") %>%
  #                                                   dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  # diameter_power_b <- diameter_power_b[diameter_power_b$ID_tofilter %in% diameter_power$ID_species,]
  # 
  # # total RMSE weighted: 10 506.11
  # # total RMSE not weighted: 15 229.31
  # 
  # diameter_linear <- diameter_resampling_linear %>% dplyr::filter(weighted == "no") %>%
  #                                                   dplyr::group_by(species) %>%
  #                                                   dplyr::filter(!is.na(intercept)) %>% 
  #                                                   dplyr::slice_sample(n = 100) %>%
  #                                                   dplyr::mutate(ID_b = X + 300) %>%
  #                                                   dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  # 
  # # total AIC diameter linear: 7 860 758
  # 
  # diameter_linear_b <- diameter_resampling_linear %>% dplyr::filter(weighted == "yes") %>%
  #                                                     dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  # diameter_linear_b <- diameter_linear_b[diameter_linear_b$ID_tofilter %in% diameter_linear$ID_species,]
  # 
  # # total RMSE weighted: 15 115.71
  # # total RMSE not weighted: 15 227.49
  
  diameter_nocomp_parameters <- diameter_power %>% dplyr::select(species, a1, a2) %>%
                                                   dplyr::group_by(species) %>%
                                                   dplyr::mutate(id = c(1:100)) %>%
                                                   dplyr::ungroup()
  
  write.csv(file = "output/new_diameter_nocomp_parameters_parameters.csv", diameter_nocomp_parameters)
  return(diameter_nocomp_parameters)
  
  # diameter_power_results_b <- diameter_power_b %>% dplyr::group_by(species) %>%
  #                                                  dplyr::summarise(mean_a1 = mean(a1),
  #                                                                    sd_a1 = sd(a1),
  #                                                                    min_a1 = quantile(a1, probs = 0.025),
  #                                                                    max_a1 = quantile(a1, probs = 0.975),
  #                                                                    se_a1 = se(a1),
  #                                                                    mean_a2 = mean(a2),
  #                                                                    sd_a2 = sd(a2),
  #                                                                    min_a2 = quantile(a2, probs = 0.025),
  #                                                                    max_a2 = quantile(a2, probs = 0.975),
  #                                                                    se_a2 = se(a2))
  
  # diameter_power_results_b <- diameter_power_results_b %>% dplyr::mutate(cv_a1 = abs(sd_a1)/abs(mean_a1),
  #                                                                  cv_a2 = abs(sd_a2)/abs(mean_a2))
  # 
  # par(mfrow = c(1,3))
  # 
  # plot(density(diameter_power_results_b$cv_a1), main = "a1", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(diameter_power_results_b$cv_a2), main = "a2", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # diameter_power_results_c <- diameter_power_results_b %>% dplyr::filter(cv_a1 > 0,
  #                                                                  cv_a1 <= 0.5,
  #                                                                  cv_a2 > 0,
  #                                                                  cv_a2 <= 0.5) # 67 sp left out of 79
  # 
  # 
  # write.csv(file = "output/diameter_power_parameters.csv", diameter_power_results_c)
  
  write.csv(file = "output/diameter_power_parameters.csv", diameter_power_results_b)
  return(diameter_power_results_b)
  
}
  

get_diameter_estimates <- function(diameter_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate diameter +/- se and sd at dbh 15 and 30cm
  
  for (i in 1:dim(diameter_parameters)[1]) {
    
    n_repetition = 100
    
    diameter_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 3))
    diameter_dbh[,1] <- rep(diameter_parameters[i,"species"], n_repetition)
    names(diameter_dbh) <- c("species", "diameter_15", "diameter_30")
    
    mean_a1 = diameter_parameters$mean_a1[i]
    sd_a1 = diameter_parameters$sd_a1[i]
    
    mean_a2 = diameter_parameters$mean_a2[i]
    sd_a2 = diameter_parameters$sd_a2[i]
    
    
    for (j in 1:n_repetition) {
      
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      
      diameter_dbh[j,"diameter_15"] = a1 * 15^a2
      diameter_dbh[j,"diameter_30"] = a1 * 30^a2 
      
    }
    
    write.csv(diameter_dbh, file =  paste0("output/diameter_estimates_", diameter_parameters[i,"species"], ".csv"))
    
  }
  
  
  dd <- list.files(path = "output/", pattern = "diameter_estimates_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  diameter_dbh_summary <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                                 dplyr::summarise(mean_diameter_15 = mean(diameter_15),
                                                                  sd_diameter_15 = sd(diameter_15),
                                                                  se_diameter_15 = se(diameter_15),
                                                                  mean_diameter_30 = mean(diameter_30),
                                                                  sd_diameter_30 = sd(diameter_30),
                                                                  se_diameter_30 = se(diameter_30))
  
  write.csv(diameter_dbh_summary, file =  "output/diameter_dbh_summary.csv")
  
}




## Checking values obtained for the diameter of tree species

diameter_validation <- function(diameter_data) {
  
  data <- diameter_data
  parameters <- read.csv(file = "output/diameter_power_parameters.csv")
  d_comparisons <- as.data.frame(matrix(nrow = dim(parameters)[1], ncol = 5))
  colnames(d_comparisons) <- c("sp", "d_15_obs", "d_15_est", "d_30_obs", "d_30_est")
  
  for (i in 1:dim(parameters)[1]) {
    
    d_comparisons[i,"sp"] <- parameters[i,"species"]
    
    df <- data[data$sp_name %in% parameters[i,"species"],]
    df <- arrange(df, df$x)
    xbis <- c(min(df$x):max(df$x))
    ymean <- parameters[i,"mean_a1"] * xbis ^ parameters[i,"mean_a2"]
    ymin <- (parameters[i,"mean_a1"]-parameters[i,"sd_a1"]) * xbis ^ (parameters[i,"mean_a2"]-parameters[i,"sd_a2"])
    ymax <- (parameters[i,"mean_a1"]+parameters[i,"sd_a1"]) * xbis ^ (parameters[i,"mean_a2"]+parameters[i,"sd_a2"])
    df_bis <- as.data.frame(cbind(xbis, ymean, ymin, ymax))
    
    df <- df %>% dplyr::mutate(abs_x15 = abs(15-x), abs_x30 = abs(30-x))
    obs_15 <- df %>% dplyr::filter(abs_x15 == min(abs_x15))
    obs_30 <- df %>% dplyr::filter(abs_x30 == min(abs_x30))
    
    d_comparisons[i,"d_15_obs"] <- mean(obs_15$y)
    d_comparisons[i,"d_30_obs"] <- mean(obs_30$y)
    d_comparisons[i,"d_15_est"] <- parameters[i,"mean_a1"] * 15 ^ parameters[i,"mean_a2"]
    d_comparisons[i,"d_30_est"] <- parameters[i,"mean_a1"] * 30 ^ parameters[i,"mean_a2"]
    
    h <- ggplot(df, aes(x, y)) + 
      labs(
        title = parameters[i,"species"],
        x = "diameter at breast height (cm)", 
        y = "crown diameter (m)") +
      geom_point(size = 0.1) +
      geom_ribbon(data = df_bis, aes(x = xbis, y = ymean, ymin = ymin, ymax = ymax), fill = "darkseagreen1", alpha = 0.5) +
      geom_line(data = df_bis, mapping = aes(xbis, ymean), colour = "darkseagreen4") +
      theme_classic()
    
    ggsave(path = "figures/diameter/",
           filename = paste("check_",parameters[i,"species"],".pdf"),
           width = 12, height = 8, units = "cm")
  }
  dev.off()
  write.csv(file = "output/diameter_comparisons.csv", d_comparisons)
  
  # plot(d_comparisons$d_15_obs, d_comparisons$d_15_est, 
  #      pch = 16, las = 1, type = "p",
  #      xlab = "observed crown diameter (m)", 
  #      ylab = "estimated crown diameter (m)",
  #      xlim = c(1,12), ylim = c(1,12),
  #      col = "darkseagreen2")
  # 
  # points(d_comparisons$d_30_obs, d_comparisons$d_30_est,
  #        pch = 16, col = "darkseagreen4")
  # lines(c(0,12), c(0,12), lwd = 3, col = "darkseagreen4")
  
}
  
  





get_diameter_c1_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from diameter models (c1)
  dd <- list.files(path = "output/", pattern = "power_diameter_c1_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  diameter_summary <- diameter_resampling_power_c1 %>% dplyr::filter(weighted == "no") %>%
                                                       dplyr::group_by(species) %>%
                                                       dplyr::summarise(nobs = n())
  
  diameter_sp_list <- diameter_summary[diameter_summary$nobs >= 100,]$species
  diameter_resampling_power_c1 <- diameter_resampling_power_c1[diameter_resampling_power_c1$species %in% diameter_sp_list,]
  
  diameter_power <- diameter_resampling_power_c1 %>% dplyr::filter(weighted == "yes") %>%
                                                     dplyr::group_by(species) %>%
                                                     dplyr::filter(!is.na(a1)) %>%
                                                     dplyr::slice_sample(n = 100)
                                                  
  # diameter_power <- diameter_resampling_power_c1 %>% dplyr::filter(weighted == "no") %>%
  #                                                    dplyr::group_by(species) %>%
  #                                                    dplyr::filter(!is.na(a1)) %>%
  #                                                    dplyr::slice_sample(n = 100) %>%
  #                                                    dplyr::mutate(ID_b = X + 300) %>%
  #                                                    dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  # 
  # 
  # diameter_power_b <- diameter_resampling_power_c1 %>% dplyr::filter(weighted == "yes") %>%
  #                                                      dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  # diameter_power_b <- diameter_power_b[diameter_power_b$ID_tofilter %in% diameter_power$ID_species,]
  
  diameter_c1_parameters <- diameter_power %>% dplyr::select(species, a1, a2, comp) %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::mutate(id = c(1:100)) %>%
                                               dplyr::ungroup()

  
  write.csv(file = "output/new_diameter_c1_parameters.csv", diameter_c1_parameters)
  return(diameter_c1_parameters)
  
  
  # diameter_power_results_b <- diameter_power_b %>% dplyr::group_by(species) %>%
  #                                                  dplyr::summarise(mean_a1 = mean(a1),
  #                                                                    sd_a1 = sd(a1),
  #                                                                    min_a1 = quantile(a1, probs = 0.025),
  #                                                                    max_a1 = quantile(a1, probs = 0.975),
  #                                                                    se_a1 = se(a1),
  #                                                                    mean_a2 = mean(a2),
  #                                                                    sd_a2 = sd(a2),
  #                                                                    min_a2 = quantile(a2, probs = 0.025),
  #                                                                    max_a2 = quantile(a2, probs = 0.975),
  #                                                                    se_a2 = se(a2),
  #                                                                    mean_comp = mean(comp),
  #                                                                    sd_comp = sd(comp),
  #                                                                    min_comp = quantile(comp, probs = 0.025),
  #                                                                    max_comp = quantile(comp, probs = 0.975),
  #                                                                    se_comp = se(comp))
  
  # diameter_power_results_b <- diameter_power_results_b %>% dplyr::mutate(cv_a1 = abs(sd_a1)/abs(mean_a1),
  #                                                                  cv_a2 = abs(sd_a2)/abs(mean_a2),
  #                                                                  cv_comp = abs(sd_comp)/abs(mean_comp))
  # 
  # par(mfrow = c(1,3))
  # 
  # plot(density(diameter_power_results_b$cv_a1), main = "a1", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(diameter_power_results_b$cv_a2), main = "a2", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(diameter_power_results_b$cv_comp), main = "ba plot", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # 
  # 
  # diameter_power_results_c <- diameter_power_results_b %>% dplyr::filter(cv_a1 > 0,
  #                                                                  cv_a1 <= 1,
  #                                                                  cv_a2 > 0,
  #                                                                  cv_a2 <= 1,
  #                                                                  cv_comp > 0,
  #                                                                  cv_comp <= 1) # 26 sp left out of 62
  # 
  # 
  # write.csv(file = "output/diameter_power_parameters_c1.csv", diameter_power_results_b)
  # return(diameter_power_results_b)
  
  
}

get_diameter_c1_estimates <- function(diameter_c1_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate diameter +/- se and sd at dbh 30cm
  
  for (i in 1:dim(diameter_c1_parameters)[1]) {
    
    n_repetition = 100
    
    diameter_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5))
    diameter_dbh[,1] <- rep(diameter_c1_parameters[i,"species"], n_repetition)
    names(diameter_dbh) <- c("species", "dbh_15_ba_5", "dbh_15_ba_20", "dbh_30_ba_5", "dbh_30_ba_20")
    
    mean_a1 = diameter_c1_parameters$mean_a1[i]
    sd_a1 = diameter_c1_parameters$sd_a1[i]
    
    mean_a2 = diameter_c1_parameters$mean_a2[i]
    sd_a2 = diameter_c1_parameters$sd_a2[i]
    
    mean_comp = diameter_c1_parameters$mean_comp[i]
    sd_comp = diameter_c1_parameters$sd_comp[i]
    
    for (j in 1:n_repetition) {
      
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      diameter_dbh[j,"dbh_15_ba_5"] = (a1 + comp * 5) * (15^a2) 
      diameter_dbh[j,"dbh_15_ba_20"] = (a1 + comp * 20) * (15^a2) 
      diameter_dbh[j,"dbh_30_ba_5"] = (a1 + comp * 5) * (30^a2) 
      diameter_dbh[j,"dbh_30_ba_20"] = (a1 + comp * 20) * (30^a2) 
      
    }
    
    write.csv(diameter_dbh, file =  paste0("output/diameter_c1_dbh_", diameter_c1_parameters[i,"species"], ".csv"))
    
  }
  
  
  dd <- list.files(path = "output/", pattern = "diameter_c1_dbh_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  diameter_dbh_summary <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                                  dplyr::summarise(mean_diameter_15_5 = mean(dbh_15_ba_5),
                                                                   sd_diameter_15_5 = sd(dbh_15_ba_5),
                                                                   se_diameter_15_5 = se(dbh_15_ba_5),
                                                                   mean_diameter_30_5 = mean(dbh_30_ba_5),
                                                                   sd_diameter_30_5 = sd(dbh_30_ba_5),
                                                                   se_diameter_30_5 = se(dbh_30_ba_5),
                                                                   mean_diameter_15_20 = mean(dbh_15_ba_20),
                                                                   sd_diameter_15_20 = sd(dbh_15_ba_20),
                                                                   se_diameter_15_20 = se(dbh_15_ba_20),
                                                                   mean_diameter_30_20 = mean(dbh_30_ba_20),
                                                                   sd_diameter_30_20 = sd(dbh_30_ba_20),
                                                                   se_diameter_30_20 = se(dbh_30_ba_20))
  
  write.csv(diameter_dbh_summary, file =  "output/diameter_dbh_c1_summary.csv")
  return(diameter_dbh_summary)
  
  
}



get_diameter_c2_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from diameter models (c2)
  dd <- list.files(path = "output/", pattern = "power_diameter_c2_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  diameter_summary <- diameter_resampling_power_c2 %>% dplyr::filter(weighted == "no") %>%
                                                       dplyr::group_by(species) %>%
                                                       dplyr::summarise(nobs = n())
  
  diameter_sp_list <- diameter_summary[diameter_summary$nobs >= 100,]$species
  diameter_resampling_power_c2 <- diameter_resampling_power_c2[diameter_resampling_power_c2$species %in% diameter_sp_list,]
  
  diameter_power <- diameter_resampling_power_c2 %>% dplyr::filter(weighted == "yes") %>%
                                                     dplyr::group_by(species) %>%
                                                     dplyr::filter(!is.na(a1)) %>%
                                                     dplyr::slice_sample(n = 100)
                                                  
  # diameter_power <- diameter_resampling_power_c2 %>% dplyr::filter(weighted == "no") %>%
  #                                                    dplyr::group_by(species) %>%
  #                                                    dplyr::filter(!is.na(a1)) %>%
  #                                                    dplyr::slice_sample(n = 100) %>%
  #                                                    dplyr::mutate(ID_b = X + 300) %>%
  #                                                    dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  # 
  # 
  # diameter_power_b <- diameter_resampling_power_c2 %>% dplyr::filter(weighted == "yes") %>%
  #                                                      dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  # diameter_power_b <- diameter_power_b[diameter_power_b$ID_tofilter %in% diameter_power$ID_species,]
  
  diameter_c2_parameters <- diameter_power %>% dplyr::select(species, a1, a2, comp) %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::mutate(id = c(1:100)) %>%
                                               dplyr::ungroup()
  
  
  write.csv(file = "output/new_diameter_c2_parameters.csv", diameter_c2_parameters)
  return(diameter_c2_parameters)
  
  # diameter_power_results_b <- diameter_power_b %>% dplyr::group_by(species) %>%
  #                                                  dplyr::summarise(mean_a1 = mean(a1),
  #                                                                    sd_a1 = sd(a1),
  #                                                                    min_a1 = quantile(a1, probs = 0.025),
  #                                                                    max_a1 = quantile(a1, probs = 0.975),
  #                                                                    se_a1 = se(a1),
  #                                                                    mean_a2 = mean(a2),
  #                                                                    sd_a2 = sd(a2),
  #                                                                    min_a2 = quantile(a2, probs = 0.025),
  #                                                                    max_a2 = quantile(a2, probs = 0.975),
  #                                                                    se_a2 = se(a2),
  #                                                                    mean_comp = mean(comp),
  #                                                                    sd_comp = sd(comp),
  #                                                                    min_comp = quantile(comp, probs = 0.025),
  #                                                                    max_comp = quantile(comp, probs = 0.975),
  #                                                                    se_comp = se(comp))
  # 
  # diameter_power_results_b <- diameter_power_results_b %>% dplyr::mutate(cv_a1 = abs(sd_a1)/abs(mean_a1),
  #                                                                  cv_a2 = abs(sd_a2)/abs(mean_a2),
  #                                                                  cv_comp = abs(sd_comp)/abs(mean_comp))
  # 
  # par(mfrow = c(1,3))
  # 
  # plot(density(diameter_power_results_b$cv_a1), main = "a1", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(diameter_power_results_b$cv_a2), main = "a2", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(diameter_power_results_b$cv_comp), main = "ba larger trees", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # 
  # 
  # diameter_power_results_c <- diameter_power_results_b %>% dplyr::filter(cv_a1 > 0,
  #                                                                  cv_a1 <= 1,
  #                                                                  cv_a2 > 0,
  #                                                                  cv_a2 <= 1,
  #                                                                  cv_comp > 0,
  #                                                                  cv_comp <= 1) # 23 sp left out of 62
  # 
  # 
  # write.csv(file = "output/diameter_power_parameters_c2.csv", diameter_power_results_b)
  # return(diameter_power_results_b)
  # 
}
  
  
get_diameter_c2_estimates <- function(diameter_c2_parameters) {

  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate diameter +/- se and sd at dbh 30cm
  
  for (i in 1:dim(diameter_c2_parameters)[1]) {
    
    n_repetition = 100
    
    diameter_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5))
    diameter_dbh[,1] <- rep(diameter_c2_parameters[i,"species"], n_repetition)
    names(diameter_dbh) <- c("species", "dbh_15_ba_5", "dbh_15_ba_20", "dbh_30_ba_5", "dbh_30_ba_20")
    
    mean_a1 = diameter_c2_parameters$mean_a1[i]
    sd_a1 = diameter_c2_parameters$sd_a1[i]
    
    mean_a2 = diameter_c2_parameters$mean_a2[i]
    sd_a2 = diameter_c2_parameters$sd_a2[i]
    
    mean_comp = diameter_c2_parameters$mean_comp[i]
    sd_comp = diameter_c2_parameters$sd_comp[i]
    
    for (j in 1:n_repetition) {
      
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      diameter_dbh[j,"dbh_15_ba_5"] = (a1 + comp * 5) * (15^a2) 
      diameter_dbh[j,"dbh_15_ba_20"] = (a1 + comp * 20) * (15^a2) 
      diameter_dbh[j,"dbh_30_ba_5"] = (a1 + comp * 5) * (30^a2) 
      diameter_dbh[j,"dbh_30_ba_20"] = (a1 + comp * 20) * (30^a2) 
      
    }
    
    write.csv(diameter_dbh, file =  paste0("output/diameter_c2_dbh_", diameter_c2_parameters[i,"species"], ".csv"))
    
  }
  
  
  dd <- list.files(path = "output/", pattern = "diameter_c2_dbh_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  diameter_dbh_summary <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                                 dplyr::summarise(mean_diameter_15_5 = mean(dbh_15_ba_5),
                                                                  sd_diameter_15_5 = sd(dbh_15_ba_5),
                                                                  se_diameter_15_5 = se(dbh_15_ba_5),
                                                                  mean_diameter_30_5 = mean(dbh_30_ba_5),
                                                                  sd_diameter_30_5 = sd(dbh_30_ba_5),
                                                                  se_diameter_30_5 = se(dbh_30_ba_5),
                                                                  mean_diameter_15_20 = mean(dbh_15_ba_20),
                                                                  sd_diameter_15_20 = sd(dbh_15_ba_20),
                                                                  se_diameter_15_20 = se(dbh_15_ba_20),
                                                                  mean_diameter_30_20 = mean(dbh_30_ba_20),
                                                                  sd_diameter_30_20 = sd(dbh_30_ba_20),
                                                                  se_diameter_30_20 = se(dbh_30_ba_20))
  
  write.csv(diameter_dbh_summary, file =  "output/diameter_dbh_c2_summary.csv")
  return(diameter_dbh_summary)
  
  
}


