get_ratio_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_resampling_beta <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  beta_summary <-   ratio_resampling_beta  %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  ratio_sp_list <- beta_summary[beta_summary$nobs >= 100,]$species
  
  ratio_resampling_beta <- ratio_resampling_beta[ratio_resampling_beta$species %in% ratio_sp_list,]

  ratio_beta <- ratio_resampling_beta %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1)) %>%
                                          dplyr::slice_sample(n = 100) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  ratio_beta_b <- ratio_resampling_beta %>% dplyr::filter(weighted == "yes") %>%
                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  ratio_beta_b <- ratio_beta_b[ratio_beta_b$ID_tofilter %in% ratio_beta$ID_species,]
  
  # total RMSE weighted: 1 970.29
  # total RMSE not weighted: 2 079.433
  
  # dbh_effect <- as.data.frame(matrix(nrow = dim(ratio_beta_b)[1], ncol = 1))
  # colnames(dbh_effect) <- "dbh_effect"
  # 
  # 
  # for (i in 1:dim(ratio_beta_b)[1]) {
  # 
  #   if (ratio_beta_b[i,"pr_z"] < 0.05) {
  #     dbh_effect[i,"dbh_effect"] = "yes" } else {
  #     dbh_effect[i,"dbh_effect"] = "no" }
  # 
  # }
  # 
  # ratio_beta_b <- cbind(ratio_beta_b, dbh_effect)
  # 
  # ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
  #                                          dplyr::summarise(mean_a1 = mean(a1),
  #                                                           sd_a1 = sd(a1),
  #                                                           min_a1 = quantile(a1, probs = 0.025),
  #                                                           max_a1 = quantile(a1, probs = 0.975),
  #                                                           se_a1 = se(a1),
  #                                                           mean_a2 = mean(a2),
  #                                                           sd_a2 = sd(a2),
  #                                                           min_a2 = quantile(a2, probs = 0.025),
  #                                                           max_a2 = quantile(a2, probs = 0.975),
  #                                                           se_a2 = se(a2),
  #                                                           sign = sum(dbh_effect == "yes"))
  # 
  # 63 species out of 131 have at least 75 resamplings out of 100 that showed a significant effect of dbh
  #
  # dbh_range <- c(10:40)
  # sp_list <- c("Fraxinus americana", "Pinus ponderosa", "Quercus rubra")
  # df <- ratio_beta_results_b[ratio_beta_results_b$species %in% sp_list,]
  # rownames(ratio_beta_results_b) <- ratio_beta_results_b$species
  # 
  # results <- as.data.frame(matrix(ncol = length(sp_list), nrow = length(dbh_range)))
  # colnames(results) <- sp_list
  # 
  # for (i in 1:length(sp_list)) {
  #   for(j in 1:length(dbh_range)) {
  #     
  #     results[j,i] <- exp(df[i,"mean_a1"] + df[i,"mean_a2"]*dbh_range[j]) / (1 + exp(df[i,"mean_a1"] + df[i,"mean_a2"]*dbh_range[j])) 
  #     
  #   }
  # }
  #   
  # par(mfrow = c(1,1))  
  # plot(results[,1] ~ dbh_range, type = "l", 
  #      ylim = c(0.30, 0.6), 
  #      col = "darkgreen", lwd = 3, las = 1,
  #      ylab = "crown ratio", xlab = "dbh in cm")
  # lines(results[,2] ~ dbh_range, type = "l",
  #       col = "darkblue", lwd = 3)
  # lines(results[,3] ~ dbh_range, type = "l",
  #       col = "darkgrey", lwd = 3)

  ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_a1 = mean(a1),
                                                            sd_a1 = sd(a1),
                                                            min_a1 = quantile(a1, probs = 0.025),
                                                            max_a1 = quantile(a1, probs = 0.975),
                                                            se_a1 = se(a1),
                                                            mean_a2 = mean(a2),
                                                            sd_a2 = sd(a2),
                                                            min_a2 = quantile(a2, probs = 0.025),
                                                            max_a2 = quantile(a2, probs = 0.975),
                                                            se_a2 = se(a2)) # 131 sp
  
  
  
  # ratio_beta_results_b <- ratio_beta_results_b %>% dplyr::mutate(cv_a1 = abs(sd_a1)/abs(mean_a1),
  #                                                                cv_a2 = abs(sd_a2)/abs(mean_a2))
  # 
  # par(mfrow = c(1,3))
  # 
  # plot(density(ratio_beta_results_b$cv_a1), main = "a1", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "", xlim = c(0,10))
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # plot(density(ratio_beta_results_b$cv_a2), main = "a2", las = 1, lwd = 2, col = "darkgreen", ylab = "", xlab = "")
  # abline(v = 0, lwd = 1, lty = 4)
  # abline(v = 0.5, lwd = 1, lty = 4)
  # 
  # ratio_beta_results_c <- ratio_beta_results_b %>% dplyr::filter(cv_a1 > 0,
  #                                                                cv_a1 <= 0.5,
  #                                                                cv_a2 > 0,
  #                                                                cv_a2 <= 0.5) # 71 sp left out of 131
  # 
  # write.csv(file = "output/ratio_beta_parameters.csv", ratio_beta_results_c)
  
  write.csv(file = "output/ratio_beta_parameters.csv", ratio_beta_results_b)
  return(ratio_beta_results_b)
  
}




get_ratio_estimates <- function(ratio_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate crown ratio at 15 and 30 cm dbh
  
  for (i in 1:dim(ratio_parameters)[1]) {
    
    n_repetition = 100
    
    ratio_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 3))
    ratio_est[,1] <- rep(ratio_parameters[i,"species"], n_repetition)
    names(ratio_est) <- c("species", "dbh_15", "dbh_30")
    
    mean_a1 = ratio_parameters$mean_a1[i]
    sd_a1 = ratio_parameters$sd_a1[i]
    
    mean_a2 = ratio_parameters$mean_a2[i]
    sd_a2 = ratio_parameters$sd_a2[i]
    
    
    for (j in 1:n_repetition) {
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      
      ratio_est[j,"dbh_15"] = exp(a1 + a2*15) / (1 + (exp(a1 + a2*15)))
      ratio_est[j,"dbh_30"] = exp(a1 + a2*30) / (1 + (exp(a1 + a2*30)))
      
      }
    
    write.csv(ratio_est, file =  paste0("output/ratio_estimates_nocomp_", ratio_parameters[i,"species"], ".csv"))
  }
  
  
  dd <- list.files(path = "output/", pattern = "ratio_estimates_nocomp_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
 ratio_dbh_nocomp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
 ratio_dbh_summary <- ratio_dbh_nocomp %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_ratio_15 = mean(dbh_15),
                                                            sd_ratio_15 = sd(dbh_15),
                                                            se_ratio_15 = se(dbh_15),
                                                            mean_ratio_30 = mean(dbh_30),
                                                            sd_ratio_30 = sd(dbh_30),
                                                            se_ratio_30 = se(dbh_30))
  
  write.csv(ratio_dbh_summary, file =  "output/ratio_dbh_nocomp_summary.csv")
  return(ratio_dbh_summary)
  
}


## Checking values obtained for the crown ratio according to dbh of tree species

ratio_dbh_validation <- function(ratio_data) {
  
  data <- ratio_data
  parameters <- read.csv(file = "output/ratio_beta_parameters.csv")
  r_comparisons <- as.data.frame(matrix(nrow = dim(parameters)[1], ncol = 5))
  colnames(r_comparisons) <- c("sp", "r_15_obs", "r_15_est", "r_30_obs", "r_30_est")
  
  for (i in 1:dim(parameters)[1]) {
    
    r_comparisons[i,"sp"] <- parameters[i,"species"]
    
    df <- data[data$sp_name %in% parameters[i,"species"],]
    df <- arrange(df, df$x)
    xbis <- c(min(df$x):max(df$x))
    ymean <- exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * xbis) / (1 + (exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * xbis)))
    ymin <- exp((parameters[i,"mean_a1"] - parameters[i,"sd_a1"]) + (parameters[i,"mean_a2"] - parameters[i,"sd_a2"]) * xbis) / (1 + (exp((parameters[i,"mean_a1"] - parameters[i,"sd_a1"]) + (parameters[i,"mean_a2"] - parameters[i,"sd_a2"]) * xbis)))
    ymax <- exp((parameters[i,"mean_a1"] + parameters[i,"sd_a1"]) + (parameters[i,"mean_a2"] + parameters[i,"sd_a2"]) * xbis) / (1 + (exp((parameters[i,"mean_a1"] + parameters[i,"sd_a1"]) + (parameters[i,"mean_a2"] + parameters[i,"sd_a2"]) * xbis)))
    df_bis <- as.data.frame(cbind(xbis, ymean, ymin, ymax))
    
    df <- df %>% dplyr::mutate(abs_x15 = abs(15-x), abs_x30 = abs(30-x))
    obs_15 <- df %>% dplyr::filter(abs_x15 == min(abs_x15))
    obs_30 <- df %>% dplyr::filter(abs_x30 == min(abs_x30))
    
    r_comparisons[i,"r_15_obs"] <- mean(obs_15$y)
    r_comparisons[i,"r_30_obs"] <- mean(obs_30$y)
    r_comparisons[i,"r_15_est"] <- exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * 15) / (1 + (exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * 15)))
    r_comparisons[i,"r_30_est"] <- exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * 30) / (1 + (exp(parameters[i,"mean_a1"] + parameters[i,"mean_a2"] * 30)))
    
    h <- ggplot(df, aes(x, y)) + 
      labs(
        title = parameters[i,"species"],
        x = "diameter at breast height (cm)", 
        y = "crown ratio") +
      geom_point(size = 0.1) +
      geom_ribbon(data = df_bis, aes(x = xbis, y = ymean, ymin = ymin, ymax = ymax), fill = "darkseagreen1", alpha = 0.5) +
      geom_line(data = df_bis, mapping = aes(xbis, ymean), colour = "darkseagreen4") +
      theme_classic()
    
    ggsave(path = "figures/ratio_dbh/",
           filename = paste("check_",parameters[i,"species"],".pdf"),
           width = 12, height = 8, units = "cm")
  }
  dev.off()
  write.csv(file = "output/ratio_comparisons.csv", r_comparisons)
  
  # plot(r_comparisons$r_15_obs, r_comparisons$r_15_est, 
  #      pch = 16, las = 1, type = "p",
  #      xlab = "observed crown ratio", 
  #      ylab = "estimated crown ratio",
  #      xlim = c(0,1), ylim = c(0,1),
  #      col = "darkseagreen2")
  # 
  # points(r_comparisons$r_30_obs, r_comparisons$r_30_est,
  #        pch = 16, col = "darkseagreen4")
  # lines(c(0,1), c(0,1), lwd = 3, col = "darkseagreen4")
  
  
}




get_ratio_c1_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_c1_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_resampling_beta <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  beta_summary <-   ratio_resampling_beta  %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  ratio_sp_list <- beta_summary[beta_summary$nobs >= 100,]$species
  
  ratio_resampling_beta <- ratio_resampling_beta[ratio_resampling_beta$species %in% ratio_sp_list,]
  
  ratio_beta <- ratio_resampling_beta %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1)) %>%
                                          dplyr::slice_sample(n = 100) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  ratio_beta_b <- ratio_resampling_beta %>% dplyr::filter(weighted == "yes") %>%
                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  ratio_beta_b <- ratio_beta_b[ratio_beta_b$ID_tofilter %in% ratio_beta$ID_species,]
  
  ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_a1 = mean(a1),
                                                            sd_a1 = sd(a1),
                                                            min_a1 = quantile(a1, probs = 0.025),
                                                            max_a1 = quantile(a1, probs = 0.975),
                                                            se_a1 = se(a1),
                                                            mean_a2 = mean(a2),
                                                            sd_a2 = sd(a2),
                                                            min_a2 = quantile(a2, probs = 0.025),
                                                            max_a2 = quantile(a2, probs = 0.975),
                                                            se_a2 = se(a2),
                                                            mean_comp = mean(comp),
                                                            sd_comp = sd(comp),
                                                            min_comp = quantile(comp, probs = 0.025),
                                                            max_comp = quantile(comp, probs = 0.975)) # 127 sp
 
   write.csv(file = "output/ratio_beta_c1_parameters.csv", ratio_beta_results_b)
   return(ratio_beta_results_b)
   
   
   
   # dbh_effect <- as.data.frame(matrix(nrow = dim(ratio_beta_b)[1], ncol = 1))
   # colnames(dbh_effect) <- "dbh_effect"
   # 
   # for (i in 1:dim(ratio_beta_b)[1]) {
   # 
   #   if (ratio_beta_b[i,"pr_z"] < 0.05) {
   #     dbh_effect[i,"dbh_effect"] = "yes" } else {
   #     dbh_effect[i,"dbh_effect"] = "no" }
   # 
   # }
   # 
   # ratio_beta_b <- cbind(ratio_beta_b, dbh_effect)
   # 
   # ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
   #                                          dplyr::summarise(mean_a1 = mean(a1),
   #                                                           sd_a1 = sd(a1),
   #                                                           min_a1 = quantile(a1, probs = 0.025),
   #                                                           max_a1 = quantile(a1, probs = 0.975),
   #                                                           se_a1 = se(a1),
   #                                                           mean_a2 = mean(a2),
   #                                                           sd_a2 = sd(a2),
   #                                                           min_a2 = quantile(a2, probs = 0.025),
   #                                                           max_a2 = quantile(a2, probs = 0.975),
   #                                                           se_a2 = se(a2),
   #                                                           mean_comp = mean(comp),
   #                                                           sd_comp = sd(comp),
   #                                                           min_comp = quantile(comp, probs = 0.025),
   #                                                           max_comp = quantile(comp, probs = 0.975),
   #                                                           sign = sum(dbh_effect == "yes"))
   # 
   # df <- ratio_beta_results_b[ratio_beta_results_b$sign >= 75,]
   # 
   # # 73 species out of 127 have at least 75 resamplings out of 100 that showed a significant effect of dbh

  
}






get_ratio_c1_estimates <- function(ratio_c1_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate crown ratio at 15 and 30 cm dbh with ba plot = 5 and 20
  
  for (i in 1:dim(ratio_c1_parameters)[1]) {
    
    n_repetition = 100
    
    ratio_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 5))
    ratio_est[,1] <- rep(ratio_c1_parameters[i,"species"], n_repetition)
    names(ratio_est) <- c("species", "dbh_15_ba_5", "dbh_15_ba_20", "dbh_30_ba_5", "dbh_30_ba_20")
    
    mean_a1 = ratio_c1_parameters$mean_a1[i]
    sd_a1 = ratio_c1_parameters$sd_a1[i]
    
    mean_a2 = ratio_c1_parameters$mean_a2[i]
    sd_a2 = ratio_c1_parameters$sd_a2[i]
    
    mean_comp = ratio_c1_parameters$mean_comp[i]
    sd_comp = ratio_c1_parameters$sd_comp[i]
    
    
    for (j in 1:n_repetition) {
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      ratio_est[j,"dbh_15_ba_5"] = exp(a1 + comp*5 + a2*15) / (1 + (exp(a1 + comp*5 + a2*15)))
      ratio_est[j,"dbh_15_ba_20"] = exp(a1 + comp*20 + a2*15) / (1 + (exp(a1 + comp*20 + a2*15)))
      ratio_est[j,"dbh_30_ba_5"] = exp(a1 + comp*5 + a2*30) / (1 + (exp(a1 + comp*5 + a2*30)))
      ratio_est[j,"dbh_30_ba_20"] = exp(a1 + comp*20 + a2*30) / (1 + (exp(a1 + comp*20 + a2*30)))
  
      
    }
    
    write.csv(ratio_est, file =  paste0("output/ratio_c1_estimates_", ratio_c1_parameters[i,"species"], ".csv"))
  }
  
  dd <- list.files(path = "output/", pattern = "ratio_c1_estimates_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_dbh_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  ratio_dbh_c1_summary <- ratio_dbh_c1 %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_ratio_15_5 = mean(dbh_15_ba_5),
                                                            sd_ratio_15_5 = sd(dbh_15_ba_5),
                                                            se_ratio_15_5 = se(dbh_15_ba_5),
                                                            mean_ratio_30_5 = mean(dbh_30_ba_5),
                                                            sd_ratio_30_5 = sd(dbh_30_ba_5),
                                                            se_ratio_30_5 = se(dbh_30_ba_5),
                                                            mean_ratio_15_20 = mean(dbh_15_ba_20),
                                                            sd_ratio_15_20 = sd(dbh_15_ba_20),
                                                            se_ratio_15_20 = se(dbh_15_ba_20),
                                                            mean_ratio_30_20 = mean(dbh_30_ba_20),
                                                            sd_ratio_30_20 = sd(dbh_30_ba_20),
                                                            se_ratio_30_20 = se(dbh_30_ba_20))
                                          
  write.csv(ratio_dbh_c1_summary, file =  "output/ratio_dbh_c1_summary.csv")
  return(ratio_dbh_c1_summary)
  
}







get_ratio_c2_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_c2_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_resampling_beta <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  beta_summary <-   ratio_resampling_beta  %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  ratio_sp_list <- beta_summary[beta_summary$nobs >= 100,]$species
  
  ratio_resampling_beta <- ratio_resampling_beta[ratio_resampling_beta$species %in% ratio_sp_list,]
  
  ratio_beta <- ratio_resampling_beta %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1)) %>%
                                          dplyr::slice_sample(n = 100) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  ratio_beta_b <- ratio_resampling_beta %>% dplyr::filter(weighted == "yes") %>%
                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  ratio_beta_b <- ratio_beta_b[ratio_beta_b$ID_tofilter %in% ratio_beta$ID_species,]
  
  ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_a1 = mean(a1),
                                                            sd_a1 = sd(a1),
                                                            min_a1 = quantile(a1, probs = 0.025),
                                                            max_a1 = quantile(a1, probs = 0.975),
                                                            se_a1 = se(a1),
                                                            mean_a2 = mean(a2),
                                                            sd_a2 = sd(a2),
                                                            min_a2 = quantile(a2, probs = 0.025),
                                                            max_a2 = quantile(a2, probs = 0.975),
                                                            se_a2 = se(a2),
                                                            mean_comp = mean(comp),
                                                            sd_comp = sd(comp),
                                                            min_comp = quantile(comp, probs = 0.025),
                                                            max_comp = quantile(comp, probs = 0.975)) # 65 sp
  
  write.csv(file = "output/ratio_beta_c2_parameters.csv", ratio_beta_results_b)
  return(ratio_beta_results_b)
  
  
  
  # dbh_effect <- as.data.frame(matrix(nrow = dim(ratio_beta_b)[1], ncol = 1))
  # colnames(dbh_effect) <- "dbh_effect"
  # 
  # for (i in 1:dim(ratio_beta_b)[1]) {
  # 
  #   if (ratio_beta_b[i,"pr_z"] < 0.05) {
  #     dbh_effect[i,"dbh_effect"] = "yes" } else {
  #     dbh_effect[i,"dbh_effect"] = "no" }
  # 
  # }
  # 
  # ratio_beta_b <- cbind(ratio_beta_b, dbh_effect)
  # 
  # ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
  #                                          dplyr::summarise(mean_a1 = mean(a1),
  #                                                           sd_a1 = sd(a1),
  #                                                           min_a1 = quantile(a1, probs = 0.025),
  #                                                           max_a1 = quantile(a1, probs = 0.975),
  #                                                           se_a1 = se(a1),
  #                                                           mean_a2 = mean(a2),
  #                                                           sd_a2 = sd(a2),
  #                                                           min_a2 = quantile(a2, probs = 0.025),
  #                                                           max_a2 = quantile(a2, probs = 0.975),
  #                                                           se_a2 = se(a2),
  #                                                           mean_comp = mean(comp),
  #                                                           sd_comp = sd(comp),
  #                                                           min_comp = quantile(comp, probs = 0.025),
  #                                                           max_comp = quantile(comp, probs = 0.975),
  #                                                           sign = sum(dbh_effect == "yes"))
  # 
  # df <- ratio_beta_results_b[ratio_beta_results_b$sign >= 75,]
  # 
  # # 29 species out of 65 have at least 75 resamplings out of 100 that showed a significant effect of dbh
  
  
}



get_ratio_c2_estimates <- function(ratio_c2_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate crown ratio at 15 and 30 cm dbh with ba larger = 5 and 20
  
  for (i in 1:dim(ratio_c2_parameters)[1]) {
    
    n_repetition = 100
    
    ratio_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 5))
    ratio_est[,1] <- rep(ratio_c2_parameters[i,"species"], n_repetition)
    names(ratio_est) <- c("species", "dbh_15_ba_5", "dbh_15_ba_20", "dbh_30_ba_5", "dbh_30_ba_20")
    
    mean_a1 = ratio_c2_parameters$mean_a1[i]
    sd_a1 = ratio_c2_parameters$sd_a1[i]
    
    mean_a2 = ratio_c2_parameters$mean_a2[i]
    sd_a2 = ratio_c2_parameters$sd_a2[i]
    
    mean_comp = ratio_c2_parameters$mean_comp[i]
    sd_comp = ratio_c2_parameters$sd_comp[i]
    
    
    for (j in 1:n_repetition) {
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      a2 = rtruncnorm(1, a = mean_a2 - 2*sd_a2, b = mean_a2 + 2*sd_a2, mean = mean_a2, sd = sd_a2)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      ratio_est[j,"dbh_15_ba_5"] = exp(a1 + comp*5 + a2*15) / (1 + (exp(a1 + comp*5 + a2*15)))
      ratio_est[j,"dbh_15_ba_20"] = exp(a1 + comp*20 + a2*15) / (1 + (exp(a1 + comp*20 + a2*15)))
      ratio_est[j,"dbh_30_ba_5"] = exp(a1 + comp*5 + a2*30) / (1 + (exp(a1 + comp*5 + a2*30)))
      ratio_est[j,"dbh_30_ba_20"] = exp(a1 + comp*20 + a2*30) / (1 + (exp(a1 + comp*20 + a2*30)))
      
      
    }
    
    write.csv(ratio_est, file =  paste0("output/ratio_c2_estimates_", ratio_c2_parameters[i,"species"], ".csv"))
  }
  
  dd <- list.files(path = "output/", pattern = "ratio_c2_estimates_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_dbh_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  ratio_dbh_c2_summary <- ratio_dbh_c2 %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_ratio_15_5 = mean(dbh_15_ba_5),
                                                            sd_ratio_15_5 = sd(dbh_15_ba_5),
                                                            se_ratio_15_5 = se(dbh_15_ba_5),
                                                            mean_ratio_30_5 = mean(dbh_30_ba_5),
                                                            sd_ratio_30_5 = sd(dbh_30_ba_5),
                                                            se_ratio_30_5 = se(dbh_30_ba_5),
                                                            mean_ratio_15_20 = mean(dbh_15_ba_20),
                                                            sd_ratio_15_20 = sd(dbh_15_ba_20),
                                                            se_ratio_15_20 = se(dbh_15_ba_20),
                                                            mean_ratio_30_20 = mean(dbh_30_ba_20),
                                                            sd_ratio_30_20 = sd(dbh_30_ba_20),
                                                            se_ratio_30_20 = se(dbh_30_ba_20))
                                          
  write.csv(ratio_dbh_c2_summary, file =  "output/ratio_dbh_c2_summary.csv")
  return(ratio_dbh_c2_summary)
  
}















