get_mean_ratio_estimates <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_nocomp_mean_rs_")
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
  
  ratio_parameters <- ratio_beta_b %>% dplyr::select(species, a1) %>%
                                       dplyr::group_by(species) %>%
                                       dplyr::mutate(id = c(1:100)) %>%
                                       dplyr::ungroup()
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea", "Salix")
  ratio_mean_parameters <- ratio_parameters[!ratio_parameters$species %in% to_remove,]
  
  write.csv(file = "output/new_ratio_beta_mean_parameters.csv", ratio_mean_parameters)
  return(ratio_beta_mean_parameters)
  
  # ratio_beta_b$a1_b <- exp(ratio_beta_b$a1) / (1 + (exp(ratio_beta_b$a1)))
  # 
  # ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
  #                                          dplyr::summarise(mean_a1 = mean(a1_b),
  #                                                           sd_a1 = sd(a1_b),
  #                                                           min_a1 = quantile(a1_b, probs = 0.025),
  #                                                           max_a1 = quantile(a1_b, probs = 0.975),
  #                                                           se_a1 = se(a1_b)) 
  # 
  # to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea", "Salix")
  # 
  # ratio_beta_results_b <- ratio_beta_results_b[!ratio_beta_results_b$species %in% to_remove,]
  # write.csv(file = "output/ratio_beta_mean_estimates.csv", ratio_beta_results_b)
  # 
  # return(ratio_beta_results_b)
  
}



## Checking values obtained for the mean ratio of tree species

ratio_mean_validation <- function(ratio_data) {
  
  data <- ratio_data
  parameters <- read.csv(file = "output/ratio_beta_mean_parameters.csv")
  
  for (i in 1:dim(parameters)[1]) {
    
    df <- data[data$sp_name %in% parameters[i,"species"],]
    df <- arrange(df, df$x)
    xbis <- c(min(df$x):max(df$x))
    ymean <- parameters[i,"mean_a1"] 
    ymin <- (parameters[i,"mean_a1"]-parameters[i,"sd_a1"]) 
    ymax <- (parameters[i,"mean_a1"]+parameters[i,"sd_a1"]) 
    df_bis <- as.data.frame(cbind(xbis, ymean, ymin, ymax))
    
    h <- ggplot(df, aes(x, y)) + 
      labs(
        title = parameters[i,"species"],
        x = "diameter at breast height (cm)", 
        y = "crown ratio") +
      geom_point(size = 0.1) +
      geom_ribbon(data = df_bis, aes(x = xbis, y = ymean, ymin = ymin, ymax = ymax), fill = "darkseagreen1", alpha = 0.5) +
      geom_line(data = df_bis, mapping = aes(xbis, ymean), colour = "darkseagreen4") +
      theme_classic()
    
    ggsave(path = "figures/ratio_mean/",
           filename = paste("check_",parameters[i,"species"],".pdf"),
           width = 12, height = 8, units = "cm")
  }
  
  dev.off()
  
}



get_ratio_c1_mean_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_c1_mean_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_resampling_beta <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  beta_summary <-   ratio_resampling_beta  %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::filter(!is.na(comp)) %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  ratio_sp_list <- beta_summary[beta_summary$nobs >= 100,]$species
  
  ratio_resampling_beta <- ratio_resampling_beta[ratio_resampling_beta$species %in% ratio_sp_list,]
  
  ratio_beta <- ratio_resampling_beta %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1) & !is.na(comp)) %>%
                                          dplyr::slice_sample(n = 100) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  ratio_beta_b <- ratio_resampling_beta %>% dplyr::filter(weighted == "yes") %>%
                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  ratio_beta_b <- ratio_beta_b[ratio_beta_b$ID_tofilter %in% ratio_beta$ID_species,]
  
  ratio_c1_parameters <- ratio_beta_b %>% dplyr::select(species, a1, comp) %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::mutate(id = c(1:100)) %>%
                                          dplyr::ungroup()
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea", "Salix")
  ratio_mean_c1_parameters <- ratio_c1_parameters[!ratio_c1_parameters$species %in% to_remove,]
  
  write.csv(file = "output/new_ratio_beta_c1_mean_parameters.csv", ratio_mean_c1_parameters)
  return(ratio_mean_c1_parameters)
  
  
  # ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
  #                                          dplyr::summarise(mean_a1 = mean(a1),
  #                                                           sd_a1 = sd(a1),
  #                                                           min_a1 = quantile(a1, probs = 0.025),
  #                                                           max_a1 = quantile(a1, probs = 0.975),
  #                                                           se_a1 = se(a1),
  #                                                           mean_comp = mean(comp),
  #                                                           sd_comp = sd(comp),
  #                                                           min_comp = quantile(comp, probs = 0.025),
  #                                                           max_comp = quantile(comp, probs = 0.975)) 
  # 
  # write.csv(file = "output/ratio_beta_mean_c1_parameters.csv", ratio_beta_results_b)
  # return(ratio_beta_results_b)
  
}




get_ratio_mean_c1_estimates <- function(ratio_mean_c1_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate crown ratio with ba plot = 5 and 20
  
  for (i in 1:dim(ratio_mean_c1_parameters)[1]) {
    
    n_repetition = 100
    
    ratio_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 3))
    ratio_est[,1] <- rep(ratio_mean_c1_parameters[i,"species"], n_repetition)
    names(ratio_est) <- c("species", "ba_5", "ba_20")
    
    mean_a1 = ratio_mean_c1_parameters$mean_a1[i]
    sd_a1 = ratio_mean_c1_parameters$sd_a1[i]
    
    mean_comp = ratio_mean_c1_parameters$mean_comp[i]
    sd_comp = ratio_mean_c1_parameters$sd_comp[i]
    
    
    for (j in 1:n_repetition) {
      
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      ratio_est[j,"ba_5"] = exp(a1 + comp*5) / (1 + (exp(a1 + comp*5)))
      ratio_est[j,"ba_20"] = exp(a1 + comp*20) / (1 + (exp(a1 + comp*20)))
      
      
    }
    
    write.csv(ratio_est, file =  paste0("output/ratio_mean_c1_estimates_", ratio_mean_c1_parameters[i,"species"], ".csv"))
  }
  
  dd <- list.files(path = "output/", pattern = "ratio_mean_c1_estimates_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_dbh_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  ratio_dbh_c1_summary <- ratio_dbh_c1 %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_ratio_5 = mean(ba_5),
                                                            sd_ratio_5 = sd(ba_5),
                                                            se_ratio_5 = se(ba_5),
                                                            mean_ratio_20 = mean(ba_20),
                                                            sd_ratio_20 = sd(ba_20),
                                                            se_ratio_20 = se(ba_20))
  
  write.csv(ratio_dbh_c1_summary, file =  "output/ratio_dbh_mean_c1_summary.csv")
  return(ratio_dbh_c1_summary)
  
}



get_ratio_c2_mean_parameters <- function() {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/", pattern = "beta_ratio_c2_mean_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_resampling_beta <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  beta_summary <-   ratio_resampling_beta  %>% dplyr::filter(weighted == "no") %>%
                                               dplyr::filter(!is.na(comp)) %>%
                                               dplyr::group_by(species) %>%
                                               dplyr::summarise(nobs = n()) 
  
  ratio_sp_list <- beta_summary[beta_summary$nobs >= 100,]$species
  
  ratio_resampling_beta <- ratio_resampling_beta[ratio_resampling_beta$species %in% ratio_sp_list,]
  
  ratio_beta <- ratio_resampling_beta %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1) & !is.na(comp)) %>%
                                          dplyr::slice_sample(n = 100) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
  
  ratio_beta_b <- ratio_resampling_beta %>% dplyr::filter(weighted == "yes") %>%
                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
  ratio_beta_b <- ratio_beta_b[ratio_beta_b$ID_tofilter %in% ratio_beta$ID_species,]
  
  ratio_c2_parameters <- ratio_beta_b %>% dplyr::select(species, a1, comp) %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::mutate(id = c(1:100)) %>%
                                          dplyr::ungroup()
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea", "Salix")
  ratio_mean_c2_parameters <- ratio_c2_parameters[!ratio_c2_parameters$species %in% to_remove,]
  
  write.csv(file = "output/new_ratio_beta_c2_mean_parameters.csv", ratio_mean_c2_parameters)
  return(ratio_mean_c2_parameters)
  
  
  
  ratio_beta_results_b <- ratio_beta_b %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_a1 = mean(a1),
                                                            sd_a1 = sd(a1),
                                                            min_a1 = quantile(a1, probs = 0.025),
                                                            max_a1 = quantile(a1, probs = 0.975),
                                                            se_a1 = se(a1),
                                                            mean_comp = mean(comp),
                                                            sd_comp = sd(comp),
                                                            min_comp = quantile(comp, probs = 0.025),
                                                            max_comp = quantile(comp, probs = 0.975)) 
  
  write.csv(file = "output/ratio_beta_mean_c2_parameters.csv", ratio_beta_results_b)
  return(ratio_beta_results_b)
  
}




get_ratio_mean_c2_estimates <- function(ratio_mean_c2_parameters) {
  
  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
  
  ### 100 resampling to estimate crown ratio at 15 and 30 cm dbh with ba plot = 5 and 20
  
  for (i in 1:dim(ratio_mean_c2_parameters)[1]) {
    
    n_repetition = 100
    
    ratio_est <- as.data.frame(matrix(nrow = n_repetition, ncol = 3))
    ratio_est[,1] <- rep(ratio_mean_c2_parameters[i,"species"], n_repetition)
    names(ratio_est) <- c("species", "ba_5", "ba_20")
    
    mean_a1 = ratio_mean_c2_parameters$mean_a1[i]
    sd_a1 = ratio_mean_c2_parameters$sd_a1[i]
    
    mean_comp = ratio_mean_c2_parameters$mean_comp[i]
    sd_comp = ratio_mean_c2_parameters$sd_comp[i]
    
    
    for (j in 1:n_repetition) {
      
      a1 = rtruncnorm(1, a = mean_a1 - 2*sd_a1, b = mean_a1 + 2*sd_a1, mean = mean_a1, sd = sd_a1)
      comp = rtruncnorm(1, a = mean_comp - 2*sd_comp, b = mean_comp + 2*sd_comp, mean = mean_comp, sd = sd_comp)
      
      ratio_est[j,"ba_5"] = exp(a1 + comp*5) / (1 + (exp(a1 + comp*5)))
      ratio_est[j,"ba_20"] = exp(a1 + comp*20) / (1 + (exp(a1 + comp*20)))
      
      
    }
    
    write.csv(ratio_est, file =  paste0("output/ratio_mean_c2_estimates_", ratio_mean_c2_parameters[i,"species"], ".csv"))
  }
  
  dd <- list.files(path = "output/", pattern = "ratio_mean_c2_estimates_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_dbh_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  ratio_dbh_c2_summary <- ratio_dbh_c2 %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_ratio_5 = mean(ba_5),
                                                           sd_ratio_5 = sd(ba_5),
                                                           se_ratio_5 = se(ba_5),
                                                           mean_ratio_20 = mean(ba_20),
                                                           sd_ratio_20 = sd(ba_20),
                                                           se_ratio_20 = se(ba_20))
                                        
  write.csv(ratio_dbh_c2_summary, file =  "output/ratio_dbh_mean_c2_summary.csv")
  return(ratio_dbh_c2_summary)
  
}





